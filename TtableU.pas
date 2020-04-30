unit TtableU;

{$DEFINE LRU}

interface

uses SysUtils, Classes;

type
  etblError = class(Exception)
  private
    FErrorCode: String;
  public
    constructor Create(aErrorCode: String);
    property ErrorCode: String read FErrorCode;
  end;

  BufferType = array [0 .. 10230] of byte;

  {$IFDEF LRU}

  basicRow = record
    rowId: int64;
    dirty: Boolean;
    buffer: BufferType;
  end;

const
  LRUstackSize = 50; // should be 50 just to test 2

type
  LRUStackrecord = record
    Entries: Integer;
    Entry: array [0 .. LRUstackSize-1] of basicRow;
  end;

  {$ENDIF}

type

  tblStructure = record
    tblColumns: array of record
      colName: string;
      colType: string;
      charSize: int64;
      allowcolNull: boolean;
      colOffset: integer;
    end;
  end;

  TTableClass = class(TFileStream)
  private
    {$IFDEF LRU}
    LRUstackBuffers: LRUStackrecord;
    {$ENDIF}
    FtableName: string;
    FtblData: tblStructure;
    FoffsetHead: word;
    FrecordLen: word;
    fsize: int64;

    procedure InsertLRUBuffer( rowId: int64; buffer: bufferType );

    procedure insertBuffer( rowId: int64; row: array of variant );

    procedure loadLRUBuffer( rowId: int64; out buffer: bufferType );

  public

    constructor Create(const tableName: String;
        isOpen: Boolean;
        const colsName: array of string;
        const colsType: array of string;
        const allowcolsNull: array of boolean);
      overload;

    constructor Create(const tableName: String;
        isOpen: Boolean;
        const colsName: array of string;
        const colsType: array of string);
      overload;

    procedure insertRow(row: array of variant);
      virtual;

    function returnRow(rowId: int64;
        out row: array of variant): boolean;
      virtual;

    procedure putRow(rowId: int64; row: array of variant);
      virtual;

    procedure deleteRow(rowId: int64);
      virtual;

    function existRow(rowId: int64): Boolean;
      virtual;

//    procedure putValueByColumnName(rowId: int64; columnName: string, value: variant);

    function getValueByColumnName(rowId: int64; columnName: string): variant;
      virtual;

    function emptyTable: boolean;
      virtual;

    function firstRow: int64;
      virtual;

    function lastRow: int64;
      virtual;

    destructor Destroy;
      override;

  end;

var
  PextendedFieldValue: Pextended;
  PcurrencyFieldValue: PCurrency;
  PstringFieldValue: PString;
  stFieldValue: array [0..20] of string[255];

implementation

uses variants;

constructor etblError.Create(aErrorCode: string);
begin
  FErrorCode := aErrorCode;
  inherited Create(aErrorCode);
end;

constructor TTableClass.create(const tableName: String;
  isOpen: Boolean;
  const colsName: array of string;
  const colsType: array of string;
  const allowcolsNull: array of boolean);
var
  I: Integer;
  Params: word;
  stType: string;
  Origin: word;
  Offset: int64;
begin
  FtableName := tableName;
  setLength(FtblData.tblColumns, Length(colsName));
  FoffsetHead := 0;
  {$IFDEF LRU}
  LRUStackBuffers.Entries := 0;
  {$ENDIF}
  FrecordLen := Sizeof(boolean);
  for I := 0 to Length(colsName) - 1 do
    begin
      FtblData.tblColumns[I].colName := colsName[I];
      FtblData.tblColumns[I].allowcolNull := allowcolsNull[I];
      FtblData.tblColumns[I].colOffset := FrecordLen;
      if allowcolsNull[I] then FrecordLen := FrecordLen + Sizeof(boolean);
      if pos('[',colsType[I]) = 0 then
        FtblData.tblColumns[I].colType := trim(colsType[I]) else
        FtblData.tblColumns[I].colType := trim(copy(colsType[I],1,pos('[',colsType[i])-1));
      stType := colsType[I];
      if (pos('STRING',stType) <> 0) then
        begin
          if pos('[',stType) = 0 then
            FtblData.tblColumns[I].charSize := 255 else
            FtblData.tblColumns[I].charSize := strToIntDef(copy(stType,pos('[',stType)+1,pos(']',stType)-pos('[',stType)-1),255);
          FrecordLen := FrecordLen + FtblData.tblColumns[I].charSize + 1;
        end;
      if (stType = 'BOOLEAN') then
        FrecordLen := FrecordLen + Sizeof(boolean);
      if (stType = 'SINGLE') then
        FrecordLen := FrecordLen + Sizeof(single);
      if (stType = 'DOUBLE') then
        FrecordLen := FrecordLen + Sizeof(double);
      if (stType = 'EXTENDED') then
        FrecordLen := FrecordLen + Sizeof(extended);
      if (stType = 'CURRENCY') then
        FrecordLen := FrecordLen + Sizeof(currency);
      if (stType = 'TDATETIME') then
        FrecordLen := FrecordLen + Sizeof(tdatetime);
      if (stType = 'INTEGER') then
        FrecordLen := FrecordLen + Sizeof(Integer);
      if (stType = 'SMALLINT') then
        FrecordLen := FrecordLen + Sizeof(smallInt);
      if (stType = 'INT64') then
        FrecordLen := FrecordLen + Sizeof(Int64);
    end;
  if isOpen then
    Params := fmOpenReadWrite or fmShareDenyNone else
    Params := fmCreate or fmShareDenyNone;
  inherited Create(tableName + '.tbl', Params);
  Offset := 0;
  Origin := soFromEnd;
  Seek(Offset, Origin);
  fsize := ((Position - FoffsetHead) div FrecordLen);
end;

constructor TTableClass.create(const tableName: String;
  isOpen: Boolean;
  const colsName: array of string;
  const colsType: array of string);
var
  colsNull: array of boolean;
  I: Integer;
begin
  setLength(colsNull,length(colsName));
  for I := 0 to length(colsName) - 1 do colsNull[i] := false;
  create(tableName,isOpen,colsName,colsType,colsNull);
end;

procedure TTableClass.InsertLRUBuffer(rowId: int64; buffer: bufferType);
var
  {$IFDEF LRU}
  I: Integer;
  found: boolean;
  J: Integer;
  {$ENDIF}
  Origin: word;
  Offset: int64;
begin
  {$IFDEF LRU}
  found := false;
  for I := 0 to LRUstackBuffers.Entries - 1 do
    begin
      if LRUstackBuffers.Entry[I].rowId = rowId then
        begin
          found := True;
          break;
        end;
    end;
  if found then
    begin
      for J := I-1 downto 0 do
        LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
    end else
    begin
      if LRUstackBuffers.Entries < LRUstackSize then
        begin
          for J := LRUstackBuffers.Entries-1 downto 0 do
            LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
          Inc(LRUstackBuffers.Entries);
        end else
        begin
          for I := 0 to LRUstackBuffers.Entries - 1 do
            if not(LRUstackBuffers.Entry[I].dirty) then
              begin
                found := True;
                break;
              end;
          if found then
            begin
              for J := I-1 downto 0 do
                LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
            end else
            begin
              Offset := (LRUstackBuffers.Entry[LRUstackBuffers.Entries-1].rowId-1) * FrecordLen + FoffsetHead;
              Origin := soFromBeginning;
              Seek(Offset,Origin);
              WriteBuffer(LRUstackBuffers.Entry[LRUstackBuffers.Entries-1].buffer,FrecordLen);
              for J := LRUstackBuffers.Entries-2  downto 0 do
                LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
            end;
        end;
    end;
  LRUstackBuffers.Entry[0].rowId := rowId;
  LRUstackBuffers.Entry[0].dirty := true;
  LRUstackBuffers.Entry[0].buffer := buffer;
  {$ELSE}
  Offset := (rowId-1) * FrecordLen + FoffsetHead;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  WriteBuffer(buffer,FrecordLen);
  {$ENDIF}
end;

procedure TTableClass.LoadLRUBuffer(rowId: int64; out buffer: bufferType);
var
  {$IFDEF LRU}
  I: Integer;
  found: boolean;
  J: Integer;
  dirty: boolean;
  {$ENDIF}
  Origin: word;
  Offset: int64;
begin
  {$IFDEF LRU}
  found := false;
  for I := 0 to LRUstackBuffers.Entries - 1 do
    begin
      if LRUstackBuffers.Entry[I].rowId = rowId then
        begin
          found := True;
          break;
        end;
    end;
  if found then
    begin
      buffer := LRUstackBuffers.Entry[I].buffer;
      dirty := LRUstackBuffers.Entry[I].dirty;
      for J := I-1 downto 0 do
        LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
    end else
    begin
      Offset := (rowId-1) * FrecordLen + FoffsetHead;
      Origin := soFromBeginning;
      Seek(Offset,Origin);
      ReadBuffer(buffer,FrecordLen);
      dirty := false;
      if LRUstackBuffers.Entries < LRUstackSize then
        begin
          for J := LRUstackBuffers.Entries-1 downto 0 do
            LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
          Inc(LRUstackBuffers.Entries);
        end else
        begin
          for I := 0 to LRUstackBuffers.Entries - 1 do
            if not(LRUstackBuffers.Entry[I].dirty) then
              begin
                found := True;
                break;
              end;
          if found then
            begin
              for J := I-1 downto 0 do
                LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
            end else
            begin
              Offset := (LRUstackBuffers.Entry[LRUstackBuffers.Entries-1].rowId-1) * FrecordLen + FoffsetHead;
              Origin := soFromBeginning;
              Seek(Offset,Origin);
              WriteBuffer(LRUstackBuffers.Entry[LRUstackBuffers.Entries-1].buffer,FrecordLen);
              for J := LRUstackBuffers.Entries-2  downto 0 do
                LRUstackBuffers.Entry[J+1] := LRUstackBuffers.Entry[J];
            end;
        end;
    end;
  LRUstackBuffers.Entry[0].rowId := rowId;
  LRUstackBuffers.Entry[0].dirty := dirty;
  LRUstackBuffers.Entry[0].buffer := buffer;
  {$ELSE}
  Offset := (rowId-1) * FrecordLen + FoffsetHead;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  ReadBuffer(buffer,FrecordLen);
  {$ENDIF}
end;


(*
  Every row has a delete option so existRow return false if it is a deleted row
  By Default, a column can receive Null Values
  Null Value preceed a Column Value to indicate if it is null
  To do: when insert reuse the deleted rows
         compact table not an option because btree could exist and use row numbers
*)

procedure TTableClass.insertBuffer(rowId: int64; row: array of variant);
var
  FdeletedRow: Boolean = false;
  buffer: bufferType;
  I, index, j: Integer;
  stSize: byte;
  TmpString: string = '';
  stType: string;
  colNull: Boolean = false;
  boolvalue: boolean = false;
  smallintvalue: smallint = 0;
  intvalue: integer = 0;
  int64value: int64 = 0;
  singlevalue: single = 0.0;
  doublevalue: double = 0.0;
  extendedvalue: extended = 0.0;
  currencyvalue: currency = 0.0;
  tdatetimevalue: tdatetime = 0.0;
begin
  FillChar(buffer, Sizeof(buffer), 0);
  index := 0;
  Move(FdeletedRow, buffer[Index], Sizeof(Boolean));
  Inc(Index, Sizeof(Boolean));
  for I := 0 to Length(row) - 1 do
    begin
      colNull := false;
      if FtblData.tblColumns[I].allowcolNull then
        begin
          colNull := row[i] = null;
          Move(colNull, buffer[Index], Sizeof(Boolean));
          Inc(Index, Sizeof(Boolean));
        end;
      stType := FtblData.tblColumns[i].colType;
      if (pos('STRING',stType) <> 0) then
        begin
          TmpString := row[i];
          stSize := FtblData.tblColumns[I].charSize;
          if stSize = 0 then stSize := 255;
          if Length(TmpString) > stSize then
            TmpString := Copy(TmpString, 1, stSize);
          buffer[Index] := Length(TmpString);
          for j := 1 to Length(TmpString) do
            buffer[Index + j] := ord(TmpString[j]);
          Inc(Index, stSize + 1)
        end;
      if (stType = 'BOOLEAN') then
        begin
          boolvalue := row[i];
          Move(boolvalue, buffer[Index], Sizeof(Boolean));
          index := index + Sizeof(Boolean);
        end;
      if (stType = 'SINGLE') then
        begin
          singlevalue := row[i];
          Move(singlevalue, buffer[Index], Sizeof(single));
          index := index + Sizeof(single);
        end;
      if (stType = 'DOUBLE') then
        begin
          doublevalue := row[i];
          Move(doublevalue, buffer[Index], Sizeof(double));
          index := index + Sizeof(double);
        end;
      if (stType = 'EXTENDED') then
        begin
          extendedvalue := row[i];
          Move(extendedvalue, buffer[Index], Sizeof(Extended));
          index := index + Sizeof(Extended);
        end;
      if (stType = 'CURRENCY') then
        begin
          currencyvalue := row[i];
          Move(currencyvalue, buffer[Index], Sizeof(currency));
          index := index + Sizeof(currency);
        end;
      if (stType = 'TDATETIME') then
        begin
          tdatetimevalue := row[i];
          Move(tdatetimevalue, buffer[Index], Sizeof(tdatetime));
          index := index + Sizeof(tdatetime);
        end;
      if (stType = 'SMALLINT') then
        begin
          smallintvalue := row[i];
          Move(smallintvalue,buffer[index],sizeof(smallInt));
          index := index + Sizeof(smallInt);
        end;
      if (stType = 'INTEGER') then
        begin
          intvalue := row[i];
          Move(intvalue,buffer[index],sizeof(Integer));
          index := index + Sizeof(Integer);
        end;
      if (stType = 'INT64') then
        begin
          int64value := row[i];
          Move(int64value,buffer[index],sizeof(Int64));
          index := index + Sizeof(Int64);
        end;
    end;
  insertLRUbuffer(rowId,buffer);
end;


procedure TTableClass.insertRow(row: array of variant);
begin
  fsize := fsize + 1;
  insertBuffer(fsize, row);
end;

function TTableClass.returnRow(rowId: int64;
  out row: array of variant): boolean;
var
  buffer: bufferType;
  fdelete: boolean;
  index: Integer;
  i, j: integer;
  stType: string;
  colNull: Boolean = false;
  booleanresult: boolean = false;
  smallintresult: smallint = 0;
  intresult: integer = 0;
  int64result: int64 = 0;
  singleresult: single = 0.0;
  doubleresult: double = 0.0;
  extendedresult: extended = 0.0;
  currencyresult: currency = 0.0;
  tdatetimeresult: tdatetime = 0.0;
  stresult: string = '';
begin
  result := false;
  if (rowId <= 0 ) or (rowId > fsize) then exit;
  loadLRUBuffer(rowId,buffer);
  Move(buffer[0],fdelete,sizeof(boolean));
  if fdelete then exit;
  result := true;
  index := Sizeof(Boolean);
////  setlength(row,Length(FtblData.tblColumns));
  for i := 0 to Length(FtblData.tblColumns) - 1 do
    begin
      if FtblData.tblColumns[I].allowcolNull then
        begin
          Move(buffer[index],colNull,sizeof(boolean));
          Inc(index,sizeOf(boolean));
          if colNull then row[i] := null;
        end;
      stType := FtblData.tblColumns[i].colType;
      if (pos('STRING',stType) <> 0) then
        begin
          stresult := '';
          for j := 1 to buffer[Index] do
            stresult := stresult + chr(Buffer[Index+j]);
          index := index + FtblData.tblColumns[i].charSize + 1;
          row[I]:= stresult
        end;
      if (stType = 'BOOLEAN') then
        begin
          Move(buffer[index],booleanresult,sizeof(boolean));
          index := index + Sizeof(boolean);
          row[I] := booleanresult;
        end;
      if (stType = 'SINGLE') then
        begin
          Move(buffer[index],singleresult,sizeof(single));
          index := index + Sizeof(single);
          row[I] := singleresult;
        end;
      if (stType = 'DOUBLE') then
        begin
          Move(buffer[index],doubleresult,sizeof(double));
          index := index + Sizeof(double);
          row[I] := doubleresult;
        end;
      if (stType = 'EXTENDED') then
        begin
          Move(buffer[index],extendedresult,sizeof(extended));
          index := index + Sizeof(extended);
          row[I] := extendedresult;
        end;
      if (stType = 'CURRENCY') then
        begin
          Move(buffer[index],currencyresult,sizeof(currency));
          index := index + Sizeof(currency);
          row[I] := currencyresult;
        end;
      if (stType = 'TDATETIME') then
        begin
          Move(buffer[index],tdatetimeresult,sizeof(tdatetime));
          index := index + Sizeof(tdatetime);
          row[I] := tdatetimeresult;
        end;
      if (stType = 'SMALLINT') then
        begin
          Move(buffer[index],smallintresult,sizeof(smallint));
          index := index + Sizeof(smallint);
          row[I] := smallintresult;
        end;
      if (stType = 'INTEGER') then
        begin
          Move(buffer[index],intresult,sizeof(integer));
          index := index + Sizeof(integer);
          row[I] := intresult;
        end;
      if (stType = 'INT64') then
        begin
          Move(buffer[index],int64result,sizeof(int64));
          index := index + Sizeof(int64);
          row[I] := int64result;
        end;
    end;
end;

function TTableClass.existRow(rowId: int64): Boolean;
var
  buffer: bufferType;
  fdelete: boolean = false;
begin
  result := false;
  if (rowId < 1 ) or (rowId > fsize) then exit;
  loadLRUBuffer(rowId,buffer);
  Move(buffer[0],fdelete,sizeof(boolean));
  result := not fdelete
end;

function TTableClass.getValueByColumnName(rowId: int64; columnName: string): variant;
var
  buffer: bufferType;
  index: Integer;
  I, j: integer;
  stType: string;
  stresult: string;
  intresult: integer;
  booleanresult: boolean = false;
  extendedresult: extended = 0.0;
  int64result: int64 = 0;
  colNull: boolean= false;
begin
  result := null;
  if (rowId <= 0) or (rowId > fsize) then exit;
  loadLRUBuffer(rowId,buffer);
  for i := 0 to Length(FtblData.tblColumns) - 1 do
    if lowerCase(columnName) = lowerCase(FtblData.tblColumns[i].colName) then
      begin
        index := FtblData.tblColumns[i].colOffset;
        if FtblData.tblColumns[i].allowcolNull then
          begin
            Move(buffer[index],colNull,sizeof(boolean));
            if colNull then exit;
          end;
        stType := FtblData.tblColumns[i].colType;
        if (pos('STRING',stType) <> 0) then
          begin
            stresult := '';
            for j := 1 to buffer[Index] do
              stresult := stresult + chr(Buffer[Index+j]);
            result := stresult;
          end;
        if (stType = 'BOOLEAN') then
          begin
            Move(buffer[index],booleanresult,sizeof(boolean));
            result := booleanresult;
          end;
        if (stType = 'SINGLE') or (stType = 'DOUBLE') or
           (stType = 'EXTENDED') or (stType = 'CURRENCY') or
           (stType = 'TDATETIME') then
          begin
            Move(buffer[index],extendedresult,sizeof(extended));
            result := extendedresult;
          end;
        if (stType = 'INTEGER')or (stType = 'SMALLINT') then
          begin
            Move(buffer[index],intresult,sizeof(integer));
            result := intresult;
          end;
        if (stType = 'INT64') then
          begin
            Move(buffer[index],int64result,sizeof(int64));
            result := int64result;
          end;
        break;
      end
end;

procedure TTableClass.putRow(rowId: int64; row: array of variant);
begin
  insertBuffer(rowId, row);
end;

procedure TTableClass.deleteRow(rowId: int64);
var
  buffer: bufferType;
  FDeletedRow: boolean;
begin
  loadLRUbuffer(rowId,buffer);
  FdeletedRow := true;
  Move(FdeletedRow, buffer[0], Sizeof(Boolean));
  insertLRUBuffer(rowId, buffer)
end;

function TTableClass.emptyTable: boolean;
var
  I: Int64;
begin
  result := true;
  if ( fsize = 0 ) or ( firstRow = 0 ) then exit;
  for I := firstRow to lastRow do
    if existRow(I) then
      begin
        result := false;
        break;
      end;
end;

function TTableClass.firstRow: int64;
var
  I: Int64;
begin
  if fsize = 0 then result := 0
   else
     for I := 1 to fsize do
       if existRow(I) then
         begin
           result := I;
           exit;
         end;
  result := 0;
end;

function TTableClass.lastRow: int64;
var
  I: Int64;
begin
  Result := fsize;
  for I := fsize downto 1 do
    if existRow(I) then
      begin
        result := I;
        exit;
      end;
  result := 0;
end;

destructor TTableClass.Destroy;
{$IFDEF LRU}
var
  i: LongInt;
  Offset: LongInt;
  Origin: Word;
  {$ENDIF}
begin
 {$IFDEF LRU}
  for I := 0 to LRUstackBuffers.Entries - 1 do
    if LRUstackBuffers.Entry[I].dirty then
      begin
        Offset := (LRUstackBuffers.Entry[I].rowId-1) * FrecordLen + FoffsetHead;
        Origin := soFromBeginning;
        Seek(Offset,Origin);
        WriteBuffer(LRUstackBuffers.Entry[I].buffer,FrecordLen);
      end;
 {$ENDIF}
  inherited Destroy;
end;

end.
