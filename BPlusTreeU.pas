unit BPlusTreeU;

{$DEFINE mLRU}

{$DEFINE RDuplicates}


interface

uses SysUtils, Classes;

(* In BPlusTree, normally the table itself is inserted in the leaf node.
   This is why there is LeafPageSize and NonLeafPageSize.
   Even if that are both set to PageSize, they are treatted differently,
   because there is always the case of leaf and not leaf.
   This BPlusTree serve for BJoinTree, this is why DataPointers are considered.
*)

const
  PageSize = 20;


type
  PageBufferType = array [0..10230] of byte;

  IndexPointerType =  Integer;
  DataPointerType =   Integer; //string[24];

  IndexPage = record
    Leaf : Boolean;
    Max : word;
    PreviousPointer, NextPointer: IndexPointerType;
    Pointers : array [1.. PageSize] of IndexPointerType; // length = PageSize
    Keys : array [1 .. PageSize-1] of array  of variant; // length = PageSize - 1
    InheritedKeys : array [1 .. PageSize-1] of array of variant;
    DataPointers : array [1 .. PageSize-1] of array of DataPointerType;
    // DataPointers don't exists in Non Leaf
  end;

  {$IFDEF LRU}

  basicPage = record
    IndexPoint: IndexPointerType;
    dirty: Boolean;
    Page: IndexPage;
  end;

const
  LRUMaxEntries = 100;

type
  LRUStackrecord = record
    Entries: Integer;
    Entry: array [0 .. LRUMaxEntries-1] of basicPage;
  end;

  {$ENDIF}

type

  EBtrError = class(Exception)
    private
      FErrorCode: String;
    public
      constructor Create(aErrorCode: String);
      property ErrorCode: String read FErrorCode;
  end;


  BtrPlusClass = class(TFileStream)
    private
      {$IFDEF LRU}
      LRUEntries: LRUStackRecord;
      {$ENDIF}
      FIndexName: string;
      FInitial: Boolean;
      File_Size: Integer;
      OffsetHead : int64;
      LengthRecord: Word;
      LeafPageSize: Word;
      NonLeafPageSize: Word;
      IndexPageSize: Word;
      ParentsInfo: array of record
        Node: IndexPointerType;
        ParentNode: IndexPointerType
      end;
      CursorPage: IndexPage;
      CursorPosition: Integer;
      FNumberOfDataRef: word;
      FKeys: array of record
               colName: string;
               colType: Integer
             end;
      FASC: array of boolean;
      FKeysLength: Word;
      FInheritedKeys: array of record
                        colName: string;
                        colType: Integer
                      end;
      FInheritedKeysLength: Word;
      function GetRoot: IndexPointerType;
      procedure SetRoot(Root: IndexPointerType);
      procedure GetIndex(IndexPoint: IndexPointerType; var Page: IndexPage);
      procedure GetLRUIndex(IndexPoint: IndexPointerType; var result: IndexPage);
      procedure PutLRUIndex(IndexPoint: IndexPointerType; Buf: IndexPage);
      procedure PutIndex(IndexPoint: IndexPointerType; Buf: IndexPage);
      function getParent(node: IndexPointerType): IndexPointerType;

      procedure Add_KeyTreePointerItem(node: IndexPointerType;
                                       child: IndexPointerType;
                                       Keys: array of variant);

      procedure Add_DataPointerKeyItem(node: IndexPointerType;
                                       Keys: array of variant;
                                       InheritedKeys: array of variant;
                                       DP: array of DataPointerType);

      procedure Add_Entry(node: IndexPointerType;
                          child: IndexPointerType;
                          Keys: array of variant;
                          InheritedKeys: array of variant;
                          DP: array of DataPointerType); overload;

      procedure Add_Entry(node: IndexPointerType;
                          Keys: array of variant;
                          InheritedKeys: array of variant;
                          DP: array of DataPointerType); overload;

      function Delete_KeyTreePointerItem(node: IndexPointerType;
                                         child: IndexPointerType;
                                         Keys: array of variant): Boolean;
      function Delete_DataPointerKeyItem(node: IndexPointerType;
                                         Keys: array of variant;
                                         DP: array of DataPointerType): Boolean;
      procedure Delete_Entry(node: IndexPointerType;
                             child: IndexPointerType;
                             Keys: array of variant;
                             DP: array of DataPointerType); overload;
      procedure Delete_Entry(node: IndexPointerType;
                             child: IndexPointerType;
                             Keys: array of variant); overload;
      procedure Delete_Entry(node: IndexPointerType;
                             Keys: array of variant;
                             DP: array of DataPointerType); overload;

      procedure replaceKey(var Keys: array of variant; var DataRef: array of DataPointerType);
      procedure internalFindKey(Keys: array of variant; var DataRef: array of DataPointerType);
      procedure internalSearchKey(var Keys: array of variant; var DataRef: array of DataPointerType);
      procedure internalMinKey(var Keys: array of variant; var InheritedKeys: array of variant;
                               out DataRef: array of DataPointerType);
      procedure internalMaxKey(var Keys: array of variant; var InheritedKeys: array of variant;
                               out DataRef: array of DataPointerType);
      procedure internalPrevKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                out DataRef: array of DataPointerType);
      procedure internalNextKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                out DataRef: array of DataPointerType);


      function GetNumberOfKeys: Integer;
      function GetNumberOfInheritedKeys: Integer;
      function GetNumberOfDataRef: Integer;
    protected
      function compare(Key1, Key2: array of variant): Integer; overload; virtual;
      function compare(Key1, Key2: array of variant; DataRef1, DataRef2: array of DataPointerType): Integer; overload; virtual;
    public
      constructor Create(AFileName: string; IsOpen: Boolean;
                         const TheKeys: array of string;
                         const ASC: array of boolean;
                         const TheInheritedKeys: array of string;
                         ANumberOfDataRef: word); overload;
      constructor Create(AFileName: string; IsOpen: Boolean;
                         const TheKeys: array of string;
                         const TheInheritedKeys: array of string;
                         ANumberOfDataRef: word);  overload;
      constructor Create(AFileName: string; IsOpen: Boolean;
                         const TheKeys: array of string;
                         const ASC: array of boolean); overload;

      destructor destroy; override;


      procedure ClearKey; virtual;
      procedure AddKey(Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType); overload;
      procedure AddKey(Keys: array of variant; aDataRef: DataPointerType); overload;
      procedure DeleteKey(Keys: array of variant; DataRef: array of DataPointerType); overload;
      procedure DeleteKey(Keys: array of variant; aDataRef: DataPointerType); overload;
      procedure FindKey(Keys: array of variant; var InheritedKeys: array of variant; var DataRef: array of DataPointerType); overload;
      procedure FindKey(Keys: array of variant; var aDataRef: DataPointerType); overload;
      procedure SearchKey(var Keys: array of variant; var InheritedKeys: array of variant; var DataRef: array of DataPointerType); overload;
      procedure SearchKey(var Keys: array of variant; var aDataRef: DataPointerType); overload;
      procedure AddInheritedKey(Keys: array of variant;
                                InheritedKeyName: string;
                                InheritedKeyValue: variant);
      procedure PrevKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure PrevKey(var Keys: array of variant; out aDataRef: DataPointerType); overload;
      procedure NextKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure NextKey(var Keys: array of variant; out aDataRef: DataPointerType); overload;
      procedure MinKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure MinKey(var Keys: array of variant; out aDataRef: DataPointerType); overload;
      procedure MaxKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure MaxKey(var Keys: array of variant; out aDataRef: DataPointerType); overload;
      function KeycolName(Index: Integer): string;
      function KeycolType(Index: Integer): Integer;
      function InheritedKeycolName(Index: Integer): string;
      function InheritedKeycolType(Index: Integer): Integer;
      property NumberOfKeys: Integer read GetNumberOfKeys;
      property NumberOfInheritedKeys: Integer read GetNumberOfInheritedKeys;
      property NumberOfDataRef: Integer read GetNumberOfDataRef;
  end;

implementation

uses Variants;

const
  NullValue = -1;
  nulldataValue = -1; //'';

constructor EBtrError.Create(aErrorCode: string);
begin
  FErrorCode := aErrorCode;
  inherited Create(aErrorCode);
end;

function BtrPlusClass.GetNumberOfKeys: Integer;
begin
  result := length(FKeys)
end;

function BtrPlusClass.GetNumberOfInheritedKeys: Integer;
begin
  result := length(FInheritedKeys)
end;

function BtrPlusClass.GetNumberOfDataRef: Integer;
begin
  result := FNumberOfDataRef
end;

function BtrPlusClass.KeycolName(Index: Integer): string;
begin
  if Index > NumberOfKeys then Index := NumberOfKeys;
  result := FKeys[Index].colName;
end;

function BtrPlusClass.KeycolType(Index: Integer): Integer;
begin
  if Index > NumberOfKeys then Index := NumberOfKeys;
  result := FKeys[Index].colType;
end;

function BtrPlusClass.InheritedKeycolName(Index: Integer): string;
begin
  if Index > NumberOfInheritedKeys then Index := NumberOfInheritedKeys;
  result := FInheritedKeys[Index].colName;
end;

function BtrPlusClass.InheritedKeycolType(Index: Integer): Integer;
begin
  if Index > NumberOfInheritedKeys then Index := NumberOfInheritedKeys;
  result := FInheritedKeys[Index].colType;
end;

constructor BtrPlusClass.Create(AFileName: string; IsOpen: Boolean;
                                const TheKeys: array of string;
                                const ASC: array of boolean;
                                const TheInheritedKeys: array of string;
                                ANumberOfDataRef: word);
var
  Params: Word;
  Offset: Int64;
  Origin: Word;
  Root: IndexPointerType;
  FOk: Boolean;
  i: Integer;
  KeysLength: Word;
  InheritedKeysLength: Word;
  Tmp: string;
  LeafIndexPageSize: Word;
  NonLeafIndexPageSize: Word;
begin
  {$IFDEF LRU}
  LRUEntries.Entries := 0;
  {$ENDIF}
  FIndexName := AFileName;



  FNumberOfDataRef := ANumberOfDataRef;

  SetLength(FKeys,Length(TheKeys));
  for i := Low(TheKeys) to High(TheKeys) do
    begin
      if Pos(':',TheKeys[i]) = 0 then
        begin
          FKeys[i].colName := TheKeys[i];
          FKeys[i].ColType := 0
        end else
        begin
          FKeys[i].colName := Copy(TheKeys[i],1,Pos(':',TheKeys[i])-1);
          Tmp := Copy(TheKeys[i],Pos(':',TheKeys[i])+1,
                      Length(TheKeys[i]));
          if Trim(UpperCase(Tmp)) = UpperCase('Integer') then FKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('LongInt') then FKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('SmallInt') then FKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('Boolean') then FKeys[i].colType := -1;
          if Trim(UpperCase(Tmp)) = UpperCase('Char') then FKeys[i].colType := -2;
          if Trim(UpperCase(Tmp)) = UpperCase('TDateTime') then FKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Single') then FKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Double') then FKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Extended') then FKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Currency') then FKeys[i].colType := -4;
          if Trim(UpperCase(Tmp)) = UpperCase('Int64') then FKeys[i].colType := -5;
          if Pos('[',Tmp) <> 0 then
            FKeys[i].colType := StrToInt(Copy(Tmp,Pos('[',Tmp)+1,
                                  Pos(']',Tmp)-1-(Pos('[',Tmp)+1)+1))
        end
    end;

  KeysLength := 0;
  for i := Low(FKeys) to High(FKeys) do
    with FKeys[i] do
      case colType of
        0: KeysLength := KeysLength + SizeOf(Integer);
       -1: KeysLength := KeysLength + SizeOf(Boolean);
       -2: KeysLength := KeysLength + SizeOf(Char);
       -3: KeysLength := KeysLength + SizeOf(Extended);
       -4: KeysLength := KeysLength + SizeOf(Currency);
       -5: KeysLength := KeysLength + SizeOf(Int64)
       else KeysLength := KeysLength + colType + 1
      end;
  FKeysLength := KeysLength;


  setlength(FASC,length(ASC));
  for i := low(ASC) to high(ASC) do FASC[i] := ASC[i];

  SetLength(FInheritedKeys,Length(TheInheritedKeys));
  for i := Low(TheInheritedKeys) to High(TheInheritedKeys) do
    begin
      if Pos(':',TheInheritedKeys[i]) = 0 then
        begin
          FInheritedKeys[i].colName := TheInheritedKeys[i];
          FInheritedKeys[i].ColType := 0
        end else
        begin
          FInheritedKeys[i].colName := Copy(TheInheritedKeys[i],1,Pos(':',TheInheritedKeys[i])-1);
          Tmp := Copy(TheInheritedKeys[i],Pos(':',TheInheritedKeys[i])+1,
                      Length(TheInheritedKeys[i]));
          if Trim(UpperCase(Tmp)) = UpperCase('Integer') then FInheritedKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('LongInt') then FInheritedKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('SmallInt') then FInheritedKeys[i].colType := 0;
          if Trim(UpperCase(Tmp)) = UpperCase('Boolean') then FInheritedKeys[i].colType := -1;
          if Trim(UpperCase(Tmp)) = UpperCase('Char') then FInheritedKeys[i].colType := -2;
          if Trim(UpperCase(Tmp)) = UpperCase('TDateTime') then FInheritedKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Single') then FInheritedKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Double') then FInheritedKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Extended') then FInheritedKeys[i].colType := -3;
          if Trim(UpperCase(Tmp)) = UpperCase('Currency') then FInheritedKeys[i].colType := -4;
          if Trim(UpperCase(Tmp)) = UpperCase('INT64') then FInheritedKeys[i].colType := -5;
          if Pos('[',Tmp) <> 0 then
            FInheritedKeys[i].colType := StrToInt(Copy(Tmp,Pos('[',Tmp)+1,
                                           Pos(']',Tmp)-1-(Pos('[',Tmp)+1)+1))
        end
    end;

  InheritedKeysLength := 0;
  for i := Low(FInheritedKeys) to High(FInheritedKeys) do
    with FInheritedKeys[i] do
      case colType of
        0: InheritedKeysLength := InheritedKeysLength + SizeOf(Integer);
       -1: InheritedKeysLength := InheritedKeysLength + SizeOf(Boolean);
       -2: InheritedKeysLength := InheritedKeysLength + SizeOf(Char);
       -3: InheritedKeysLength := InheritedKeysLength + SizeOf(Extended);
       -4: InheritedKeysLength := InheritedKeysLength + SizeOf(Currency);
       -5: InheritedKeysLength := InheritedKeysLength + SizeOf(Int64)
       else InheritedKeysLength := InheritedKeysLength + colType + 1
      end;
  FInheritedKeysLength := InheritedKeysLength;

  LeafPageSize := PageSize;

  NonLeafPageSize := PageSize;

  IndexPageSize := SizeOf(Boolean) + SizeOf(Word) +
                   SizeOf(IndexPointerType) + SizeOf(IndexPointerType) +
                   (KeysLength * (length(FKeys))) * (PageSize - 1);
  LeafIndexPageSize := IndexPageSize +
                       (InheritedKeysLength * (length(FInheritedKeys)) +
                        FNumberOfDataRef * SizeOf(DataPointerType)) * (PageSize - 1);
  NonLeafIndexPageSize := IndexPageSize +
                          SizeOf(IndexPointerType) * (PageSize);
  IndexPageSize := SizeOf(Boolean) + SizeOf(Word) +
                   SizeOf(IndexPointerType) + SizeOf(IndexPointerType) +
                   (KeysLength * (length(FKeys)) +
                    InheritedKeysLength * (length(FInheritedKeys)) +
                    FNumberOfDataRef * SizeOf(DataPointerType)) * (PageSize - 1)+
                    SizeOf(IndexPointerType) * (PageSize);
  AFileName := AFileName + '.idx';
  FOK := FileExists(AFileName);
  LengthRecord := IndexPageSize;
  if IsOpen then
    begin
      if not FOK then
        begin
          raise EBtrError.Create('File is not created');
          Exit
        end;
      Params := fmOpenReadWrite or fmShareDenyNone;
      inherited Create(AFileName,Params);
    end else
    begin
      if FOK then
        begin
          raise EBtrError.Create('File is created');
          Exit
        end;
        Params := fmCreate or fmShareDenyNone;
        inherited Create(AFileName,Params);
        Root := NullValue;
        SetRoot(Root)
    end;
  OffsetHead := SizeOf(IndexPointerType);
  Offset := 0;
  Origin := soFromEnd;
  Seek(Offset,Origin);
  File_Size := Position;
  File_Size := ((Position - OffsetHead) div LengthRecord) + 1;
  FInitial := True
end;

constructor BtrPlusClass.Create(AFileName: string; IsOpen: Boolean;
                                const TheKeys: array of string;
                                const TheInheritedKeys: array of string;
                                ANumberOfDataRef: word);
var
  i: Integer;
  ASC: array of boolean = nil;
begin
  setLength(ASC,length(Thekeys));
  for i := low(ASC) to high(ASC) do ASC[i] := true;
  create(AFileName, IsOpen, TheKeys, ASC, TheInheritedKeys, ANumberOfDataRef);
end;

constructor BtrPlusClass.Create(AFileName: string; IsOpen: Boolean;
                                const TheKeys: array of string;
                                const ASC: array of boolean);
var
  TheInheritedKeys: array of string;
  ANumberOfDataRef: word;
begin
  TheInheritedKeys := nil;
  ANumberOfDataRef := 1;
  create(AFileName, IsOpen, TheKeys, ASC, TheInheritedKeys, ANumberOfDataRef);
end;

destructor BtrPlusClass.destroy;
{$IFDEF LRU}
var
  i: integer;
{$ENDIF}
begin
  {$IFDEF LRU}
  for i := 0 to LRUEntries.Entries - 1 do
    if LRUEntries.Entry[i].dirty then
      putIndex(LRUEntries.Entry[i].IndexPoint,LRUEntries.Entry[i].Page);
  {$ENDIF}

  inherited Destroy;
end;

function BtrPlusClass.GetRoot: IndexPointerType;
var
  Offset: Int64;
  Origin: Word;
begin
  result := NullValue;
  Offset := 0;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  ReadBuffer(result,SizeOf(IndexPointerType))
end;

procedure BtrPlusClass.SetRoot(Root: IndexPointerType);
var
  Offset: Int64;
  Origin: Word;
begin
  Offset := 0;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  WriteBuffer(Root,SizeOf(IndexPointerType))
end;

procedure BtrPlusClass.GetLRUIndex(IndexPoint: IndexPointerType; var result: IndexPage);
{$IFDEF LRU}
var
  found: boolean;
  index: integer;
  i, j: integer;
  page: basicPage;
{$ENDIF}
begin
  {$IFDEF LRU}
  found := false;
  index := LRUEntries.Entries;
  for i := 0 to LRUEntries.Entries - 1 do
    if LRUEntries.Entry[I].IndexPoint = IndexPoint then
      begin
       found := true;
       index := i;
       page := LRUEntries.Entry[I];
       result := page.Page;
       Break;
      end;
  if (index = LRUEntries.Entries) then
    if (LRUEntries.Entries < LRUMaxEntries) then
      LRUEntries.Entries += 1 else
      begin
        index := LRUMaxEntries - 1;
        if LRUEntries.Entry[index].dirty then
          putIndex(LRUEntries.Entry[index].IndexPoint,LRUEntries.Entry[index].Page);
      end;
  if i <> 0 then
    i:=i;
  for j := index-1 downto 0 do
    LRUEntries.Entry[j+1] := LRUEntries.Entry[j];
  if not found then
    begin
      page.dirty := false;
      page.IndexPoint := IndexPoint;
      GetIndex(IndexPoint,page.Page);
      result := page.Page
    end;
  LRUEntries.Entry[0] := page;
  {$ELSE}
  Getindex(IndexPoint,result);
  {$ENDIF}
end;

procedure BtrPlusClass.PutLRUIndex(IndexPoint: IndexPointerType; Buf: IndexPage);
{$IFDEF LRU}
var
  found: boolean;
  index: integer;
  i, j: integer;
  page: basicPage;
{$ENDIF}
begin
  {$IFDEF LRU}
  found := false;
  index := LRUEntries.Entries;
  for i := 0 to LRUEntries.Entries - 1 do
    if LRUEntries.Entry[I].IndexPoint = IndexPoint then
      begin
       found := true;
       index := i;
       LRUEntries.Entry[I].dirty := true;
       LRUEntries.Entry[I].Page := Buf;
       page := LRUEntries.Entry[I];
       Break;
      end;
  if (index = LRUEntries.Entries) then
    if (LRUEntries.Entries < LRUMaxEntries) then
      LRUEntries.Entries += 1 else
      begin
        index := LRUMaxEntries - 1;
        if LRUEntries.Entry[index].dirty then
          putIndex(LRUEntries.Entry[index].IndexPoint,LRUEntries.Entry[index].Page);
      end;
  if i <> 0 then
    i:=i;
  for j := index-1 downto 0 do
    LRUEntries.Entry[j+1] := LRUEntries.Entry[j];
  if not found then
    begin
      page.dirty := true;
      page.IndexPoint := IndexPoint;
      page.Page := Buf
    end;
  LRUEntries.Entry[0] := page;
  {$ELSE}
  putindex(IndexPoint,buf);
  {$ENDIF}
end;

procedure BtrPlusClass.GetIndex(IndexPoint: IndexPointerType; var Page: IndexPage);
var
  Offset: Int64;
  Origin: Word;
  i, j: Integer;
  BufPos: LongInt;
  Buffer: PageBufferType;
  Len: Byte;
  k: Integer;
  TmpInt: Integer;
  TmpBoolean: Boolean;
  TmpChar: Char;
  TmpExtended: Extended;
  TmpCurrency: Currency;
  TmpString: string;
  TmpInt64: Int64;
begin

  Offset := (IndexPoint - 1)* LengthRecord + OffsetHead;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  ReadBuffer(Buffer,IndexPageSize);

  BufPos := 0;
  Page.Leaf := Buffer[BufPos] <> 0;
  Inc(BufPos,SizeOf(Byte));
  Move(Buffer[BufPos],Page.Max,SizeOf(Word));
  Inc(BufPos,SizeOf(Word));

  Move(Buffer[BufPos],Page.PreviousPointer,SizeOf(IndexPointerType));
  Inc(BufPos,SizeOf(IndexPointerType));
  Move(Buffer[BufPos],Page.NextPointer,SizeOf(IndexPointerType));
  Inc(BufPos,SizeOf(IndexPointerType));

  if Page.Leaf then
    begin
      for i := 1 to LeafPageSize - 1 do
        begin
          for j := 0 to length(FKeys) - 1 do
            begin
              setlength(Page.Keys[i],length(FKeys));
              Page.Keys[i,j] := Unassigned;
            end;
          for j := 0 to length(FInheritedKeys) - 1 do
            begin
              setlength(Page.InheritedKeys[i],length(FInheritedKeys));
              Page.InheritedKeys[i,j] := Unassigned;
            end;
          for j := 0 to FNumberOfDataRef - 1 do
            begin
              setlength(Page.DataPointers[i],FNumberOfDataRef);
              Page.DataPointers[i,j] := nullDataValue;
            end;
        end;
      for i := 1 to Page.Max do
        begin
          //SetLength(Page.Keys[i],Length(FKeys));
          for j := Low(FKeys) to High(FKeys) do
            with FKeys[j] do
              case colType of
                0: begin
                     TmpInt := 0;
                     Move(Buffer[BufPos],TmpInt,SizeOf(Integer));
                     Page.Keys[i,j] := TmpInt;
                     Inc(BufPos,SizeOf(Integer))
                   end;

               -1: begin
                     TmpBoolean := false;
                     Move(Buffer[BufPos],TmpBoolean,SizeOf(Boolean));
                     Page.Keys[i,j] := TmpBoolean;
                     Inc(BufPos,SizeOf(Boolean))
                   end;
               -2: begin
                     TmpChar := #0;
                     Move(Buffer[BufPos],TmpChar,SizeOf(Char));
                     Page.Keys[i,j] := TmpChar;
                     Inc(BufPos,SizeOf(Char))
                   end;
               -3: begin
                     TmpExtended := 0;
                     Move(Buffer[BufPos],TmpExtended,SizeOf(Extended));
                     Page.Keys[i,j] := TmpExtended;
                     Inc(BufPos,SizeOf(Extended))
                   end;
               -4: begin
                     TmpCurrency := 0;
                     Move(Buffer[BufPos],TmpCurrency,SizeOf(Currency));
                     Page.Keys[i,j] := TmpCurrency;
                     Inc(BufPos,SizeOf(Currency))
                   end;
               -5: begin
                     TmpInt64 := 0;
                     Move(Buffer[BufPos],TmpInt64,SizeOf(INT64));
                     Page.Keys[i,j] := TmpInt64;
                     Inc(BufPos,SizeOf(INT64))
                   end
              else begin
                     Len := 0;
                     Move(Buffer[BufPos],Len,SizeOf(Byte));
                     Inc(BufPos,SizeOf(Byte));
                     TmpString := '';
                     for k := 1 to Len do
                       TmpString := TmpString + char(Buffer[BufPos+k-1]);
                     Page.Keys[i,j] := TmpString;
                     Inc(BufPos,colType)
                   end;

              end;
        end;

      //Inc(BufPos,(LeafPageSize + 1) * SizeOf(IndexPointerType));
      for i := 1 to Page.Max do
        begin
          //SetLength(Page.DataPointers[i],FNumberOfDataRef);
          for j := 0 to FNumberOfDataRef - 1 do
            begin
              Move(Buffer[BufPos],Page.DataPointers[i,j],SizeOf(DataPointerType));
              Inc(BufPos,SizeOf(DataPointerType))
            end;
        end;
      for i := 1 to Page.Max do
        begin
          //SetLength(Page.InheritedKeys[i],Length(FInheritedKeys));
          for j := Low(FInheritedKeys) to High(FInheritedKeys) do
            with FInheritedKeys[j] do
              case colType of
                0: begin
                     Move(Buffer[BufPos],TmpInt,SizeOf(Integer));
                     Page.InheritedKeys[i,j] := TmpInt;
                     Inc(BufPos,SizeOf(Integer))
                   end;

               -1: begin
                     Move(Buffer[BufPos],TmpBoolean,SizeOf(Boolean));
                     Page.InheritedKeys[i,j] := TmpBoolean;
                     Inc(BufPos,SizeOf(Boolean))
                   end;
               -2: begin
                     Move(Buffer[BufPos],TmpChar,SizeOf(Char));
                     Page.InheritedKeys[i,j] := TmpChar;
                     Inc(BufPos,SizeOf(Char))
                   end;
               -3: begin
                     Move(Buffer[BufPos],TmpExtended,SizeOf(Extended));
                     Page.InheritedKeys[i,j] := TmpExtended;
                     Inc(BufPos,SizeOf(Extended))
                   end;
               -4: begin
                     Move(Buffer[BufPos],TmpCurrency,SizeOf(Currency));
                     Page.InheritedKeys[i,j] := TmpCurrency;
                     Inc(BufPos,SizeOf(Currency))
                   end;
               -5: begin
                     Move(Buffer[BufPos],TmpInt64,SizeOf(INT64));
                     Page.InheritedKeys[i,j] := TmpInt64;
                     Inc(BufPos,SizeOf(INT64))
                   end
              else begin
                     Move(Buffer[BufPos],Len,SizeOf(Byte));
                     Inc(BufPos,SizeOf(Byte));
                     TmpString := '';
                     for k := 1 to Len do
                       TmpString := TmpString + char(Buffer[BufPos+k-1]);
                     Page.InheritedKeys[i,j] := TmpString;
                     Inc(BufPos,colType)
                   end
              end;
        end
    end else
    begin

      for i := 1 to NonLeafPageSize do
        Page.Pointers[i] := nullValue;

      for i := 1 to NonLeafPageSize{Page.Max + 1} do
        begin
          Move(Buffer[BufPos],Page.Pointers[i],SizeOf(IndexPointerType));
          Inc(BufPos,SizeOf(IndexPointerType));
        end;
      //Inc(BufPos,(LeafPageSize + 1) * SizeOf(IndexPointerType));

      for i := 1 to NonLeafPageSize - 1 do
        begin
          for j := 0 to length(FKeys) - 1 do
            begin
              setlength(Page.Keys[i],length(FKeys));
              Page.Keys[i,j] := Unassigned;
            end;
{          for j := 0 to length(FInheritedKeys) - 1 do
            begin
              setlength(Page.InheritedKeys[i],length(FInheritedKeys));
              Page.InheritedKeys[i,j] := Unassigned;
            end;
          for j := 0 to FNumberOfDataRef - 1 do
            begin
              setlength(Page.DataPointers[i],FNumberOfDataRef);
              Page.DataPointers[i,j] := nullDataValue;
            end;
}        end;

      for i := 1 to Page.Max do
        begin
          //SetLength(Page.Keys[i],Length(FKeys));
          for j := Low(FKeys) to High(FKeys) do
            with FKeys[j] do
              case colType of
                0: begin
                     Move(Buffer[BufPos],TmpInt,SizeOf(Integer));
                     Page.Keys[i,j] := TmpInt;
                     Inc(BufPos,SizeOf(Integer))
                   end;

               -1: begin
                     Move(Buffer[BufPos],TmpBoolean,SizeOf(Boolean));
                     Page.Keys[i,j] := TmpBoolean;
                     Inc(BufPos,SizeOf(Boolean))
                   end;
               -2: begin
                     Move(Buffer[BufPos],TmpChar,SizeOf(Char));
                     Page.Keys[i,j] := TmpChar;
                     Inc(BufPos,SizeOf(Char))
                   end;
               -3: begin
                     Move(Buffer[BufPos],TmpExtended,SizeOf(Extended));
                     Page.Keys[i,j] := TmpExtended;
                     Inc(BufPos,SizeOf(Extended))
                   end;
               -4: begin
                     Move(Buffer[BufPos],TmpCurrency,SizeOf(Currency));
                     Page.Keys[i,j] := TmpCurrency;
                     Inc(BufPos,SizeOf(Currency))
                   end;
               -5: begin
                     Move(Buffer[BufPos],TmpInt64,SizeOf(INT64));
                     Page.Keys[i,j] := TmpInt64;
                     Inc(BufPos,SizeOf(INT64))
                   end
              else begin
                     Move(Buffer[BufPos],Len,SizeOf(Byte));
                     Inc(BufPos,SizeOf(Byte));
                     TmpString := '';
                     for k := 1 to Len do
                       TmpString := TmpString + char(Buffer[BufPos+k-1]);
                     Page.Keys[i,j] := TmpString;
                     Inc(BufPos,colType)
                   end;

              end;
        end;
{      for i := 1 to  Page.Max do
        begin
          for j := 0 to FNumberOfDataRef - 1 do
            begin
              Move(Buffer[BufPos],Page.DataPointers[i,j],SizeOf(DataPointerType));
              Inc(BufPos,SizeOf(DataPointerType))
            end
        end;
}
      // SetLength(Page.InheritedKeys[i],Length(FInheritedKeys));

              // Page.InheritedKeys[i,j] := nil;
              // SetLength(Page.DataPointers[i],FNumberOfDataRef);
    end;

end;

procedure BtrPlusClass.PutIndex(IndexPoint: IndexPointerType;
                                Buf: IndexPage);
var
  Offset: Int64;
  Origin: Word;
  i, j: Integer;
  BufPos: LongInt;
  Buffer: PageBufferType;
  TmpInt: Integer;
  TmpBoolean: Boolean;
  TmpChar: Boolean;
  TmpExtended: Extended;
  TmpCurrency: Currency;
  TmpString: string;
  TmpInt64: INT64;
  k: Integer;
  Len: Byte;
begin
  BufPos := 0;

 {

  Offset := IndexPoint * LengthRecord + OffsetHead;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  WriteBuffer(Buf,SizeOf(IndexPage)+52);
  exit;

 }



  Buffer[BufPos] := ord(Buf.Leaf);
  Inc(BufPos,SizeOf(Byte));
  Move(Buf.Max,Buffer[BufPos],SizeOf(Word));
  Inc(BufPos,SizeOf(Word));

  Move(Buf.PreviousPointer,Buffer[BufPos],SizeOf(IndexPointerType));
  Inc(BufPos,SizeOf(IndexPointerType));

  Move(Buf.NextPointer,Buffer[BufPos],SizeOf(IndexPointerType));
  Inc(BufPos,SizeOf(IndexPointerType));

  if Buf.Leaf then
    begin
{      for i := 1 to LeafPageSize do
        begin
          Move(Buf.Pointers[i] ,Buffer[BufPos],SizeOf(IndexPointerType));
          Inc(BufPos,SizeOf(IndexPointerType));
        end;
}      //Inc(BufPos,(LeafPageSize + 1) * SizeOf(IndexPointerType));

      for i := 1 to Buf.Max do
        begin
          for j := Low(FKeys) to High(FKeys) do
            with FKeys[j] do
              case colType of
                0: begin
                     TmpInt := Buf.Keys[i,j];
                     Move(TmpInt,Buffer[BufPos],SizeOf(Integer));
                     Inc(BufPos,SizeOf(Integer))
                   end;

               -1: begin
                     TmpBoolean := Buf.Keys[i,j];
                     Move(TmpBoolean,Buffer[BufPos],SizeOf(Boolean));
                     Inc(BufPos,SizeOf(Boolean))
                   end;
               -2: begin
                     TmpChar := Buf.Keys[i,j];
                     Move(TmpChar,Buffer[BufPos],SizeOf(Char));
                     Inc(BufPos,SizeOf(Char))
                   end;
               -3: begin
                     TmpExtended := Buf.Keys[i,j];
                     Move(TmpExtended,Buffer[BufPos],SizeOf(Extended));
                     Inc(BufPos,SizeOf(Extended))
                   end;
               -4: begin
                     TmpCurrency := Buf.Keys[i,j];
                     Move(TmpCurrency,Buffer[BufPos],SizeOf(Currency));
                     Inc(BufPos,SizeOf(Currency))
                   end;
               -5: begin
                     TmpInt64 := Buf.Keys[i,j];
                     Move(TmpInt64,Buffer[BufPos],SizeOf(INT64));
                     Inc(BufPos,SizeOf(INT64))
                   end
              else begin
                     TmpString := Buf.Keys[i,j];
                     Len := Length(TmpString);
                     Move(Len,Buffer[BufPos],SizeOf(Byte));
                     Inc(BufPos,SizeOf(Byte));
                     for k := Length(TmpString) + 1 to colType do TmpString := TmpString + ' ';
                     for k := 1 to coltype do
                       Buffer[BufPos+k-1] := ord(TmpString[k]);
                     Inc(BufPos,colType)
                   end

              end;
        end;
      for i := 1 to Buf.Max do
        begin
          for j := 0 to FNumberOfDataRef - 1 do
            begin
              Move(Buf.DataPointers[i,j],Buffer[BufPos],SizeOf(DataPointerType));
              Inc(BufPos,SizeOf(DataPointerType))
            end
        end;
      for i := 1 to Buf.Max do
        begin
          for j := Low(FInheritedKeys) to High(FInheritedKeys) do
                with FInheritedKeys[j] do
                  case colType of
                    0: begin
                         TmpInt := Buf.InheritedKeys[i,j];
                         Move(TmpInt,Buffer[BufPos],SizeOf(Integer));
                         Inc(BufPos,SizeOf(Integer))
                       end;

                   -1: begin
                         TmpBoolean := Buf.InheritedKeys[i,j];
                         Move(TmpBoolean,Buffer[BufPos],SizeOf(Boolean));
                         Inc(BufPos,SizeOf(Boolean))
                       end;
                   -2: begin
                         TmpChar := Buf.InheritedKeys[i,j];
                         Move(TmpChar,Buffer[BufPos],SizeOf(Char));
                         Inc(BufPos,SizeOf(Char))
                       end;
                   -3: begin
                         TmpExtended := Buf.InheritedKeys[i,j];
                         Move(TmpExtended,Buffer[BufPos],SizeOf(Extended));
                         Inc(BufPos,SizeOf(Extended))
                       end;
                   -4: begin
                         TmpCurrency := Buf.InheritedKeys[i,j];
                         Move(TmpCurrency,Buffer[BufPos],SizeOf(Currency));
                         Inc(BufPos,SizeOf(Currency))
                       end;
                   -5: begin
                         TmpInt64 := Buf.InheritedKeys[i,j];
                         Move(TmpInt64,Buffer[BufPos],SizeOf(INT64));
                         Inc(BufPos,SizeOf(INT64))
                       end
                  else begin
                         TmpString := Buf.InheritedKeys[i,j];
                         Len := Length(TmpString);
                         Move(Len,Buffer[BufPos],SizeOf(Byte));
                         Inc(BufPos,SizeOf(Byte));
                         for k := Length(TmpString) + 1 to colType do TmpString := TmpString + ' ';
                         for k := 1 to coltype do
                           Buffer[BufPos+k-1] := ord(TmpString[k]);
                         Inc(BufPos,colType)
                       end

                  end;
        end;
    end else
    begin

      for i := 1 to NonLeafPageSize do
        begin
          Move(Buf.Pointers[i] ,Buffer[BufPos],SizeOf(IndexPointerType));
          Inc(BufPos,SizeOf(IndexPointerType));
        end;

      for i := 1 to Buf.Max do
          begin
            for j := Low(FKeys) to High(FKeys) do
              with FKeys[j] do
                case colType of
                  0: begin
                       TmpInt := Buf.Keys[i,j];
                       Move(TmpInt,Buffer[BufPos],SizeOf(Integer));
                       Inc(BufPos,SizeOf(Integer))
                     end;

                 -1: begin
                       TmpBoolean := Buf.Keys[i,j];
                       Move(TmpBoolean,Buffer[BufPos],SizeOf(Boolean));
                       Inc(BufPos,SizeOf(Boolean))
                     end;
                 -2: begin
                       TmpChar := Buf.Keys[i,j];
                       Move(TmpChar,Buffer[BufPos],SizeOf(Char));
                       Inc(BufPos,SizeOf(Char))
                     end;
                 -3: begin
                       TmpExtended := Buf.Keys[i,j];
                       Move(TmpExtended,Buffer[BufPos],SizeOf(Extended));
                       Inc(BufPos,SizeOf(Extended))
                     end;
                 -4: begin
                       TmpCurrency := Buf.Keys[i,j];
                       Move(TmpCurrency,Buffer[BufPos],SizeOf(Currency));
                       Inc(BufPos,SizeOf(Currency))
                     end;
                 -5: begin
                       TmpInt64 := Buf.Keys[i,j];
                       Move(TmpInt64,Buffer[BufPos],SizeOf(INT64));
                       Inc(BufPos,SizeOf(INT64))
                     end
                else begin
                       TmpString := Buf.Keys[i,j];
                       Len := Length(TmpString);
                       Move(Len,Buffer[BufPos],SizeOf(Byte));
                       Inc(BufPos,SizeOf(Byte));
                       for k := Length(TmpString) + 1 to colType do TmpString := TmpString + ' ';
                       for k := 1 to coltype do
                         Buffer[BufPos+k-1] := ord(TmpString[k]);
                       Inc(BufPos,colType)
                     end

                end;
            end;
{       for i := 1 to Buf.Max do
            for j := 0 to FNumberOfDataRef - 1 do
              begin
                Move(Buf.DataPointers[i,j],Buffer[BufPos],SizeOf(DataPointerType));
                Inc(BufPos,SizeOf(DataPointerType))
              end;
}
    end;

  Offset := (IndexPoint - 1) * LengthRecord + OffsetHead;
  Origin := soFromBeginning;
  Seek(Offset,Origin);
  writeBuffer(Buffer,IndexPageSize);

  //WriteBuffer(Buffer,SizeOf(PageBufferType))
end;


procedure BtrPlusClass.ClearKey;
begin
  FInitial := True
end;

function BtrPlusClass.compare(Key1, Key2: array of variant): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(Key1) = 0 then Exit;
  for i := Low(Key1) to High(Key1) do
    begin
      if i > high(key2) then exit;
      if Key1[i] < Key2[i] then
        begin
          result := -1;
          if not FASC[i] then result := -result;
          Break
        end else
        if Key1[i] > Key2[i] then
          begin
            result := 1;
            if not FASC[i] then result := -result;
            Break
          end
    end;
end;

function BtrPlusClass.compare(Key1, Key2: array of variant; DataRef1, DataRef2: array of DataPointerType): Integer;
var
  i: Integer;
begin
  Result := Compare(Key1,Key2);
  if (Length(DataRef1) = 0) or (Length(DataRef2) = 0) then Exit;
  if Result = 0 then
    for i := 0 to FNumberOfDataRef - 1 do
      begin
        if DataRef1[i] < DataRef2[i] then
          begin
            result := -1;
            Break
          end else
          if DataRef1[i] > DataRef2[i] then
            begin
              result := 1;
              Break
            end
      end
end;

function BtrPlusClass.getParent(node: IndexPointerType): IndexPointerType;
var
  i: Integer;
begin
  result := NullValue;
  for i := Low(ParentsInfo) to High(ParentsInfo) do
    if ParentsInfo[i].Node = node then
      begin
        result := ParentsInfo[i].ParentNode;
        Break
      end
end;

(* The use of duplicates is very important for Join,
   they differ by DataPointers *)

procedure BtrPlusClass.Add_KeyTreePointerItem(node: IndexPointerType;
                                              child: IndexPointerType;
                                              Keys: array of variant);
var
  nodePage: IndexPage;
  i, j: Integer;
  KeyPosition, PointerPosition: Integer;
  {$IFDEF Duplicates}
  tmpNode: IndexPointerType;
  {$ENDIF}
begin
  {$IFDEF Duplicates}
  tmpNode := node;
  while tmpNode <> nullValue do
    begin
      GetLRUIndex(tmpnode,nodePage);
      tmpNode := nullValue;
      if nodePage.Max <> 0 then
        begin
          if compare(Keys,nodePage.Keys[nodePage.Max]) = 0 then
            tmpNode := nodePage.NextPointer else
            begin
              KeyPosition := nodePage.Max + 1;
              for i := 1 to nodePage.Max do
                if compare(Keys,nodePage.Keys[i]) = -1 then
                  begin
                    keyposition := i;
                    Break;
                  end;
              for i := nodePage.Max downto keyPosition do
                begin
                  for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                    nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
                end;
              PointerPosition := KeyPosition + 1;
            end
        end else
        begin
          KeyPosition := 1;
          PointerPosition := 2;
        end;
    end;
  {$ELSE}
  GetLRUIndex(node,nodePage);
  if nodePage.Max <> 0 then
    begin
      KeyPosition := nodePage.Max + 1;
      for i := 1 to nodePage.Max do
        if compare(Keys,nodePage.Keys[i]) = -1 then
          begin
            keyposition := i;
            Break;
          end;
      for i := nodePage.Max downto keyPosition do
        begin
          for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
            nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
        end;
      PointerPosition := KeyPosition + 1;
    end else
    begin
      KeyPosition := 1;
      PointerPosition := 2;
    end;
  {$ENDIF}
  for i := nodePage.Max+1 downto PointerPosition do
      nodePage.Pointers[i+1] := nodePage.Pointers[i];
  for j := Low(Keys) to High(Keys) do
    nodePage.Keys[KeyPosition,j] := Keys[j];
  nodePage.Pointers[PointerPosition] := child;
  Inc(nodePage.Max);
  PutLRUIndex(node,nodePage);

(******************************)
GetLRUIndex(node,nodePage)
(******************************)

end;

procedure BtrPlusClass.Add_DataPointerKeyItem(node: IndexPointerType;
                                              Keys: array of variant;
                                              InheritedKeys: array of variant;
                                              DP: array of DataPointerType);
var
  nodePage: IndexPage;
  i, j: Integer;
  KeyPosition: Integer;
  {$IFDEF Duplicates}
  tmpNode: IndexPointerType;
  {$ENDIF}
begin
  {$IFDEF Duplicates}
  tmpNode := node;
  while tmpNode <> nullValue do
    begin
      GetLRUIndex(tmpnode,nodePage);
      tmpNode := nullValue;
      if nodePage.Max <> 0 then
        begin
          if nodePage.NextPointer <> nullvalue then
            if compare(Keys,nodePage.Keys[nodePage.Max]) = 0 then
              begin
              tmpNode := nodePage.NextPointer;
              KeyPosition :=1; continue end else
          KeyPosition := nodePage.Max + 1;
          for i := 1 to nodePage.Max do
            if compare(Keys,nodePage.Keys[i]) = -1  then
              begin
                keyposition := i;
                Break;
              end;
          for i := nodePage.Max  downto keyPosition do
            begin
              for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
              for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
                nodePage.DataPointers[i+1,j] := nodePage.DataPointers[i,j];
              for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
                nodePage.InheritedKeys[i+1,j] := nodePage.InheritedKeys[i,j];
            end
        end else
        begin
          KeyPosition := 1;
        end;
    end;
  {$ELSE}
  GetLRUIndex(node,nodePage);
  if nodePage.Max <> 0 then
    begin
      KeyPosition := nodePage.Max + 1;
      for i := 1 to nodePage.Max do
        if compare(Keys,nodePage.Keys[i]{,DP,nodePage.DataPointers[i]}) = -1  then
          begin
            keyposition := i;
            Break;
          end;
      for i := nodePage.Max  downto keyPosition do
        begin
          for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
            nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
          for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
            nodePage.DataPointers[i+1,j] := nodePage.DataPointers[i,j];
          for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
            nodePage.InheritedKeys[i+1,j] := nodePage.InheritedKeys[i,j];
        end;
    end else
    begin
      KeyPosition := 1
    end;
    {$ENDIF}
  for i := Low(Keys) to High(Keys) do
    nodePage.Keys[KeyPosition,i] := Keys[i];
  for i := Low(InheritedKeys) to High(InheritedKeys) do
    nodePage.InheritedKeys[keyPosition,i] := InheritedKeys[i];
  for i := Low(DP) to High(DP) do
    nodePage.DataPointers[KeyPosition,i] := DP[i];
  Inc(nodePage.Max);
  PutLRUIndex(node,nodePage);
  FInitial := False;
  CursorPage := nodePage;
  CursorPosition := KeyPosition
end;

procedure BtrPlusClass.Add_Entry(node: IndexPointerType;
                                 child: IndexPointerType;
                                 Keys: array of variant;
                                 InheritedKeys: array of variant;
                                 DP: array of DataPointerType);
var
  Control: Boolean;
  NewPage: IndexPage;
  TmpPage: IndexPage;
  nodePage: IndexPage;
  nodeAdjacent: IndexPointerType;
  adjacentPage: IndexPage;
  KeysTmp: array [1..PageSize] of array of variant;
  InheritedKeysTmp: array [1..PageSize] of array of variant;
  DPTmp: array [1..PageSize] of array of DataPointerType;
  i, j: Integer;
  APosition: Integer;
  V1: array of variant;
  VIK1: array of variant;
  DP1: array of DataPointerType;
  m: Integer;
  Root: IndexPointerType;
begin
  GetLRUIndex(node,nodePage);

  if nodePage.Leaf then Control := nodePage.Max < LeafPageSize - 1
   else Control := nodePage.Max < NonLeafPageSize - 1; // Keys: 1 .. PageSize - 1

  if Control then // There is space in the node
    begin
      if nodePage.Leaf then Add_DataPointerKeyItem(node, Keys, InheritedKeys, DP)
       else Add_KeyTreePointerItem(node, child, Keys)
    end else
    begin // Split node


      nodeAdjacent := File_Size;
      Inc(File_Size);

      adjacentPage.Leaf := nodePage.Leaf;
      adjacentPage.Max := 0;

      APosition := nodePage.Max + 1;

      if adjacentPage.Leaf then
        begin
          for i := 1 to LeafPageSize - 1 do
            begin
              SetLength(adjacentPage.Keys[i],length(FKeys));
              for j := 0 to Length(FKeys) - 1 do
                adjacentPage.keys[i,j] := Unassigned;
              SetLength(adjacentPage.InheritedKeys[i],length(FInheritedKeys));
              for j := 0 to Length(FInheritedKeys) - 1 do
                adjacentPage.Inheritedkeys[i,j] := Unassigned;
              SetLength(adjacentPage.DataPointers[i],FNumberofDataRef);
              for j := 0 to FNumberOfDataRef - 1 do
                adjacentPage.DataPointers[i,j] := NullDataValue;
            end;
        end else
        begin
          for i := 1 to NonLeafPageSize do
            adjacentPage.Pointers[i] := NullValue;
          for i := 1 to NonLeafPageSize - 1 do
            begin
              SetLength(adjacentPage.Keys[i],length(FKeys));
              for j := 0 to Length(FKeys) - 1 do
                adjacentPage.keys[i,j] := Unassigned;
            end;
          for i := 1 to NonLeafPageSize do
            adjacentPage.Pointers[i] := NullValue;

        end;

      if nodePage.Leaf then
        begin
          for i := 1 to nodePage.Max do
            if compare(Keys,nodePage.Keys[i]) = -1 then
              begin
                APosition := i;
                Break
              end;

          for i := 1 to LeafPageSize  do
             begin
               SetLength(KeysTmp[i],Length(FKeys));
               SetLength(InheritedKeysTmp[i],Length(FInheritedKeys));
               SetLength(DPTmp[i],FNumberOfDataRef)
             end;


          for i := 1 to nodePage.Max + 1 do
            if (i < APosition) then
              begin
                for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                  KeysTmp[i,j] := nodePage.Keys[i,j];
                for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
                  InheritedKeysTmp[i,j] := nodePage.InheritedKeys[i,j];
                for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
                  DPTmp[i,j] := nodePage.DataPointers[i,j];
              end else
              if (i = APosition) then
                begin
                  for j := Low(Keys) to High(Keys) do
                    keysTmp[i,j] := Keys[j];
                  for j := Low(InheritedKeys) to High(InheritedKeys) do
                    InheritedkeysTmp[i,j] := InheritedKeys[j];
                  for j := Low(DP) to High(DP) do
                    DPTmp[i,j] := DP[j];
                end else
                begin
                  for j := Low(nodePage.Keys[i-1]) to High(nodePage.Keys[i-1]) do
                    KeysTmp[i,j] := nodePage.Keys[i-1,j];
                  for j := Low(nodePage.InheritedKeys[i-1]) to High(nodePage.InheritedKeys[i-1]) do
                    InheritedKeysTmp[i,j] := nodePage.InheritedKeys[i-1,j];
                  for j := Low(nodePage.DataPointers[i-1]) to High(nodePage.DataPointers[i-1]) do
                    DPTmp[i,j] := nodePage.DataPointers[i-1,j];
                end;
          SetLength(V1,Length(FKeys));
          for j := Low(KeysTmp[((nodePage.Max+1)div 2)+1]) to High(KeysTmp[((nodePage.Max+1)div 2)+1]) do
            V1[j] := KeysTmp[((nodePage.Max+1)div 2)+1,j];
          SetLength(VIK1,Length(FInheritedKeys));
          for j := Low(InheritedKeysTmp[((nodePage.Max+1)div 2)+1]) to High(InheritedKeysTmp[((nodePage.Max+1)div 2)+1]) do
            VIK1[j] := InheritedKeysTmp[((nodePage.Max+1)div 2)+1,j];
          SetLength(DP1,FNumberOfDataRef);
          for j := Low(DPTmp[((nodePage.Max+1)div 2)+1]) to High(DPTmp[((nodePage.Max+1)div 2)+1]) do
            DP1[j] := DPTmp[((nodePage.Max+1)div 2)+1,j];
        end else
        begin
          for i := 1 to nodePage.Max do
            if compare(Keys,nodePage.Keys[i]) = -1 then
              begin
                APosition := i;
                Break
              end;

          for i := 1 to NonLeafPageSize - 1 do
              SetLength(KeysTmp[i],Length(FKeys));

          for i := 1 to nodePage.Max do
            if (i < APosition) then
              begin
                for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                  KeysTmp[i,j] := nodePage.Keys[i,j];
              end else
              if (i = APosition) then
                begin
                  for j := Low(Keys) to High(Keys) do
                    keysTmp[i,j] := Keys[j];
                end else
                begin
                  for j := Low(nodePage.Keys[i-1]) to High(nodePage.Keys[i-1]) do
                    KeysTmp[i,j] := nodePage.Keys[i-1,j];
                end;

          SetLength(V1,Length(FKeys));
          for j := Low(KeysTmp[((nodePage.Max+1)div 2)+1]) to High(KeysTmp[((nodePage.Max+1)div 2)+1]) do
            V1[j] := KeysTmp[((nodePage.Max+1)div 2)+1,j];
        end;

      // Note that V1 must be either node.Km or V
      // V1 is not enough by itself, should be also considered DatRef

      m := 1;
      if compare(Keys,V1) <> 0 then
        begin
          for i := 1 to nodePage.Max do
            begin
              if compare(nodePage.Keys[i],V1) = 0 then
                begin
                  m := i;
                  Break
                end
            end
        end else m := ((nodePage.Max+ 1) div 2) + 1;

      if nodePage.Leaf then
        begin

          // move node.Pm,node.Km,...,node.Pn-1,node.Kn-1 to nodeAdjacent
          for i := m to LeafPageSize - 1 do
            begin
              Inc(adjacentPage.Max);
              for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[i,j];
              for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
                adjacentPage.InheritedKeys[adjacentPage.Max,j] := nodePage.InheritedKeys[i,j];
              for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
                adjacentPage.DataPointers[adjacentPage.Max,j] := nodePage.DataPointers[i,j];
            end;

          nodePage.Max := m - 1;
          PutLRUIndex(node,nodePage);
          PutLRUIndex(nodeAdjacent,adjacentPage);

          if compare(keys,V1(*******),DP,DP1(*************)) < 0 then
            begin
              Add_DataPointerKeyItem(node, Keys,InheritedKeys,DP);
              GetLRUIndex(node,nodePage)
            end else
            begin
              Add_DataPointerKeyItem(nodeAdjacent, Keys, InheritedKeys,DP);
             (************* GetLRUIndex(node,nodePage);**************)
              GetLRUIndex(nodeAdjacent,adjacentPage)
            end
        end else
        begin
          if compare(Keys,V1) = 0 then // V is smallest value to go to nodeAdjacent
            begin
              // add child,node.Km,...,node.Pn-1,node.Kn-1,node.Pn to nodeAdjacent
              Inc(adjacentPage.Max);
              adjacentPage.Pointers[adjacentPage.Max] := child;
              setlength(adjacentPage.Keys[adjacentPage.Max],length(FKeys));
              for j := Low(nodePage.Keys[m]) to high(nodePage.Keys[m]) do
                adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[m,j];

                for i := m + 1 to nodePage.Max do
                  begin
                    Inc(adjacentPage.Max);
                    setlength(adjacentPage.Keys[adjacentPage.Max],length(FKeys));
                    for j := Low(nodePage.Keys[i]) to high(nodePage.Keys[i]) do
                      adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[i,j];
                    adjacentPage.Pointers[adjacentPage.Max] := nodePage.Pointers[i]
                  end;

                adjacentPage.Pointers[adjacentPage.Max+1] := nodePage.Pointers[nodePage.Max+1];
                (***************************************
                PutLRUIndex(node,nodePage);
                PutLRUIndex(nodeAdjacent,adjacentPage);
                GetLRUIndex(node,nodePage);
                GetLRUIndex(nodeAdjacent,adjacentPage)
                ***************************************)
            end else
            begin

              // add node.Pm,...,node.Pn-1,node.Kn-1,node.Pn to nodeAdjacent
              for i := m to nodePage.Max do
                begin
                  Inc(adjacentPage.Max);
                  setlength(adjacentPage.Keys[adjacentPage.Max],length(FKeys));
                  for j := Low(nodePage.Keys[i]) to high(nodePage.Keys[i]) do
                    adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[i,j];
                  adjacentPage.Pointers[adjacentPage.Max] := nodePage.Pointers[i]
                end;
              adjacentPage.Pointers[adjacentPage.Max+1] := nodePage.Pointers[nodePage.Max+1]
            end;

          // delete node.Km,...,node.Pn-1,node.Kn-1,node.Pn to node
          nodePage.Max := m - 1;
          PutLRUIndex(node,nodePage);
          PutLRUIndex(nodeAdjacent,adjacentPage);
          if compare(Keys,V1) < 0 then
            begin
              Add_KeyTreePointerItem(node, child, Keys);
              GetLRUIndex(node,nodePage)
            end else
            if compare(Keys,V1) > 0 then
              begin
                Add_KeyTreePointerItem(nodeAdjacent, child, Keys);
                (****************GetLRUIndex(node,nodePage);*****************)
                GetLRUIndex(nodeAdjacent,adjacentPage)
              end
              // Case of V = V1 handled already
        end;

      Root := GetRoot;
      if node <> root then
        Add_Entry(getParent(node),nodeAdjacent,V1,VIK1,DP1) // P1 will be a pointer to a page
       else
        begin
          for i := 1 to  NonLeafPageSize - 1 do
            begin
              setlength(NewPage.keys[i],length(FKeys));
              for j := 0 to Length(FKeys) - 1 do
                NewPage.keys[i,j] := Unassigned;
            end;
          for i := 1 to  NonLeafPageSize do
            NewPage.Pointers[i] := nullValue;

          NewPage.Leaf := False;
          NewPage.Max := 1;

          //NewPage.PreviousPointer := -1;

          NewPage.Pointers[1] := node;

          for j := Low(V1) to High(V1) do
            NewPage.Keys[1,j] := V1[j];

          NewPage.Pointers[2] := nodeAdjacent;

          //NewPage.NextPointer := -1;

          Root := File_Size;
          Inc(File_Size);
          PutLRUIndex(Root,NewPage);
          SetRoot(Root)
        end;

      {$IFDEF Duplicates}
      if( nodePage.NextPointer <> NullValue) then
        begin
          GetLRUIndex(nodePage.NextPointer,TmpPage);
          TmpPage.PreviousPointer := nodeAdjacent;
          PutLRUIndex(nodePage.NextPointer,TmpPage)
        end;
      adjacentPage.PreviousPointer := node;

      adjacentPage.NextPointer := nodePage.NextPointer;
      nodePage.NextPointer := nodeAdjacent;
      PutLRUIndex(node,nodePage);
      PutLRUIndex(nodeAdjacent,adjacentPage)
      {$ELSE}
      if nodePage.Leaf then
        begin
          // could work without Leaf too to get previous sibling
          if( nodePage.NextPointer <> NullValue) then
            begin
              GetLRUIndex(nodePage.NextPointer,TmpPage);
              TmpPage.PreviousPointer := nodeAdjacent;
              PutLRUIndex(nodePage.NextPointer,TmpPage)
            end;
          adjacentPage.PreviousPointer := node;

          adjacentPage.NextPointer := nodePage.NextPointer;
          nodePage.NextPointer := nodeAdjacent;
          PutLRUIndex(node,nodePage);
          PutLRUIndex(nodeAdjacent,adjacentPage)
        end
      {$ENDIF}

    end
end;

procedure BtrPlusClass.Add_Entry(node: IndexPointerType;
                                 Keys: array of variant;
                                 InheritedKeys: array of variant;
                                 DP: array of DataPointerType);
begin
  Add_Entry(node,nullValue,Keys,InheritedKeys,DP)
end;

procedure BtrPlusClass.AddKey(Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i, j: Integer;
  NewPage: IndexPage;
  index: Integer;
begin
  Root := GetRoot;
  ParentsInfo := nil;
  if root = nullValue then
    begin

      // create an empty index page

      NewPage.Leaf := True;
      NewPage.Max := 0;
      NewPage.PreviousPointer := nullValue;
      NewPage.NextPointer := nullValue;



      for i := 1 to  LeafPageSize - 1 do
        begin
          //SetLength(NewPage.Keys[i],Length(FKeys));
          //SetLength(NewPage.InheritedKeys[i],Length(FInheritedKeys));
          //SetLength(NewPage.DataPointers[i],FNumberOfDataRef);
          setlength(NewPage.keys[i],length(FKeys));
          for j := 0 to Length(FKeys) - 1 do
            NewPage.keys[i,j] := Unassigned;
          setlength(NewPage.Inheritedkeys[i],length(FInheritedKeys));
          for j := 0 to Length(FInheritedKeys) - 1 do
            NewPage.Inheritedkeys[i,j] := Unassigned;
          setlength(NewPage.DataPointers[i],FNumberOfDataRef);
          for j := 0 to FNumberOfDataRef - 1 do
              NewPage.DataPointers[i,j] := NullDataValue;
       end;

      for i := 0 to  LeafPageSize do
        NewPage.Pointers[i] := nullValue; // Not necessary

      Root := File_Size;
      Inc(File_Size);
      PutLRUIndex(Root,NewPage); // NewPage should be an empty page
      SetRoot(Root);
      TmpPoint := Root

    end else
    begin

      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := internalPage.Max + 1;
          for i := 1 to InternalPage.Max  do
            if compare(Keys,InternalPage.Keys[i]) < 0 then
              begin
                index := i;
                Break;
              end;
          SetLength(ParentsInfo,Length(ParentsInfo)+1);
          ParentsInfo[High(ParentsInfo)].Node := InternalPage.Pointers[index];
          ParentsInfo[High(ParentsInfo)].ParentNode := TmpPoint;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end
    end;
  Add_Entry(TmpPoint, Keys, InheritedKeys, DataRef)
end;

procedure BtrPlusClass.AddKey(Keys: array of variant; aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType = nil;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  DataRef[High(DataRef)] := aDataRef;
  addkey(keys,InheritedKeys,DataRef);
end;

function BtrPlusClass.Delete_KeyTreePointerItem(node: IndexPointerType;
                                                child: IndexPointerType;
                                                Keys: array of variant): Boolean;
var
  nodePage: IndexPage;
  i, j: Integer;
  KeyPosition, PointerPosition: Integer;
  {$IFDEF Duplicates}
  tmpNode: IndexPointerType;
  {$ENDIF}
begin
  result := False;
  {$IFDEF Duplicates}
  tmpNode := node;
  while tmpNode <> nullValue do
    begin
      GetLRUIndex(tmpnode,nodePage);
      tmpNode := nullValue;
      for i := 1 to nodePage.Max do
        if compare(Keys,nodePage.Keys[i]) = 0 then
          if (child = nodePage.Pointers[i+1]) then
            begin
              // Continue with nodePage.NextPointer
              result := True;
              Break
            end;

      if not result then
        if nodePage.Max <> 0 then
          if compare(Keys,nodePage.Keys[nodePage.Max]) = 0 then
            tmpNode := nodePage.NextPointer else
            Exit else
        Exit
    end;
  {$ELSE}
  GetLRUIndex(node,nodePage);
  // search in the page for the right key
  for i := 1 to nodePage.Max do
    if compare(Keys,nodePage.Keys[i]) = 0 then
      if (child = nodePage.Pointers[i+1]) then
        begin
          // Continue with nodePage.NextPointer
          result := True;
          Break
        end;
  if not result then Exit;
  {$ENDIF}
  KeyPosition := i;
  for i := keyPosition to nodePage.Max-1 do
    begin
      for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
        nodePage.Keys[i,j] := nodePage.Keys[i+1,j];
    end;
  PointerPosition := KeyPosition + 1;
  for i := PointerPosition to nodePage.Max do
    nodePage.Pointers[i] := nodePage.Pointers[i+1];
  Dec(nodePage.Max);
  PutLRUIndex(node,nodePage)
end;

function BtrPlusClass.Delete_DataPointerKeyItem(node: IndexPointerType;
                                                Keys: array of variant;
                                                DP: array of DataPointerType): Boolean;
var
  nodePage: IndexPage;
  i, j: Integer;
  KeyPosition: Integer;
  {$IFDEF Duplicates}
  tmpNode: IndexPointerType;
  {$ENDIF}
begin
  result := False;
  {$IFDEF Duplicates}
  tmpNode := node;
  while tmpNode <> nullValue do
    begin
      GetLRUIndex(tmpnode,nodePage);
      tmpNode := nullValue;
      for i := 1 to nodePage.Max do
        if compare(Keys,nodePage.Keys[i],DP,nodePage.DataPointers[i]) = 0 then
          begin
            result := True;
            Break
          end;
      if not result then
        if nodePage.Max <> 0 then
          if compare(Keys,nodePage.Keys[nodePage.Max]) = 0 then
            tmpNode := nodePage.NextPointer else
            Exit else
        Exit
    end;
  {$ELSE}
  GetLRUIndex(node,nodePage);
  for i := 1 to nodePage.Max do
    if compare(Keys,nodePage.Keys[i],DP,nodePage.DataPointers[i]) = 0 then
      begin
        result := True;
        Break
      end;
  if not result then Exit;
  {$ENDIF}
  KeyPosition := i;
  for i := keyPosition to nodePage.Max - 1  do
    begin
      for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
        nodePage.Keys[i,j] := nodePage.Keys[i+1,j];
      for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
        nodePage.InheritedKeys[i,j] := nodePage.InheritedKeys[i+1,j];
      for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
        nodePage.DataPointers[i,j] := nodePage.DataPointers[i+1,j];
    end;
  Dec(nodePage.Max);
  PutLRUIndex(node,nodePage)
end;

procedure BtrPlusClass.Delete_Entry(node: IndexPointerType;
                                    child: IndexPointerType;
                                    Keys: array of variant;
                                    DP: array of DataPointerType);
var
  nodePage: IndexPage;
  Root: IndexPointerType;
  i, j, k: Integer;
  parentPage: IndexPage;
  adjacentPage: IndexPage;
  NextPage: IndexPage;
  Count: Integer;
  Coalesce: Boolean;
  nodeAdjacent: IndexPointerType;
  V1: array of variant;
  predecessor: Boolean;
  tmpKey: array of variant;
  tmpDP: array of DataPointerType;
  tmpInheritedKey: array of variant;
  tmpPointer: IndexPointerType;
  control: Boolean;
begin

  // Search for KEY using search operation
  // we will reach a leaf and get “Found”
  // Delete KEY from that leaf

  GetLRUIndex(node,nodePage);
  if nodePage.Leaf then
    Delete_DataPointerKeyItem(node, Keys, DP) else
    Delete_KeyTreePointerItem(node, child, Keys);
  Root := GetRoot;
  if node = root then // if (node is the root and node has only one remaining child) then
    begin
      GetLRUIndex(node,nodePage);
      if nodePage.Max = 0 then
        begin

          // What if the root becomes empty during merging of nodes?
          // make the only-one child of the original root to be the new root

          // with recursive call, may coalesce nodes occurs, so the root becomes with one valid pointer
          // make the child of node the new root of the tree and delete node
          // Normally work with any case of child Leaf or Non Leaf
          if nodePage.Leaf then Root := -1 else
            Root := nodePage.Pointers[1];
          SetRoot(Root)

           // delete node
        end
    end else
    begin
      if nodePage.Leaf then
        Control := nodePage.Max <= (LeafPageSize div 2) else
        Control := nodePage.Max <= (NonLeafPageSize div 2);
      GetLRUIndex(node,nodePage);
      if Control then // if (node has too few values/pointers) then
        begin
          // Let nodeAdjacent be the previous or next child of parent(node)
          Coalesce := False;
          GetLRUIndex(getParent(node),parentPage);
          // Could have a parent because node is not a root
          // parent(node) is not a leaf
          // try to found the one with who coalesce, because coalesce is easier than redistribute
          // Let V1 be the value between pointers node and nodeAdjacent in parent(node)
          for i := 1 to parentPage.Max + 1 do
            if parentPage.Pointers[i] = node then
              begin
                // node should be found
                // i represent the index of the pointer to node
                // Take nodeAdjacent and V1

                SetLength(V1,Length(FKeys));

                if nodePage.Leaf then
                  Count := 0
                 else
                  Count := 1; // We add the key from parent

                // Lending of child:
                if i = 1 then
                  begin
                    // if the right sibling of the current node contains >= (ceil(node/2)+1) keys
                    // borrow the leftmost child from it
                    predecessor := False;
                    nodeAdjacent := parentPage.Pointers[i+1];
                    GetLRUIndex(nodeAdjacent,adjacentPage);

                    for j := Low(parentPage.Keys[i]) to High(parentPage.Keys[i]) do
                      V1[j] := parentPage.Keys[i,j];

                    if nodePage.Leaf then
                      begin
                        if (nodePage.Max + adjacentPage.Max + Count) <= (LeafPageSize - 1) then Coalesce := True
                      end else
                      begin
                        if (nodePage.Max + adjacentPage.Max + Count) <= (NonLeafPageSize - 1) then Coalesce := True
                      end
                  end;
                if (i > 1) and (i < parentPage.Max + 1) then
                  begin
                    if nodePage.Leaf then
                      begin
                        // Case 1: if the left sibling of the current node contains >= (ceil(node/2)+1) keys
                        // borrow the rightmost child from it

                        nodeAdjacent := parentPage.Pointers[i-1];
                        GetLRUIndex(nodeAdjacent,adjacentPage);
                        if (nodePage.Max + adjacentPage.Max + Count) <= (LeafPageSize - 1) then
                          begin
                            predecessor := True;

                            for j := Low(parentPage.Keys[i-1]) to High(parentPage.Keys[i-1]) do
                              V1[j] := parentPage.Keys[i-1,j];

                            Coalesce := True
                          end
                         else
                          begin

                            // Case 2: if the right sibling of the current node contains >= (ceil(node/2)+1) keys
                            // borrow the leftmost child from it

                            nodeAdjacent := parentPage.Pointers[i+1];
                            GetLRUIndex(nodeAdjacent,adjacentPage);

                            if (nodePage.Max + adjacentPage.Max + Count) <= (LeafPageSize - 1) then
                              begin
                                predecessor := False;

                                for j := Low(parentPage.Keys[i]) to High(parentPage.Keys[i]) do
                                  V1[j] := parentPage.Keys[i,j];

                                Coalesce := True
                              end
                          end
                      end else
                      begin
                        // Case 1: if the left sibling of the current node contains >= (ceil(node/2)+1) keys
                        // borrow the rightmost child from it

                        nodeAdjacent := parentPage.Pointers[i-1];
                        GetLRUIndex(nodeAdjacent,adjacentPage);
                        if (nodePage.Max + adjacentPage.Max + Count) <= (NonLeafPageSize - 1) then
                          begin
                            predecessor := True;

                            // be sure here no problem because Pointer exists

                            for j := Low(parentPage.Keys[i-1]) to High(parentPage.Keys[i-1]) do
                              V1[j] := parentPage.Keys[i-1,j];

                            Coalesce := True
                          end else
                          begin
                            // Case 2: if the right sibling of the current node contains >= (ceil(node/2)+1) keys
                            // borrow the leftmost child from it
                            nodeAdjacent := parentPage.Pointers[i+1];
                            GetLRUIndex(nodeAdjacent,adjacentPage);
                            if nodePage.Max + adjacentPage.Max + Count <= (NonLeafPageSize - 1) then
                              begin
                                predecessor := False;

                                for j := Low(parentPage.Keys[i]) to High(parentPage.Keys[i]) do
                                  V1[j] := parentPage.Keys[i,j];
                                Coalesce := True
                              end
                          end
                      end
                  end;
                if (i =  parentPage.Max + 1) and (parentPage.Max > 0 ) then
                  begin
                    // if the left sibling of the current node contains >= (ceil(node/2)+1) keys
                    // borrow the rightmost child from it
                    predecessor := True;
                    nodeAdjacent := parentPage.Pointers[i-1];
                    GetLRUIndex(nodeAdjacent,adjacentPage);

                    for j := Low(parentPage.Keys[i-1]) to High(parentPage.Keys[i-1]) do
                      V1[j] := parentPage.Keys[i-1,j];
                    if nodePage.Leaf then
                      begin
                        if (nodePage.Max + adjacentPage.Max + Count) <= (LeafPageSize-1) then Coalesce := True
                      end else
                      begin
                        if (nodePage.Max + adjacentPage.Max + Count) <= (NonLeafPageSize-1) then Coalesce := True
                      end
                  end
              end;

           GetLRUIndex(node,nodePage);
          // if (entries in node and nodeAdjacent can fit in a single node) then
          if coalesce then
            begin // Coalesce nodes
              // if (node is a predecessor of nodeAdjacent) then swap_variables(node,nodeAdjacent)
              if predecessor then
                begin
                  // if (node is not a leaf) then
                  if not (nodePage.Leaf) then
                    begin
                      // append V1 and all pointers and values in node to nodeAdjacent else
                      Inc(adjacentPage.Max);
                      for j := Low(V1) to High(V1) do adjacentPage.keys[adjacentPage.Max,j] := V1[j];

                      for i := 1 to nodePage.Max do
                        begin
                          Inc(adjacentPage.Max);
                          for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                            adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[i,j];
                          adjacentPage.Pointers[adjacentPage.Max] := nodePage.Pointers[i]
                        end;
                      adjacentPage.Pointers[adjacentPage.Max+1] := nodePage.Pointers[nodePage.Max+1];
                      PutLRUIndex(nodeAdjacent,adjacentPage)

                    end else
                    begin
                      // append all (Ki,Pi) pairs in node to nodeAdjacent; set nodeAdjacent.Pn = node.Pn
                      for i := 1 to nodePage.Max do
                        begin
                          Inc(adjacentPage.Max);
                          for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                            adjacentPage.Keys[adjacentPage.Max,j] := nodePage.Keys[i,j];
                          for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
                            adjacentPage.InheritedKeys[adjacentPage.Max,j] := nodePage.InheritedKeys[i,j];
                          for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
                            adjacentPage.DataPointers[adjacentPage.Max,j] := nodePage.DataPointers[i,j];
                        end;
                      adjacentPage.NextPointer := nodePage.NextPointer;
                      PutLRUIndex(nodeAdjacent,adjacentPage)
                    end;

                  //******************************************************
                  if nodePage.NextPointer <> nullValue then
                    begin
                      GetLRUIndex(nodePage.NextPointer,NextPage);
                      NextPage.PreviousPointer := nodeAdjacent;
                      PutLRUIndex(nodePage.NextPointer,NextPage);
                    end;
                  //******************************************************

                  //delete_Entry(getparent(node),V1,node);
                  delete_Entry(getparent(node),node,V1); // delete node node
                  // there is no datapointers in parent
                  // dispose(node)
                end else
                begin
                  // if (node is not a leaf) then
                  if not (nodePage.Leaf) then
                    begin
                      // append V1 and all pointers and values in nodeAdjacent to node else
                      Inc(nodePage.Max);

                      for j := Low(V1) to High(V1) do nodePage.keys[nodePage.Max,j] := V1[j];
                      for i := 1 to adjacentPage.Max do
                        begin
                          Inc(nodePage.Max);
                          for j := Low(adjacentPage.Keys[i]) to High(adjacentPage.Keys[i]) do
                            nodePage.Keys[nodePage.Max,j] := adjacentPage.Keys[i,j];
                          nodePage.Pointers[nodePage.Max] := adjacentPage.Pointers[i]
                        end;
                      nodePage.Pointers[nodePage.Max+1] := adjacentPage.Pointers[adjacentPage.Max+1];
                      PutLRUIndex(node,nodePage)
                    end else
                    begin
                      // append all (Ki,Pi) pairs in nodeAdjacent to node; set node.Pn = nodeAdjacent.Pn
                      for i := 1 to adjacentPage.Max do
                        begin
                          Inc(nodePage.Max);
                          for j := Low(adjacentPage.Keys[i]) to High(adjacentPage.Keys[i]) do
                            nodePage.Keys[nodePage.Max,j] := adjacentPage.Keys[i,j];
                          for j := Low(adjacentPage.InheritedKeys[i]) to High(adjacentPage.InheritedKeys[i]) do
                            nodePage.InheritedKeys[nodePage.Max,j] := adjacentPage.InheritedKeys[i,j];
                          for j := Low(adjacentPage.DataPointers[i]) to High(adjacentPage.DataPointers[i]) do
                            nodePage.DataPointers[nodePage.Max,j] := adjacentPage.DataPointers[i,j];
                        end;
                      nodePage.NextPointer := adjacentPage.NextPointer;
                      PutLRUIndex(node,nodePage)
                    end;

                  //******************************************************
                  if adjacentPage.NextPointer <> nullValue then
                    begin
                      GetLRUIndex(adjacentPage.NextPointer,NextPage);
                      NextPage.PreviousPointer := node;
                      PutLRUIndex(adjacentPage.NextPointer,NextPage);
                    end;
                  //******************************************************

                  // delete_Entry(getparent(node),V1,nodeAdjacent);
                  delete_Entry(getparent(node),nodeAdjacent,V1); // delete node nodeAdjacent
                  // there is no datapointers in parent
                  // dispose(nodeAdjacent)
                end
            end
           else
            begin // Redistribution: borrow an entry from nodeAdjacent
              // if (nodeAdjacent is a predecessor of node) then
              if predecessor then
                begin
                  SetLength(tmpKey,Length(FKeys));
                  // if (node is a non-leaf node) then
                  if not(nodePage.Leaf) then
                    begin
                      // let m be such that nodeAdjacent.Pm is the last pointer in nodeAdjacent
                      for j := Low(adjacentPage.Keys[adjacentPage.Max]) to High(adjacentPage.Keys[adjacentPage.Max]) do
                        tmpKey[j] := adjacentPage.Keys[adjacentPage.Max,j];
                      tmpPointer := adjacentPage.Pointers[adjacentPage.Max+1];
                      // remove (nodeAdjacent.Km-1,nodeAdjacent.Pm) from nodeAdjacent
                      Dec(adjacentPage.Max);
                      PutLRUIndex(nodeAdjacent,adjacentPage);
                      // insert (nodeAdjacent.Pm,V1) as the first pointer and value in node, by shifting other pointers and values right
                      for i := nodePage.Max downto 1 do
                        for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                          nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
                      for i := nodePage.Max+1 downto 1 do
                        nodePage.Pointers[i+1] := nodePage.Pointers[i];
                      Inc(nodePage.Max);
                      for j := Low(V1) to High(V1) do
                        nodePage.Keys[1,j] := V1[j];
                      nodePage.Pointers[1] := tmpPointer;
                      // replace V1 in parent(node) by nodeAdjacent.km-1
                      control := true;
                      for i := 1 to parentPage.Max do
                        begin
                          for j := Low(V1) to High(V1) do
                            control := control and (parentPage.Keys[i,j] = V1[j]);
                            if control then
                              begin
                                for k := Low(tmpKey) to High(tmpKey) do
                                  ParentPage.Keys[i,k] := tmpKey[k];
                                Break
                              end
                        end;
                      PutLRUIndex(node,nodePage);
                      PutLRUIndex(GetParent(node),parentPage)
                    end else
                    begin
                      // let m be such that (nodeAdjacent.Pm,nodeAdjacent.Km) is the last pointer/value pair in nodeAdjacent
                      for j := Low(adjacentPage.Keys[adjacentPage.Max]) to High(adjacentPage.Keys[adjacentPage.Max]) do
                        tmpKey[j] := adjacentPage.Keys[adjacentPage.Max,j];
                      SetLength(tmpInheritedKey,Length(FInheritedKeys));
                      for j := Low(adjacentPage.InheritedKeys[adjacentPage.Max]) to High(adjacentPage.InheritedKeys[adjacentPage.Max]) do
                        tmpInheritedKey[j] := adjacentPage.InheritedKeys[adjacentPage.Max,j];
                      SetLength(tmpDP,FNumberOfDataRef);
                      for j := Low(adjacentPage.DataPointers[adjacentPage.Max]) to High(adjacentPage.DataPointers[adjacentPage.Max]) do
                        tmpDP[j] := adjacentPage.DataPointers[adjacentPage.Max,j];

                      // remove (nodeAdjacent.Pm,nodeAdjacent.Km) from nodeAdjacent
                      Dec(adjacentPage.Max);
                      PutLRUIndex(nodeAdjacent,adjacentPage);
                      // insert (nodeAdjacent.Pm,nodeAdjacent.Km) as the first pointer and value in node, by shifting other pointers and values right
                      for i := nodePage.Max downto 1 do
                        begin
                          for j := Low(nodePage.Keys[i]) to High(nodePage.Keys[i]) do
                            nodePage.Keys[i+1,j] := nodePage.Keys[i,j];
                          for j := Low(nodePage.InheritedKeys[i]) to High(nodePage.InheritedKeys[i]) do
                            nodePage.InheritedKeys[i+1,j] := nodePage.InheritedKeys[i,j];
                          for j := Low(nodePage.DataPointers[i]) to High(nodePage.DataPointers[i]) do
                            nodePage.InheritedKeys[i+1,j] := nodePage.InheritedKeys[i,j];
                        end;
                      Inc(nodePage.Max);
                      for j := Low(tmpKey) to High(tmpKey) do
                        nodePage.Keys[1,j] := tmpKey[j];
                      for j := Low(tmpInheritedKey) to High(tmpInheritedKey) do
                        nodePage.InheritedKeys[1,j] := tmpInheritedKey[j];
                      for j := Low(tmpDP) to High(tmpDP) do
                        nodePage.DataPointers[1,j] := tmpDP[j];

                      // replace V1 in parent(node) by nodeAdjacent.km
                      control := true;
                      for i := 1 to parentPage.Max do
                        begin
                          for j := Low(V1) to High(V1) do
                            control := control and (parentPage.Keys[i,j] = V1[j]);
                            if control then
                              begin
                                for k := Low(tmpKey) to High(tmpKey) do
                                  ParentPage.Keys[i,k] := tmpKey[k];
                                Break
                              end
                        end;
                      PutLRUIndex(node,nodePage);
                      PutLRUIndex(GetParent(node),parentPage)
                    end
                end else // ... symmetric to the then case
                begin
                   SetLength(tmpKey,Length(FKeys));
                  // if (node is a non-leaf node) then
                  if not(nodePage.Leaf) then
                    begin
                      // let m be such that nodeAdjacent.P1 is the first pointer in nodeAdjacent
                      for j := Low(nodePage.Keys[2]) to High(nodePage.Keys[2]) do
                        tmpKey[j] := nodePage.Keys[2,j];
                      tmpPointer := nodePage.Pointers[2];
                      // remove (nodeAdjacent.K1,nodeAdjacent.P1) from nodeAdjacent

                      for i := 2 to adjacentPage.Max do
                        for j := Low(adjacentPage.Keys[i]) to High(adjacentPage.Keys[i]) do
                          adjacentPage.Keys[i-1,j] := adjacentPage.Keys[i,j];
                      for i := 2 to adjacentPage.Max + 1 do
                        adjacentPage.Pointers[i-1] := adjacentPage.Pointers[i];

                      Dec(adjacentPage.Max);
                      PutLRUIndex(nodeAdjacent,adjacentPage);

                      // insert (V1,nodeAdjacent.P1) as the last value and pointer in node

                      Inc(nodePage.Max);
                      for j := Low(V1) to High(V1) do
                        nodePage.Keys[nodePage.Max,j] := V1[j];
                      adjacentPage.Pointers[nodePage.Max+1] := tmpPointer;
                      // replace V1 in parent(node) by nodeAdjacent.k2
                      control := true;
                      for i := 1 to parentPage.Max do
                        begin
                          for j := Low(V1) to High(V1) do
                            control := control and (parentPage.Keys[i,j] = V1[j]);
                            if control then
                              begin
                                for k := Low(tmpKey) to High(tmpKey) do
                                  ParentPage.Keys[i,k] := tmpKey[k];
                                Break
                              end
                        end;
                      PutLRUIndex(node,nodePage);
                      PutLRUIndex(GetParent(node),parentPage);
                    end else
                    begin
                      // let m be such that nodeAdjacent.P1 is the first pointer pair in nodeAdjacent

                      for j := Low(nodePage.Keys[2]) to High(nodePage.Keys[2]) do
                        tmpKey[j] := nodePage.Keys[2,j];
                      SetLength(tmpInheritedKey,Length(FInheritedKeys));
                      for j := Low(nodePage.InheritedKeys[2]) to High(nodePage.InheritedKeys[2]) do
                        tmpInheritedKey[j] := nodePage.InheritedKeys[2,j];
                      SetLength(tmpDP,FNumberOfDataRef);
                      for j := Low(nodePage.DataPointers[2]) to High(nodePage.DataPointers[2]) do
                        tmpDP[j] := nodePage.DataPointers[2,j];

                      // remove (nodeAdjacent.K1,nodeAdjacent.P1) from nodeAdjacent
                      for i := 2 to adjacentPage.Max do
                        begin
                        for j := Low(adjacentPage.Keys[i]) to High(adjacentPage.Keys[i]) do
                          adjacentPage.Keys[i-1,j] := adjacentPage.Keys[i,j];
                        for j := Low(adjacentPage.InheritedKeys[i]) to High(adjacentPage.InheritedKeys[i]) do
                          adjacentPage.InheritedKeys[i-1,j] := adjacentPage.InheritedKeys[i,j];
                        for j := Low(adjacentPage.DataPointers[i]) to High(adjacentPage.DataPointers[i]) do
                          adjacentPage.DataPointers[i-1,j] := adjacentPage.DataPointers[i,j];
                        end;
                      Dec(adjacentPage.Max);
                      PutLRUIndex(nodeAdjacent,adjacentPage);

                      // insert V1 as the last value and pointer in node
                      Inc(nodePage.Max);
                      for j := Low(V1) to High(V1) do
                        nodePage.Keys[nodePage.Max,j] := V1[j];
                      for j := Low(tmpInheritedKey) to high(tmpInheritedKey) do
                        nodePage.InheritedKeys[nodePage.Max,j] := tmpInheritedKey[j];
                      for j := Low(tmpDP) to High(tmpDP) do
                        nodePage.DataPointers[nodePage.Max,j] := tmpDP[j];
                      adjacentPage.Pointers[1] := tmpPointer;

                      // replace V1 in parent(node) by nodeAdjacent.k2
                      control := true;
                      for i := 1 to parentPage.Max do
                        begin
                          for j := Low(V1) to High(V1) do
                            control := control and (parentPage.Keys[i,j] = V1[j]);
                            if control then
                              begin
                                for k := Low(tmpKey) to High(tmpKey) do
                                  ParentPage.Keys[i,k] := tmpKey[k];
                                Break
                              end
                        end;
                      PutLRUIndex(node,nodePage);
                      PutLRUIndex(GetParent(node),parentPage)
                    end
                end
            end;
          // PutLRUIndex(node,nodePage);
          // PutLRUIndex(nodeAdjacent,adjacentPage);
          // PutLRUIndex(GetParent(node),parentPage)
        end
    end
end;

procedure BtrPlusClass.Delete_Entry(node: IndexPointerType;
                                    child: IndexPointerType;
                                    Keys: array of variant);
begin
  Delete_Entry(node, child, Keys,[]);
end;

procedure BtrPlusClass.Delete_Entry(node: IndexPointerType;
                                    Keys: array of variant;
                                    DP: array of DataPointerType);
begin
  Delete_Entry(node,nullValue,Keys,DP)
end;

procedure BtrPlusClass.ReplaceKey(var Keys: array of variant; var DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i, j: Integer;
  index: integer;
begin
  FInitial := True;
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NulldataValue;
  Root := GetRoot;
  if Root <> nullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := InternalPage.Max + 1;

          for i := 1 to InternalPage.Max do
            if compare(Keys,InternalPage.Keys[i]) <= 0 then
              begin
                index := i;
                Break;
              end;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      for i := 1 to InternalPage.Max do
        if compare(Keys,InternalPage.Keys[i]) <= 0 then
          begin
            FInitial := False;
            CursorPage := InternalPage;
            CursorPosition := i;
            for j := Low(DataRef) to High(DataRef) do
              DataRef[j] := InternalPage.DataPointers[i,j];

            PutLRUIndex(TmpPoint,InternalPage);
            for j := Low(DataRef) to High(DataRef) do
              DataRef[j] := InternalPage.DataPointers[i,j];

            Break
          end;
      if FInitial then
        begin
          TmpPoint := InternalPage.NextPointer;
          if TmpPoint <> NullValue then
            begin
              GetLRUIndex(TmpPoint,InternalPage);
              FInitial := False;
              CursorPage := InternalPage;
              CursorPosition := 1;
              for j := Low(InternalPage.Keys[CursorPosition]) to High(InternalPage.Keys[CursorPosition]) do
                Keys[j] := InternalPage.Keys[CursorPosition,j];
              for j := Low(DataRef) to High(DataRef) do
                DataRef[j] := InternalPage.DataPointers[CursorPosition,j];

              PutLRUIndex(TmpPoint,InternalPage);
              for j := Low(DataRef) to High(DataRef) do
                DataRef[j] := InternalPage.DataPointers[CursorPosition,j];

            end
        end
    end
end;

procedure BtrPlusClass.internalSearchKey(var Keys: array of variant; var DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i, j: Integer;
  index: integer;
begin
  FInitial := True;
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NulldataValue;
  Root := GetRoot;
  if Root <> nullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := InternalPage.Max + 1;

          for i := 1 to InternalPage.Max do
            if compare(Keys,InternalPage.Keys[i]) <= 0 then
              begin
                index := i;
                Break;
              end;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      for i := 1 to InternalPage.Max do
        if compare(Keys,InternalPage.Keys[i]) <= 0 then
          begin
            FInitial := False;
            CursorPage := InternalPage;
            CursorPosition := i;
            for j := Low(InternalPage.Keys[i]) to High(InternalPage.Keys[i]) do
              Keys[j] := InternalPage.Keys[i,j];
            for j := Low(DataRef) to High(DataRef) do
              DataRef[j] := InternalPage.DataPointers[i,j];
            Break
          end;
      if FInitial then
        begin
          TmpPoint := InternalPage.NextPointer;
          if TmpPoint <> NullValue then
            begin
              GetLRUIndex(TmpPoint,InternalPage);
              FInitial := False;
              CursorPage := InternalPage;
              CursorPosition := 1;
              for j := Low(InternalPage.Keys[CursorPosition]) to High(InternalPage.Keys[CursorPosition]) do
                Keys[j] := InternalPage.Keys[CursorPosition,j];
              for j := Low(DataRef) to High(DataRef) do
                DataRef[j] := InternalPage.DataPointers[CursorPosition,j];
            end
        end
    end
end;

{
procedure BtrPlusClass.DeleteKey(Keys: array of variant; DataRef: array of DataPointerType);
begin
  internalfindkey(keys,DataRef);
  if DataRef[0] = NullDataValue then exit else
   ReplaceKey(Keys,DataRef);
end;
}

procedure BtrPlusClass.DeleteKey(Keys: array of variant; DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i: Integer;
  index: Integer;
begin
  Root := GetRoot;
  ParentsInfo := nil;
  if root <> nullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := internalPage.Max + 1;
          for i := 1 to InternalPage.Max  do
            if compare(Keys,InternalPage.Keys[i],DataRef,InternalPage.DataPointers[i]) < 0 then
              begin
                index := i;
                Break;
              end;
          SetLength(ParentsInfo,Length(ParentsInfo)+1);
          ParentsInfo[High(ParentsInfo)].Node := InternalPage.Pointers[index];
          ParentsInfo[High(ParentsInfo)].ParentNode := TmpPoint;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      Delete_Entry(TmpPoint, Keys, DataRef)
    end
end;

procedure BtrPlusClass.DeleteKey(Keys: array of variant; aDataRef: DataPointerType);
var
  DataRef: array of DataPointerType;
begin
  setLength(DataRef,1);
  DataRef[High(DataRef)] := aDataRef;
  DeleteKey(keys,DataRef)
end;

procedure BtrPlusClass.FindKey(Keys: array of variant; var InheritedKeys: array of variant; var DataRef: array of DataPointerType);
var
  TmpKeys: array of variant;
  // TmpInheritedKeys: array of variant;
  // tmpDataRef: array of DataPointerType;
  i: Integer;
  Control: Boolean;
begin
  SetLength(TmpKeys,Length(Keys));
  for i := Low(Keys) to High(Keys) do
    TmpKeys[i] := Keys[i];


  SearchKey(TmpKeys,InheritedKeys,DataRef);

  {
   setlength(tmpkeys,length(Keys));
   setlength(tmpInheritedkeys,length(InheritedKeys));
   setlength(tmpDataRef,length(DataRef));

   ClearKey;
   repeat
     nextkey(tmpkeys,tmpInheritedKeys,tmpdataref);
     control := tmpDataRef[0] <> NulldataValue;
     for i := Low(Keys) to High(Keys) do
       if not VarIsEmpty(Keys[i]) then
         Control := Control and (TmpKeys[i] = Keys[i]);
   until control or (tmpDataRef[0] = NulldataValue);

   if (tmpDataRef[0] <> NulldataValue) then
     begin
       for i := 0 to length(keys) - 1 do
         keys[i] := TmpKeys[i];
       for i := 0 to length(Inheritedkeys) - 1 do
         Inheritedkeys[i] := TmpInheritedKeys[i];
       for i := 0 to length(DataRef) - 1 do
         DataRef[i] := TmpDataRef[i];
     end;
  }

  Control := DataRef[0] <> NulldataValue;

  for i := Low(Keys) to High(Keys) do
    if not VarIsEmpty(Keys[i]) then
      Control := Control and (TmpKeys[i] = Keys[i]);

  if not Control then
    for i := Low(DataRef) to High(DataRef) do
      DataRef[i] := NulldataValue

end;

procedure BtrPlusClass.FindKey(Keys: array of variant; var aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  DataRef[High(DataRef)] := aDataRef;
  FindKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

procedure BtrPlusClass.SearchKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                 var DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i, j: Integer;
  index: integer;
begin
  FInitial := True;
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NulldataValue;
  Root := GetRoot;
  if Root <> nullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := InternalPage.Max + 1;

          for i := 1 to InternalPage.Max do
            if compare(Keys,InternalPage.Keys[i]) <= 0 then
              begin
                index := i;
                Break;
              end;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      for i := 1 to InternalPage.Max do
        if compare(Keys,InternalPage.Keys[i]) <= 0 then
          begin
            FInitial := False;
            CursorPage := InternalPage;
            CursorPosition := i;
            for j := Low(InternalPage.Keys[i]) to High(InternalPage.Keys[i]) do
              Keys[j] := InternalPage.Keys[i,j];
            for j := Low(InternalPage.InheritedKeys[i]) to High(InternalPage.InheritedKeys[i]) do
              InheritedKeys[j] := InternalPage.InheritedKeys[i,j];
            for j := Low(DataRef) to High(DataRef) do
              DataRef[j] := InternalPage.DataPointers[i,j];
            Break
          end;
      if FInitial then
        begin
          TmpPoint := InternalPage.NextPointer;
          if TmpPoint <> NullValue then
            begin
              GetLRUIndex(TmpPoint,InternalPage);
              FInitial := False;
              CursorPage := InternalPage;
              CursorPosition := 1;
              for j := Low(InternalPage.Keys[CursorPosition]) to High(InternalPage.Keys[CursorPosition]) do
                Keys[j] := InternalPage.Keys[CursorPosition,j];
              for j := Low(InternalPage.InheritedKeys[CursorPosition]) to High(InternalPage.InheritedKeys[CursorPosition]) do
                InheritedKeys[j] := InternalPage.InheritedKeys[CursorPosition,j];
              for j := Low(DataRef) to High(DataRef) do
                DataRef[j] := InternalPage.DataPointers[CursorPosition,j];
            end
        end
    end
end;

procedure BtrPlusClass.SearchKey(var Keys: array of variant; var aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  DataRef[High(DataRef)] := aDataRef;
  SearchKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

procedure BtrPlusClass.AddInheritedKey(Keys: array of variant;
                                       InheritedKeyName: string;
                                       InheritedKeyValue: variant);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  i, j: Integer;
  index: integer;

begin
  FInitial := True;
  Root := GetRoot;
  if Root <> nullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          index := InternalPage.Max + 1;

          for i := 1 to InternalPage.Max do
            if compare(Keys,InternalPage.Keys[i]) <= 0 then
              begin
                index := i;
                Break;
              end;
          TmpPoint := InternalPage.Pointers[index];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      for i := 1 to InternalPage.Max do
        if compare(Keys,InternalPage.Keys[i]) <= 0 then
          begin
            FInitial := False;
            CursorPage := InternalPage;
            CursorPosition := i;
            for j := Low(InternalPage.Keys[i]) to High(InternalPage.Keys[i]) do
              if Keys[j] = InternalPage.Keys[i,j] then break;
            for j := low(InternalPage.InheritedKeys[CursorPosition]) to high(InternalPage.InheritedKeys[CursorPosition]) do
              if InheritedKeycolName(j) = InheritedKeyName then
                begin
                  InternalPage.InheritedKeys[i,j] := InheritedKeyValue;
                  PutLRUIndex(TmpPoint,InternalPage);
                  Break;
                end;
            Break
          end;
      if FInitial then
        begin
          TmpPoint := InternalPage.NextPointer;
          if TmpPoint <> NullValue then
            begin
              GetLRUIndex(TmpPoint,InternalPage);
              FInitial := False;
              CursorPage := InternalPage;
              CursorPosition := 1;

              for j := Low(InternalPage.Keys[CursorPosition]) to High(InternalPage.Keys[CursorPosition]) do
                if Keys[j] = InternalPage.Keys[CursorPosition,j] then break;
              for j := low(InternalPage.InheritedKeys[CursorPosition]) to high(InternalPage.InheritedKeys[CursorPosition]) do
                if InheritedKeycolName(j) = InheritedKeyName then
                  begin
                    InternalPage.InheritedKeys[CursorPosition,j] := InheritedKeyValue;
                    PutLRUIndex(TmpPoint,InternalPage);
                    Break;
                  end;

            end
        end
    end
end;

procedure BtrPlusClass.internalFindKey(Keys: array of variant; var DataRef: array of DataPointerType);
var
  TmpKeys: array of variant;
  // TmpInheritedKeys: array of variant;
  // tmpDataRef: array of DataPointerType;
  i: Integer;
  Control: Boolean;
begin
  SetLength(TmpKeys,Length(Keys));
  for i := Low(Keys) to High(Keys) do
    TmpKeys[i] := Keys[i];

  internalSearchKey(TmpKeys,DataRef);

  Control := DataRef[0] <> NulldataValue;

  for i := Low(Keys) to High(Keys) do
    if not VarIsEmpty(Keys[i]) then
      Control := Control and (TmpKeys[i] = Keys[i]);

  if not Control then
    for i := Low(DataRef) to High(DataRef) do
      DataRef[i] := NulldataValue

end;

procedure BtrPlusClass.PrevKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType);
begin
  internalPrevKey(Keys,InheritedKeys,DataRef)
end;

procedure BtrPlusClass.internalPrevKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                       out DataRef: array of DataPointerType);
var
  j: Integer;
  InternalPage: IndexPage;
begin
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NullDataValue;
  if FInitial then MaxKey(Keys,InheritedKeys,DataRef) else
    begin
      Dec(CursorPosition);
      if CursorPosition >= 1 then
        begin
          for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
            Keys[j] := CursorPage.Keys[CursorPosition,j];
          for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
            InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
          for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
            DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
        end else
        if CursorPage.PreviousPointer = NullValue then
          FInitial := True else
          begin
            GetLRUIndex(CursorPage.PreviousPointer,InternalPage);
            CursorPage := InternalPage;
            CursorPosition := InternalPage.Max;
            for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
              Keys[j] := CursorPage.Keys[CursorPosition,j];
            for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
              InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
            for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
              DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
          end
    end
end;

procedure BtrPlusClass.PrevKey(var Keys: array of variant; out aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  PrevKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

procedure BtrPlusClass.NextKey(var Keys: array of variant; var InheritedKeys: array of variant;
                               out DataRef: array of DataPointerType);
begin
    internalNextKey(Keys,InheritedKeys,DataRef)
end;

procedure BtrPlusClass.internalNextKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                       out DataRef: array of DataPointerType);
var
  InternalPage: IndexPage;
  j: Integer;
begin
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NullDataValue;
  if FInitial then MinKey(Keys,InheritedKeys,DataRef) else
    begin
      Inc(CursorPosition);
      if CursorPosition <= CursorPage.Max then
        begin
          for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
            Keys[j] := CursorPage.Keys[CursorPosition,j];
          for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
            InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
          for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
            DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
        end else
        if CursorPage.NextPointer = NullValue then
          FInitial := True else
          begin
            GetLRUIndex(CursorPage.NextPointer,InternalPage);
            CursorPage := InternalPage;
            CursorPosition := 1;
            for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
              Keys[j] := CursorPage.Keys[CursorPosition,j];
            for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
              InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
            for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
              DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
          end
    end
end;

procedure BtrPlusClass.NextKey(var Keys: array of variant; out aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  NextKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

procedure BtrPlusClass.MinKey(var Keys: array of variant; var InheritedKeys: array of variant; out DataRef: array of DataPointerType);
begin
  internalMinKey(Keys,InheritedKeys,DataRef)
end;

procedure BtrPlusClass.internalMinKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                      out DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  j: Integer;
begin
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NullDataValue;
  Root := GetRoot;
  if Root <> NullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          TmpPoint := InternalPage.Pointers[1];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      FInitial := False;
      CursorPage := InternalPage;
      CursorPosition := 1;
      for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
        Keys[j] := CursorPage.Keys[CursorPosition,j];
      for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
        InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
      for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
        DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
    end
end;

procedure BtrPlusClass.MinKey(var Keys: array of variant; out aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  MinKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

procedure BtrPlusClass.MaxKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                      out DataRef: array of DataPointerType);
begin
  internalMaxKey(Keys,InheritedKeys,DataRef)
end;

procedure BtrPlusClass.internalMaxKey(var Keys: array of variant; var InheritedKeys: array of variant;
                                      out DataRef: array of DataPointerType);
var
  Root: IndexPointerType;
  TmpPoint: IndexPointerType;
  InternalPage: IndexPage;
  j: Integer;
begin
  for j := Low(DataRef) to High(DataRef) do
    DataRef[j] := NullDataValue;
  Root := GetRoot;
  if Root <> NullValue then
    begin
      TmpPoint := Root;
      GetLRUIndex(TmpPoint,InternalPage);
      while not(InternalPage.Leaf) do
        begin
          TmpPoint := InternalPage.Pointers[InternalPage.Max+1];
          GetLRUIndex(TmpPoint,InternalPage)
        end;
      FInitial := False;
      CursorPage := InternalPage;
      CursorPosition := InternalPage.Max;
      for j := Low(CursorPage.Keys[CursorPosition]) to High(CursorPage.Keys[CursorPosition]) do
        Keys[j] := CursorPage.Keys[CursorPosition,j];
      for j := Low(CursorPage.InheritedKeys[CursorPosition]) to High(CursorPage.InheritedKeys[CursorPosition]) do
        InheritedKeys[j] := CursorPage.InheritedKeys[CursorPosition,j];
      for j := Low(CursorPage.DataPointers[CursorPosition]) to High(CursorPage.DataPointers[CursorPosition]) do
        DataRef[j] := CursorPage.DataPointers[CursorPosition,j];
    end
end;

procedure BtrPlusClass.MaxKey(var Keys: array of variant; out aDataRef: DataPointerType);
var
  InheritedKeys: array of variant;
  DataRef: array of DataPointerType;
begin
  InheritedKeys := nil;
  setLength(DataRef,1);
  MaxKey(keys,InheritedKeys,DataRef);
  aDataRef := DataRef[High(DataRef)];
end;

end.
