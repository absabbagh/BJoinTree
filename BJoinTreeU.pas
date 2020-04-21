unit BJoinTreeU;

interface

uses BPlusTreeU;

type

  DataDictionaryStructure = array of record
    case byte of
      0: (TableName: string[60]);
      1: (ColumnName: string[60];
          ColumnType: string[60];
          TableNameRef: string[60]);
      2: (AliasName: string[60];
          TableRef: string[60]);
    end;

  GraphStructure = array of record
     node: string;
     adjacents: array of record
       link: string;
       CommonKey: string
     end
  end;

  virtualTableStructure = array of string;
  RowStructure = array of array of dataPointerType;

  JoinPathGraphStructure = array of record
     node: virtualTableStructure;
     adjacent: record
       link: virtualTableStructure;
       Keys: array of record
         (*/////////// not necessary
         flagTA: Boolean; // Table / Adjacent
         flagIK: Boolean; // Key / Inherited key
         KeyIndex: Integer; // Postion of the index in Table / Adjacent
         *)
         FromTable: string;
         KeyName: string
       end;
       InheritedKeys: array of record
       (*/////////// not necessary
         flagTA: Boolean; // Table / Adjacent
         flagIK: Boolean; // Key / Inherited key
         KeyIndex: Integer; // Postion of the index in Table / Adjacent
       *)
         FromTable: string;
         KeyName: string
       end
     end
  end;

  BJoinTreeClass = class
    private
      FDataDictionary: DataDictionaryStructure;
      FileName: string;
      FBaseTables: array of string;
      DirectJoinList: array of record
        FromTable: string;
        ToTable: string;
        Key: string
      end;
      FJoinPathList: JoinPathGraphStructure;
      FBTrees: array of BtrPlusClass;
      FNewRows: RowStructure;
      FDeletedRows: RowStructure;
      function GetBaseTables: virtualTableStructure;
      function GetNewRows: RowStructure;
      function GetDeletedRows: RowStructure;
      procedure generateJoinGraph(BaseTables: array of string; out JoinGraph: GraphStructure);
      function eqlVirtualTables(Table1, Table2: virtualTableStructure): Boolean;
      function getIndexFromJoinGraph(JoinGraph: GraphStructure; FromTable: string): Integer;
      function getIndexFromJoinPathList(JoinPathList: JoinPathGraphStructure; FromTable: string): Integer; overload;
      function getIndexFromJoinPathList(JoinPathList: JoinPathGraphStructure; FromTable: virtualTableStructure): Integer; overload;
      procedure getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable, ToTable:  virtualTableStructure; out BaseTable: string; out key: string); overload; // should be array of string as return function for multi-columns key
      procedure getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable:  virtualTableStructure; ToTable: string; out BaseTable: string; out key: string); overload; // should be array of string as return function for multi-columns key
      procedure getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable: string; ToTable: virtualTableStructure; out BaseTable: string; out key: string); overload; // should be array of string as return function for multi-columns key
      procedure generateJoinPathList(JoinBaseTables: array of string; JoinGraph: GraphStructure; out JoinPathList: JoinPathGraphStructure;
                                     TheKeys: array of string);
      function getTypeFromDataDictionary(TheColumnName: string; TheTableNameRef: string): string;
      function getPositionFromDataDictionary(TheColumnName: string; TheTableNameRef: string): Integer;
      procedure AddJoinKey(TableName: virtualTableStructure; Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType);
      procedure DeleteJoinKey(TableName: virtualTableStructure; Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType);
    protected
    public
      constructor create(TheFileName: string; TheBaseTables: array of string);
      destructor destroy; override;
      procedure AddJoin(FromTheTable, ToTheTable: string; TheKey: string); virtual;
      procedure createBTrees(JoinBaseTables: array of string; IsOpen: Boolean;
                             TheKeys: array of string);

      function GetnumberOfKeys(BTreeIndex: Integer): Integer;
      function GetnumberOfInheritedKeys(BTreeIndex: Integer): Integer;
      function GetnumberOfDataRef(BTreeIndex: Integer): Integer;

      procedure AddKey(TableName: string; Row: array of variant; DataRef: DataPointerType);
      procedure DeleteKey(TableName: string; Row: array of variant; DataRef: DataPointerType);
      procedure AddTableToDictionary(TableName: string);
      procedure AddColumnToDictionary(ColumnName, ColumnType: string; TableNameRef: string);

      procedure ClearKey(BtreeIndex: Integer); overload;
      procedure PrevKey(BtreeIndex: Integer;
                        var Keys: array of variant;
                        var InheritedKeys: array of variant;
                        out DataRef: array of DataPointerType);  overload;
      procedure NextKey(BtreeIndex: Integer;
                        var Keys: array of variant;
                        var InheritedKeys: array of variant;
                        out DataRef: array of DataPointerType);  overload;
      procedure MinKey(BtreeIndex: Integer;
                        var Keys: array of variant;
                        var InheritedKeys: array of variant;
                        out DataRef: array of DataPointerType);  overload;
      procedure MaxKey(BtreeIndex: Integer;
                        var Keys: array of variant;
                        var InheritedKeys: array of variant;
                        out DataRef: array of DataPointerType);  overload;

      procedure ClearKey;  overload;
      procedure FindKey(Keys: array of variant; var DataRef: array of DataPointerType);
      procedure SearchKey(var Keys: array of variant; var DataRef: array of DataPointerType);
      procedure PrevKey(var Keys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure NextKey(var Keys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure MinKey(var Keys: array of variant; out DataRef: array of DataPointerType); overload;
      procedure MaxKey(var Keys: array of variant; out DataRef: array of DataPointerType); overload;
      property BaseTables: virtualTableStructure read GetBaseTables;
      property NewRows: RowStructure read GetNewRows;
      property DeletedRows: RowStructure read GetDeletedRows;
  end;

implementation

uses SysUtils;

{ TODO : a.b = c.d
a.b = c.b and a.e = and c.e
t.a = s.b opr expr
save as(t.a) (s.b xopr expr) where xopr is the oposite of opr
t1.k1 = t2.k2 call addkey with t.k1, t2.k2 for values in idx(t2.k2)
that equals to values in idx(t1.k1)
t.k1 = t2.k2 + 1 call addkey and find values in idx(t2.k2) that
satisfy idx(t1.k1) = idx(t2.k2) + 1 and call addkey with t.k1, t2.k2 - 1}

const
  nullDataValue = -1;//'';

function BJoinTreeClass.GetBaseTables: virtualTableStructure;
begin
  result := FJoinPathList[High(FBTrees)].node;
end;

function BJoinTreeClass.GetNewRows: RowStructure;
begin
  result := FNewRows;
end;

function BJoinTreeClass.GetDeletedRows: RowStructure;
begin
  result := FDeletedRows;
end;

function BJoinTreeClass.getTypeFromDataDictionary(TheColumnName: string; TheTableNameRef: string): string;
var
  i: Integer;
begin
  result := '';
  for i := Low(FDataDictionary) to High(FDataDictionary) do
    with FDataDictionary[i]do
      if (ColumnName = TheColumnName) and (TableNameRef = TheTableNameRef) then
        begin
          result := ColumnType;
          Exit
        end
end;

function BJoinTreeClass.getPositionFromDataDictionary(TheColumnName: string; TheTableNameRef: string): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := Low(FDataDictionary) to High(FDataDictionary) do
    with FDataDictionary[i]do
      if (TableNameRef = TheTableNameRef) then
        if (ColumnName = TheColumnName) then Exit else Inc(result);
end;

procedure BJoinTreeClass.AddTableToDictionary(TableName: string);
begin
  SetLength(FDataDictionary,Length(FDataDictionary)+1);
  FDataDictionary[High(FDataDictionary)].TableName := TableName
end;

procedure BJoinTreeClass.AddColumnToDictionary(ColumnName, ColumnType: string; TableNameRef: string);
begin
  SetLength(FDataDictionary,Length(FDataDictionary)+1);
  FDataDictionary[High(FDataDictionary)].ColumnName := ColumnName;
  FDataDictionary[High(FDataDictionary)].ColumnType := ColumnType;
  FDataDictionary[High(FDataDictionary)].TableNameRef := TableNameRef
end;

constructor BJoinTreeClass.create(TheFileName: string; TheBaseTables: array of string);
var
  i: Integer;
begin
  FileName := TheFileName;
  SetLength(FBaseTables,Length(TheBaseTables));
  for i := Low(TheBaseTables) to High(TheBaseTables) do
    FBaseTables[i] := TheBaseTables[i];
  DirectJoinList := nil;
  FBTrees := nil;
  FDataDictionary := nil;
  FNewRows := nil;
  FDeletedRows := nil;
end;

destructor BJoinTreeClass.destroy();
var
  i: Integer;
begin
  for i := Low(FBtrees) to High(FBtrees) do
    FBtrees[i].Free
end;

procedure BJoinTreeClass.AddJoin(FromTheTable, ToTheTable: string; TheKey: string);
begin
  SetLength(DirectJoinList,Length(DirectJoinList)+1);
  with DirectJoinList[High(DirectJoinList)] do
    begin
      FromTable := FromTheTable;
      ToTable := ToTheTable;
      Key := TheKey
    end
end;

function BJoinTreeClass.eqlVirtualTables(Table1, Table2: virtualTableStructure): Boolean;
var
  i, j: Integer;
  Control: Boolean;
begin
  result := Length(Table1) = Length(Table2);
  for i := Low(Table1) to High(Table1) do
    begin
      Control := False;
      for j := Low(Table2) to High(Table2) do
        if Table1[i] = Table2[j] then Control := True;
      result := result and Control
    end
end;

function BJoinTreeClass.getIndexFromJoinGraph(JoinGraph: GraphStructure; FromTable: string): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := Low(JoinGraph) to High(JoinGraph) do
    begin
      if FromTable = JoinGraph[i].node then
        begin
          result := i;
          Break
        end
    end
end;

function BJoinTreeClass.getIndexFromJoinPathList(JoinPathList: JoinPathGraphStructure; FromTable: string): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := Low(JoinPathList) to High(JoinPathList) do
    begin
      if FromTable = JoinPathList[i].node[0] then
        begin
          result := i;
          Break
        end
    end
end;

function BJoinTreeClass.getIndexFromJoinPathList(JoinPathList: JoinPathGraphStructure; FromTable: virtualTableStructure): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := Low(JoinPathList) to High(JoinPathList) do
    if eqlVirtualTables(FromTable,JoinPathList[i].node) then
      begin
        result := i;
        Break
      end
end;

procedure BJoinTreeClass.generateJoinGraph(BaseTables: array of string; out JoinGraph: GraphStructure);
var
  i: Integer;
begin
  SetLength(JoinGraph,Length(BaseTables));
  for i := Low(BaseTables) to High(BaseTables) do
    with JoinGraph[i] do
      begin
        node := BaseTables[i];
        adjacents := nil
      end;
  for i := Low(DirectJoinList) to High(DirectJoinList) do
    with DirectJoinList[i] do
      with JoinGraph[getIndexFromJoinGraph(JoinGraph,FromTable)] do
        begin
          SetLength(adjacents,Length(adjacents)+1);
          adjacents[High(adjacents)].Link := ToTable;
          adjacents[High(adjacents)].CommonKey := Key
        end
end;

procedure BJoinTreeClass.getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable, ToTable:  virtualTableStructure; out BaseTable: string; out key: string);
var
  i, j, k: Integer;
begin
  for i := Low(FromTable) to High(FromTable) do
    with JoinGraph[getIndexFromJoinGraph(JoinGraph,FromTable[i])] do
        for j := Low(adjacents) to High(adjacents) do
          for k := Low(ToTable) to High(ToTable) do
            if adjacents[j].Link = ToTable[k] then
              begin
                BaseTable := FromTable[i];
                Key := adjacents[j].CommonKey;
                Exit
              end
end;

procedure BJoinTreeClass.getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable: virtualTableStructure; ToTable: string; out BaseTable: string; out key: string);
var
  i, j: Integer;
begin
  for i := Low(FromTable) to High(FromTable) do
    with JoinGraph[getIndexFromJoinGraph(JoinGraph,FromTable[i])] do
      for j := Low(adjacents) to High(adjacents) do
        if adjacents[j].Link = ToTable then
          begin
            BaseTable := FromTable[i];
            Key := adjacents[j].CommonKey;
            Exit
          end
end;

procedure BJoinTreeClass.getFirstAdjacentListKey(JoinGraph: GraphStructure; FromTable: string; ToTable: virtualTableStructure; out BaseTable: string; out key: string);
var
  j, k: Integer;
begin
  with JoinGraph[getIndexFromJoinGraph(JoinGraph,FromTable)] do
    for j := Low(adjacents) to High(adjacents) do
      for k := Low(ToTable) to High(ToTable) do
        if adjacents[j].Link = ToTable[k] then
          begin
            BaseTable := FromTable;
            Key := adjacents[j].CommonKey;
            Exit
          end
end;

procedure BJoinTreeClass.generateJoinPathList(
                  JoinBaseTables: array of string;
                  JoinGraph: GraphStructure;
                  out JoinPathList: JoinPathGraphStructure;
                  TheKeys: array of string);
var
  i, j, k, l: Integer;
  queue: array of string;
  path: array of string;
  tableElement: string;
  found: Boolean;
  buf1: virtualTableStructure;
  buf2: array of record
          tableName: string;
          key: string // should be array of string
        end;
  control: Boolean;
  FromTable: string;
  KeyName: string;
begin
  queue := nil;
  SetLength(queue,Length(queue)+1);
  queue[High(queue)] := JoinBaseTables[0];
  path := nil;
  SetLength(path,Length(path)+1);
  path[High(path)] := JoinBaseTables[0];
  repeat
    tableElement := queue[Low(queue)];
    with JoinGraph[getIndexFromJoinGraph(JoinGraph,tableElement)] do
    for j := Low(adjacents) to High(adjacents) do
      begin
        found := False;
        for i := Low(JoinBaseTables) to High(JoinBaseTables) do
          if adjacents[j].Link = JoinBaseTables[i] then
            begin
              found := True;
              Break
            end;
        if found then
          begin
            found := False;
            for i := low(path) to high(path) do
              if adjacents[j].Link = path[i] then
                begin
                  found := True;
                  Break
                end;
            if not found then
              begin
                SetLength(queue,Length(queue)+1);
                queue[High(queue)] := adjacents[j].Link;
                SetLength(path,Length(path)+1);
                path[High(path)] := adjacents[j].Link
              end
          end
      end;
    queue := copy(queue,1,Length(queue))
  until queue = nil;

  if path = nil then Exit;
  SetLength(JoinPathList,Length(JoinBaseTables));
  for i := Low(JoinBaseTables) to High(JoinBaseTables) do
    with JoinPathList[i] do
      begin
        SetLength(node,1);
        node[0] := path[i];
        adjacent.link := nil;
        adjacent.Keys := nil;
        adjacent.InheritedKeys := nil
      end;
  buf1 := nil;
  SetLength(buf1,1);
  buf1[0] := path[0];
  for i := 1 to High(path) do
    begin
      with JoinPathList[getIndexFromJoinPathList(JoinPathList,path[i])] do
        begin
          Setlength(adjacent.Link,Length(Buf1));
          for j := Low(Buf1) to High(Buf1) do
            adjacent.Link[j] := Buf1[j];
          SetLength(adjacent.Keys,1);
          getFirstAdjacentListKey(JoinGraph, Path[i], Buf1, adjacent.Keys[0].FromTable, adjacent.Keys[0].KeyName)
        end;
      with JoinPathList[getIndexFromJoinPathList(JoinPathList,Buf1)] do
        begin
          Setlength(adjacent.Link,1);
          adjacent.Link[0] := path[i];
          SetLength(adjacent.Keys,1);
          getFirstAdjacentListKey(JoinGraph, Buf1, Path[i], adjacent.Keys[0].FromTable, adjacent.Keys[0].KeyName)
        end;
      SetLength(buf1,Length(buf1)+1);
      buf1[High(buf1)] := path[i];
      SetLength(JoinPathList,Length(JoinPathList)+1);
      SetLength(JoinPathList[High(JoinPathList)].node,Length(buf1));
      for j := Low(Buf1) to High(Buf1) do
        JoinPathList[High(JoinPathList)].node[j] := Buf1[j]
    end;

  for i := Low(TheKeys) to High(TheKeys) do
    begin
      FromTable := Copy(TheKeys[i],1,Pos('.',TheKeys[i])-1);
      KeyName := Copy(TheKeys[i],Pos('.',TheKeys[i])+1,Length(TheKeys[i]));
      with JoinPathList[High(JoinPathList)].adjacent do
        begin
          SetLength(Keys,Length(Keys)+1);
          Keys[High(Keys)].FromTable := FromTable;
          Keys[High(Keys)].KeyName := KeyName
        end
    end;
  buf2 := nil;
  for i := High(JoinPathList) downto Low(JoinPathList) do
    with JoinPathList[i] do
      begin
        for j := Low(node) to High(node) do
          for k := Low(Buf2) to High(Buf2) do
            if buf2[k].tableName = node[j] then
              with adjacent do
                begin
                  control := False;
                  for l := low(Keys) to High(Keys) do
                    if Keys[l].FromTable =  buf2[k].tableName then
                      if Keys[l].KeyName = buf2[k].key then
                        control := True;
                  if not control then
                    for l := Low(InheritedKeys) to High(InheritedKeys) do
                      if InheritedKeys[l].FromTable =  buf2[k].tableName then
                        if InheritedKeys[l].KeyName = buf2[k].key then
                          control := True;
                  if not control then
                    begin
                      Setlength(InheritedKeys,Length(InheritedKeys)+1);
                      InheritedKeys[High(InheritedKeys)].FromTable := buf2[k].tableName;
                      InheritedKeys[High(InheritedKeys)].KeyName := buf2[k].key
                    end
                end;
        if adjacent.Keys <> nil then
          begin
            for l := Low(adjacent.Keys) to High(adjacent.Keys) do
              begin
                SetLength(buf2,Length(buf2)+1);
                buf2[High(buf2)].tableName := adjacent.Keys[l].FromTable;
                buf2[High(buf2)].key := adjacent.Keys[l].KeyName
              end
          end
      end;


  (* ///////////// not necessary
  for i := 0 to length(FBaseTables) - 1 do
    begin
      for l := 0 to length(JoinPathList[i].adjacent.keys) - 1 do
        begin
          JoinPathList[i].adjacent.Keys[l].flagTA := true;
          JoinPathList[i].adjacent.Keys[l].flagIK := false;
          JoinPathList[i].adjacent.Keys[l].KeyIndex := l;
        end;
      for l := 0 to length(JoinPathList[i].adjacent.Inheritedkeys) - 1 do
        begin
          JoinPathList[i].adjacent.InheritedKeys[l].flagTA := true;
          JoinPathList[i].adjacent.Keys[l].flagIK := true;
          JoinPathList[i].adjacent.InheritedKeys[l].KeyIndex := l;
        end;
    end;
  i := length(FBaseTables);
  for l := 0 to length(JoinPathList[length(FBaseTables)].adjacent.keys) - 1 do
    begin
      if JoinPathList[length(FBaseTables)].adjacent.Keys[l].FromTable = JoinPathList[0].node[0] then
        begin
          JoinPathList[i].adjacent.InheritedKeys[l].flagTA := true;
          JoinPathList[i].adjacent.Keys[l].flagIK := true;
          JoinPathList[i].adjacent.InheritedKeys[l].KeyIndex := l;




          JoinPathList[length(FBaseTables)].adjacent.Keys[l].TATable := 0;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].KeyIndex := l;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].flagKey := true;
        end;
      if JoinPathList[length(FBaseTables)].adjacent.Keys[l].FromTable = JoinPathList[1].node[0] then
        begin
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].TATable := 1;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].KeyIndex := l;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].flagKey := true;
        end;
    end;

  for l := 0 to length(JoinPathList[length(FBaseTables)].adjacent.Inheritedkeys) - 1 do
    begin
      if JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].FromTable = JoinPathList[0].node[0] then
        begin
          JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].TATable := 0;
          JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].KeyIndex := l;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].flagKey := false;
        end;
      if JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].FromTable = JoinPathList[1].node[0] then
        begin
          JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].TATable := 1;
          JoinPathList[length(FBaseTables)].adjacent.InheritedKeys[l].KeyIndex := l;
          JoinPathList[length(FBaseTables)].adjacent.Keys[l].flagKey := false;
        end;
    end;

  for i := length(FBaseTables) + 1 to length(JoinPathList) - 1  do
    begin
      for l := 0 to length(JoinPathList[i].adjacent.keys) - 1 do
        begin
          if JoinPathList[i].adjacent.Keys[l].FromTable =
             JoinPathList[i-length(FBaseTables)+1].node[0] then
             begin
               JoinPathList[i].adjacent.Keys[l].TATable := 1;//i-length(FBaseTables)+1;
               JoinPathList[i].adjacent.Keys[l].KeyIndex := l;
               JoinPathList[i].adjacent.Keys[l].flagKey := true;
             end else
             begin
               JoinPathList[i].adjacent.Keys[l].TATable := 0;//i-1;
               JoinPathList[i].adjacent.Keys[l].KeyIndex := l;
               JoinPathList[i].adjacent.Keys[l].flagKey := true;
             end;
        end;
      for l := 0 to length(JoinPathList[i].adjacent.Inheritedkeys) - 1 do
        begin
          if JoinPathList[i].adjacent.InheritedKeys[l].FromTable =
             JoinPathList[i-length(FBaseTables)+1].node[0] then
             begin
               JoinPathList[i].adjacent.InheritedKeys[l].TATable := 1;//i-length(FBaseTables)+1;
               JoinPathList[i].adjacent.InheritedKeys[l].KeyIndex := l;
               JoinPathList[i].adjacent.InheritedKeys[l].flagKey := false;
             end else
             begin
               JoinPathList[i].adjacent.InheritedKeys[l].TATable := 0;//i-1;
               JoinPathList[i].adjacent.InheritedKeys[l].KeyIndex := l;
               JoinPathList[i].adjacent.InheritedKeys[l].flagKey := false;
             end;
        end;
    end;
*)
end;

procedure BJoinTreeClass.createBTrees(JoinBaseTables: array of string; IsOpen: Boolean;
                                      TheKeys: array of string);
var
  FBTreeName: string;
  JoinGraph: GraphStructure;
  i, j: Integer;
  FKeys: array of string;
  FInheritedKeys: array of string;
  stnode: string;
  stkeys: string;
  stInheritedKeys: string;
begin
  generateJoinGraph(FBaseTables,JoinGraph);
  generateJoinPathList(JoinBaseTables, JoinGraph, FJoinPathList,TheKeys);
  {
  stnode := '';
  for i := Low(FJoinPathList) to High(FJoinPathList) do
    begin
      stnode := 'node';
      stKeys := '';
      stInheritedKeys := '';
      for j := Low(FJoinPathList[i].node) to high(FJoinPathList[i].node) do
        stnode := stnode + inttostr(i) +  ': ' + FJoinPathList[i].node[j] + '--';
      stnode := stnode + '> Adjacents: ';
      for j := Low(FJoinPathList[i].adjacent.link) to high(FJoinPathList[i].adjacent.link) do
        stnode := stnode + FJoinPathList[i].adjacent.link[j]  + '--';
      stkeys := stkeys + '> Keys: ';
      for j := Low(FJoinPathList[i].adjacent.keys) to high(FJoinPathList[i].adjacent.keys) do
        stkeys := stkeys + 'From: ' + FJoinPathList[i].adjacent.keys[j].FromTable   + '--Key: ' + FJoinPathList[i].adjacent.keys[j].KeyName;

      stInheritedKeys := stInheritedKeys + '> InheritedKeys: ';
      for j := Low(FJoinPathList[i].adjacent.Inheritedkeys) to high(FJoinPathList[i].adjacent.Inheritedkeys) do
        stInheritedKeys := stInheritedKeys + 'From: ' + FJoinPathList[i].adjacent.Inheritedkeys[j].FromTable   + '--Key: ' +
                  FJoinPathList[i].adjacent.Inheritedkeys[j].KeyName;
    end;
  }

  // if an index exists for base tables use it

  for i := Low(FJoinPathList) to High(FJoinPathList) do
    begin
      SetLength(FBTrees,Length(FBTrees)+1);
      FBTreeName := FileName + IntToStr(i);
      with FJoinPathList[i] do
        begin
          FKeys := nil;
          for j := Low(adjacent.Keys) to High(adjacent.Keys) do
            begin
              SetLength(FKeys,Length(FKeys)+1);
              with adjacent.Keys[j] do
                FKeys[High(FKeys)] := KeyName + ':' + getTypeFromDataDictionary(KeyName,FromTable)
            end;
          FInheritedKeys := nil;
          for j := Low(adjacent.InheritedKeys) to High(adjacent.InheritedKeys) do
            begin
              SetLength(FInheritedKeys,Length(FInheritedKeys)+1);
              with adjacent.InheritedKeys[j] do
                FInheritedKeys[High(FInheritedKeys)] := KeyName + ':' + getTypeFromDataDictionary(KeyName,FromTable)
            end;
          FBTrees[High(FBTrees)] := BtrPlusClass.Create(FBTreeName,IsOpen,FKeys,FInheritedKeys,Length(node))
        end
    end
end;

function BJoinTreeClass.GetnumberOfKeys(BTreeIndex: Integer): Integer;
begin
  result := FBTrees[BTreeIndex].NumberOfKeys
end;

function BJoinTreeClass.GetnumberOfInheritedKeys(BTreeIndex: Integer): Integer;
begin
  result := FBTrees[BTreeIndex].NumberOfInheritedKeys
end;

function BJoinTreeClass.GetnumberOfDataRef(BTreeIndex: Integer): Integer;
begin
  result := FBTrees[BTreeIndex].NumberOfDataRef
end;

procedure BJoinTreeClass.AddJoinKey(TableName: virtualTableStructure; Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType);
var
  index: integer;
  BTreeIndex: Integer;
  AdjacentTable: virtualTableStructure;
  BTreeAdjacentIndex: Integer;
  AdjacentDataRef: array of DataPointerType;
  found: Boolean;
  i, j: Integer;
  ConcatenateTable: virtualTableStructure;
  BTreeConcatenateIndex: Integer;
  AdjacentKeys: array of variant;
  OldAdjacentKeys: array of variant;
  AdjacentInheritedKeys: array of variant;
  ConcatenateKeys: array of variant;
  ConcatenateInheritedKeys: array of variant;
  ConcatenateDataRef: array of DataPointerType;
  TNIndex, ATIndex: Integer;


begin
  BTreeIndex := getIndexFromJoinPathList(FJoinPathList,TableName);
  FBTrees[BtreeIndex].AddKey(Keys,InheritedKeys,DataRef);

  AdjacentTable := FJoinPathList[BTreeIndex].adjacent.link;
  if AdjacentTable = nil then Exit;

  BTreeAdjacentIndex := getIndexFromJoinPathList(FJoinPathList,AdjacentTable);
  SetLength(AdjacentDataRef,FBTrees[BtreeAdjacentIndex].NumberOfDataRef);
  //Length(FJoinPathList[BTreeAdjacentIndex].node));
  SetLength(AdjacentKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys));
  for i := Low(Keys) to High(Keys) do AdjacentKeys[i] := Keys[i];


  SetLength(OldAdjacentKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys));
  SetLength(AdjacentInheritedKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys));

  FBTrees[BtreeAdjacentIndex].FindKey(AdjacentKeys,AdjacentInheritedKeys,AdjacentDataRef);
  found := AdjacentDataRef[0] <> nullDataValue;

  while found do
    begin
      // ReturnKeys(B+Tree(T[k]), keys[k], InheritedKeys[k], [DPk])
      SetLength(ConcatenateTable,Length(TableName)+Length(AdjacentTable));
      SetLength(ConcatenateDataRef,Length(DataRef)+Length(AdjacentDataRef));
      concatenateKeys := nil;
      concatenateInheritedKeys := nil;

      TNIndex := 0;
      ATIndex := 0;
      i := 0;
      repeat
        if TNIndex < Length(TableName) then
          begin
            if ATIndex < Length (AdjacentTable) then
              begin
                if getIndexFromJoinPathList(FJoinPathList,TableName[TNIndex]) < getIndexFromJoinPathList(FJoinPathList,AdjacentTable[ATIndex]) then
                  begin
                    ConcatenateTable[i] := TableName[TNIndex];
                    ConcatenateDataRef[i] := DataRef[TNIndex];
                    Inc(TNIndex);
                    inc(i)
                  end else
                  begin
                    ConcatenateTable[i] := AdjacentTable[ATIndex];
                    ConcatenateDataRef[i] := AdjacentDataRef[ATIndex];
                    Inc(ATIndex);
                    inc(i)
                  end
              end else
              begin
                while TNIndex < Length(TableName) do
                  begin
                    ConcatenateTable[i] := TableName[TNIndex];
                    ConcatenateDataRef[i] := DataRef[TNIndex];
                    Inc(TNIndex);
                    inc(i)
                  end
              end
          end else
          begin
            while ATIndex < Length(AdjacentTable) do
              begin
                ConcatenateTable[i] := AdjacentTable[ATIndex];
                ConcatenateDataRef[i] := AdjacentDataRef[ATIndex];
                Inc(ATIndex);
                inc(i)
              end
          end;
      until (TNIndex = Length(TableName)) and (ATIndex = Length(AdjacentTable));

 {
      ConcatenateTable := FJoinPathList[length(FBaseTables) + Length(TableName) + Length(AdjacentTable) - 2].node;

      for i := 0 to length(AdjacentDataRef)- 1 do
        concatenateDataRef[i] := AdjacentDataRef[i];

      for i := 0 to length(DataRef) - 1 do
        concatenateDataRef[i+length(AdjacentDataRef)] := DataRef[i];

}

{


      BTreeConcatenateIndex := getIndexFromJoinPathList(FJoinPathList,ConcatenateTable);



      for i := 0 to High(FJoinPathList[BTreeConcatenateIndex].adjacent.Keys) do
        begin
          setlength(concatenateKeys,length(concatenateKeys)+1);
          if not FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].flagKey then
            begin
              if FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].TATable = 1 then
                ConcatenateKeys[High(ConcatenateKeys)] :=
                  Keys[FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyIndex]
               else
                ConcatenateKeys[High(ConcatenateKeys)] :=
                  AdjacentKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyIndex]
            end else
            begin
              if FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].TATable = 1 then
                ConcatenateKeys[High(ConcatenateKeys)] :=
                  InheritedKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyIndex] else
                ConcatenateKeys[High(ConcatenateKeys)] :=
                  AdjacentInheritedKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyIndex]
            end;
        end;

      for i := 0 to High(FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys) do
        begin


          setlength(concatenateInheritedKeys,length(concatenateInheritedKeys)+1);
          if not FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].flagKey then
            begin
              if FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].TATable = 1 then
                ConcatenateInheritedKeys[High(ConcatenateKeys)] :=
                  inheritedKeys[0FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyIndex]
               else
                ConcatenateInheritedKeys[High(ConcatenateKeys)] :=
                  AdjacentKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyIndex]
            end else
            begin
              if FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].TATable = 1 then
                ConcatenateInheritedKeys[High(ConcatenateKeys)] :=
                  InheritedKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyIndex] else
                ConcatenateInheritedKeys[High(ConcatenateKeys)] :=
                  AdjacentInheritedKeys[FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyIndex]
            end;





        end;

      concatenatekeys := nil;
      concatenateInheritedKeys := nil;




}


      BTreeConcatenateIndex := getIndexFromJoinPathList(FJoinPathList,ConcatenateTable);

      for i := Low(FJoinPathList[BTreeConcatenateIndex].adjacent.Keys) to High(FJoinPathList[BTreeConcatenateIndex].adjacent.Keys) do
        begin
          found := false;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.Keys) to High(FJoinPathList[BTreeIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := Keys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := InheritedKeys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := AdjacentKeys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := AdjacentInheritedKeys[j];
                break
              end
        end;
      for i := Low(FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys) do
        begin
          found := false;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.Keys) to High(FJoinPathList[BTreeIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := Keys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := InheritedKeys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := AdjacentKeys[j];
                found := true;
                break
              end;
          if found then continue;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := AdjacentInheritedKeys[j];
                break
              end
        end;

      //SetLength(ConcatenateDataRef,Length(DataRef)+Length(AdjacentDataRef));
      //for i := Low(DataRef) to High(DataRef) do ConcatenateDataRef[i] := DataRef[i];
      //for i := Low(AdjacentDataRef) to High(AdjacentDataRef) do ConcatenateDataRef[Length(DataRef)+i] := AdjacentDataRef[i];

      if BTreeConcatenateIndex = High(FBTrees) then
        begin
          setlength(FNewRows,length(FNewRows)+1);
          setlength(FNewRows[high(FNewRows)],length(ConcatenateDataRef));
          for index := 0 to length(ConcatenateDataRef) - 1 do
            FNewRows[high(FNewRows),index] := ConcatenateDataRef[index]
        end;



      AddJoinKey (ConcatenateTable,ConcatenateKeys,ConcatenateInheritedKeys,ConcatenateDataRef);
      for i := Low(AdjacentKeys) to High(AdjacentKeys) do
        OldAdjacentKeys[i] := AdjacentKeys[i];

      FBTrees[BtreeAdjacentIndex].NextKey(AdjacentKeys,AdjacentInheritedKeys,AdjacentDataRef);

      found := AdjacentDataRef[0] <> nullDataValue;
      for i := Low(AdjacentKeys) to High(AdjacentKeys) do
        found := found and (AdjacentKeys[i] = OldAdjacentKeys[i])
    end
  (*
  Call AddKey (B+Tree(T[i]), keys[i], InheritedKeys[i], [DPi]) for the index of table T[i]
  Locate the entry of T[i] in the JoinPathList
  From its adjacent List, locate the Table T[k] adjacent to it and do the following
    Locate the entry of T[k] in the JoinPathList
    FindKey(B+Tree(T[k]), Keys[i])
    While found(keys[i]) do
      ReturnKeys(B+Tree(T[k]), keys[k], InheritedKeys[k], [DPk])
      Locate the entry of T[ik] in the PathJoinList
      From its adjacent List, locate the definition of the keys and inherited keys
      From keys[i], inheritedkeys[i] , keys[k], inheritedkeys[k] get  the keys and inherited keys of T[ik]
      AddJoinKey (T[ik] , Keys[ik], InheritedKeys[ik], [DPik])
      NextKey(B+Tree(T[k]), Keys[i])
  *)
end;

procedure BJoinTreeClass.AddKey(TableName: string; Row: array of variant; DataRef: DataPointerType);
var
  i: Integer;
  Keys: array of variant;
  InheritedKeys: array of variant;
  virtualTableName: virtualTableStructure;
begin
  with FJoinPathList[getIndexFromJoinPathList(FJoinPathList,TableName)] do
    begin
      SetLength(Keys,Length(adjacent.Keys));
      for i := Low(adjacent.Keys) to High(adjacent.Keys) do
        Keys[i] := Row[getPositionFromDataDictionary(adjacent.keys[i].KeyName,node[0])];
      SetLength(InheritedKeys,Length(adjacent.InheritedKeys));
      for i := Low(adjacent.InheritedKeys) to High(adjacent.InheritedKeys) do
        InheritedKeys[i] := Row[getPositionFromDataDictionary(adjacent.Inheritedkeys[i].KeyName,node[0])]
    end;
  SetLength(virtualTableName,1);
  virtualTableName[0] := TableName;
  FNewRows := nil;
  AddJoinKey(virtualTableName,Keys,InheritedKeys,[DataRef])
  (*
  When a new row Rm from table Ti get inserted do the following:
    Locate the entry of Ti in the JoinPathList
    From its adjacent List, locate the definition of the keys and inherited keys
    From Row Rm get the columns constituting the keys and the inherited keys
    Call AddJoinKey (Ti, Keys, InheritedKeys, DPi) where DPi is the row id of row Rm.
  *)
end;

procedure BJoinTreeClass.DeleteJoinKey(TableName: virtualTableStructure; Keys: array of variant; InheritedKeys: array of variant; DataRef: array of DataPointerType);
var
  BTreeIndex: Integer;
  AdjacentTable: virtualTableStructure;
  BTreeAdjacentIndex: Integer;
  AdjacentDataRef: array of DataPointerType;
  found: Boolean;
  i, j: Integer;
  ConcatenateTable: virtualTableStructure;
  BTreeConcatenateIndex: Integer;
  AdjacentKeys: array of variant;
  OldAdjacentKeys: array of variant;
  AdjacentInheritedKeys: array of variant;
  ConcatenateKeys: array of variant;
  ConcatenateInheritedKeys: array of variant;
  ConcatenateDataRef: array of DataPointerType;
  TNIndex, ATIndex: Integer;
  index: Integer;
begin
  BTreeIndex := getIndexFromJoinPathList(FJoinPathList,TableName);
  FBTrees[BtreeIndex].DeleteKey(Keys,DataRef);
  AdjacentTable := FJoinPathList[BTreeIndex].adjacent.link;
  if AdjacentTable = nil then Exit;
  BTreeAdjacentIndex := getIndexFromJoinPathList(FJoinPathList,AdjacentTable);
  SetLength(AdjacentDataRef,Length(FJoinPathList[BTreeAdjacentIndex].node));
  SetLength(AdjacentKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys));
  for i := Low(Keys) to High(Keys) do
    AdjacentKeys[i] := Keys[i];
  SetLength(OldAdjacentKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys));
  SetLength(AdjacentInheritedKeys,Length(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys));
  FBTrees[BtreeAdjacentIndex].FindKey(AdjacentKeys,AdjacentInheritedKeys,AdjacentDataRef);
  ConcatenateKeys := nil;
  ConcatenateInheritedKeys := nil;
  found := AdjacentDataRef[0] <> nullDataValue;
  while found do
    begin
      // ReturnKeys(B+Tree(T[k]), keys[k], InheritedKeys[k], [DPk])
      SetLength(ConcatenateTable,Length(TableName)+Length(AdjacentTable));
      SetLength(ConcatenateDataRef,Length(DataRef)+Length(AdjacentDataRef));
      concatenateKeys := nil;
      concatenateInheritedKeys := nil;
      TNIndex := 0;
      ATIndex := 0;
      i := 0;
      repeat
        if TNIndex < Length(TableName) then
          begin
            if ATIndex < Length (AdjacentTable) then
              begin
                if getIndexFromJoinPathList(FJoinPathList,TableName[TNIndex]) < getIndexFromJoinPathList(FJoinPathList,AdjacentTable[ATIndex]) then
                  begin
                    ConcatenateTable[i] := TableName[TNIndex];
                    ConcatenateDataRef[i] := DataRef[TNIndex];
                    Inc(TNIndex);
                    inc(i)
                  end else
                  begin
                    ConcatenateTable[i] := AdjacentTable[ATIndex];
                    ConcatenateDataRef[i] := AdjacentDataRef[ATIndex];
                    Inc(ATIndex);
                    inc(i)
                  end
              end else
              begin
                while TNIndex < Length(TableName) do
                  begin
                    ConcatenateTable[i] := TableName[TNIndex];
                    ConcatenateDataRef[i] := DataRef[TNIndex];
                    Inc(TNIndex);
                    inc(i)
                  end
              end
          end else
          begin
            while ATIndex < Length(AdjacentTable) do
              begin
                ConcatenateTable[i] := AdjacentTable[ATIndex];
                ConcatenateDataRef[i] := AdjacentDataRef[ATIndex];
                Inc(ATIndex);
                inc(i)
              end
          end;
      until (TNIndex = Length(TableName)) and (ATIndex = Length(AdjacentTable));
      BTreeConcatenateIndex := getIndexFromJoinPathList(FJoinPathList,ConcatenateTable);
      for i := Low(FJoinPathList[BTreeConcatenateIndex].adjacent.Keys) to High(FJoinPathList[BTreeConcatenateIndex].adjacent.Keys) do
        begin
          for j := Low(FJoinPathList[BTreeIndex].adjacent.Keys) to High(FJoinPathList[BTreeIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := Keys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := InheritedKeys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := AdjacentKeys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.Keys[i].KeyName) then
              begin
                SetLength(ConcatenateKeys,Length(ConcatenateKeys)+1);
                ConcatenateKeys[High(ConcatenateKeys)] := AdjacentInheritedKeys[j];
                break
              end
        end;
      for i := Low(FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys) do
        begin
          for j := Low(FJoinPathList[BTreeIndex].adjacent.Keys) to High(FJoinPathList[BTreeIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := Keys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := InheritedKeys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.Keys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.Keys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := AdjacentKeys[j];
                break
              end;
          for j := Low(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys) to High(FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys)do
            if (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].FromTable = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].FromTable) and
               (FJoinPathList[BTreeAdjacentIndex].adjacent.InheritedKeys[j].KeyName = FJoinPathList[BTreeConcatenateIndex].adjacent.InheritedKeys[i].KeyName) then
              begin
                SetLength(ConcatenateInheritedKeys,Length(ConcatenateInheritedKeys)+1);
                ConcatenateInheritedKeys[High(ConcatenateInheritedKeys)] := AdjacentInheritedKeys[j];
                break
              end
        end;

      //SetLength(ConcatenateDataRef,Length(DataRef)+Length(AdjacentDataRef));
      //for i := Low(DataRef) to High(DataRef) do ConcatenateDataRef[i] := DataRef[i];
      //for i := Low(AdjacentDataRef) to High(AdjacentDataRef) do ConcatenateDataRef[Length(DataRef)+i] := AdjacentDataRef[i];

      if BTreeConcatenateIndex = High(FBTrees) then
        begin
          setlength(FDeletedRows,length(FDeletedRows)+1);
          setlength(FDeletedRows[high(FDeletedRows)],length(ConcatenateDataRef));
          for index := 0 to length(ConcatenateDataRef) - 1 do
            FDeletedRows[high(FDeletedRows),index] := ConcatenateDataRef[index]
        end;

      DeleteJoinKey (ConcatenateTable,ConcatenateKeys,ConcatenateInheritedKeys,ConcatenateDataRef);
      for i := Low(AdjacentKeys) to High(AdjacentKeys) do
        OldAdjacentKeys[i] := AdjacentKeys[i];
      FBTrees[BtreeAdjacentIndex].NextKey(AdjacentKeys,AdjacentInheritedKeys,AdjacentDataRef);
      found := AdjacentDataRef[0] <> nullDataValue;
      for i := Low(AdjacentKeys) to High(AdjacentKeys) do
        found := found and (AdjacentKeys[i] = OldAdjacentKeys[i])
    end
  (*
  Call DeleteKey (B+Tree(T[i]), keys[i], InheritedKeys[i], [DPi]) for the index of table T[i]
  Locate the entry of T[i] in the JoinPathList
  From its adjacent List, locate the Table T[k] adjacent to it and do the following
    Locate the entry of T[k] in the JoinPathList
    FindKey(B+Tree(T[k]), Keys[i])
    While found(keys[i]) do
      ReturnKeys(B+Tree(T[k]), keys[k], InheritedKeys[k], [DPk])
      Locate the entry of T[ik] in the PathJoinList
      From its adjacent List, locate the definition of the keys and inherited keys
      From keys[i], inheritedkeys[i] , keys[k], inheritedkeys[k] get  the keys and inherited keys of T[ik]
      DeleteJoinKey (T[ik] , Keys[ik], InheritedKeys[ik], [DPik])
      NextKey(B+Tree(T[k]), Keys[i])
  *)
end;

procedure BJoinTreeClass.DeleteKey(TableName: string; Row: array of variant; DataRef: DataPointerType);
var
  i: Integer;
  Keys: array of variant;
  InheritedKeys: array of variant;
  virtualTableName: virtualTableStructure;
begin
  with FJoinPathList[getIndexFromJoinPathList(FJoinPathList,TableName)] do
    begin
      SetLength(Keys,Length(adjacent.Keys));
      for i := Low(adjacent.Keys) to High(adjacent.Keys) do
        Keys[i] := Row[getPositionFromDataDictionary(adjacent.keys[i].KeyName,node[0])];
      SetLength(InheritedKeys,Length(adjacent.InheritedKeys));
      for i := Low(adjacent.InheritedKeys) to High(adjacent.InheritedKeys) do
        InheritedKeys[i] := Row[getPositionFromDataDictionary(adjacent.Inheritedkeys[i].KeyName,node[0])]
    end;
  SetLength(virtualTableName,1);
  virtualTableName[0] := TableName;
  FDeletedRows := nil;
  DeleteJoinKey(virtualTableName,Keys,InheritedKeys,[DataRef])
  (*
  When a new row Rm from table Ti get inserted do the following:
    Locate the entry of Ti in the JoinPathList
    From its adjacent List, locate the definition of the keys and inherited keys
    From Row Rm get the columns constituting the keys and the inherited keys
    Call DeleteJoinKey (Ti, Keys, InheritedKeys, DPi) where DPi is the row id of row Rm.
  *)
end;

procedure BJoinTreeClass.ClearKey(BtreeIndex: Integer);
begin
  FBTrees[BTreeIndex].ClearKey
end;

procedure BJoinTreeClass.PrevKey(BtreeIndex: Integer;
                                 var Keys: array of variant;
                                 var InheritedKeys: array of variant;
                                 out DataRef: array of DataPointerType);
begin
  FBTrees[BTreeIndex].prevKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.NextKey(BtreeIndex: Integer;
                                 var Keys: array of variant;
                                 var InheritedKeys: array of variant;
                                 out DataRef: array of DataPointerType);
begin
  FBTrees[BTreeIndex].NextKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.MinKey(BtreeIndex: Integer;
                                 var Keys: array of variant;
                                 var InheritedKeys: array of variant;
                                 out DataRef: array of DataPointerType);
begin
  FBTrees[BTreeIndex].MinKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.MaxKey(BtreeIndex: Integer;
                                var Keys: array of variant;
                                var InheritedKeys: array of variant;
                                out DataRef: array of DataPointerType);
begin
  FBTrees[BTreeIndex].MaxKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.ClearKey;
begin
  FBTrees[High(FJoinPathList)].ClearKey
end;

procedure BJoinTreeClass.FindKey(Keys: array of variant; var DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].FindKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.SearchKey(var Keys: array of variant; var DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].SearchKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.PrevKey(var Keys: array of variant; out DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].PrevKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.NextKey(var Keys: array of variant; out DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].NextKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.MinKey(var Keys: array of variant; out DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].MinKey(Keys,InheritedKeys,DataRef)
end;

procedure BJoinTreeClass.MaxKey(var Keys: array of variant; out DataRef: array of DataPointerType);
var
  InheritedKeys: array of variant;
begin
  InheritedKeys := nil;
  FBTrees[High(FJoinPathList)].MaxKey(Keys,InheritedKeys,DataRef)
end;

end.
