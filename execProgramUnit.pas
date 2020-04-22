unit execProgramUnit;
// usr/local/bin/


//{$DEFINE mongodb}
{$DEFINE tablesdb}

interface

{ DONE : To show the databases for a user:
  select system databases from userid;
         To switch the database:
  select system dbname from databases;
         To show the tables of a database:
  select system tables from dbname
         To show the columns of a table:
  select system columns from tblname
         To show the join indexes of a database:
  select system indexes from dbname;
         To show the indexes of a table:
  select system indexes from tblname;
         To show the constraints of a table:
  select system constraints from table; }

{ TODO : Nested SQL
         More than one common key
         add to Join the on clause key = key1
         Having }

{ DONE : Aggregate functions
         Group by
         Ordered by
 }

uses
  sql, lexlib,  yacclib,
  {$IFDEF tablesdb}   BPlusTreeU, TtableU, {$ENDIF}
  BJoinTreeU,
  {$IFDEF mongodb} Mongo, MongoDB, MongoCollection, MongoDBCursorIntf,
  BSONTypes, {$ENDIF}
  variants;

const
  lendbOjects = 48;

{$IFDEF mongodb}
var
  GMongo: TMongo;
  GDB: TMongoDB;
  GCollection: TMongoCollection;
{$ENDIF}


type
  typeset = (inttype, smallinttype, int64type, extendedtype, currencytype,
    tdatetype, ttimetype, tdatetimetype, booleantype, stringtype);

  tblStructure = record
    tblName: string;
    // timestamp: TDateTime;
    {$IFDEF mongodb}
    Collection: TMongoCollection;
    {$ENDIF}
    {$IFDEF tablesdb}
    storage: TTableClass;
    {$ENDIF}
    numCols: integer;
    columns: array of record
      colname: string;
      colSQLtypename: string;
      coltype: inttype..stringtype;
      // Default Value
      colhasAutoIncrement: boolean;
      colHasDefault: boolean;
      colDefaultValue: variant;
      coltypescale: record
        case byte of
          0: (size: int64);
          1: (fltprecision: byte);
          2: (precision, scale: byte);
      end;
    end;

    constraints: array of record
      cnstrname: string;
      checkCondition: progInstrunctionsType;
      // when the check condition is over one column then every operator refer to the column
      // stack instructions
      // Instructions are on columns and opeartors and return true or false

      // A check constraint is a type of integrity constraint in SQL which specifies a requirement that must be met by each row in a database table.
      // The constraint must be a predicate. It can refer to a single column, or multiple columns of the table.
      // The result of the predicate can be either TRUE, FALSE, or UNKNOWN, depending on the presence of NULLs.
      // If the predicate evaluates to UNKNOWN, then the constraint is not violated and the row can be inserted or updated in the table.
      // This is contrary to predicates in WHERE clauses in SELECT or UPDATE statements.

      // trgdelete, trgupdate: trgStructure;// use triggers for on delte or on update cascade
      case cnstrtype: byte of
        // 4 for references, 5 for check condition and 6 for triggers
        0: (nullCol: byte);
        // Column allow null Values, true by default, the value if the column order
        1: (nnullCol: byte);
        // Column doesn''t allow null Values, the value if the column order
        2: (uqCols: array [0..0] of longword; uqindex_name: string[lendbOjects]);
        // Unique key constraints are used to ensure that data is not duplicated in two rows in the database.
        // One row in the database is allowed to have null for the value of the unique key constraint.
        3: (pkCols: array [0..0] of longword; pkindex_name: string[lendbOjects]);
        // You can have only one primary key, but that can consist of as many columns as you need to uniquely identify your rows
        4: (Cols: array [0..0] of longword;
            reftblName: string[lendbOjects]; // primary table referenced
            refCols: array [0..0] of longword;
            // optional columns from the primary tables if they don't exist so the columns with the same name and type
           );
        5: (ckCols: array [0..0] of longword);
    end;

    trgdata: array of record
      trgname: string;
      sync: (before, after); // false for before true for after
      updelins: (Delete, insert, update);
      upCols: array [0..0] of longword;
      tblName: string;
        (*
        forEachRow: boolean;
        whenCondition: array of string; // stack instrctions
        triggerAction: (insert, update, delete, select)
        *)
    end;

    idxdata: array of record
      idxname: string;
      {$IFDEF tablesdb}
      idxstorage: BtrPlusClass;
      {$ENDIF}
      idxkeys: array of record
        colName: string;
        colorder: boolean;
      end;
    end;
  end;

  dbjoinidxStructure = record
    idxname: string;
    idxstorage: BJoinTreeClass;
    joinBaseTables: array of string;
    joincouples: array of record
      fromTable: string;
      toTable: string;
      keyNames: array of string; // List of common Columns
    end;
    idxkeys: array of record
      tblName: string;
      colName: string;
      colOrder: boolean;
    end;
  end;

  viewStructure = record
    viewName: string;
    ViewInstructions: progInstrunctionsType;
    numCols: integer;
    columns: array of record
      tblname: string;
      colname: string;
      aliasname: string;
    end;

    idxname: string;

  end;


type
  dbStructure = record
    dbName: string;
    tables: array of tblStructure;
    joinidxdata: array of dbjoinidxStructure;
    views: array of viewStructure;
  end;

var
  workingSchema: dbStructure;

  outputst: string;

procedure createTables;

procedure openTables;

procedure closeTables;

function GetTableStructure(tblName: string): integer;

function UserIdExists(userId: string): boolean; overload;

function UserIdExists(userId: string; password: string): boolean; overload;

function TableExists(tblName: string): boolean;

function ViewExists(ViewName: string): boolean;

procedure connectToDB(DBName: string; outData: string);

procedure disconnectFromDB;

procedure ParseSQLStatement(sqlStatement: string; out sqlMemProg: progInstrunctionsType);

procedure ExecuteProgram(sqlMemProg: progInstrunctionsType; dbUserId: string; dbName: string);

function returnMiscMessage: string;

function returnErrMessage: string;

implementation

uses
  SysUtils,
  binU,
  SDFUnit,
  regexpr;

  var
    Lexer: TLexer;
    parser: TParser;

const
  tableColumnSeperator: char = '_';

{$IFDEF UNIX}
const
   Path: string = 'opt/'; //'/opt/';

{$ELSE}
var
  Path: string = '';
{$ENDIF}

{$IFDEF tablesdb}
var
  DDusers: TTableClass;
  DDroles: TTableClass;
  DDuser_roleprivilege_object: TTableClass;
  DDrole_privilege: TTableClass;
  DDdatabases: TTableClass;
  DDviews: TTableClass;
  DDviewinstructions: TTableClass;
  DDtables: TTableClass;
  DDtablecolumns: TTableClass;
  DDviewcolumns: TTableClass;
  DDconstraints: TTableClass;
  DDcolumnsconstraint: TTableClass;
  DDcheckinstructions: TTableClass;
  DDrefcolumns: TTableClass;
  DDindexes: TTableClass;
  DDbasetables: TTableClass;
  DDkeysindexes: TTableClass;
  DDjoinindexes: TTableClass;
  DDkeysjointables: TTableClass;
  outText: TextFile;
{$ENDIF}

{$IFDEF mongodb}
var
  DataDictionaryCollection: TMongoCollection;
  TransactionsCollection:  TMongoCollection;
  SequencesCollection:  TMongoCollection;
{$ENDIF}

  { TODO : check all the fields in BSON, if someone is messing put it as default or yyerror }
  { DONE : To put boolean values as a new kind in sqlLex and sql.y }

procedure closeSchemaTables;
var
  index: integer;
  index1: integer;
begin
  {$IFDEF mongodb}
  for index := low(workingSchema.tables) to High(workingSchema.tables) do
    with workingSchema.tables[index] do Collection.Free;
  // close join indexes
  {$ENDIF}
  {$IFDEF tablesdb}
  for index := low(workingSchema.tables) to High(workingSchema.tables) do
    begin
      workingSchema.tables[index].storage.Free;
      for index1 := low(workingSchema.tables[index].idxdata) to high(workingSchema.tables[index].idxdata) do
        workingSchema.tables[index].idxdata[index1].idxstorage.Free
    end;
  for index := low(workingSchema.joinidxdata) to high(workingSchema.joinidxdata) do
    workingSchema.joinidxdata[index].idxstorage.Free
  // close indexes
  {$ENDIF}
end;

{$IFDEF mongodb}
procedure connectToMongoDB(DBName: string; CollectionName: string);
var
  DataDictionaryInstance: IBSONObject;
  privilegeArray: IBSONArray;
begin

  //GRefList := TInterfaceList.Create;

  GMongo := TMongo.Create;

  // GMongo.Connect('50.118.49.133',27017);
  GMongo.Connect();
  GDB := GMongo.getDB(DBName);

  GCollection := GDB.GetCollection(CollectionName); // should be removed data if returned in screen
  DataDictionaryCollection := GDB.GetCollection('datadictionary');

  // create user and privileges for empty datadictionary
  if DataDictionaryCollection.Count = 0 then
    begin
      DataDictionaryInstance := TBSONObject.Create;
      DataDictionaryInstance.Put('kind','user');
      DataDictionaryInstance.Put('user_id','root');
      DataDictionaryInstance.Put('password','password');
      DataDictionaryInstance.Put('created_by',NULL);
      DataDictionaryInstance.Put('_id', TBSONObjectId.NewFrom);
      DataDictionaryCollection.Insert(DataDictionaryInstance);

      privilegeArray := TBSONArray.Create;
      privilegeArray.Put('create user');
      privilegeArray.Put('grant any');
      // privilegeArray.Put('all');
      privilegeArray.Put('grant option');

      DataDictionaryInstance := TBSONObject.Create;
      DataDictionaryInstance.Put('kind','privilege');
      DataDictionaryInstance.Put('privilege_array',privilegeArray);

      // DataDictionaryInstance.Put('dbobject','*.*');

      DataDictionaryInstance.Put('belongs_to','root');
      DataDictionaryInstance.Put('_id', TBSONObjectId.NewFrom);
      DataDictionaryCollection.Insert(DataDictionaryInstance);
    end;
  Transactionscollection := GDB.GetCollection('sys_transactions');
  SequencesCollection := GDB.GetCollection('sys_sequences');

end;
{$ENDIF}

{$IFDEF tablesdb}
procedure connectToTablesDB(DBName: string; outData: string);
var
  row: array of variant = nil;
begin
  Path := DBName + '/';
  if createDir(Path) then
    begin
      createTables;
      setlength(row,3);
      row[0] := 'root';
      row[1] := 'password';
      row[2] := 'null';
      DDusers.insertRow(row);
      closeTables;
    end;
  outData := outData + '.txt';
  AssignFile(outText,Path + outData);
  rewrite(outText);
  openTables;
end;
{$ENDIF}

{$IFDEF mongodb}
procedure disconnectFromMongoDB;
begin
  closeSchemaTables;
  GCollection.free;
  DataDictionaryCollection.free;
  Transactionscollection.Free;
  SequencesCollection.free;
  GDB.free;
  GMongo.free;
end;
{$ENDIF}

{$IFDEF tablesdb}
procedure disconnectFromTablesDB;
begin
  closeFile(outText);
  closeSchemaTables;
  closeTables;
end;
{$ENDIF}

procedure connectToDB(DBName: string; outData: string);
begin
  {$IFDEF mongodb}
  connectToMongoDB(DBName,outData);
  {$ENDIF}
  {$IFDEF tablesdb}
  connectToTablesDB(DBName,outData);
  {$ENDIF}
end;

procedure disconnectFromDB;
begin
  {$IFDEF mongodb}
  disconnectFromMongoDB;
  {$ENDIF}
  {$IFDEF tablesdb}
  disconnectFromTablesDB;
  {$ENDIF}
end;

{$IFDEF mongodb}
var
  ACursor: IMongoDBCursor;
{$ENDIF}

function convertType(SQLType: string): string;
begin
  if (SQLType = 'CHARACTER') then
    Result := 'STRING';
  if (SQLType = 'CHARACTER VARYING') then
    Result := 'STRING';
  if (SQLType = 'CLOB') then
    Result := 'STRING';

  if (SQLType = 'DATETIME') then
    Result := 'TDATETIME';
  if (SQLType = 'TIMESTAMP') then
    Result := 'TDATETIME';
  if (SQLType = 'DATE') then
    Result := 'TDATE';
  if (SQLType = 'TIME') then
    Result := 'TTIME';

  if (SQLType = 'BOOLEAN') then
    Result := 'BOOLEAN';

  if (SQLType = 'SINGLE') then
    Result := 'SINGLE';
   if (SQLType = 'DOUBLE') then
    Result := 'DOUBLE';
  if (SQLType = 'EXTENDED') then
    Result := 'EXTENDED';
  if (SQLType = 'REAL') then
    Result := 'SINGLE';
  if (SQLType = 'FLOAT') then
    Result := 'DOUBLE';
  if (SQLType = 'DOUBLE PRECISION') then
    Result := 'DOUBLE';
  if (SQLType = 'DEC') then
    Result := 'EXTENDED';
  if (SQLType = 'NUMERIC') then
    Result := 'CURRENCY';

  if (SQLType = 'INTEGER') then
    Result := 'INTEGER'; //INTEGER = LONGINT or Cardinal = LongWord
  if (SQLType = 'SMALLINT') then
    Result := 'SMALLINT'; // SmallInt or Word
  if (SQLType = 'BIGINT') then
    Result := 'INT64'; // Int64
end;

function loadTableFields(tblName: string): tblStructure;
var
  I: integer;
begin
  tblName := lowercase(trim(tblName));
  for I := low(workingSchema.tables) to high(workingSchema.tables) do
    if (workingSchema.tables[I].tblName = tblName) then
    begin
      Result := workingSchema.tables[I];
      exit;
    end;
end;

{ Oracle. Oracle's E-Business Suite (also known as Applications/Apps or EB-Suite/EBS)
  consists of a collection of enterprise resource planning (ERP), customer relationship management (CRM),
  and supply-chain management (SCM) computer applications either developed or acquired by Oracle. }

{$IFDEF mongodb}
function createDefaultSchema(userId: string; dbName: string): boolean;
var
  NestedUserIdInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
begin
  result := False;

 // 'select * from datadicyionary where userid = ' + userId;

  NestedUserIdInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NestedDatabaseInstance.Put('$eq','database');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  if DatabaseCursor.Count <> 0 then
    begin
      // if there is a database, should be the Default as the first one
      // it is created for the user or sys_suid if there is no users yet
      result := True;
    end else
    begin
      // DATABASE_metaData(DBNAME)
      DatabaseInstance := TBSONObject.Create;
      DatabaseInstance.Put('kind','database');
      DatabaseInstance.Put('database_name',dbName);
      DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
      GCollection.Insert(DatabaseInstance);
    end;
end;
{$ENDIF}

{$IFDEF tablesdb}
function createDefaultSchema(dbName: string): boolean;
var
  rowId: int64;
  row: array of variant;
begin
  result := False;

  rowId := DDdatabases.firstRow;
  repeat
    if DDdatabases.existRow(rowId) then
      begin
        // if there is a database, should be the Default as the first one
        // it is created for the user or sys_suid if there is no users yet
        Result := True;
        break;
      end;
    rowId := rowId + 1;
  until rowId > DDdatabases.lastRow;

  if not Result then
    begin
      row := nil;
      setLength(row, 1);
      row[0] := dbName;
      DDdatabases.insertRow(row);
    end;
end;
{$ENDIF}

{$IFDEF mongodb}
function DatabaseExists(dbName: string): boolean;
var
  NesteddbNameInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
begin
  dbName := lowercase(dbName);
  result := False;

  NesteddbNameInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedDatabaseInstance.Put('$eq','database');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

  if DatabaseCursor.Count <> 0 then
    begin
      // if there is a database, should be the Default as the first one
      // it is created for the user or sys_suid if there is no users yet
      result := True;
    end
end;
{$ENDIF}

{$IFDEF tablesdb}
function DatabaseExists(dbName: string): boolean;
var
  rowId: int64;
  database_Name: string;
begin
  dbName := lowercase(dbName);
  Result := False;

  rowId := DDdatabases.firstRow;
  repeat
    if DDdatabases.existRow(rowId) then
      begin
        database_Name := DDdatabases.getValueByColumnName(rowId, 'database_name');
        if (database_Name = dbName) then
        begin
          // if there is a database, should be the Default as the first one
          // it is created for the user or sys_suid if there is no users yet
          Result := True;
          break;
        end;
      end;
    rowId := rowId + 1;
  until rowId > DDdatabases.lastRow;
end;
{$ENDIF}

{$IFDEF mongodb}
function ConstraintNameExists(cnstrName: string): boolean;
var
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
  constraintName: string;
begin
  result := False;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NestedDatabaseInstance.Put('$eq','constraint');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance :=  DatabaseCursor.Next;
      constraintName := DatabaseInstance.Items['constraint_name'].AsString;
      if cnstrName = constraintName then
      begin
        result := True;
        break;
      end;
    end;
end;
{$ENDIF}

{$IFDEF tablesdb}
function ConstraintNameExists(cnstrName: string): boolean;
var
  rowId: int64;
  constraintName: string;
begin
  result := False;
  rowId := DDconstraints.firstRow;
  repeat
    if DDconstraints.existRow(rowId) then
    begin
      constraintName :=
        DDconstraints.getValueByColumnName(rowId, 'constraint_name');
      if cnstrName = constraintName then
        begin
          result := True;
          break;
        end;
    end;
    rowId := rowId + 1;
  until rowId > DDconstraints.lastRow;
end;
{$ENDIF}

{$IFDEF mongodb}
function IndexNameExists(IdxName: string): boolean;
var
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
begin
  result := false;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NestedDatabaseInstance.Put('$eq','index');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance :=  DatabaseCursor.Next;
      if IdxName =
         DatabaseInstance.Items['index_name'].AsString then
        begin
          result := True;
          break;
        end;
   end;
end;

{$ENDIF}

{$IFDEF tablesdb}
function IndexNameExists(IdxName: string): boolean;
var
  rowId: Integer;
begin
  result := False;
  rowId := DDIndexes.firstRow;
  repeat
    if DDIndexes.existRow(rowId) then
      begin
        if IdxName =
           DDIndexes.getValueByColumnName(rowId, 'Index_name') then
          begin
            result := True;
            break;
          end;
      end;
    rowId := rowId + 1;
  until rowId > DDIndexes.lastRow;
end;
{$ENDIF}

{$IFDEF mongodb}
function UserIdExists(userId: string): boolean;
var
  NestedUserIdInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
begin
  userId := lowercase(userId);
  result := False;

  NestedUserIdInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NestedUserIdInstance.Put('$eq',userId);
  NestedDatabaseInstance.Put('$eq','user');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('user_id',NestedUserIdInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

  if DatabaseCursor.Count <> 0 then result := True;
end;

function UserIdExists(userId: string; Password: string): boolean;
var
  NestedUserIdInstance: IBSONObject;
  NestedPasswordInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
begin
  userId := lowercase(userId);
  result := False;

  NestedPasswordInstance := TBSONObject.Create;
  NestedUserIdInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NestedPasswordInstance.Put('$eq',Password);
  NestedUserIdInstance.Put('$eq',userId);
  NestedDatabaseInstance.Put('$eq','user');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('user_id',NestedUserIdInstance);
  DatabaseInstance.Put('password',NestedPasswordInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

  if DatabaseCursor.Count <> 0 then result := true;


end;
{$ENDIF}

{$IFDEF tablesdb}
function UserIdExists(userId: string): boolean;
var
  rowId1: int64;
begin
  userId := lowercase(userId);
  result := False;

  rowId1 := DDusers.firstRow;
  repeat
    if DDusers.existRow(rowId1) then
      if userId = DDusers.getValueByColumnName(rowId1,'user_id') then
        begin
          result := true;
          break
        end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtables.lastRow;
end;

function UserIdExists(userId: string; Password: string): boolean;
var
  rowId1: int64;
begin
  userId := lowercase(userId);
  result := False;

  rowId1 := DDusers.firstRow;
  repeat
    if DDusers.existRow(rowId1) then
      if (userId = DDusers.getValueByColumnName(rowId1,'user_id')) and
         (password = DDusers.getValueByColumnName(rowId1,'password')) then
        begin
          result := true;
          break
        end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtables.lastRow;
end;
{$ENDIF}

function loadSchema(dbName: string): boolean;
var
  {$IFDEF tablesdb}
  rowId1, rowId2, rowId3, rowId4: int64;
  joinindex: boolean;
  idxAsc: array of boolean;
  view_Name: string;
  colsname, colstype: array of string;
  allowcolsNull: array of boolean;
  {$ENDIF}
  table_Name: string;
  index: integer;
  index1: integer;
  index2: integer;
  index3: integer;
  constraint_Name: string;
  constraint_type: string;
  column_name: string;
  defaultType: integer;
  TheBaseTables: array of string;
  tblFields: tblStructure;
  colname: string;
  TheKeys: array of string;
  refTableName: string;
  refcolumn_name: string;
  {$IFDEF mongodb}
  NestedTypeInstance: IBSONObject;
  NesteddbNameInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
  lcount: Integer;
  cklcount: Integer;
  {$ENDIF}

begin
  result := False;

  dbName := lowercase(dbName);
  if DatabaseExists(dbName) then
    begin
      result := true;
      workingSchema.dbName := dbName;
      workingSchema.tables := nil;
      workingSchema.views := nil;
      workingSchema.joinidxdata := nil;
    end else
    begin
      workingSchema.dbName := '';
      workingSchema.tables := nil;
      workingSchema.views := nil;
      workingSchema.joinidxdata := nil;
      // yyerror('Database not found'); not necessary
      exit;
    end;

  // load all the views
  {$IFDEF mongodb}
  NesteddbNameInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedDatabaseInstance.Put('$eq','view');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;
      setLength(workingSchema.views, length(workingSchema.views) + 1);
      with workingSchema.views[high(workingSchema.views)] do
        begin
          viewName := DatabaseInstance.Items['view_name'].AsString;
          ViewInstructions := nil;
          lcount := DatabaseInstance.Items['number_of_instructions'].AsInteger;
          for index1 := 0  to lcount - 1 do
            begin
              setlength(ViewInstructions,length(ViewInstructions)+1);
              with ViewInstructions[high(ViewInstructions)] do
                begin
                  mnemonic := DatabaseInstance.Items['mnemonic'+intToStr(index1)].AsInteger;
                  boolvalue := DatabaseInstance.Items['boolvalue'+intToStr(index1)].AsBoolean;
                  value := DatabaseInstance.Items['value'+intToStr(index1)].AsInteger;
                  stvalue := DatabaseInstance.Items['stvalue'+intToStr(index1)].AsString;
                  printInstruction := DatabaseInstance.Items['printInstruction'+intToStr(index1)].AsString
                end
            end;
          numCols := 0;
          // load the number of columns (Available in number_of_columns)
          columns := nil; // to load the columns
        end;
    end;
  {$ENDIF}

  {$IFDEF tablesdb}
  rowId1 := DDviews.firstRow;
  repeat
    if DDviews.existRow(rowId1) then
      begin
        if dbName = DDviews.getValueByColumnName(rowId1, 'database_name') then
          begin
            setLength(workingSchema.views, length(workingSchema.views) + 1);
            with workingSchema.views[high(workingSchema.views)] do
              begin
                viewName := DDviews.getValueByColumnName(rowId1, 'view_name');
                ViewInstructions := nil;
                numCols := DDviews.getValueByColumnName(rowId1, 'number_of_columns');
                // load the number of columns (Available as fourth column in DDviews)
                columns := nil; // to load the columns
                idxname := DDviews.getValueByColumnName(rowId1, 'index_name'); // to load the index
              end;
          end;
      end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtables.lastRow;

  // load views instructions
  rowId1 := DDviewinstructions.firstRow;
  repeat
    if DDviewinstructions.existRow(rowId1) then
      begin
        if DDviewinstructions.getValueByColumnName(rowId1, 'database_name') = dbName then
          begin
            view_Name := DDviewinstructions.getValueByColumnName(rowId1, 'view_name');
            for index := low(workingSchema.views) to high(workingSchema.views) do
              if workingSchema.views[index].viewName = view_name then
                break;
            setlength( workingSchema.views[index].ViewInstructions, length( workingSchema.views[index].ViewInstructions) + 1 );
            with workingSchema.views[index].ViewInstructions[high(
                workingSchema.views[index].ViewInstructions)] do
              begin
                mnemonic := DDviewinstructions.getValueByColumnName(rowId1,'mnemonic');
                boolvalue := DDviewinstructions.getValueByColumnName(rowId1,'boolvalue');
                value := DDviewinstructions.getValueByColumnName(rowId1,'value');
                stvalue := DDviewinstructions.getValueByColumnName(rowId1,'st_value');
                printInstruction := DDviewinstructions.getValueByColumnName(rowId1,'printInstruction')
              end;
          end;
      end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDviewinstructions.lastRow;
  {$ENDIF}

  // load all the tables

  {$IFDEF mongodb}
  NesteddbNameInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedDatabaseInstance.Put('$eq','table');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;
      setLength(workingSchema.tables, length(workingSchema.tables) + 1);
      with workingSchema.tables[high(workingSchema.tables)] do
      begin
        tblName := DatabaseInstance.Items['table_name'].AsString;
        collection := nil; // load the table
        numCols := 0;
        // load the number of columns (Available in number_of_columns)
        columns := nil; // to load the columns
        constraints := nil;
        idxData := nil; // to load the indexes
      end;
    end;
  {$ENDIF}

  {$IFDEF tablesdb}
  rowId1 := DDtables.firstRow;
  repeat
    if DDtables.existRow(rowId1) then
    begin
      if dbName = DDtables.getValueByColumnName(rowId1, 'database_name') then
      begin
        setLength(workingSchema.tables, length(workingSchema.tables) + 1);
        table_Name := DDtables.getValueByColumnName(rowId1, 'table_name');
        with workingSchema.tables[high(workingSchema.tables)] do
        begin
          tblName := table_Name;
          storage := nil; // load the table
          numCols := 0;
          // load the number of columns (Available as third column in DDtables)
          columns := nil; // to load the columns
          constraints := nil;
          idxData := nil; // to load the indexes
        end;
      end;
    end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtables.lastRow;
  {$ENDIF}

  // load all the columns

  {$IFDEF mongodb}
  NesteddbNameInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedDatabaseInstance.Put('$eq','tablecolumn');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;
      table_Name := DatabaseInstance.Items['table_name'].AsString; // table_name could be view_name
      for index := low(workingSchema.tables) to high(workingSchema.tables) do
        if workingSchema.tables[index].tblName = table_name then
          break;
      workingSchema.tables[index].numCols := workingSchema.tables[index].numCols + 1;
      setlength( workingSchema.tables[index].columns, length( workingSchema.tables[index].columns) + 1 );
      with workingSchema.tables[index].columns[high(workingSchema.tables[index].columns)] do
        begin
          colname := DatabaseInstance.Items['column_name'].AsString;
          colSQLtypeName := DatabaseInstance.Items['type_name'].AsString;
          if convertType(colSQLtypeName) = 'INTEGER' then
            coltype := intType;
          if convertType(colSQLtypeName) = 'SMALLINT' then
            coltype := smallintType;
          if convertType(colSQLtypeName) = 'INT64' then
            coltype := int64Type;
          if (convertType(colSQLtypeName) = 'SINGLE') or
            (convertType(colSQLtypeName) = 'DOUBLE') or
            (convertType(colSQLtypeName) = 'EXTENDED') then
            begin
              coltype := extendedType;
              coltypeScale.precision := DatabaseInstance.Items['dim1'].AsInteger;
              coltypeScale.scale := DatabaseInstance.Items['dim2'].AsInteger;
            end;
          if convertType(colSQLtypeName) = 'CURRENCY' then
            begin
              coltype := currencyType;
              coltypeScale.precision := DatabaseInstance.Items['dim1'].AsInteger;
              coltypeScale.scale := DatabaseInstance.Items['dim2'].AsInteger;
            end;
          if convertType(colSQLtypeName) = 'TDATETIME' then
            coltype := tdatetimeType;
          if convertType(colSQLtypeName) = 'TDATE' then
            coltype := tdateType;
          if convertType(colSQLtypeName) = 'TIME' then
            coltype := ttimeType;
          if convertType(colSQLtypeName) = 'BOOLEAN' then
            coltype := booleanType;
          if convertType(colSQLtypeName) = 'STRING' then
            begin
              colTypeScale.size := DatabaseInstance.Items['dim1'].AsInteger;
              coltype := stringType;
            end;
          defaultType := DatabaseInstance.Items['kind_default'].AsInteger;
          colHasAutoIncrement := (defaultType = 8);
          colHasDefault := (defaultType <> -1) and (defaultType <> 8);
          case defaultType of
            0: colDefaultValue := DatabaseInstance.Items['int_default'].Value;
            2: colDefaultValue := DatabaseInstance.Items['int64_default'].Value;
            3, 5: colDefaultValue := DatabaseInstance.Items['ext_default'].Value;
            4: colDefaultValue := DatabaseInstance.Items['currency_default'].Value;
            6: colDefaultValue := DatabaseInstance.Items['boolean_default'].Value;
            7: colDefaultValue := DatabaseInstance.Items['st_default'].Value;
          end;
        end;
    end;
  {$ENDIF}


  {$IFDEF tablesdb}
  rowId1 := DDtablecolumns.firstRow;
  repeat
    if DDtablecolumns.existRow(rowId1) then
      begin
        if DDtablecolumns.getValueByColumnName(rowId1, 'database_name') = dbName then
          begin
            table_Name := DDtablecolumns.getValueByColumnName(rowId1, 'table_name');
            for index := low(workingSchema.tables) to high(workingSchema.tables) do
              if workingSchema.tables[index].tblName = table_name then
                break;
            workingSchema.tables[index].numCols := workingSchema.tables[index].numCols + 1;
            setlength( workingSchema.tables[index].columns, length( workingSchema.tables[index].columns) + 1 );

            with workingSchema.tables[index].columns[high(workingSchema.tables[index].columns)] do
              begin
                colname := DDtablecolumns.getValueByColumnName(rowId1, 'column_name');
                colSQLtypeName := DDtablecolumns.getValueByColumnName(rowId1, 'type_name');
                if convertType(colSQLtypeName) = 'INTEGER' then
                  coltype := intType;
                if convertType(colSQLtypeName) = 'SMALLINT' then
                  coltype := smallintType;
                if convertType(colSQLtypeName) = 'INT64' then
                  coltype := int64Type;
                if (convertType(colSQLtypeName) = 'SINGLE') or
                  (convertType(colSQLtypeName) = 'DOUBLE') or
                  (convertType(colSQLtypeName) = 'EXTENDED') then
                  begin
                    coltype := extendedType;
                    coltypeScale.precision :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'dim1'), 0);
                    coltypeScale.scale :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'dim2'), 0);
                  end;
                if convertType(colSQLtypeName) = 'CURRENCY' then
                  begin
                    coltype := currencyType;
                    colTypeScale.precision :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'dim1'), 0);
                    colTypeScale.scale :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'dim2'), 0);
                  end;
                if convertType(colSQLtypeName) = 'TDATETIME' then
                  coltype := tdatetimeType;
                if convertType(colSQLtypeName) = 'TDATE' then
                  coltype := tdateType;
                if convertType(colSQLtypeName) = 'TIME' then
                  coltype := ttimeType;
                if convertType(colSQLtypeName) = 'BOOLEAN' then
                  coltype := booleanType;
                if convertType(colSQLtypeName) = 'STRING' then
                  begin
                    colTypeScale.size :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'dim1'), 0);
                    coltype := stringType;
                  end;
                defaultType :=
                  strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'kinddefault'), -1);
                colHasAutoIncrement := (defaultType = 8);
                colHasDefault := (defaultType <> -1) and (defaultType <> 8);
                case defaultType of
                  0: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'intdefault');
                  1: colDefaultValue := null;
                  2: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'int64default');
                  3, 5: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                          colDefaultValue := null else
                          colDefaultValue :=
                            DDtablecolumns.getValueByColumnName(rowId1, 'extdefault');
                  4: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'currencydefault');
                  6: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'booleandefault');
                  7: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'stdefault');
                end;
              end;
          end;
      end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtablecolumns.lastRow;
  {$ENDIF}

  // load all constraints for the database

  {$IFDEF mongodb}
  NesteddbNameInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedDatabaseInstance.Put('$eq','constraint');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;

      table_Name := DatabaseInstance.Items['table_name'].AsString;
      for index := low(workingSchema.tables) to high(workingSchema.tables) do
        if workingSchema.tables[index].tblName = table_name then
          break;
      with workingSchema.tables[index] do
        begin
          setLength(constraints, length(constraints) + 1);
          constraint_Name := DatabaseInstance.Items['constraint_name'].AsString;
          constraints[high(constraints)].cnstrname := constraint_Name;
          constraint_type := DatabaseInstance.Items['constraint_type'].AsString;


          // check all columns constraints for the table

          for index2 := 0 to DatabaseInstance.Items['column_count'].AsInteger - 1 do
            begin
              column_Name := DatabaseInstance.Items['column_name'+intToStr(index2)].AsString;

              if (constraint_type = 'NULL') then
                begin
                  for index1 := 0 to numCols - 1 do
                    if column_name = columns[index1].colname then
                      with constraints[high(constraints)] do
                      begin
                        cnstrtype := 0; // Allow Null Values
                        nullCol := Index1;
                        break;
                      end;
                end;

              if (constraint_type = 'NOT NULL') then
                begin
                  for index1 := 0 to numCols - 1 do
                    if column_name = columns[index1].colname then
                      with constraints[high(constraints)] do
                        begin
                          cnstrtype := 1; // Doesn't allow Null Values
                          nnullCol := Index1;
                          break;
                        end;
                end;

              if (constraint_type = 'UNIQUE') then
                begin
                  // NOT NULL must be specified to make the column(s) a key.
                  // It is possible to put UNIQUE constraints on nullable columns but the SQL standard
                  // states that the constraint does not guarantee uniqueness of nullable columns
                  // (uniqueness is not enforced for rows where any of the columns contains a null).
                  for index1 := 0 to numCols - 1 do
                    if column_name = columns[index1].colname then
                      with constraints[high(constraints)] do
                        begin
                          cnstrtype := 2; // not nullable
                          uqCols[0] := setbit(uqCols[0], Index1);
                          break;
                        end;
                end;

              if (constraint_type = 'PRIMARY KEY') then
                begin
                  for index1 :=
                    0 to workingSchema.tables[index].numCols - 1 do
                    if column_name = columns[index1].colname then
                      with constraints[high(constraints)] do
                        begin
                          cnstrtype := 3; // not nullable
                          pkCols[0] := setbit(pkCols[0], Index1);
                          break;
                        end;
                end;

              if (constraint_type = 'CHECK') then
                begin
                  for index1 :=
                    0 to workingSchema.tables[index].numCols - 1 do
                    if column_name = columns[index1].colname then
                      with constraints[high(constraints)] do
                        begin
                          cnstrtype := 5; // CHECK
                          ckCols[0] := setbit(ckCols[0], Index1);
                          break;
                        end;
                end;

              if (constraint_type = 'REFERENCES') then
                begin
                  with constraints[high(constraints)] do
                    begin
                      cnstrtype := 4; // REFERENCES - FOREIGN KEY
                      for index1 :=
                        0 to workingSchema.tables[index].numCols - 1 do
                        if column_name = columns[index1].colname then
                          with constraints[high(constraints)] do
                            begin
                              Cols[0] := setbit(Cols[0], Index1);
                              break;
                            end;
                    end;
                end;
            end;

          if (constraint_type = 'CHECK') then
            with constraints[high(constraints)] do
              begin
                checkCondition := nil;
                for index1 := 0 to DatabaseInstance.Items['checkinstruction_count'].AsInteger - 1 do
                  begin
                    setlength(checkCondition,length(checkCondition)+1);
                    checkCondition[high(checkCondition)].mnemonic := DatabaseInstance.Items['mnemonic'+intToStr(index1)].AsInteger;
                    checkCondition[high(checkCondition)].boolvalue := DatabaseInstance.Items['boolvalue'+intToStr(index1)].AsBoolean;
                    checkCondition[high(checkCondition)].value := DatabaseInstance.Items['value'+intToStr(index1)].AsFloat;
                    checkCondition[high(checkCondition)].stvalue := DatabaseInstance.Items['stvalue'+intToStr(index1)].AsString;
                    checkCondition[high(checkCondition)].printInstruction := DatabaseInstance.Items['printinstruction'+intToStr(index1)].AsString;
                  end;
              end;

          if (constraint_type = 'REFERENCES') then
            begin
              refTableName := DatabaseInstance.Items['references_table'].AsString;
              constraints[high(constraints)].reftblName := refTableName;
              tblFields := loadTableFields(refTableName);
              for index2 := 0 to DatabaseInstance.Items['refcolumn_count'].AsInteger - 1 do
                begin
                  refcolumn_Name := DatabaseInstance.Items['refcolumn_name'+intToStr(index2)].AsString;
                  with constraints[high(constraints)] do
                    begin
                      for index1 :=
                        0 to tblFields.numCols - 1 do
                        if refcolumn_name = tblFields.columns[index1].colname then
                          with constraints[high(constraints)] do
                            begin
                              refCols[0] := setbit(refCols[0], Index1);
                              break;
                            end;
                    end;
                end;
            end;

        end;
    end;

  for index := low(workingSchema.tables) to high(workingSchema.tables) do
    with workingSchema.tables[index] do
      Collection := GDB.GetCollection(tblName + '_' + dbName);
  {$ENDIF}

  {$IFDEF tablesdb}
  rowId1 := DDconstraints.firstRow;
  repeat
    if DDconstraints.existRow(rowId1) then
    begin
      if DDconstraints.getValueByColumnName(rowId1, 'database_name') = dbName then
      begin
        table_Name := DDconstraints.getValueByColumnName(rowId1, 'table_name');
        for index := low(workingSchema.tables) to high(workingSchema.tables) do
          if workingSchema.tables[index].tblName = table_name then
            break;
        with workingSchema.tables[index] do
        begin
          setLength(constraints, length(constraints) + 1);
          constraint_Name :=
            DDconstraints.getValueByColumnName(rowId1, 'constraint_name');
          constraints[high(constraints)].cnstrname := constraint_Name;
          constraint_type :=
            DDconstraints.getValueByColumnName(rowId1,
            'constraint_type');

          // check all columns constraints for the table

          rowId2 := DDcolumnsconstraint.firstRow;
          repeat
            if DDcolumnsconstraint.existRow(rowId2) then
              if constraint_Name =
                DDcolumnsconstraint.getValueByColumnName(rowId2, 'constraint_name') then
              begin

                column_Name :=
                  DDcolumnsconstraint.getValueByColumnName(rowId2, 'column_name');

                if (constraint_type = 'NULL') then
                  begin
                    for index1 := 0 to numCols - 1 do
                      if column_name = columns[index1].colname then
                        with constraints[high(constraints)] do
                        begin
                          cnstrtype := 0; // Allow Null Values
                          nullCol := Index1;
                          break;
                        end;
                  end;

                if (constraint_type = 'NOT NULL') then
                  begin
                    for index1 := 0 to numCols - 1 do
                      if column_name = columns[index1].colname then
                        with constraints[high(constraints)] do
                          begin
                            cnstrtype := 1; // Doesn't allow Null Values
                            nnullCol := Index1;
                            break;
                          end;
                  end;

                if (constraint_type = 'UNIQUE') then
                  begin
                    // NOT NULL must be specified to make the column(s) a key.
                    // It is possible to put UNIQUE constraints on nullable columns but the SQL standard
                    // states that the constraint does not guarantee uniqueness of nullable columns
                    // (uniqueness is not enforced for rows where any of the columns contains a null).
                    for index1 := 0 to numCols - 1 do
                      if column_name = columns[index1].colname then
                        with constraints[high(constraints)] do
                          begin
                            cnstrtype := 2; // not nullable
                            uqCols[0] := setbit(uqCols[0], Index1);
                            break;
                          end;
                  end;

                if (constraint_type = 'PRIMARY KEY') then
                  begin
                    for index1 :=
                      0 to workingSchema.tables[index].numCols - 1 do
                      if column_name = columns[index1].colname then
                        with constraints[high(constraints)] do
                          begin
                            cnstrtype := 3; // not nullable
                            pkCols[0] := setbit(pkCols[0], Index1);
                            break;
                          end;
                  end;

                if (constraint_type = 'CHECK') then
                  begin
                    for index1 :=
                      0 to workingSchema.tables[index].numCols - 1 do
                      if column_name = columns[index1].colname then
                        with constraints[high(constraints)] do
                          begin
                            cnstrtype := 5; // CHECK
                            ckCols[0] := setbit(ckCols[0], Index1);
                            break;
                          end;
                  end;

                if (constraint_type = 'REFERENCES') then
                  begin
                    with constraints[high(constraints)] do
                      begin
                        cnstrtype := 4; // REFERENCES - FOREIGN KEY
                        for index1 :=
                          0 to workingSchema.tables[index].numCols - 1 do
                          if column_name = columns[index1].colname then
                            with constraints[high(constraints)] do
                              begin
                                Cols[0] := setbit(Cols[0], Index1);
                                break;
                              end;
                      end;
                  end;

              end;
            rowId2 := rowId2 + 1;
          until rowId2 > DDcolumnsconstraint.lastRow;

          if (constraint_type = 'CHECK') then
            with constraints[high(constraints)] do
              begin
                checkCondition := nil;
                rowId3 := DDcheckinstructions.firstRow;
                repeat
                  if DDcheckinstructions.existRow(rowId2) then
                    if constraint_Name = DDcheckinstructions.getValueByColumnName(rowId3, 'constraint_name') then
                      begin
                        setlength(checkCondition,length(checkCondition)+1);
                        checkCondition[high(checkCondition)].mnemonic := DDcheckinstructions.getValueByColumnName(rowId3,'mnemonic');
                        checkCondition[high(checkCondition)].boolvalue := DDcheckinstructions.getValueByColumnName(rowId3,'boolvalue');
                        checkCondition[high(checkCondition)].value := DDcheckinstructions.getValueByColumnName(rowId3,'value');
                        checkCondition[high(checkCondition)].stvalue := DDcheckinstructions.getValueByColumnName(rowId3,'stvalue');
                        checkCondition[high(checkCondition)].printInstruction := DDcheckinstructions.getValueByColumnName(rowId1,'printInstruction')
                     end;
                until rowId3 > DDcheckinstructions.lastRow
              end;

          if (constraint_type = 'REFERENCES') then
            begin
              refTableName :=  DDconstraints.getValueByColumnName(rowId1, 'references_table');
              constraints[high(constraints)].reftblName := refTableName;
              tblFields := loadTableFields(refTableName);
              rowId4 := DDrefcolumns.firstRow;
              repeat
                if DDrefcolumns.existRow(rowId4) then
                  if constraint_Name = DDrefcolumns.getValueByColumnName(rowId4, 'constraint_name') then
                    begin
                      refcolumn_Name := DDrefcolumns.getValueByColumnName(rowId4, 'refcolumn_name');
                      with constraints[high(constraints)] do
                        begin
                          for index1 :=
                            0 to tblFields.numCols - 1 do
                            if refcolumn_name = tblFields.columns[index1].colname then
                              with constraints[high(constraints)] do
                                begin
                                  refCols[0] := setbit(refCols[0], Index1);
                                  break;
                                end;
                        end;
                   end;
              until rowId4 > DDrefcolumns.lastRow
            end;



        end;
      end;
    end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDconstraints.lastRow;

  for index := low(workingSchema.tables) to high(workingSchema.tables) do
    begin
      with workingSchema.tables[index] do
        begin
          setlength(colsname, numCols);
          setlength(colstype, numCols);
          setLength(allowcolsNull, numCols);
          for index1 := 0 to numCols - 1 do
            begin
              colsname[index1] := columns[index1].colname;
              colstype[index1] := converttype(columns[index1].colSQLtypename);
              if colstype[index1] = 'STRING' then
                if columns[index1].coltypescale.size <> 0 then
                  colstype[index1] :=
                    colstype[index1] + '[' + IntToStr(columns[index1].coltypescale.size) + ']'
                else
                  colstype[index1] := colstype[index1] + '[255]';
              allowcolsNull[index1] := True;
              for index2 := low(constraints) to high(constraints) do
                with constraints[index2] do
                  begin
                    case cnstrtype of
                      1: if nnullcol = index1 then
                          allowcolsNull[index1] := False;
                      2: if isbitset(uqCols[0], index1) then
                          allowcolsNull[index1] := False;
                      3: if isbitset(pkCols[0], index1) then
                          allowcolsNull[index1] := False;
                    end;
                  end;
            end;
        end;  { TODO : check for null and call load with null also for indexes }
      workingSchema.tables[index].storage :=
        TTableClass.Create(Path + workingSchema.tables[index].tblName + '_' + workingSchema.dbName, True, colsname, colstype, allowcolsNull);
    end;
{$ENDIF}


  { TODO : check for null and call load with null also for indexes }

  {$IFDEF mongodb}
  NesteddbNameInstance := TBSONObject.Create;
  NestedTypeInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedTypeInstance.Put('$eq','mono');
  NestedDatabaseInstance.Put('$eq','index');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('type',NestedTypeInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;
      table_Name := DatabaseInstance.Items['table_name'].AsString;
      for index := low(workingSchema.tables) to high(workingSchema.tables) do
        if workingSchema.tables[index].tblName = table_Name then
          break;
      setLength(workingSchema.tables[index].idxdata, length(
        workingSchema.tables[index].idxdata) + 1);
      workingSchema.tables[index].idxdata[high(
        workingSchema.tables[index].idxdata)].idxname :=
        DatabaseInstance.Items['index_name'].AsString;
      with workingSchema.tables[index].idxdata[high(
            workingSchema.tables[index].idxdata)] do
        begin
          lcount := DatabaseInstance.Items['columns_count'].AsInteger;
          setLength(idxkeys, lcount);
          for index1 := 0 to lcount - 1 do
            with idxkeys[index1] do
              begin
                colName :=
                  DatabaseInstance.Items['column_name'+inttostr(index1)].AsString;
                colOrder :=
                  DatabaseInstance.Items['column_order'+inttostr(index1)].AsBoolean;
              end;
        end;
    end;

  NesteddbNameInstance := TBSONObject.Create;
  NestedTypeInstance := TBSONObject.Create;
  NestedDatabaseInstance := TBSONObject.Create;
  DatabaseInstance := TBSONObject.Create;
  NesteddbNameInstance.Put('$eq',dbname);
  NestedTypeInstance.Put('$eq','join');
  NestedDatabaseInstance.Put('$eq','index');
  DatabaseInstance.Put('kind',NestedDatabaseInstance);
  DatabaseInstance.Put('type',NestedTypeInstance);
  DatabaseInstance.Put('database_name',NesteddbNameInstance);

  DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
  while DatabaseCursor.HasNext do
    begin
      DatabaseInstance := DatabaseCursor.Next;

      setLength(workingSchema.joinidxdata, length(workingSchema.joinidxdata) + 1);
      workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxname :=
        DatabaseInstance.Items['index_name'].AsString;
      workingSchema.joinidxdata[high(workingSchema.joinidxdata)].joincouples := nil;
      workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxkeys := nil;

      lcount := DatabaseInstance.Items['basetables_count'].AsInteger;
      for index1 := 0 to lcount -1 do
        with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
          begin
            setlength(joinbaseTables, length(joinbaseTables) + 1);
            joinbaseTables[index1] := DatabaseInstance.Items['basetable_name'+inttostr(index1)].AsString;
          end;

      lcount := DatabaseInstance.Items['columns_count'].AsInteger;
      for index1 := 0 to lcount -1 do
        with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
          begin
            setlength(idxkeys, length(idxkeys) + 1);
            with idxkeys[high(idxkeys)] do
              begin
                tblName :=
                  DatabaseInstance.Items['table_name'+inttostr(index1)].AsString;
                colName :=
                  DatabaseInstance.Items['column_name'+inttostr(index1)].AsString;
                ColOrder :=
                  DatabaseInstance.Items['column_order'+inttostr(index1)].AsBoolean;
              end;

          end;

      lcount := DatabaseInstance.Items['join_couples'].AsInteger;
      for index1 := 0 to lcount - 1 do
        begin
          with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
            begin
              setlength(joincouples, length(joincouples) + 1);

              joincouples[high(joincouples)].fromTable :=
                  DatabaseInstance.Items['from_table'+inttostr(index1)].AsString;

              joincouples[high(joincouples)].ToTable :=
                DatabaseInstance.Items['to_table'+inttostr(index1)].AsString;

              cklcount := DatabaseInstance.Items['keys_count'+inttostr(index1)].AsInteger;
              for index2 := 0 to cklcount - 1 do
                begin
                  setLength(joincouples[high(joincouples)].keyNames,
                    length(joincouples[high(joincouples)].keyNames) + 1);
                  joincouples[high(joincouples)].keyNames
                    [high(joincouples[high(joincouples)].keyNames)] :=
                      DatabaseInstance.Items['key_name'+intToStr(index1)+'-'+intToStr(index2)].AsString;
                end;
            end;
        end;

      //create the bjointree and open it
      with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
        begin
          TheBaseTables := nil;
          for index1 := 0 to length(joinbaseTables) - 1 do
            begin
              setlength(TheBaseTables, length(TheBaseTables) + 1);
              TheBaseTables[high(TheBaseTables)] := joinbaseTables[index1];
            end;
          idxstorage := BJoinTreeClass.Create(Path + idxName, TheBaseTables);

          for index2 := Low(TheBaseTables) to High(TheBaseTables) do
            begin
              idxstorage.AddTableToDictionary(TheBaseTables[index2]);
              // take all his columns and add them to Dictionary
              // check the null values
              tblFields := loadTableFields(TheBaseTables[index2]);
              for index3 := 0 to tblFields.numCols - 1 do
                begin
                  colname := tblFields.columns[index3].colname;
                  case tblFields.columns[index3].coltype of
                    intType, smallintType, int64Type:
                      idxstorage.AddColumnToDictionary(
                        colname, 'Integer', TheBaseTables[index2]);
                    extendedType:
                      idxstorage.AddColumnToDictionary(
                        colname, 'Extended', TheBaseTables[index2]);
                    TDateTimeType, TDateType, TTimeType:
                      idxstorage.AddColumnToDictionary(
                        colname, 'TDateTime', TheBaseTables[index2]);
                    currencyType:
                      idxstorage.AddColumnToDictionary(
                        colname, 'Currency', TheBaseTables[index2]);
                    booleanType:
                      idxstorage.AddColumnToDictionary(
                        colname, 'Boolean', TheBaseTables[index2]);
                    stringType:
                      idxstorage.AddColumnToDictionary(colname, 'string[' +
                        IntToStr(tblFields.columns[index3].coltypescale.size) + ']', TheBaseTables[index2]);
                  end;
                end;
            end;

          for index1 := low(joincouples) to high(joincouples) do
            with joincouples[index1] do
              idxstorage.AddJoin(fromTable, toTable, keyNames[0]);
          TheKeys := nil;
          setlength(TheKeys, length(idxkeys));
          for index1 := low(idxkeys) to high(idxkeys) do
            TheKeys[index1] :=
              idxkeys[index1].tblName + '.' + idxkeys[index1].colName;
          idxstorage.createBTrees(TheBaseTables, True, TheKeys);
        end;
    end;
  {$ENDIF}

  {$IFDEF tablesdb}





  rowId1 := DDindexes.firstRow;
  repeat
    if DDindexes.existRow(rowId1) then
      begin
        if dbName = DDindexes.getValueByColumnName(rowId1, 'database_name') then
        begin
          table_Name := DDindexes.getValueByColumnName(rowId1, 'table_name');
          if table_Name <> 'sys_JoinIndex' then // mono index
            begin
              for index := low(workingSchema.tables) to high(workingSchema.tables) do
                if workingSchema.tables[index].tblName = table_Name then
                  break;
              setLength(workingSchema.tables[index].idxdata, length(workingSchema.tables[index].idxdata) + 1);
              workingSchema.tables[index].idxdata[high(workingSchema.tables[index].idxdata)].idxname :=
                DDindexes.getValueByColumnName(rowId1, 'index_name');
              joinIndex := False;
            end
           else // join index
            begin
              setLength(workingSchema.joinidxdata, length(workingSchema.joinidxdata) + 1);
              workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxname :=
                DDindexes.getValueByColumnName(rowId1, 'index_name');
              workingSchema.joinidxdata[high(workingSchema.joinidxdata)].joincouples := nil;
              workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxkeys := nil;
              joinIndex := True;

              rowId2 := DDbasetables.firstRow;
              repeat
                if DDbasetables.existRow(rowId2) then
                  begin
                    if workingSchema.joinidxdata[high(workingSchema.tables[index].idxdata)].idxname =
                         DDbasetables.getValueByColumnName(rowId2, 'index_name') then
                      begin
                        with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                          begin
                            setLength(joinBaseTables , Length(joinBaseTables) + 1);
                            joinBaseTables [high(joinBaseTables )] :=
                              DDbasetables.getValueByColumnName(rowId2, 'basetable_name');
                          end;
                      end
                  end;
                rowId2 := rowId2 + 1;
              until rowId2 > DDbasetables.lastRow;

              rowId2 := DDjoinindexes.firstRow;
              repeat
                if DDjoinindexes.existRow(rowId2) then
                  begin
                    with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                      begin
                        setlength(joincouples, length(joincouples) + 1);
                        joincouples[high(joincouples)].fromTable :=
                          DDjoinindexes.getValueByColumnName(rowId2, 'from_table');
                        joincouples[high(joincouples)].toTable :=
                          DDjoinindexes.getValueByColumnName(rowId2, 'to_table');

                        rowId3 := DDkeysjointables.firstRow;
                        repeat
                          if DDkeysjointables.existRow(rowId3) then
                            begin
                              if DDkeysjointables.getValueByColumnName(rowId3, 'index_table') <> idxname then continue;
                              if (DDkeysjointables.getValueByColumnName(rowId3, 'from_table') = joincouples[high(joincouples)].fromTable) and
                                 (DDkeysjointables.getValueByColumnName(rowId3, 'to_table') = joincouples[high(joincouples)].toTable) then
                                begin
                                  setLength(joincouples[high(joincouples)].keyNames,
                                    length(joincouples[high(joincouples)].keyNames) + 1);
                                  joincouples[high(joincouples)].keyNames[high(joincouples[high(joincouples)].keyNames)] :=
                                    DDkeysjointables.getValueByColumnName(rowId3, 'key_name');
                                end
                            end;

                        until rowId3 >  DDkeysjointables.lastRow;
                      end;
                  end;
                rowId2 := rowId2 + 1;
              until rowId2 > DDjoinindexes.lastRow;
            end;

          // load all keyindexes
          rowId2 := DDkeysindexes.firstRow;
          repeat
            if DDkeysindexes.existRow(rowId2) then
              begin
                if workingSchema.tables[index].idxdata[high(workingSchema.tables[index].idxdata)].idxname =
                     DDkeysindexes.getValueByColumnName(rowId2, 'index_name') then
                  if not joinIndex then
                    begin
                      with workingSchema.tables[index].idxdata[high(workingSchema.tables[index].idxdata)] do
                        begin
                          setLength(idxkeys, Length(idxKeys) + 1);
                          with idxkeys[high(idxkeys)] do
                            begin
                              colName :=
                                DDkeysindexes.getValueByColumnName(rowId2, 'column_name');
                              colOrder :=
                                uppercase(DDkeysindexes.getValueByColumnName(
                                rowId2, 'column_order')) = 'TRUE';
                            end;
                        end;
                    end
                   else // join index
                    begin
                      with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                        begin
                          setlength(idxkeys, length(idxkeys) + 1);
                          with idxkeys[high(idxkeys)] do
                            begin
                              tblName :=
                                DDkeysindexes.getValueByColumnName(rowId2, 'table_name');
                              colName := DDkeysindexes.getValueByColumnName(rowId2, 'column_name');
                              ColOrder :=
                                uppercase(DDkeysindexes.getValueByColumnName(rowId2, 'column_order')) = 'TRUE';
                            end;
                        end;
                    end;
              end;
            rowId2 := rowId2 + 1;
          until rowId2 > DDkeysindexes.lastRow;

          if not joinIndex then
            begin
              TheKeys := nil;
              idxASC := nil;
              with workingSchema.tables[index] do
                begin
                  for index1 :=
                    low(idxdata[high(idxdata)].idxkeys)
                    to high(idxdata[high(idxdata)].idxkeys) do
                    with idxdata[high(workingSchema.tables[index].idxdata)].idxkeys[index1] do
                      begin
                        for index2 := 0 to numCols - 1 do
                          begin
                            if colname = columns[index2].colname then
                              break;
                          end;
                        setlength(TheKeys, length(TheKeys) + 1);


                        Thekeys[High(Thekeys)] :=
                          columns[index2].colname + ': ' + convertType(columns[index2].colSQLtypename);
                        if convertType(columns[index2].colSQLtypename) = 'STRING' then
                          if columns[index2].coltypescale.size  <> 0 then
                            Thekeys[High(Thekeys)] :=
                              Thekeys[High(Thekeys)] + '[' + IntToStr(columns[index2].coltypescale.size) + ']'
                           else
                            Thekeys[High(Thekeys)] :=
                              Thekeys[High(Thekeys)] + '[255]';


                        setlength(idxASC, length(idxASC) + 1);
                        idxASC[index1] := colOrder;
                      end;
                  idxdata[high(idxdata)].idxstorage :=
                    BtrPlusClass.Create(Path + idxdata[high(idxdata)].idxname,
                    True, Thekeys, idxAsc);
                end;
            end
           else
            begin

              // You can make couples have a flag for symmetry
              // You can get all the Base Tables from coupleTables

              //create the bjointree and open it
              with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                begin
                  TheBaseTables := nil;
                  for index1 := 0 to length(joinbaseTables) - 1 do
                    begin
                      setlength(TheBaseTables, length(TheBaseTables) + 1);
                      TheBaseTables[high(TheBaseTables)] := joinbaseTables[index1];
                    end;
                  idxstorage := BJoinTreeClass.Create(Path + idxName, TheBaseTables);

                  for index2 := Low(TheBaseTables) to High(TheBaseTables) do
                    begin
                      idxstorage.AddTableToDictionary(TheBaseTables[index2]);
                      // take all his columns and add them to Dictionary
                      // check the null values
                      tblFields := loadTableFields(TheBaseTables[index2]);
                      for index3 := 0 to tblFields.numCols - 1 do
                        begin
                          colname := tblFields.columns[index3].colname;
                          case tblFields.columns[index3].coltype of
                            intType, smallintType, int64Type:
                              idxstorage.AddColumnToDictionary(
                                colname, 'Integer', TheBaseTables[index2]);
                            extendedType:
                              idxstorage.AddColumnToDictionary(
                                colname, 'Extended', TheBaseTables[index2]);
                            TDateTimeType, TDateType, TTimeType:
                              idxstorage.AddColumnToDictionary(
                                colname, 'TDateTime', TheBaseTables[index2]);
                            currencyType:
                              idxstorage.AddColumnToDictionary(
                                colname, 'Currency', TheBaseTables[index2]);
                            booleanType:
                              idxstorage.AddColumnToDictionary(
                                colname, 'Boolean', TheBaseTables[index2]);
                            stringType:
                              idxstorage.AddColumnToDictionary(colname, 'string[' +
                                IntToStr(tblFields.columns[index3].coltypescale.size) + ']', TheBaseTables[index2]);
                          end;
                        end;
                    end;

                  for index1 := low(joincouples) to high(joincouples) do
                    with joincouples[index1] do
                      idxstorage.AddJoin(fromTable, toTable, keyNames[0]);
                  TheKeys := nil;
                  setlength(TheKeys, length(idxkeys));
                  for index1 := low(idxkeys) to high(idxkeys) do
                    TheKeys[index1] :=
                      idxkeys[index1].tblName + '.' + idxkeys[index1].colName;
                  idxstorage.createBTrees(TheBaseTables, True, TheKeys);
                end;

            end;
        end;
      end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDindexes.lastRow;

  {$ENDIF}

end;

function GetTableStructure(tblName: string): integer;
var
  I: integer;
begin
  tblName := trim(lowercase(tblName));
  Result := -1;
  for I := low(workingSchema.tables) to high(workingSchema.tables) do
    if (workingSchema.tables[I].tblName = tblName) then
    begin
      Result := I;
      exit;
    end;

end;

function TableExists(tblName: string): boolean;
var
  I: integer;
begin
  result := false;
  tblName := trim(lowercase(tblName));
  for I := low(workingSchema.tables) to high(workingSchema.tables) do
    if workingSchema.tables[I].tblName = tblName then
    begin
      Result := True;
      exit;
    end;
end;

function ViewExists(viewName: string): boolean;
var
  I: integer;
begin
  result := false;
  viewName := trim(lowercase(viewName));
  for I := low(workingSchema.views) to high(workingSchema.views) do
    if workingSchema.views[I].viewName = viewName then
    begin
      Result := True;
      exit;
    end;
end;

type
  columnStructure = record
    columnName: string;
    columnTypeName: string;
    charTypeSize: integer;
    numTypeSize: array [0..1] of integer;
    hascolumnAutoIncrement: boolean;
    hascolumnDefault: boolean;
    columnDefaultValue: variant;
  end;

type
  constraintStructure = record
    constraintName: string;
    constraintKind: integer;
    constraintType: string;
    columnsName: array of string;
    refcolumnsName: array of string;
    refTable: string;
    checkInstructions: progInstrunctionsType;
  end;

 type
   aggregateset = (nullkind, avgkind, countkind, maxkind, minkind, sumkind);


 type
   resulttabletype = record
     ownertable: array of record
       colname: string;
       tblname: string;
       aliasname: array of string;
     end;
     resultFields: tblStructure;
     {$IFDEF tablesdb}
     resultRow: array of variant;
     {$ENDIF}
     {$IFDEF mongodb}
     resultIBSONInstance: IBSONObject;
     {$ENDIF}
   end;

 type
   conditionInstructionstype = array of singleInstructionType;


   runstacksingletype = record // extvalue for all numbers
     caseValue: integer;       // 7 for Null Value - 8 for dates - 9 for options
     boolValue: boolean;
     intValue: integer;
     int64Value: integer;
     dblValue: double;
     extValue: extended;
     curValue: currency;
     strValue: string;
   end;

   runstacktype = array of runstacksingletype;


{$INCLUDE functionselectextractrow.inc}
{$INCLUDE functioncomprow.inc}

{$IFDEF mongodb}
procedure sys_LoadCSV(parser: TParser; userId, dbName:string; CSVFileName:string);
var
  CSVName: string;
  FileName: string;
  ExtName: string;
  Tablename: string;
  CSVTextFile: Text;
  SDFInstance: SDFClass;
  Line: string;
  LineCounter: integer;
  RowsInserted: int64;
  tblindex: integer;
  found: boolean;
  I, J: integer;
  sqlInsert: string;
  tmp: string;
  columnsHeader: array of string;
  columnsType: array of inttype..stringtype;
  counter: Integer;
  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;
  st: string;
begin

//  CSVFileName := 'hello-pippo-' + CSVFileName;
  yymiscmsgs := nil;
  yyerrmsgs := nil;

  StartTime := Now;



  CSVName := CSVFileName;



  openTables;
  loadSchema(dbName);
  FileName := ExtractFileName(CSVName);
  ExtName := ExtractFileExt(CSVName);
  if upCase(ExtName) = '.CSV' then
    FileName := copy(FileName, 1, Pos('.', FileName) - 1)
  else
  begin
    yyerror('Not a .CSV file');
    Exit;
  end;
  FileName := copy (FileName,pos('-', FileName)+1,length(FileName));
  FileName := copy (FileName,pos('-', FileName)+1,length(FileName));
  Tablename := FileName;
  if not TableExists(TableName) then
  begin
    yyerror('Table ' + TableName + ' doesn''t Exist ');
    Exit;
  end;
  if not FileExists(Path+CSVName) then
  begin
    yyerror('File ' + CSVName + ' doesn''t Exist ');
    Exit;
  end;


  // Check if a missing column has default value, if not is nullable

  AssignFile(CSVTextFile, Path + CSVName);
  Reset(CSVTextFile);
  SDFInstance := SDFClass.Create();
  LineCounter := 0;
  RowsInserted := 0;
  tblindex := GetTableStructure(FileName);
  columnsHeader := nil;
  setlength(columnsHeader,workingSchema.tables[tblIndex].numCols);
  columnsType := nil;
  setlength(columnsType,workingSchema.tables[tblIndex].numCols);
  counter := 0;

  while not EOF(CSVTextFile) do
    begin
      readLn(CSVTextFile, Line);
      if Line = '' then
        continue;
      SDFInstance.GetLexemes(Line);
      Inc(LineCounter);


      if LineCounter = 1 then
      begin
        // Check for the columns Name
        with workingSchema.tables[tblIndex] do
          begin
            if SDFInstance.CountLexemes < Length(columns) then
            begin
              yyacceptmessage('Number of columns in CSV file are less than ' +
                'the number of columns in the table');
              Exit;
            end;
            found := True;
            for I := 0 to SDFInstance.CountLexemes - 1 do
            begin
              found := False;
              for J := 0 to Length(columns) - 1 do
                if uppercase(columns[J].colname) =
                  trim(upperCase(SDFInstance.Lexemes[I])) then
                begin
                  found := True;
                  columnsHeader[counter] := columns[J].colname;
                  columnsType[counter] := columns[J].coltype;
                  counter := counter + 1;
                  break;
                end;
              if not found then
                break;
            end;
            if not found then
            begin
              yyerror('Missing columns');
              Exit;
            end;
          end;
          continue;
      end;
      with workingSchema.tables[tblIndex] do
        begin
          sqlInsert := 'INSERT INTO ' + Filename + ' (';
          for J := 0 to Length(columns) - 1 do
            sqlInsert := sqlInsert + columnsHeader[J] + ', ';
          sqlInsert := copy(sqlInsert, 1, length(sqlInsert) - 2) + ') VALUES (';

          for I := 0 to SDFInstance.CountLexemes - 1 do
          begin
            tmp := '';

            tmp := columns[I].colname;



            tmp := '';
            if (columnsType[I] = TDateTimeType) or (columnstype[I] = stringtype) then
              begin
                tmp := ''' ';
                tmp := trim(tmp);
              end;
            for j := 1 to length(SDFInstance.Lexemes[I]) do
              tmp := tmp + SDFInstance.Lexemes[I][j];
            if (columnsType[I] = TDateTimeType) or (columnsType[I] = stringtype) then
                sqlInsert := sqlInsert + trim(tmp) +'''' + ',' else sqlInsert := sqlInsert + trim(tmp) + ',';
            TMP := TRIM(TMP);
          end;
          sqlInsert := copy(sqlInsert, 1, length(sqlInsert) - 1) + ');';

          //memo4.Lines.add(sqlinsert);
          sqlMemProg := nil;
          yyInputText := sqlInsert;


          repeat
            parser.parse();
          until (sqlMemProg <> nil) or (yyerrmsgs <> nil);

          RowsInserted := RowsInserted + 1;

          {
          if IsConsole then
            WriteLn(RowsInserted) else
            begin
              sqlUnit.Form1.Edit6.Text := inttostr(RowsInserted);
              sqlUnit.Form1.Edit6.Repaint;
            end;
          }

          if yyerrmsgs <> nil then
            yyerror('at row' + inttostr(Rowsinserted));
          selectColsInstructions := nil;

          executeProgram(sqlMemProg, userId, dbName);
        end;
    end;

  yymiscmsgs := nil;
  if yyerrmsgs = nil then
   yyacceptmessage(intToStr(Rowsinserted) + ': Rows inserted in ' + TableName);

  Diff := Now - StartTime;
  DecodeTime(Diff, Hour, Min, Sec, MSec);
  st := 'Elapsed Time = ';
  if Hour <> 0 then
    st := st + IntToStr(Hour) + ' Hours ';
  if Min <> 0 then
    st := st + IntToStr(Min) + ' Minutes ';
  if sec <> 0 then
    st := st + IntToStr(Sec) + ' Seconds ';
  st := st + IntToStr(Msec) + ' MilliSeconds';
  // Writeln('Elapsed Time: ' + st);
  // form1.memo4.Lines.Add(st);


end;

procedure sys_LoadSQL(parser: TParser; userId: string; dbName:string; SQLFileName:string);
var
  ExtName: string;
  SQLInstructions: array of string;
  SQLTextFile: Text;
  Line: string;
  I: Integer;
  flagCreate: boolean;
  IBSONInstance: IBSONObject;
  flagcomment: boolean;
  QueryCollection: TMongoCollection;
begin
  yymiscmsgs := nil;
  yyerrmsgs := nil;
  SQLFileName := ExtractFileName(SQLFileName);
  ExtName := ExtractFileExt(SQLFileName);
  if not (upCase(ExtName) = '.SQL') then
   begin
    yyerror('Not a .SQL file');
    Exit;
  end;

  if not FileExists( Path + SQLFileName) then
  begin
    yyerror('File ' + SQLFileName + ' doesn''t Exist ');
    Exit;
  end;

  SQLInstructions := nil;
  setlength(SQLInstructions,1);
  AssignFile(SQLTextFile, Path + SQLFileName);
  Reset(SQLTextFile);
  flagcomment := false;
  while not EOF(SQLTextFile) do
    begin
      readLn(SQLTextFile, Line);

      if pos('//',Line) <> 0 then
       Line := copy(Line,1,pos('//',Line)-1);
      if pos('/*',Line) <> 0 then
        begin
          flagComment := true;
          Line := copy(Line,1,pos('/*',Line)-1);
        end;
      if flagcomment then
        if pos('*/',Line) <> 0 then
          begin
            Line := copy(Line,pos('*/',Line)+2,length(Line));
            flagComment := false;
          end else Line := '';

      if pos(';', Line) = 0 then
        SQLInstructions[High(SQLInstructions)] :=  SQLInstructions[High(SQLInstructions)] + ' ' + Line
       else
        begin
          SQLInstructions[High(SQLInstructions)] := SQLInstructions[High(SQLInstructions)] + copy(Line,1,pos(';',Line));
          setlength(SQLInstructions,length(SQLInstructions)+1);
          SQLInstructions[High(SQLInstructions)] := SQLInstructions[High(SQLInstructions)] + copy(Line,pos(';',Line)+1,length(Line));
        end;
    end;
  setlength(SQLInstructions,length(SQLInstructions)-1);



  QueryCollection := GDB.GetCollection('query');
  for I := low(SQLInstructions) to high(SQLInstructions) do
  begin

    IBSONInstance :=  TBSONObject.Create;

    IBSONInstance.Put('queryname','sys_' + SQLFileName + '_q'+ IntToStr(I+1));

    IBSONInstance.Put('querytext',SQLInstructions[I]);

    IBSONInstance.Put('querydbname',dbName);

    IBSONInstance.Put('userId',userID);

    IBSONInstance.Put('username','undefined');

    IBSONInstance.Put('submitted',123456789);

    IBSONInstance.Put('_id', TBSONObjectId.NewFrom);

    QueryCollection.Insert(IBSONInstance);


    sqlMemProg := nil;

    yyInputText := SQLInstructions[I];


    repeat
      parser.parse();
    until (sqlMemProg <> nil) or (yyerrmsgs <> nil);

    selectColsInstructions := nil;

    flagCreate := Pos('CREATE DATABASE ', UpperCase(yyInputText)) <> 0;
    openTables;
    {if not flagCreate then
      loadSchema(userId, dbName);}
    if trim(yyInputText) <> '' then
      executeProgram(sqlMemProg, userId, dbName);
    {if not flagCreate then
      closeSchemaTables;}
    closeTables;
  end;

end;
{$ENDIF}

function SupUser(userId: string): string;
begin
  // check the created_by if it is null then return the same userId
  result := userId
end;

function CanUserUseDatabase(UserId: string; dbName: string): Boolean;
begin
  result := true;
end;

function CanUserUseTable(dbuserId: string; dbName: string; tblName: string): Boolean;
begin
  result := true;
end;

{ TODO : Make sure the dimensions are right for extended and strings.. Use truncate }
function IscompatibleType(var varVar: variant; sqltype_name: string; dim1, dim2: integer): variant;
var
  basicType: longInt;
  stVar: string;
  intValue: Extended;
  fracValue: Extended;
  extendedFieldValue: double;
  ErrorCode: Integer;
  i: Integer;
  DefaultFormat: string;
  Year, month, day: word;
  hour, minute, second, millisecond: Word;
begin
  result := varVar;
  basicType := VarType(varVar) and VarTypeMask;
  case basicType of
    varEmpty: exit;
    varNull: exit;
    varSmallInt, varshortInt, varByte:
      begin
        if (sqltype_name = 'SMALLINT') or
           (sqltype_name = 'INTEGER') or
           (sqltype_name = 'BIGINT') or
           (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') or
           (sqltype_name = 'DATE') or
           (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIME') or
           (sqltype_name = 'TIMESTAMP') then
          exit;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            stVar := IntToStr(varVar);
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
        if (sqltype_name = 'BOOLEAN') then
          begin
            if varVar > 0 then result := true else result := false;
          end;
      end;

    varInteger, varWord:
      begin
        if (sqltype_name = 'SMALLINT') then
          begin
            if (varVar > high(smallint)) or (varVar < low(smallint)) then
              begin
                result := null;
                yyerror('Value is not in the range of SMALLINT');
                exit
              end;
          end;
        if (sqltype_name = 'INTEGER') or
           (sqltype_name = 'BIGINT') or
           (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') or
           (sqltype_name = 'DATE') or
           (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIME') or
           (sqltype_name = 'TIMESTAMP') then
          exit;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            stVar := IntToStr(varVar);
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
        if (sqltype_name = 'BOOLEAN') then
          begin
            if varVar > 0 then result := true else result := false;
          end;
      end;

    varLongWord, varInt64, varqword:
      begin
        if (sqltype_name = 'SMALLINT') then
          begin
            if (varVar > high(smallint)) or (varVar < low(smallint)) then
              begin
                result := null;
                yyerror('Value is not in the range of SMALLINT');
                exit
              end;
          end;
        if (sqltype_name = 'INTEGER') then
          begin
            if (varVar > high(integer)) or (varVar < low(integer)) then
              begin
                result := null;
                yyerror('Value is not in the range of INTEGER');
                exit
              end;
          end;
        if (sqltype_name = 'BIGINT') or
           (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') or
           (sqltype_name = 'DATE') or
           (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIME') or
           (sqltype_name = 'TIMESTAMP') then
          exit;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            stVar := IntToStr(varVar);
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
        if (sqltype_name = 'BOOLEAN') then
          begin
            if varVar > 0 then result := true else result := false;
          end;
      end;

    varSingle, varDouble, varCurrency:
      begin
        intValue := Int(varVar);
        fracValue := frac(varVar);
        if (sqltype_name = 'SMALLINT') then
          begin
            if (intvalue > High(smallint)) or (intvalue < Low(smallint)) then
              begin
                result := null;
                yyerror('Value is not in the range of SMALLINT');
                exit
              end;
            result := intValue
          end;
        if (sqltype_name = 'INTEGER') then
          begin
            if (intvalue > High(integer)) or (intvalue < Low(integer)) then
              begin
                result := null;
                yyerror('Value is not in the range of INTEGER');
                exit
              end;
            result := intValue
         end;
        if (sqltype_name = 'BIGINT') then
          begin
            if (intvalue > High(int64)) or (intvalue < Low(int64)) then
              begin
                result := null;
                yyerror('Value is not in the range of INT64');
                exit
              end;
            result := intValue
          end;
        if (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') then
          begin
            if dim1 <> 0 then
              begin
                stVar := floatToStr(intValue);
                if length(stVar) > dim1 - dim2 then
                  begin
                    result := null;
                    yyerror('Value to big for dimension: ' + intToStr(dim1));
                    exit
                  end;
              end;
            if dim2 <> 0 then
              begin
                stVar := floatToStr(fracValue);
                if length(stVar) > dim2 then
                  begin
                    stVar := copy(stVar,1, dim2);
                    result := intValue + strToFloat('0.'+stVar);
                  end;
              end;
          end;
        if (sqltype_name = 'DATE') or
           (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIME') or
           (sqltype_name = 'TIMESTAMP') then
          exit;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            stVar := floatToStr(varVar);
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
        if (sqltype_name = 'BOOLEAN') then
          begin
            if varVar > 0 then result := true else result := false;
          end;
      end;
    varDate:
      begin
        Exit;
      end;
    varOleStr, varDispatch, varVariant, varUnknown,
    varArray, varByRef, varStrArg, varAny, varTypeMask: exit;
    varError:
      begin
        result := null;
        yyerror(' Type Error ');
        exit;
      end;
    varBoolean:
      begin
        if (sqltype_name = 'SMALLINT') or
           (sqltype_name = 'INTEGER') or
           (sqltype_name = 'BIGINT') or
           (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') then
          if varVar then result := 1 else result := 0;

        if (sqltype_name = 'DATE') or
           (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIME') or
           (sqltype_name = 'TIMESTAMP') then
          begin
            result := null;
            yyerror(' Type Error ');
            exit;
          end;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            if varVar then stVar := 'True' else stVar := 'False';
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
        if (sqltype_name = 'BOOLEAN') then exit;
      end;
    varString:
      begin
        if (sqltype_name = 'SMALLINT') then
          begin
            val(varVar,extendedFieldValue,ErrorCode);
            if ErrorCode = 0 then
              if (extendedFieldValue >= low(smallInt)) and (extendedFieldValue <= high(smallInt)) then
                 result := Round(extendedFieldValue)
               else
                begin
                  result := null;
                  yyerror('Value is not in the range of SMALLINT');
                  Exit;
                end
             else
              begin
                result := null;
                yyerror('Type Error ');
                Exit;
              end;
          end;
        if (sqltype_name = 'INTEGER') then
          begin
            val(varVar,extendedFieldValue,ErrorCode);
            if ErrorCode = 0 then
              if (extendedFieldValue >= low(Integer)) and (extendedFieldValue <= high(Integer)) then
                 result := Round(extendedFieldValue)
               else
                begin
                  result := null;
                  yyerror('Value is not in the range of INTEGER');
                  Exit;
                end
             else
              begin
                result := null;
                yyerror('Type Error ');
                Exit;
              end;
          end;
        if (sqltype_name = 'BIGINT') then
          begin
            val(varVar,extendedFieldValue,ErrorCode);
            if ErrorCode = 0 then
              if (extendedFieldValue >= low(Int64)) and (extendedFieldValue <= high(Int64)) then
                 result := Round(extendedFieldValue)
               else
                begin
                  result := null;
                  yyerror('Value is not in the range of INT64');
                  Exit;
                end
             else
              begin
                result := null;
                yyerror('Type Error ');
                Exit;
              end;
          end;

        if (sqltype_name = 'SINGLE') or
           (sqltype_name = 'DOUBLE') or
           (sqltype_name = 'EXTENDED') then
          begin
            val(varVar,extendedFieldValue,ErrorCode);
            if ErrorCode = 0 then
              result := extendedFieldValue
             else
              begin
                result := null;
                yyerror('Type Error ');
                Exit;
              end;
          end;

        // The TDateTime type holds a date and time value.
        // It is stored as a Double variable, with the date as the integral part, and time as fractional part.
        // The date is stored as the number of days since 30 Dec 1899. It really should be 31 Dec.
        // It appears that the reason for Delphi starting at 30 Dec 1899 is to make it as compatible as possible with Excel
        // while at the same time not adopting Excel's incorrectness about dates. Historically, Excel played second fiddle to
        // Lotus 1-2-3. Lotus (which may have got this error from Visicalc) incorrectly considered 1900 to be a leap year hence
        // a value of 60 gives you 29 Feb 1900 in Excel but is interpreted as 28 Feb 1900 in Delphi due to Delphi starting 1 day before.
        // From the 01 Mar 1901 the two date systems give the same result for a given number.
        // 01 Jan 1900 has a days value of 2.

        // Because TDateTime is actually a double, you can perform calculations on it as if it were a number.
        // This is useful for calculations such as the difference between two dates.

        if (sqltype_name = 'DATETIME') or
           (sqltype_name = 'TIMESTAMP') then
          begin
            (*
               y 	=   Year last 2 digits
               yy 	=   Year last 2 digits
               yyyy 	=   Year as 4 digits
               m 	=   Month number no-leading 0
               mm 	=   Month number as 2 digits
               mmm 	=   Month using ShortDayNames (Jan)
               mmmm 	=   Month using LongDayNames (January)
               d 	=   Day number no-leading 0
               dd 	=   Day number as 2 digits
               ddd 	=   Day using ShortDayNames (Sun)
               dddd 	=   Day using LongDayNames  (Sunday)
               ddddd 	=   Day in ShortDateFormat
               dddddd 	=   Day in LongDateFormat

               c 	=   Use ShortDateFormat + LongTimeFormat
               h 	=   Hour number no-leading 0
               hh 	=   Hour number as 2 digits
               n	=   Minute number no-leading 0
               nn 	=   Minute number as 2 digits
               s 	=   Second number no-leading 0
               ss 	=   Second number as 2 digits
               z	=   Milli-sec number no-leading 0s
               zzz 	=   Milli-sec number as 3 digits
               t 	=   Use ShortTimeFormat
               tt 	=   Use LongTimeFormat

               am/pm 	=   Use after h : gives 12 hours + am/pm
               a/p 	=   Use after h : gives 12 hours + a/p
               ampm 	=   As a/p but TimeAMString,TimePMString
               / 	=   Substituted by DateSeparator value
               :	=   Substituted by TimeSeparator value


               Important : if you want to see characters such as dd in
                           the formatted output, placing them in " marks
                           will stop them being interpreted as date or time elements.

               In addition to this formatting, various of the above options
               are affected by the following variables, withe their default values :
                 DateSeparator 	                = /
                 TimeSeparator 	                = :
                 ShortDateFormat 	        = dd/mm/yyyy
                 LongDateFormat 	        = dd mmm yyyy
                 TimeAMString                   = AM
                 TimePMString 	                = PM
                 ShortTimeFormat 	        = hh:mm
                 LongTimeFormat 	        = hh:mm:ss
                 ShortMonthNames 	        = Jan Feb ...
                 LongMonthNames 	        = January, February ...
                 ShortDayNames                  = Sun, Mon ...
                 LongDayNames 	                = Sunday, Monday ...
                 TwoDigitYearCenturyWindow 	= 50
            *)

            DefaultFormat := 'yyyy/mm/dd hh:nn:ss:zzz';
            stVar := varVar;
            stVar := trim(stVar);
            if pos('/',stVar) <> 0 then
              begin
                year := strToIntDef(copy(stVar,1,pos('/',stVar)-1),2000);
                stVar := copy(stVar,pos('/',stVar)+1, length(stVar));
                month := strToIntDef(copy(stVar,1,pos('/',stVar)-1),1);
                stVar := copy(stVar,pos('/',stVar)+1, length(stVar));
                if pos(' ',stVar) = 0 then
                  begin
                    day := strToIntDef(stVar,1);
                    extendedFieldValue := EncodeDate(year, month, day);
                  end  else
                  begin
                    day := strToIntDef(copy(stVar,1,pos(' ',stVar)-1),1);
                    stVar := copy(stVar,pos(' ',stVar)+1, length(stVar));

                    stVar := copy(stVar,pos(' ',stVar)+1, length(stVar));
                    hour := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                    stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                    if pos(':',stVar) = 0 then
                      begin
                        minute := strToIntDef(stVar,0);
                        second := 0;
                        millisecond := 0
                      end else
                      begin
                        minute := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                        stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                        if pos(':',stVar) = 0 then
                          begin
                            second := strToIntDef(stVar,0);
                            millisecond := 0
                          end else
                          begin
                            second := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                            stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                            millisecond := strToIntDef(stVar,0);
                          end
                      end;
                    extendedFieldValue := EncodeDate(year, month, day);
                    if extendedFieldValue >= 0 then
                      extendedFieldValue := extendedFieldValue + EncodeTime(hour, minute, second, millisecond)
                     else
                       extendedFieldValue := extendedFieldValue - EncodeTime(hour, minute, second, millisecond);
                  end;
                result := extendedFieldValue
              end;


            (*
            extendedFieldValue := EncodeDate(1778, 04, 28);
            extendedFieldValue := extendedFieldValue - EncodeTime(5, 34, 56, 789); // - in case extendedfieldvalue less then zero else +
            decodedate(extendedFieldValue, year, month, day);
            decodetime(extendedFieldValue, hour, minute, second, millisecond);
            st := DateToStr(extendedFieldValue);
            st := TimeToStr(extendedFieldValue);
            *)

          end;

        if (sqltype_name = 'DATE') then
          begin
            DefaultFormat := 'yyyy/mm/dd';
            stVar := varVar;
            stVar := trim(stVar);
            if pos('/',stVar) <> 0 then
              begin
                year := strToIntDef(copy(stVar,1,pos('/',stVar)-1),2000);
                stVar := copy(stVar,pos('/',stVar)+1, length(stVar));
                month := strToIntDef(copy(stVar,1,pos('/',stVar)-1),1);
                stVar := copy(stVar,pos('/',stVar)+1, length(stVar));
                day := strToIntDef(stVar,1);
              end;
            extendedFieldValue := EncodeDate(year, month, day);
            result := extendedFieldValue
          end;

        if (sqltype_name = 'TIME') then
          begin
            DefaultFormat := 'hh:nn:ss:zzz';
            stVar := varVar;
            stVar := trim(stVar);
            if pos(':',stVar) <> 0 then
              begin
                hour := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                if pos(':',stVar) = 0 then
                  begin
                    minute := strToIntDef(stVar,0);
                    second := 0;
                    millisecond := 0
                  end else
                  begin
                    minute := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                    stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                    if pos(':',stVar) = 0 then
                      begin
                        second := strToIntDef(stVar,0);
                        millisecond := 0
                      end else
                      begin
                        second := strToIntDef(copy(stVar,1,pos(':',stVar)-1),1);
                        stVar := copy(stVar,pos(':',stVar)+1, length(stVar));
                        millisecond := strToIntDef(stVar,0);
                      end
                  end;
                extendedFieldValue := EncodeTime(hour, minute, second, millisecond);
              end;
            result := extendedFieldValue
          end;
        if (sqltype_name = 'BOOLEAN') then
          begin
            if trim(lowercase(varVar)) = 'true' then varVar := 'True';
            if trim(lowercase(varVar)) = 'false' then varVar := 'False';
          end;
        if (sqltype_name = 'CHARACTER') or
           (sqltype_name = 'CHARACTER VARYING') then
          begin
            stVar := varVar;
            if dim1 <> 0 then
              if length(stVar) > dim1 then stVar := copy(stVar,1,dim1)
             else
              if (sqltype_name = 'CHARACTER') then
                for i := length(stVar) + 1 to dim1 do
                  stVar := stVar + ' ';
            result := stVar;
          end;
      end;
  end;
end;

procedure ParseSQLStatement(sqlStatement: string; out sqlMemProg: progInstrunctionsType);
begin
  yyerrmsgs := nil;
  yymiscmsgs := nil;
  sqlMemProg := nil;
  if pos('//',sqlStatement) <> 0 then
    sqlStatement := copy(sqlStatement,1,pos('//',sqlStatement)-1)
   else
    while pos('/*',sqlStatement) <> 0 do
      sqlStatement := copy(sqlStatement,1,pos('/*',sqlStatement)-1) + copy(sqlStatement,pos('*/',sqlStatement)+2,length(sqlStatement));
  if trim(sqlStatement) <> '' then
    begin
      yyInputText := sqlStatement;
      repeat
        parser.parse();
      until (sqlMemProg <> nil) or (yyerrmsgs <> nil) or (yymiscmsgs <> nil);
    end;
end;

function returnMiscMessage: string;
begin
  if yymiscmsgs <> nil then result := yymiscmsgs[0] else result := ''
end;

function returnErrMessage: string;
begin
  if yyerrmsgs <> nil then result := yyerrmsgs[0] else result := ''
end;

var
  cursors: array of record
    name: string;
    declared: boolean;
    open: boolean;
    {$IFDEF mongodb}
    Collection: TMongoCollection;
    Cursor: IMongoDBCursor;
    {$ENDIF}
  end = nil;



// check for DOUBLE PRECISION -- EXTENDED not working properly saving the right number


procedure ExecuteProgram(sqlMemProg: progInstrunctionsType;
                         dbUserId: string;
                         dbName: string);
type

  DataPointerType =  Integer;

  JSONPairType = record
    key: string;
    Value: record
      kind: (valkndStr, valkndObj, valkndArr);
      str: string;
      obj: array of array of JSONPairType;
//      arr: array of array of JValuelementType
    end;
  end;

  JSONValueType = record
    kind: (valStr, valObj, valArr);
    str: string;
    obj: array of array of JSONPairType;
    Arr: array of array of JSONValueType
  end;

const
   nullDataValue = -1;//'';
var

  fromtables: array of record
    Name: string;
    aliasname: array of string;
    fromFields: tblStructure;
    fromRow: array of variant;
    {$IFDEF mongodb}
    fromIBSONInstance: IBSONObject;
    Cursor: IMongoDBCursor;
    {$ENDIF}
  end = nil;

  ldbName: string = '';
  lUserId: string = '';
  stk: runstacktype;
  i: integer;
  tblName: string = '';
  aliasName: string = '';
  colsName: array of string = nil;
  colTypeName: string = '';
  charSize: integer = 0;
  numSize: array [0..1] of integer;
  columnList: array of columnStructure = nil;
  constraintList: array of constraintStructure = nil;
  cnstrName: string;
  cnstrType: string;
  dfltValue: variant;
  valuesList: array of variant;
  j: integer;
  found: boolean;
  database_Name: string = '';
  row: array of variant;
  dimSize: integer;
  index: integer;
  hasDefault: boolean = false;
  hasAutoincrement: boolean = false;
  colsType: array of string = nil;
  tblFields: tblStructure;
  intFieldValue: integer;
  int64FieldValue: int64;
  booleanFieldValue: boolean;
  nnullFieldValue: boolean;
  indexName: string = '';
  index_type: string = '';
  ordAsc: boolean = True;
  idxAsc: array of boolean = nil;
  Thekeys: array of string;
  expr: progInstrunctionsType = nil;
  fromTableslen: byte = 0;
  aliasindex: integer;
  tblcolName: string = '';
  conditionInstructions: conditionInstructionstype = nil;
  allowcolsNull: array of boolean = nil;
  tbltimestamp: double;
  executePlan: record
    joinFlag: boolean;
    useIndex: boolean;
    Index: array of record
      Number: Integer;
      Name: string;
      colName: string;
      Value:variant;
      mnemonic: integer;
    end
  end;

  ljoinBaseTables: array of string = nil;
  ljoincouples: array of record
    fromTable: string;
    toTable: string;
    keyNames: array of string;
  end = nil;
  lidxkeys: array of record
    tblName: string;
    colName: string;
    colOrder: boolean;
  end = nil;
  TheBaseTables: array of string = nil;
  flag: boolean;
  colName: string= '';
  foundTable: boolean;
  index1, index2: integer;
  type_name: string;
  runstk: runstackType;
  st: string;
  colDefaultValue: variant;
  resultindex: integer;
  resulttable: resultTableType;
  fromindex: integer;
  fromlengthLimit: integer;
  dbCounter: integer = 0;
  table_name: string = '';
  Keys: array of variant;
  InheritedKeys: array of variant;
  index3: integer;
  index_name: string;
  defaultType: integer;
  column_name: string;
  dim1, dim2: integer;
  constraint_Type: string = '';
  setInstructions: array of array of singleInstructionType = nil;
  isexpr: boolean;
  exprName: string;
  grpkind: aggregateset = nullkind;
  SDFInstance: SDFClass;
  ResultRows: Integer = 0;
  DeletedRows: Integer = 0;
  UpdatedRows: Integer = 0;
  Maxdataref: array of dataPointerType = nil;
  dataref: array of dataPointerType = nil;
  index4: Integer;
  {$IFDEF mongodb}
  IBSONInstance: IBSONObject;
  oldIBSONInstance: IBSONObject;
  IBSONInstance2: IBSONObject;
  lValueType: TBSONValueType;
  NestedTypeInstance: IBSONObject;
  NestedtblNameInstance: IBSONObject;
  NesteddbNameInstance: IBSONObject;
  NestedUserIdInstance: IBSONObject;
  NestedDatabaseInstance: IBSONObject;
  DatabaseInstance: IBSONObject;
  DatabaseCursor: IMongoDBCursor;
  BaseCursor: IMongoDBCursor;
  BaseIBSONInstance: IBSONObject;
  oldDatabaseInstance: IBSonObject;
  distinctCollection: TMongoCollection;
  NestedTransactionInstance: IBSonObject;
  TransactionInstance: IBSonObject;
  TransactionsCursor: IMongoDBCursor;
  NestedcolNameInstance: IBSONObject;
  oldsequencesInstance: IBSONObject;
  sequencesInstance: IBSONObject;
  sequencesCursor: IMongoDBCursor;
  AggregateCollection: TMongoCollection;
  AggregateCursor: IMongoDBCursor;
  AggregateInstance: IBSONObject;
  DistinctAggregateCursor: IMongoDBCursor;
  DistinctAggregateInstance: IBSONObject;
  NestedDistinctAggregateInstance: IBSONObject;
  NestedGroupByInstance: IBSONObject;
  GroupByInstance: IBSONObject;
  GroupByCursor: IMongoDBCursor;
  SortIBSONInstance: IBSonObject;
  HintIBSONInstance: IBSonObject;
  GroupBy_Collection: TMongoCollection;
  GroupBy_Instance: IBSONObject;
  GroupBy_Cursor: IMongoDBCursor;
  oldGroupBy_Instance: IBSONObject;
  NestedPrivilegeInstance: IBSONObject;
  privilegeArray: IBSONArray;
  DataDictionaryInstance: IBSONObject;
  updInstance: IBSONObject;
  GCursor: IMongoDBCursor;
  GInstance: IBSONObject;
  {$ENDIF}
  extendedFieldValue: double;
  currencyFieldValue: currency;
  stringFieldValue: string;
  colNames: array of string = nil;
  colTypes: array of typeset = nil;
  rowIndex: array[0..254] of Integer;
  currentTableIndex: Integer;
  flagdone: boolean;
  Row2: array of variant;
  whTheBaseTables: array of string;
  file_name: string = '';
  lcount: Integer;
  exprList: array of array of singleInstructionType = nil;
  index5, index6: integer;
  varFieldValue: variant;
  stcondition: string;
  RowsInserted: integer;
  counter: LongInt;
  SR: TRawByteSearchRec;
  ladd_column: boolean = false;
  ldrop_column: boolean = false;
  lrename_table: boolean = false;
  lrename_column: boolean = false;
  nextfound: boolean;
  stID: string;

  JSONString: record
    kind: (kndObj, kndArr);
    objJSON: array of array of JSONPairType;
    arrJSON: array of array of JSONValueType;
  end;
  JPairs: array of JSonPairType = nil;
  JMembers: array of array of JSONPairType = nil;
  membersCounter: Integer = 0;
  JElements: array of array of JSONValueType = nil;
  elementsCounter: Integer = 0;
  flagDistinctClause: boolean = false;

  flagOrderClause: boolean = false;

  TransactionName: string = '';
  SavePointName: string = '';


  seq_counter: Int64;
  ordercolumnList: array of record
    tblname: string;
    colname: string;
    colorder: boolean;
  end = nil;

  errorcode: Integer;

  cursorName: string;
  useSelect: boolean = false;
  flagAggregate: boolean = false;
  AggregateRowId: Integer;
  DistinctAggregateRowId: Integer;

  groupByColumns: array of string = nil;
  GroupByRowId: Integer;
  flagAllColumnsAggregate: boolean = false;
  flagExpressionAggregate: boolean = false;
  aggregatestk: runstacktype = nil;
  aggregateConditionInstruction: singleInstructiontype;
  ConditionInstruction: singleInstructiontype;
  colType: inttype..stringtype;
  conditionstk: runstacktype = nil;
  AggregateColumns: array of string = nil;
  AggregateCol: Integer = 0;
  AggregateValue: Extended = 0;
  havingconditionInstructions: conditionInstructionstype = nil;
  lPassword: string;
  lPrivilege: array of string = nil;
  dbObject: string = '';
  flagOptionGrant: Boolean;
  flagAll: Boolean;
  setstk: runstacktype = nil;
  reftblName: string = '';
  vardef: variant;
  code: Integer;
  inscols: array of record
    name: string;
    value: variant
  end = nil;
  fromtblName: string = '';
  totblName: string = '';
  fromtocolname: string ='';
  tofromcolname: string ='';
  optionCreateViewCommand: boolean = false;
  viewName: string = '';
  flagSubquery: boolean = false;
  subqueryMemProg: progInstrunctionsType = nil;
  {$IFDEF tablesdb}
  rowid: DataPointerType;
  rowId1, rowId2: Int64;
  colsTxt: string = '';
  idxstorage: BtrPlusClass;
  {$ENDIF}


begin
  if not ((sqlMemProg[high(sqlMemProg)].mnemonic = 1) or
          (sqlMemProg[high(sqlMemProg)].mnemonic = 157) or
          (sqlMemProg[high(sqlMemProg)].mnemonic = 159) or
          (sqlMemProg[high(sqlMemProg)].mnemonic = 165)) then
    if workingSchema.dbName = '' then
      begin
        yyerror('No Database has been selected');
        exit;
      end;
  optionCreateViewCommand := sqlMemProg[high(sqlMemProg)].mnemonic = 226;


 // fromTables := nil;
  dbName := lowercase(trim(dbName));
  charsize := 0;
  numsize[0] := 0;
  numsize[1] := 0;
  dfltValue := null;

  //  workingSchema.dbName := 'sample';

  for i := low(sqlMemProg) to High(sqlMemProg) do
    begin
      if flagSubquery and (sqlMemProg[i].mnemonic <> 68) then
        begin
          setLength(subqueryMemProg,length(subqueryMemProg)+1);
          subqueryMemProg[high(subqueryMemProg)] := sqlMemProg[i];
          continue
        end;

      case sqlMemProg[i].Mnemonic of

        196: // EMPTY JSON OBJECT
        begin
          JSONString.kind := kndObj;
          JSONString.objJSON := nil;
          JSONString.arrJSON := nil;
        end;

        197: // MEMBERS OBJECT
        begin
          setLength(JMembers,length(JMembers)+1);
          setLength(JMembers[High(JMembers)],membersCounter);
          for index := membersCounter - 1 downto 0 do
            begin
              JMembers[High(JMembers),index] := JPairs[High(JPairs)];
              setLength(JPairs,length(JPairs)-1);
            end;
          membersCounter := 0;
          JSONString.kind := kndObj;
          JSONString.objJSON := JMembers;
          JSONString.arrJSON := nil;
        end;

        198: // EMPTY JSON ARRAY
        begin
          JSONString.kind := kndArr;
          JSONString.objJSON := nil;
          JSONString.arrJSON := nil;
        end;

        199: // ELEMENTS ARRAY
        begin
          setLength(JElements,length(JElements)+1);
          setLength(JElements[High(JElements)],elementsCounter);
          {
          for index := elementsCounter - 1 downto 0 do
            begin
              JMembers[High(JMembers),index] := JPairs[High(JPairs)];
              setLength(JPairs,length(JPairs)-1);
            end;
            }
          elementsCounter := 0;
          JSONString.kind := kndArr;
          JSONString.objJSON := nil;
          JSONString.arrJSON := JElements;
        end;

        (*
        200: // JSON MEMBER
        begin
          membersCounter := membersCounter + 1;
        end;

        201: // JSON ELEMENT
        begin
          elementsCounter := elementsCounter + 1;
        end;

        202: // JSON PAIR
        begin
          setlength(JPairs,length(JPairs)+1);
          JPairs[High(JPairs)].key := stk[High(stk)];
          setLength(stk, Length(stk) - 1);
          case JValue[0].kind of
            valStr: JPairs[High(JPairs)].Value.kind := valkndStr;
            valObj: JPairs[High(JPairs)].Value.kind := valkndObj;
            valArr: JPairs[High(JPairs)].Value.kind := valkndArr;
          end;
          JPairs[High(JPairs)].Value.str := JValue[0].str;
          if JValue[0].kind = valStr then
            begin
              setlength(lCoupleJason,length(lCoupleJason)+1);
              lCoupleJason[high(lCoupleJason)].keyName := JPairs[High(JPairs)].key;
              lCoupleJason[high(lCoupleJason)].value := JPairs[High(JPairs)].Value.str
            end;
          JPairs[High(JPairs)].Value.obj := JValue[0].obj;
          JValue := nil;
          //JPairs[High(JPairs)].Value.arr := JValue[0].arr;
        end;

        203: // JSON STRING VALUE
        begin
          setlength(JValue,length(JValue)+1);
          JValue[High(JValue)].kind := valStr;
          JValue[High(JValue)].str := stk[High(stk)];
          JValue[High(JValue)].obj := nil;
          setLength(stk, Length(stk) - 1);
        end;

        204: // JSON OBJECT VALUE
        begin
          setlength(JValue,length(JValue)+1);
          JValue[High(JValue)].kind := valObj;
          JValue[High(JValue)].str := '';
          JValue[High(JValue)].obj := JSONString.objJSON;
          JValue[High(JValue)].Arr := nil;
          JSONString.objJSON := nil;
          JMembers := nil;
        end;

        205: // JSON ARRAY VALUE
        begin
          setlength(JValue,length(JValue)+1);
          JValue[0].kind := valArr;
          JValue[0].str := '';
          JValue[0].obj := nil;
          JValue[0].Arr := JSONString.arrJSON;
          JSONString.arrJSON := nil;
          JElements := nil;
        end;

        206: // JSON STRING
        begin

        end;
  *)

        {$IFDEF mongodb}
        186: // LOADCSV
        begin
          sys_LoadCSV(parser, dbuserId, dbName, file_Name);
        end;

        187: // LOADSQL
        begin
          sys_LoadSQL(parser, dbuserId, dbName, file_Name);
        end;
        {$ENDIF}

        188: // FILE NAME
        begin
          file_name := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        229: // dot
        begin
          // Expr has the column name
          // should look more for full name with ldbname.tblname.colname or dbname.tblname.colname
          tblname := tblname;
          aliasname := aliasname;
          if aliasName <> '' then
            begin
              setLength(Expr,length(Expr) + 1);
              Expr[high(Expr)].mnemonic := 179;
              Expr[high(Expr)].value := 0;
              Expr[high(Expr)].stvalue := aliasName + tableColumnSeperator + colName;
              Expr[high(Expr)].printInstruction := 'EXPRESSION ALIAS';
            end;

          {
          setlength(selectColsInstructions[high(selectColsInstructions)],
            length(expr) + 2);
          with selectColsInstructions[high(selectColsInstructions), index + 2] do
            begin

              mnemonic := 179;
              Value := 0;
              if aliasName <> '' then
              stvalue := aliasName else stValue := tblName;
              printInstruction := 'EXPRESSION ALIAS';

          }
        end;

        207: // SET TRANSACTION
        begin
          // save into log file: START TRANSACTION PROCESSID, PROCESS NUMBER / NAME TIMESTAMP
          {$IFDEF mongodb}
          TransactionInstance := TBSONObject.Create;
          TransactionInstance.Put('trn_kind','START');
          TransactionInstance.Put('p_id',GetProcessID);
          //// TransactionInstance.Put('trn_counter',TransactionsCollection.Count);
          TransactionInstance.Put('trn_name',TransactionName);
          TransactionInstance.Put('ts',now);
          TransactionsCollection.Insert(TransactionInstance);
          {$ENDIF}
          TransactionName := '';
        end;

        208: // ROLLBACK TRANSACTION
        begin
          // check if the Process Counter
          // check for the name if exist
          // Rollback doesn''t need a record
          // go back

          {
          TransactionInstance := TBSonObject.Create;
          TransactionInstance.Put('trn_kind','ROLLBACK');
          TransactionInstance.Put('p_id',GetProcessID);
          TransactionInstance.Put('trn_name',TransactionName);
          TransactionInstance.Put('ts',now);
          TransactionsCollection.Insert(TransactionInstance);
          TransactionName := '';
          }

          {$IFDEF mongodb}
          NestedTransactionInstance := TBSonObject.Create;
          TransactionInstance := TBSonObject.Create;
          NestedTransactionInstance.Put('$eq',GetProcessID);
          TransactionInstance.Put('p_id',NestedTransactionInstance);

          TransactionsCursor := TransactionsCollection.Find(TransactionInstance);

          HintIBSONInstance := TBSonObject.Create;
          HintIBSONInstance.Put('$natural',-1);
          TransactionsCursor.Hint(SortIBSONInstance);
          while TransactionsCursor.HasNext do
            begin
              TransactionInstance := TransactionsCursor.Next;
              // in database, rows are saved into ram commit wrote to log file and redo it in case of failure
              // rollback just cancel all the instruction, till save point or start transaction, from the log file
  {            stKind := TransactionInstance.Items['trn_kind'].AsString;
              if stKind = 'start' then finish;
              if stKind = 'read' then nothing;
              if stKind = 'insert' then delete;
              if stKind = 'update' then restore old value;
              if stKind = 'delete' then insert;
  }




              // go back till you find start
            end;
          {$ENDIF}




          // Go back and return to undo all the staff done by Getprocessid
          // search all transactions  previous transaction

        end;

        209: // ROLLBACK TO
        begin
          // check for the process id
          // check for the name if exist
          // go back  to savepoint

          {$IFDEF mongodb}
          TransactionInstance.Put('trn_kind','ROLLBACK TO');
          TransactionInstance.Put('p_id',GetProcessID);
          TransactionInstance.Put('trn_name',TransactionName);
          TransactionInstance.Put('ts',now);
          {$ENDIF}
          // Go back and return to undo all the staff done by Getprocessid till
          // savepoint, delete all saveoints that come after it
        end;

        210: // COMMIT TRANSACTION
        begin
          // check for the process id
          // check for the name if exist
          // COMMIT

          {$IFDEF mongodb}
          TransactionInstance.Put('trn_kind','COMMIT');
          TransactionInstance.Put('p_id',GetProcessID);
          TransactionInstance.Put('trn_name',TransactionName);
          TransactionInstance.Put('ts',now);
          {$ENDIF}
        end;

        211: // TRANSACTION NAME
        begin
          TransactionName := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        212: // HOLD SAVEPOINT
        begin
          // CREATE A SAVEPOINT
          {$IFDEF mongodb}
          TransactionInstance.Put('trn_kind','HOLD SAVEPOINT');
          TransactionInstance.Put('p_id',GetProcessID);
          TransactionInstance.Put('trn_counter',TransactionsCollection.Count);
          TransactionInstance.Put('trn_name',TransactionName);
          TransactionInstance.Put('ts',now);
          {$ENDIF}
        end;

        213:  // SAVEPOINT NAME
        begin
          SavePointName := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        214: // RELEASE SAVEPOINT
        begin
          // RELEASE SAVEPOINT
          // CREATE A SAVEPOINT
          // GO BACK AND DELETE SAVEPOINT

        end;


        {
          cursor name
          use cursor
          coming to select if used then error cursor used
          else use table with the name of the cursor
          open cursor check if used, put to the first if empty then yyerror not found
          fetch get the current and move the cursor
          close id not used anymore delete the table
          cursor_declaration used to
        }

        215: // CURSOR NAME
        begin
          cursorName := lowercase(stk[High(stk)].strValue);
          setLength(stk, Length(stk) - 1);
          cursorname := 'sys_cursor_' + cursorName + '_' + dbName;
          found := false;
          for index := 0 to length(cursors) - 1 do
            begin
              if cursorName = cursors[index].name then
                begin
                  found := true;
                  break;
                end;
            end;
          if not found then
            begin
              setLength(cursors,length(cursors)+1);
              cursors[high(Cursors)].name := cursorName;
              cursors[high(Cursors)].declared := false;
              cursors[high(Cursors)].open := false;
              {$IFDEF mongodb}
              cursors[high(Cursors)].Collection := GDB.GetCollection(CursorName);
              {$ENDIF}
            end;
        end;

        216: // START CURSOR DECLARATION
        begin
          for index := length(cursors) - 1 downto 0 do
            begin
              if cursorName = cursors[index].name then
                begin
                  if cursors[index].declared then
                    begin
                      yyerror('Cursor already declared: ' + cursorname);
                      Exit
                    end
                   else
                    begin
                      cursors[index].declared := true;
                      useSelect := true;
                      {$IFDEF mongodb}
                      GCollection.Drop;
                      cursors[index].Collection.Drop;
                      {$ENDIF}
                      break;
                    end;
                end;
            end;
        end;

        220: // END CURSOR DECLARATION
        begin
          useSelect := false;
        end;

        217: // OPEN CURSOR
        begin
          for index := 0 to length(cursors) - 1 do
            begin
              if cursorName = cursors[index].name then
                begin
                  if not cursors[index].declared then
                    yyerror('Cursor not declared: ' + cursorname) else
                    begin
                      cursors[index].open := true;
                      {$IFDEF mongodb}
                      cursors[index].Cursor := cursors[index].Collection.Find();
                      {$ENDIF}
                    end;
                  break;
                end;
            end;
        end;

        218: // FETCH CURSOR
        begin
          for index := 0 to length(cursors) - 1 do
            begin
              if cursorName = cursors[index].name then
                begin
                  if not cursors[index].open then
                    yyerror('Cursor not open: ' + cursorname) else
                    begin
                      {$IFDEF mongodb}
                      if cursors[index].Cursor.HasNext then
                        begin
                          IBSONInstance := cursors[index].Cursor.Next;
                          resParams := nil;
                          for index1 := 4 to IBSONInstance.Count - 1 do
                            begin
                              setLength(resParams,length(resParams)+1);
                              resParams[High(resParams)] := IBSONInstance.Item[index1].AsString;
                            end;
                        end
                       else
                        yyerror('End of data in: ' + cursorName);
                      {$ENDIF}
                    end;
                  break;
                end;
            end;
        end;

        219: // CLOSE CURSOR
        begin
          for index := 0 to length(cursors) - 1 do
            begin
              if cursorName = cursors[index].name then
                begin
                  if not cursors[index].open then
                    yyerror('Cursor not open: ' + cursorname) else
                    begin
                      {$IFDEF mongodb}
                      cursors[index].Collection.Drop;
                      cursors[index].Collection.Free;
                      {$ENDIF}
                    end;
                  break;
                end;
            end;
          for index1 := index to length(cursors) - 2 do
            cursors[index1] := cursors[index1+1];
          setLength(cursors,length(cursors)-1);
        end;

        88: // PUSH: value
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 4;
          stk[High(stk)].extValue := sqlMemProg[i].Value;
          stk[High(stk)].strValue := FloatToStr(sqlMemProg[i].Value);
        end;

        89: // PUSH: literal
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 6;
          stk[High(stk)].strValue := sqlMemProg[i].stvalue;
        end;

        221: // PUSH: boolean
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 0;
          stk[High(stk)].boolValue := sqlMemProg[i].Boolvalue;
          stk[High(stk)].strValue  := BoolToStr(sqlMemProg[i].Boolvalue,True);
        end;

        241: // PUSH: null
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 7;
          stk[High(stk)].strValue  := 'null';
        end;

        234: // PUSH: option
        begin
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 9;
          stk[High(stk)].strValue := sqlMemProg[i].stvalue;
        end;

        119: // PUSH: name
        begin
          setLength(stk, Length(stk) + 1);
          stk[High(stk)].caseValue := 6;
          stk[High(stk)].strValue := sqlMemProg[i].stvalue;
        end;

        158: // USER ID: POP lUserId and insert it into a structure
        begin
          lUserId := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        // distinct should be inserted after from
        // should lookfor union before

        222: // UNION
         begin
           fromTables := nil;
           fromTablesLen := 0;
         end;

        72: // UNION ALL
        begin
          fromTables := nil;
          fromTablesLen := 0;
          selectColsInstructions := nil;
          conditionInstructions := nil;
          resultrows := 1;
        end;

        73: // INTERSECT
        begin
          fromTables := nil;
          fromTablesLen := 0;
          selectColsInstructions := nil;
          conditionInstructions := nil;
          resultrows := 1;
        end;

        74: // MINUS
        begin
          fromTables := nil;
          fromTablesLen := 0;
          selectColsInstructions := nil;
          conditionInstructions := nil;
          resultrows := 1;
        end;

       (* SHOW ALL DATABASES for a specific USERID, it's more complicared now
          check all the users and find the database that can see
          for every user we have a list of all database he can see
          if he can see at least one object of the database, he can use the database
          if granted he can see the tables of superuser and create his own databases
          he can see and modify all the objects of users created by him
       *)

        157: // SHOW ALL DATABASES
        begin
          found := False;
          SDFInstance := SDFClass.Create;

          if luserId = '' then luserId := dbuserId;

          {$IFDEF mongodb}
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedDatabaseInstance.Put('$eq','database');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              if not found then
                begin
                  sqlResults := nil;
                  if luserId <> dbuserId then
                    begin
                      setLength(sqlResults, length(sqlResults) + 1);
                      sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'REQUEST FOR',True);
                    end;
                  setLength(sqlResults, length(sqlResults) + 1);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'DATABASE NAME',True);
                end;
              database_Name := DatabaseInstance.Items['database_name'].AsString;
              (* The dbuserId is the one that issue the command.
                 If luserId is created by him, so he can requests to see
                   all his databases *)
              if CanUserUseDatabase(luserId,database_Name) then
                begin
                  if luserId <> dbuserId then
                    begin
                      setLength(sqlResults, length(sqlResults) + 1);
                      sqlResults[high(sqlResults)] :=  SDFInstance.AddString(sqlResults[high(sqlResults)],luserId,True);
                    end;
                  setLength(sqlResults, length(sqlResults) + 1);
                  sqlResults[high(sqlResults)] :=  SDFInstance.AddString(sqlResults[high(sqlResults)],database_Name,True);

                  resultRows := dbCounter + 1;
                  IBSONInstance := TBSONObject.Create;
                  IBSONInstance.Put('sys_queryId', lqueryId);
                  IBSONInstance.Put('sys_user', dbuserId);
                  IBSONInstance.Put('rowId', resultRows);
                  IBSONInstance.Put('request_for', luserId);
                  IBSONInstance.Put('DATABASE NAME', database_name);
                  IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                  GCollection.Insert(IBSONInstance);
                  dbCounter := dbCounter + 1;
                  found := True;
                end;
            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          rowId1 := DDdatabases.firstRow;
          repeat
            if DDdatabases.existRow(rowId1) then
              begin
                if not found then
                  begin
                    sqlResults := nil;
                    {
                    if luserId <> dbuserId then
                      begin
                        setLength(sqlResults, length(sqlResults) + 1);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'REQUEST FOR',True);
                      end;
                    }
                    setLength(sqlResults, length(sqlResults) + 1);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'ROW ID',True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'USER ID',True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'DATABASE NAME',True);
                  end;
                database_Name :=
                  DDdatabases.getValueByColumnName(rowId1, 'database_name');
                if CanUserUseDatabase(luserId,database_Name) then
                  begin
                    {
                    if luserId <> dbuserId then
                      begin
                        setLength(sqlResults, length(sqlResults) + 1);
                        sqlResults[high(sqlResults)] :=  SDFInstance.AddString(sqlResults[high(sqlResults)],luserId,True);
                      end;
                    }
                    resultRows := dbCounter + 1;
                    setLength(sqlResults, length(sqlResults) + 1);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],IntToStr(resultRows),True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],luserId,True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],database_Name,True);

                    colsTxt := ''; //'sys_queryId: ' + lqueryId + ' ' + 'sys_user: ' + dbUserId + ' ';

                    {
                    if luserId <> dbuserId then
                      begin
                        colsIdTxt := colsIdTxt + 'request_for' + #8;
                        colsValTxt := colsValTxt + luserId + #8;
                        colsTxt := colsTxt + 'request_for: ' + luserId + ' ';
                      end;
                    }

                    colsTxt := colsTxt + 'UserID: ' + luserId + ' ';
                    colsTxt := colsTxt + 'rowId: ' + intToStr(resultRows) + ' ' + 'DATABASE NAME: ' + database_name;

                    // writeln(outText,colsIdTxt);
                    // writeln(outText,colsValTxt);
                    writeln(outText,colsTxt);

                    dbCounter := dbCounter + 1;
                    found := True;
                  end;
              end;
            rowId1 := rowId1 + 1;
          until rowId1 > DDdatabases.lastRow;
          {$ENDIF}

          SDFInstance.Free;

          if dbCounter <> 0  then
            yyacceptmessage(IntToStr(dbCounter) + ' databases have been found')
           else
            yyerror('No database found');

        end;

        (* check if the user has access to the database *)
        159: // SWITCH DATABASE
        begin

          if not CanUserUseDatabase(dbuserId,ldbName) then
            begin
              yyerror('User cannot use the database' + ldbName);
              Exit;
            end;

          {$IFDEF mongodb}
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','database');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          if DatabaseCursor.Count <> 0 then
            begin
              loadSchema(ldbname);
              yyacceptmessage( 'Switch to Database: ' + ldbName )
             end
           else yyerror( 'Database ' + ldbName + ' don''t exist' );
           {$ENDIF}

           {$IFDEF tablesdb}
           rowId1 := DDdatabases.firstRow;
           repeat
             if DDdatabases.existRow(rowId1) then
               begin
                 database_Name :=
                   DDdatabases.getValueByColumnName(rowId1, 'database_name');
                 if database_Name = ldbname then
                   begin
                     loadSchema(ldbname);
                     yyacceptmessage( 'Switch to Database: ' + ldbName );
                     break
                   end;
               end;
             rowId1 := rowId1 + 1;
           until rowId1 > DDdatabases.lastRow;
           if rowId1 > DDdatabases.lastRow then yyerror( 'Database ' + ldbName + ' don''t exist' );
           {$ENDIF}

        end;

        160: // SHOW ALL TABLES
        begin
          found := False;
          SDFInstance := SDFClass.Create;

          if ldbName = '' then  ldbName := dbName;
          if not CanUserUseDatabase(dbuserId,ldbName) then
            begin
              yyerror('User cannot use the database ' + ldbName);
              Exit;
            end;

          {$IFDEF mongodb}
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance :=  DatabaseCursor.Next;
              if not found then
                begin
                  sqlResults := nil;
                  setLength(sqlResults, length(sqlResults) + 1);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'TABLE NAME',True);
                end;
              table_Name := DatabaseInstance.Items['table_name'].AsString;
              if CanUserUseTable(dbuserId,ldbName,table_Name) then
                begin
                  setLength(sqlResults, length(sqlResults) + 1);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],table_Name,True);

                  resultRows := dbCounter + 1;
                  IBSONInstance := TBSONObject.Create;
                  IBSONInstance.Put('sys_queryId', lqueryId);
                  IBSONInstance.Put('sys_user', dbuserId);
                  IBSONInstance.Put('rowId', resultRows);
                  IBSONInstance.Put('TABLE NAME', table_name);
                  IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                  GCollection.Insert(IBSONInstance);

                  dbCounter := dbCounter + 1;
                  found := True;
                end;
            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          rowId1 := DDtables.firstRow;
          repeat
            if DDtables.existRow(rowId1) then
              begin
                database_Name :=
                  DDtables.getValueByColumnName(rowId1, 'database_name');
                if ldbName = database_Name then
                  begin
                    if not found then
                      begin
                        sqlResults := nil;
                        setLength(sqlResults, length(sqlResults) + 1);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'ROW ID',True);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'TABLE NAME',True);
                      end;
                    table_Name :=
                      DDtables.getValueByColumnName(rowId1, 'table_name');
                    if CanUserUseTable(dbuserId,ldbName,table_Name) then
                      begin
                        resultRows := dbCounter + 1;
                        setLength(sqlResults, length(sqlResults) + 1);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],IntToStr(resultRows),True);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],table_Name,True);

                        colsTxt := ''; //'sys_queryId: ' + lqueryId + ' ' + 'sys_user: ' + dbUserId + ' ';
                        colsTxt := colsTxt + 'rowId: ' + intToStr(resultRows) + ' ' + 'TABLE NAME: ' + table_name;
                        // writeln(outText,colsIdTxt);
                        // writeln(outText,colsValTxt);
                        writeln(outText,colsTxt);

                        dbCounter := dbCounter + 1;
                        found := True;
                      end;
                  end;
              end;
            rowId1 := rowId1 + 1;
          until rowId1 > DDtables.lastRow;
          {$ENDIF}

          SDFInstance.Free;

          if dbCounter <> 0 then
            yyacceptmessage(IntToStr(dbCounter) + ' tables have been found')
           else
            yyerror('No table found');
        end;

        161: // SHOW ALL COLUMNS
        begin

          found := False;
          SDFInstance := SDFClass.Create;

          if ldbName = '' then  ldbName := dbName;
          if not CanUserUseDatabase(dbuserId,ldbName) then
            begin
              yyerror('User cannot use the database ' + ldbName);
              Exit;
            end;

          if not CanUserUseTable(dbuserId,ldbName,tblName) then
            begin
              yyerror('User don''t have access to table ' + tblName + ' from database ' + ldbName);
              Exit;
            end;

          {$IFDEF mongodb}
          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblName);

          NesteddbNameInstance.Put('$eq',dbname);
          NestedDatabaseInstance.Put('$eq','tablecolumn');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance :=  DatabaseCursor.Next;

              if not found then
                begin
                  sqlResults := nil;
                  setLength(sqlResults, length(sqlResults) + 1);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'COLUMN NAME',True);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'TYPE',True);
                  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'DEFAULT',True);
                end;

              column_Name := DatabaseInstance.Items['column_name'].AsString;

              type_name := DatabaseInstance.Items['type_name'].AsString;
              dim1 := DatabaseInstance.Items['dim1'].AsInteger;
              dim2 := DatabaseInstance.Items['dim2'].AsInteger;
              if dim1 <> 0 then
                begin
                  type_name := type_name + '(' + intToStr(dim1);
                  if dim2 <> 0 then
                    type_name := type_name + ',' + intToStr(dim2);
                  type_name := type_name + ')'
                end;

              defaultType := DatabaseInstance.Items['kind_default'].AsInteger;
              case defaultType of
                0: colDefaultValue := DatabaseInstance.Items['int_default'].AsInteger;
                2: colDefaultValue := DatabaseInstance.Items['int64_default'].AsInt64;
                3, 5: colDefaultValue := DatabaseInstance.Items['ext_default'].AsFloat;
                4: colDefaultValue := DatabaseInstance.Items['currency_default'].AsFloat;
                6: colDefaultValue := DatabaseInstance.Items['boolean_default'].AsBoolean;
                7: colDefaultValue := DatabaseInstance.Items['st_default'].AsString;
                8: colDefaultValue := 'AUTOINCREMENT';
               -1: colDefaultValue := ''
              end;

{
              cnsNestedtblNameInstance := TBSONObject.Create;
              cnsNesteddbNameInstance := TBSONObject.Create;
              cnsNestedUserIdInstance := TBSONObject.Create;
              cnsNestedDatabaseInstance := TBSONObject.Create;
              cnsDatabaseInstance := TBSONObject.Create;
              cnsNestedtblNameInstance.Put('$eq',tblName);

              cnsNesteddbNameInstance.Put('$eq',dbname);
              cnsNestedUserIdInstance.Put('$eq',dbuserId);
              cnsNestedDatabaseInstance.Put('$eq','constraint');
              cnsDatabaseInstance.Put('kind',cnsNestedDatabaseInstance);
              cnsDatabaseInstance.Put('user_id',cnsNestedUserIdInstance);
              cnsDatabaseInstance.Put('database_name',cnsNesteddbNameInstance);
              cnsDatabaseInstance.Put('table_name',cnsNestedtblNameInstance);

              cnsDatabaseCursor := DataDictionaryCollection.Find(cnsDatabaseInstance);
              while cnsDatabaseCursor.HasNext do
                begin
                  cnsDatabaseInstance :=  DatabaseCursor.Next;
                  lcount := cnsDatabaseInstance.Items['columns_count'].AsInteger;
                  for index2 := 0 to lcount - 1 do
                    if column_name = cnsDatabaseInstance.Items['column_name'+inttostr(index2)].AsString then
                      begin
                        constraint_Type := cnsDatabaseInstance.Items['constraint_type'].AsString;
                        column_constraint := column_constraint  + constraint_Type;
                      end;
                end;
}

              setLength(sqlResults, length(sqlResults) + 1);
              sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],column_Name,True);
              sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],type_Name,True);
              sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],colDefaultValue,True);


              resultRows := dbCounter + 1;
              IBSONInstance := TBSONObject.Create;
              IBSONInstance.Put('sys_queryId', lqueryId);
              IBSONInstance.Put('sys_user', dbuserId);
              IBSONInstance.Put('rowId', resultRows);
              IBSONInstance.Put('TABLE NAME', tblname);
              IBSONInstance.Put('COLUMN NAME', column_Name);
              IBSONInstance.Put('TYPE NAME', type_name);
              IBSONInstance.Put('DEFAULT VALUE', colDefaultValue);

              IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
              GCollection.Insert(IBSONInstance);

              dbCounter := dbCounter + 1;
              found := True;

            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          rowId1 := DDtablecolumns.firstRow;
          repeat
            if DDtablecolumns.existRow(rowId1) then
              begin
                database_Name :=
                  DDtablecolumns.getValueByColumnName(rowId1, 'database_name');
                table_name :=
                  DDtablecolumns.getValueByColumnName(rowId1, 'table_name');

                if ((dbName = database_Name) and
                    (tblName = table_Name)) then
                  begin
                    if not found then
                      begin
                        sqlResults := nil;
                        setLength(sqlResults, length(sqlResults) + 1);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'ROW ID',True);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'COLUMN NAME',True);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'TYPE',True);
                        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'DEFAULT',True);
                      end;

                    column_Name :=
                      DDtablecolumns.getValueByColumnName(rowId1, 'column_name');

                    type_name :=
                      DDtablecolumns.getValueByColumnName(rowId1, 'type_name');
                    dim1 :=
                      StrToInt(DDtablecolumns.getValueByColumnName(rowId1, 'dim1'));
                    dim2 :=
                      StrToInt(DDtablecolumns.getValueByColumnName(rowId1, 'dim2'));
                    if dim1 <> 0 then
                      begin
                        type_name := type_name + '(' + intToStr(dim1);
                        if dim2 <> 0 then
                          type_name := type_name + ',' + intToStr(dim2);
                        type_name := type_name + ')'
                      end;

                    defaultType :=
                      strToIntDef(DDtablecolumns.getValueByColumnName(rowId1, 'kinddefault'), -1);
                    case defaultType of
                      0: colDefaultValue :=
                          StrToInt(DDtablecolumns.getValueByColumnName(rowId1, 'intdefault'));
                      2: colDefaultValue :=
                          DDtablecolumns.getValueByColumnName(rowId1, 'int64default');
                      3, 5: colDefaultValue :=
                          DDtablecolumns.getValueByColumnName(rowId1, 'extdefault');
                      4: colDefaultValue :=
                          DDtablecolumns.getValueByColumnName(rowId1, 'currencydefault');
                      6: colDefaultValue :=
                          DDtablecolumns.getValueByColumnName(rowId1, 'booleandefault');
                      7: colDefaultValue :=
                          DDtablecolumns.getValueByColumnName(rowId1, 'stdefault');
                      8: colDefaultValue := 'AUTOINCREMENT';
                     -1: colDefaultValue := ''
                    end;

                    {
                    column_constraint := '';
                    rowId2 := DDconstraints.firstRow;
                    repeat
                      if DDconstraints.existRow(rowId2) then
                        begin
                          database_userId :=
                            DDconstraints.getValueByColumnName(rowId2, 'userid');
                          database_Name :=
                            DDconstraints.getValueByColumnName(rowId2, 'database_name');
                          table_name :=
                            DDconstraints.getValueByColumnName(rowId2, 'table_name');
                          constraint_kind :=
                            StrToInt(DDconstraints.getValueByColumnName(rowId2,
                            'constraint_kind'));
                          if ((database_userId = dbuserId) and
                            (UpperCase(dbName) = UpperCase(database_Name)) and
                            (UpperCase(tblName) = UpperCase(table_name)) and
                            (constraint_kind = 0)) then
                            begin
                              // check for the constraint column if column name =
                              rowId3 := DDcolumnsconstraint.firstRow;
                              repeat
                                if DDcolumnsconstraint.existRow(rowId3) then
                                  begin
                                    if DDconstraints.getValueByColumnName(rowId2,
                                      'constraint_name') = DDcolumnsconstraint.
                                      getValueByColumnName(rowId3, 'constraint_name') then
                                      begin

                                        if column_name =
                                          DDcolumnsconstraint.getValueByColumnName(rowId3,
                                          'column_name') then
                                          begin
                                            constraint_Type :=
                                              StrToInt(DDconstraints.getValueByColumnName(rowId2,
                                              'constraint_type'));
                                            case constraint_Type of
                                              0: column_constraint := column_constraint + 'NULL ';
                                              1: column_constraint := column_constraint + 'NOT NULL ';
                                              2: column_constraint := column_constraint + 'UNIQUE ';
                                              3: column_constraint :=
                                                  column_constraint + 'PRIMARY KEY ';
                                              4: column_constraint := column_constraint + 'REFERENCES ';
                                              5: column_constraint := column_constraint + 'CHECK ';
                                              6: column_constraint :=
                                                  column_constraint + 'FOREIGN KEY ';
                                            end;
                                          end;
                                      end;
                                  end;
                                rowId3 := rowId3 + 1;
                              until rowId3 > DDcolumnsconstraint.lastRow;

                            end;
                        end;
                      rowId2 := rowId2 + 1;
                    until rowId2 > DDconstraints.lastRow;
                    }

                    resultRows := dbCounter + 1;
                    setLength(sqlResults, length(sqlResults) + 1);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],IntToStr(resultRows),True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],column_Name,True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],type_Name,True);
                    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],colDefaultValue,True);
                    colsTxt := ''; //'sys_queryId: ' + lqueryId + ' ' + 'sys_user: ' + dbUserId + ' ';
                    colsTxt := colsTxt + 'rowId: ' + intToStr(resultRows) + ' ' + 'COLUMN NAME: ' + column_name + ' ';
                    colsTxt := colsTxt + 'TYPE: ' + type_name + ' ';
                    if defaultType <> -1 then
                      colsTxt := colsTxt + 'DEFAULT: ' + string(colDefaultValue);
                    writeln(outText,colsTxt);
                    dbCounter := dbCounter + 1;
                    found := True;

                  end;
            end;
            rowId1 := rowId1 + 1;
          until rowId1 > DDtablecolumns.lastRow;

          {$ENDIF}

          SDFInstance.Free;

          if dbCounter <> 0 then
            yyacceptmessage(IntToStr(dbCounter) + ' columns have been found')
           else
            yyerror('No column found');
        end;

        190: // DROP TABLE
        begin

          if not CanUserUseDatabase(dbuserId,dbName) then
            begin
              yyerror('User cannot use the database ' + ldbName);
              Exit;
            end;

          if not TableExists(tblName) then
            begin
              yyerror('Table don''t exists: ' +  tblName);
              exit;
            end;

          if not CanUserUseTable(dbuserId,dbName,tblName) then
            begin
              yyerror('User not allowed to access to table ' + tblName + ' from database ' + ldbName);
              Exit;
            end;

          if ldbName = '' then ldbName := dbName;

          {$IFDEF mongodb}
          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblname);
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance :=  DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
              tblFields := loadTableFields(tblName);
              tblFields.Collection.Drop;  // drop indexes are droped automatically
              tblFields.Collection.Free;
            end;

          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblname);
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','tablecolumn');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;

          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblname);
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','constraint');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;

          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','index');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              if DatabaseInstance.Items['type'].AsString = 'mono' then
                begin
                  if DatabaseInstance.Items['table_name'].AsString = tblname then
                    DataDictionaryCollection.Remove(DatabaseInstance);
                end else
                begin
                  index_name := DatabaseInstance.Items['index_name'].AsString;
                  lcount := DatabaseInstance.Items['tables_count'].AsInteger;
                  lcount := 2 * lcount - 1;
                  for index := 0 to lcount - 1 do
                    begin
                      DeleteFile(Path + index_Name + intTostr(index) + '.idx');
                      (*
                      tmpCollection := GDB.GetCollection(index_name + intToStr(index));
                      tmpCollection.Drop;
                      tmpCollection.Free;
                      *)
                    end;
                  DataDictionaryCollection.Remove(DatabaseInstance);
                  for index1 := 0 to high(workingschema.joinidxdata ) do
                    if (workingschema.joinidxdata[index1].idxname = index_Name) then break;
                  for index2 := index1+1 to high(workingschema.joinidxdata) do
                    workingschema.joinidxdata[index2-1] := workingschema.joinidxdata[index2];
                  setLength(workingSchema.joinidxdata,length(workingSchema.joinidxdata)-1);
                end;
            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          for rowId1 := DDtables.firstRow to DDtables.lastRow do
            begin
              database_Name :=
                DDtables.getValueByColumnName(rowId1, 'database_name');
              if (database_name = ldbname) then
                begin
                  table_name := DDTables.getValueByColumnName(rowId1, 'table_name');
                  if tblName = table_Name then
                    begin
                      DeleteFile(Path + table_Name);
                      DDtables.deleteRow(rowId1);
                    end;
                end;
            end;
          for rowId1 := DDtablecolumns.firstRow to DDtablecolumns.lastRow do
            begin
              database_Name :=
                DDtablecolumns.getValueByColumnName(rowId1, 'database_name');
              if (database_name = ldbname) then
                begin
                  table_name := DDTables.getValueByColumnName(rowId1, 'table_name');
                  if tblName = table_Name then
                    DDtablecolumns.deleteRow(rowId1);
                end;
            end;
          for rowId1 := DDconstraints.firstRow to DDconstraints.lastRow do
            begin
              database_Name :=
                DDconstraints.getValueByColumnName(rowId1, 'database_name');
              if (database_name = ldbname) then
                begin
                  table_name := DDTables.getValueByColumnName(rowId1, 'table_name');
                  if tblName = table_Name then
                    begin
                      for rowId2 :=
                          DDcolumnsconstraint.firstRow to DDcolumnsconstraint.lastRow do
                        if DDcolumnsconstraint.getValueByColumnName(rowid2,'constraint_name') =
                             DDconstraints.getValueByColumnName(rowid1, 'constraint_name') then
                          DDcolumnsconstraint.deleteRow(rowId2);
                      DDconstraints.deleteRow(rowId1);
                    end
                end;
            end;

          // table_name = 'sys_joinindex' mean join index
          for rowId1 := DDindexes.firstRow to DDindexes.lastRow do
            begin
              database_Name :=
                DDindexes.getValueByColumnName(rowId1, 'database_name');
              if (database_name = dbname) then
                begin
                  table_name := DDindexes.getValueByColumnName(rowId1, 'table_name');
                  if tblName = table_Name then
                    begin
                      for rowId2 := DDkeysindexes.firstRow to DDkeysindexes.lastRow do
                        if DDindexes.getValueByColumnName(rowId1, 'index_name') =
                           DDkeysindexes.getValueByColumnName(rowId2, 'index_name') then
                          DDkeysindexes.deleteRow(rowId2);
                      index_name := DDindexes.getValueByColumnName(rowId1, 'index_name');
                      DeleteFile(Path + index_Name +'.idx');
                      DDindexes.deleteRow(rowId1);
                    end else
                    if table_name = 'sys_joinindex' then
                      begin
                        for rowId2 := DDkeysindexes.firstRow to DDkeysindexes.lastRow do
                          if DDindexes.getValueByColumnName(rowId1, 'index_name') =
                             DDkeysindexes.getValueByColumnName(rowId2, 'index_name') then
                            DDkeysindexes.deleteRow(rowId2);
                        lcount := 0;
                        for rowId2 := DDjoinindexes.firstRow to DDjoinindexes.lastRow do
                          if DDindexes.getValueByColumnName(rowid1, 'index_name') =
                             DDjoinindexes.getValueByColumnName(rowid2, 'index_name') then
                            begin
                              lcount := lcount + 1;
                              DDjoinindexes.deleteRow(rowId2);
                            end;
                        index_name := DDindexes.getValueByColumnName(rowId1, 'index_name');
                        lcount := lcount + 1;
                        for index := 0 to lcount - 1 do
                          DeleteFile(Path + index_Name + intTostr(index) + '.idx');
                        DDindexes.deleteRow(rowId1);
                        for index1 := 0 to high(workingschema.joinidxdata ) do
                          if (workingschema.joinidxdata[index1].idxname = index_Name) then break;
                        for index2 := index1+1 to high(workingschema.joinidxdata) do
                          workingschema.joinidxdata[index2-1] := workingschema.joinidxdata[index2];
                        setLength(workingSchema.joinidxdata,length(workingSchema.joinidxdata)-1);
                      end;
                end;
            end;
          {$ENDIF}

          for index1 := 0 to high(workingschema.tables) do
            if (workingschema.tables[index1].tblName = tblName) then break;
          for index2 := index1+1 to high(workingschema.tables) do
            workingschema.tables[index2-1] := workingschema.tables[index2];
          setLength(workingSchema.tables,length(workingSchema.tables)-1) ;

          yyacceptmessage('Table ' + tblName + ' dropped');
        end;

        191: // DROP INDEX
        begin
          {$IFDEF mongodb}
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',dbname);
          NestedDatabaseInstance.Put('$eq','index');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              index_name := DatabaseInstance.Items['index_name'].AsString;
              if indexName + '_' + workingSchema.dbName = index_name then
                begin
                  index_Type := DatabaseInstance.Items['type'].AsString;
                  if index_Type = 'mono' then
                    begin
                      table_name := DatabaseInstance.Items['table_name'].AsString;
                      DataDictionaryCollection.Remove(DatabaseInstance);
                      for index1 := 0 to high(workingschema.tables) do
                        if (workingschema.tables[index1].tblName = table_Name) then break;
                      for index2 := low(workingschema.tables[index1].idxdata) to high(workingschema.tables[index1].idxdata) do
                        if workingschema.tables[index1].idxdata[index2].idxname = index_name then
                          break;
                      for index3 := index2+1 to high(workingschema.tables[index1].idxdata) do
                        workingschema.tables[index1].idxdata[index3-1] := workingschema.tables[index1].idxdata[index3];
                      setLength(workingschema.tables[index1].idxdata,length(workingschema.tables[index1].idxdata)-1) ;
                    end else
                    yyerror('Use command drop join index instead');
                end;
            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          // table_name = 'sys_joinindex' mean join index
          for rowId1 := DDindexes.firstRow to DDindexes.lastRow do
            begin
              database_Name :=
                DDindexes.getValueByColumnName(rowId1, 'database_name');
              if (database_name = dbname) then
                begin
                  index_name :=
                    DDindexes.getValueByColumnName(rowId1, 'index_name');
                  if indexName = index_Name then
                    begin
                      table_name := DDindexes.getValueByColumnName(rowId1, 'table_name');
                      if table_name = 'sys_joinindex' then
                        yyerror('Use command drop join index instead')
                       else
                        begin
                          for rowId2 := DDkeysindexes.firstRow to DDkeysindexes.lastRow do
                            if DDkeysindexes.getValueByColumnName(rowId2, 'index_name') = index_name then
                              DDkeysindexes.deleteRow(rowId2);
                          DDindexes.deleteRow(rowId1);
                          for index2 := low(workingschema.tables[index1].idxdata) to high(workingschema.tables[index1].idxdata) do
                            if workingschema.tables[index1].idxdata[index2].idxname = index_name then
                              break;
                          workingschema.tables[index1].idxdata[index2].idxstorage.free;
                          DeleteFile(Path + index_Name +'.idx');
                          DDindexes.deleteRow(rowId1);
                          for index3 := index2+1 to high(workingschema.tables[index1].idxdata) do
                            workingschema.tables[index1].idxdata[index3-1] := workingschema.tables[index1].idxdata[index3];
                          setLength(workingschema.tables[index1].idxdata,length(workingschema.tables[index1].idxdata)-1) ;
                      end
                    end;
                end;
            end;
          {$ENDIF}

        end;

        192: // DROP JOIN INDEX
        begin
          {$IFDEF mongodb}
          NestedTypeInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',dbname);
          NestedTypeInstance.Put('$eq','join');
          NestedDatabaseInstance.Put('$eq','index');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('type',NestedTypeInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              index_name := DatabaseInstance.Items['index_name'].AsString;
              if indexname + workingSchema.dbName = index_name then
                begin
                  lcount := DatabaseInstance.Items['tables_count'].AsInteger;
                  lcount := 2 * lcount - 1;
                  for index := 0 to lcount - 1 do
                    begin
                      DeleteFile(Path + index_Name + intTostr(index) + '.idx');
                      (*
                      tmpCollection := GDB.GetCollection(index_name + intToStr(index));
                      tmpCollection.Drop;
                      tmpCollection.Free;
                      *)
                    end;
                  DataDictionaryCollection.Remove(DatabaseInstance);
                  break;
                end;
            end;
          for index1 := 0 to high(workingschema.joinidxdata ) do
            if (workingschema.joinidxdata[index1].idxname = index_Name) then break;
          for index2 := index1+1 to high(workingschema.joinidxdata) do
            workingschema.joinidxdata[index2-1] := workingschema.joinidxdata[index2];
          setLength(workingSchema.joinidxdata,length(workingSchema.joinidxdata)-1);
          {$ENDIF}

          {$IFDEF tablesdb}

          {$ENDIF}

        end;

(*
165: // DROP DATABASE
begin
  found := False;
  rowId1 := DDdatabases.firstRow;
  repeat
    if DDdatabases.existRow(rowId1) then
    begin
      database_userId := DDdatabases.getValueByColumnName(rowId1, 'userid');
      database_Name :=
        DDdatabases.getValueByColumnName(rowId1, 'database_name');
      if (database_userId = dbuserId) and (database_name = db_name) then
      begin
        DDdatabases.deleteRow(rowId1);
        for RowId2 := ddtables.firstRow to DDtables.lastRow do
        begin
          database_userId :=
            DDtables.getValueByColumnName(rowId2, 'userid');
          database_Name :=
            DDtables.getValueByColumnName(rowId2, 'database_name');
          if (database_userId = dbuserId) and
            (database_name = db_name) then
          begin
            table_name :=
              DDTables.getValueByColumnName(RowId2, 'table_name');
            DeleteFile(Path + table_name);
            DDtables.deleteRow(rowId2);
          end;
        end;
        for RowId3 := DDtablecolumns.firstRow to DDtablecolumns.lastRow do
        begin
          database_userId :=
            DDtablecolumns.getValueByColumnName(rowId3, 'userid');
          database_Name :=
            DDtablecolumns.getValueByColumnName(rowId3, 'database_name');
          if (database_userId = dbuserId) and
            (database_name = db_name) then
            DDtablecolumns.deleteRow(rowId3);
        end;
        for RowId3 := ddconstraints.firstRow to DDconstraints.lastRow do
        begin
          database_userId :=
            DDconstraints.getValueByColumnName(rowId3, 'userid');
          database_Name :=
            DDconstraints.getValueByColumnName(rowId3, 'database_name');
          if (database_userId = dbuserId) and
            (database_name = db_name) then
          begin
            for rowId4 :=
              ddcolumnsconstraint.firstRow to ddcolumnsconstraint.lastRow do
            begin
              if DDconstraints.getValueByColumnName(
                rowid3, 'constraint_name') =
                DDcolumnsconstraint.getValueByColumnName(rowid4,
                'constraint_name') then
                DDcolumnsconstraint.deleteRow(rowId4);
            end;
            DDconstraints.deleteRow(rowId3);
          end;
        end;



        for RowId3 := ddindexes.firstRow to DDindexes.lastRow do
        begin
          database_userId :=
            DDindexes.getValueByColumnName(rowId3, 'userid');
          database_Name :=
            DDindexes.getValueByColumnName(rowId3, 'database_name');
          if (database_userId = dbuserId) and
            (database_name = db_name) then
          begin
            for rowId4 :=
              ddkeysindexes.firstRow to ddkeysindexes.lastRow do
            begin
              if DDindexes.getValueByColumnName(
                rowid3, 'index_name') =
                DDkeysindexes.getValueByColumnName(rowid4, 'index_name') then
                DDkeysindexes.deleteRow(rowId4);
            end;
            for rowId4 :=
              ddjoinindexes.firstRow to ddjoinindexes.lastRow do
            begin
              if DDindexes.getValueByColumnName(
                rowid3, 'index_name') =
                DDjoinindexes.getValueByColumnName(rowid4, 'index_name') then
                DDjoinindexes.deleteRow(rowId4);
            end;
            index_name :=
              DDTables.getValueByColumnName(RowId3, 'index_name');
            DeleteFile(Path + index_Name);
            DDindexes.deleteRow(rowId3);
          end;
        end;

        found := True;
      end;
    end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDdatabases.lastRow;
  if found then
    yyacceptmessage('Database dropped')
  else
    yyerror('Database doesnt''t exist');

end;



        165: // DROP DATABASE
        begin
          found := False;

          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','database');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance := DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
              found := True;
            end;

          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              table_name := DatabaseInstance.Items['table_name'].AsString;
              tmpCollection := GDB.GetCollection(table_name + '_' + dbName );
              tmpCollection.Drop; // drop indexes are done automatically
              tmpCollection.Free;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;

          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','tablecolumn');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;

          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','constraint');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;

          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NesteddbNameInstance.Put('$eq',ldbname);
          NestedDatabaseInstance.Put('$eq','index');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          while DatabaseCursor.HasNext do
            begin
              DatabaseInstance := DatabaseCursor.Next;
              if DatabaseInstance.Items['type'].AsString = 'join' then
                begin
                  index_name := DatabaseInstance.Items['index_name'].AsString;
                  lcount := DatabaseInstance.Items['tables_count'].AsInteger;
                  lcount := 2 * lcount - 1;
                  for index := 0 to lcount - 1 do
                    begin
                      if FindFirst(index_name + intToStr(index) + '.idx', faArchive, SR) = 0 then
                        DeleteFile(SR.name);
                      { tmpCollection := GDB.GetCollection(index_name + intToStr(index));
                      tmpCollection.Drop;
                      tmpCollection.Free; }
                    end;
                  FindClose(SR);
                end;
              DataDictionaryCollection.Remove(DatabaseInstance);
            end;
        { TODO : working schema }
          if found then
            begin
              workingschema.dbName := '';
              workingschema.tables := nil;
              workingschema.joinidxdata := nil;
              yyacceptmessage('Database dropped')
            end
          else
            yyerror('Database: ' + ldbName + ' not found');

        end;
*)

      228: // CREATE USER
        begin
          // dbUserID is the user who issue the command, should have "GRANT ALL" and "CREATE USERS" Privileges

          {$IFDEF mongodb}
          NestedUserIdInstance := TBSONObject.Create;
          PrivilegeArray := TBSONArray.Create;
          NestedPrivilegeInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedUserIdInstance.Put('$eq',dbUserId);
          PrivilegeArray.Put('grant option');
          PrivilegeArray.Put('create users');
          NestedPrivilegeInstance.Put('$in',privilegearray);
          NestedDatabaseInstance.Put('$eq','privilege');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('belongs_to',NestedUserIdInstance);
          DatabaseInstance.Put('privilege_array',NestedPrivilegeInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          if DatabaseCursor.count = 0 then
            begin
              yyerror('USER ID: ' +  dbUserID + ' cannot GRANT PRIVILEGES ');
              Exit;
            end;

          found := UserIdExists(lUserID);
          if found then
            begin
              yyerror('USER ID: ' +  lUserID + ' already exist ');
              Exit;
            end;

          yyacceptmessage(lUserId + ' has been created');
          // USERID_metaData(NEWUSERID, PASSWORD, CREATED_BY)
          DatabaseInstance := TBSONObject.Create;
          DatabaseInstance.Put('kind','user');
          DatabaseInstance.Put('user_id',luserId);
          DatabaseInstance.Put('password',lPassword);
          DatabaseInstance.Put('created_by',dbuserId);
          DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
          DataDictionaryCollection.Insert(DatabaseInstance);
          {$ENDIF}


        end;


      1: //  OPR CDB: EXECUTE CREATE DATABASE dbName
      begin

        (* Check if the DBName exist could be done by a linear scan into
             datadictionaty, next would be faster by using the b-join tree *)

        (* Check the user if exists and has the right to create database *)

        (* cheking Data Dictionary DatabaseExists better *)
        found := DatabaseExists(ldbName);

        if found then
          begin
            yyerror('Schema: ' +  ldbName + ' already exist ');
            Exit;
          end;

        // change the schema and close tables from old schema
        if workingSchema.tables <> nil then
          begin
            for index := low(workingSchema.tables) to High(workingSchema.tables) do
              begin
                {$IFDEF mongodb}
                workingSchema.tables[index].Collection.Free;
                {$ENDIF}
                {$IFDEF tablesdb}
                for index2 := 0 to high(workingSchema.tables[index].idxdata) do
                  workingSchema.tables[index].idxdata[index2].idxstorage.Free;
                workingSchema.tables[index].storage.Free;
                {$ENDIF}
              end;
            for index := low(workingSchema.joinidxdata) to High(workingSchema.joinidxdata) do
              workingSchema.joinidxdata[index].idxstorage.Free;
          end;

        workingSchema.dbName := ldbName;
        workingSchema.tables := nil; // New schema with no tables
        workingSchema.joinidxdata := nil;
        yyacceptmessage('Found New Schema ' + ldbName);


        {$IFDEF mongodb}
        // DATABASE_metaData(DBNAME)
        DatabaseInstance := TBSONObject.Create;
        DatabaseInstance.Put('kind','database');
        DatabaseInstance.Put('database_name',ldbname);
        DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
        DataDictionaryCollection.Insert(DatabaseInstance);
        {$ENDIF}

        {$IFDEF tablesdb}
        // DATABASE_metaData(DBNAME)
        row := nil;
        setLength(row, 1);
        row[0] := ldbName;
        DDdatabases.insertRow(row);
        {$ENDIF}

      end;

      2: // Opr DBN: POP ldbName and insert it into a structure
      begin
        ldbName := lowercase(stk[High(stk)].strValue);
        setLength(stk, Length(stk) - 1);
      end;

      3: // Opr CREATE Table
      begin

        (* cheking Data Dictionary *)
        found := TableExists(tblName);
        if found then
          begin
            yyerror('Table ' + tblName + ' already exist ');
            Exit;
          end;

        found := ViewExists(tblName);
        if found then
          begin
            yyerror('View ' + tblName + ' already exist ');
            Exit;
          end;

        found := DatabaseExists(dbName);
        if not found then
          begin
            yyerror('Database ' + dbName + ' not found');
            Exit;
          end;

        // Table_metaData(table_name,database_name,numberOfAttributes)

        {$IFDEF mongodb}
        DatabaseInstance := TBSONObject.Create;
        DatabaseInstance.Put('kind','table');
        DatabaseInstance.Put('database_name',workingSchema.dbName);
        DatabaseInstance.Put('table_name',tblname);
        lcount := Length(columnList);
        DatabaseInstance.Put('number_of_columns',lcount); // Number Of Attributes
        DatabaseInstance.Put('number_of_documents',0); // Number Of Rows
        tbltimestamp := now;
        DatabaseInstance.Put('timestamp',tbltimestamp);
        DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
        DataDictionaryCollection.Insert(DatabaseInstance);
        {$ENDIF}

        {$IFDEF tablesdb}
        row := nil;
        setLength(row, 4);
        row[0] := tblName;
        row[1] := workingSchema.dbName;
        row[2] := Length(columnList); // number Of Attributes
        row[3] := Integer(0); // Number Of Rows
        DDtables.insertRow(row);
        DDtables.returnRow(DDtables.lastRow,row);
        {$ENDIF}

        {$IFDEF mongodb}
        for index1 := low(columnList) to high(columnList) do
          begin
            // Column_metaData(column_name,table_name,database_name,position,type_name,Default)
            DatabaseInstance := TBSONObject.Create;
            DatabaseInstance.Put('kind','tablecolumn');
            DatabaseInstance.Put('database_name',workingSchema.dbName);
            DatabaseInstance.Put('table_name',tblname);
            DatabaseInstance.Put('column_name',columnlist[index1].columnName);
            setLength(colsName, length(colsName) + 1);
            colsName[high(colsName)] := columnlist[index1].columnName;
            DatabaseInstance.Put('position',index1);
            DatabaseInstance.Put('type_name',columnlist[index1].columnTypeName);
            setLength(colsType, length(colsType) + 1);
            colsType[high(colsType)] := convertType(columnlist[index1].columnTypeName);
            with columnlist[index1] do
              if charTypeSize <> 0 then
                begin
                  dimSize := charTypeSize;
                  colsType[high(colsType)] :=
                    colsType[high(colsType)] + '[' + IntToStr(charTypeSize) + ']';
                end
               else dimSize := numTypeSize[0];
            DatabaseInstance.Put('dim1',dimSize); // 'dim1'
            DatabaseInstance.Put('dim2',columnlist[index1].numTypeSize[1]); // 'dim2'

            with columnlist[index1] do
              if hascolumnAutoIncrement then
                begin
                  sequencesInstance := TBSONObject.Create;
                  sequencesInstance.Put('database_name',dbName);
                  sequencesInstance.Put('table_name',tblName);
                  sequencesInstance.Put('column_name',colsName[high(colsName)]);
                  sequencesInstance.Put('counter',1);
                  sequencesCollection.Insert(sequencesInstance);
                  DatabaseInstance.Put('kind_default',8)  // AUTOINCREMENT
                end
               else
                begin
                  if hascolumnDefault then
                    begin
                      if (convertType(columnTypeName) = 'INTEGER') or
                        (convertType(columnTypeName) = 'SMALLINT') then
                        begin
                          DatabaseInstance.Put('kind_default',0);  // INTEGER
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('int_default',varDef);
                        end;
                      if convertType(columnTypeName) = 'INT64' then
                        begin
                          DatabaseInstance.Put('kind_default',2);  // INT64
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('int64_default',varDef);
                        end;
                      if (convertType(columnTypeName) = 'SINGLE') or
                         (convertType(columnTypeName) = 'DOUBLE') or
                         (convertType(columnTypeName) = 'EXTENDED') then
                        begin
                          DatabaseInstance.Put('kind_default',3);  // EXTENDED
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('ext_default',varDef);
                        end;
                      if convertType(columnTypeName) = 'CURRENCY' then
                        begin
                          DatabaseInstance.Put('kind_default',4);  // CURRENCY
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('currency_default',varDef);
                        end;
                      if (convertType(columnTypeName) = 'TDATETIME') or
                         (convertType(columnTypeName) = 'TDATE') or
                         (convertType(columnTypeName) = 'TTIME') then
                        begin
                          DatabaseInstance.Put('kind_default',5);  // TDATETIME
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('ext_default',varDef);
                        end;
                      if convertType(columnTypeName) = 'BOOLEAN' then
                        begin
                          DatabaseInstance.Put('kind_default',6);  // BOOLEAN
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          DatabaseInstance.Put('boolean_default',varDef);
                        end;
                      if convertType(columnTypeName) = 'STRING' then
                        begin
                          DatabaseInstance.Put('kind_default',7);  // STRING
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,charTypeSize,numTypeSize[1]);
                          DatabaseInstance.Put('st_default',varDef);
                        end;
                    end
                   else
                    begin
                      (* If you specify NULL as the default value for a column, you cannot
                         specify a NOT NULL constraint as part of the column definition.
                         NULL is not a valid default value for a column that is part of a
                         primary key.
                      *)
                      DatabaseInstance.Put('kind_default',-1);  // DEFAULT NOT AVAILABLE
                    end;
                end;

              DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
              DataDictionaryCollection.Insert(DatabaseInstance);

          end;
        {$ENDIF}

        {$IFDEF tablesdb}
        for index1 := low(columnList) to high(columnList) do
          begin
            // Column_metaData(column_name,table_name,database_name,position,type_name,Default)

            row := nil;
            setLength(row, 16);
            row[0] := columnlist[index1].columnName;
            setLength(colsName, length(colsName) + 1);
            colsName[high(colsName)] := columnlist[index1].columnName;
            row[1] := tblName;
            row[2] := workingSchema.dbName;
            row[3] := index1; // position
            row[4] := columnlist[index1].columnTypeName;
            setLength(colsType, length(colsType) + 1);
            colsType[high(colsType)] :=
              convertType(columnlist[index1].columnTypeName);
            with columnlist[index1] do
              if charTypeSize <> 0 then
              begin
                dimSize := charTypeSize;
                colsType[high(colsType)] :=
                  colsType[high(colsType)] + '[' + IntToStr(charTypeSize) + ']';
              end
              else
                dimSize := numTypeSize[0];
            row[5] := dimSize; // 'dim1'
            row[6] := columnlist[index1].numTypeSize[1]; // 'dim2'

            row[9] := 0;
            row[10] := '';
            row[11] := 0;
            row[12] := 0.0;
            row[13] := 0.0;
            row[14] := False;
            row[15] := 1; // for AUTOINCREMENT

            with columnlist[index1] do
              if hascolumnAutoIncrement then
                begin
                  row[7] := 8; // AUTOINCREMENT
                  row[8] := false;
                end
               else
                begin
                  if hascolumnDefault then
                    begin
                      if (convertType(columnTypeName) = 'INTEGER') or
                         (convertType(columnTypeName) = 'SMALLINT') then
                        begin
                          row[7] := 0;  // 0 for integer
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[9] := vardef
                        end;
                      if convertType(columnTypeName) = 'INT64' then
                        begin
                          row[7] := 2;  // 2 for int64
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[11] := vardef
                        end;
                      if (convertType(columnTypeName) = 'SINGLE') or
                         (convertType(columnTypeName) = 'DOUBLE') or
                         (convertType(columnTypeName) = 'EXTENDED') then
                        begin
                          row[7] := 3;  // 3 for extended
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[12] := vardef
                        end;
                      if convertType(columnTypeName) = 'CURRENCY' then
                        begin
                          row[7] := 4;  // 4 for currency
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[13] := vardef
                        end;
                      if convertType(columnTypeName) = 'TDATETIME' then
                        begin
                          row[7] := 5;  // 5 for tdatetime
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then
                            row[12] := vardef else
                            row[12] := now;
                        end;
                      if convertType(columnTypeName) = 'BOOLEAN' then
                        begin
                          row[7] := 6;  // 6 for boolean
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[14] := vardef
                        end;
                      if convertType(columnTypeName) = 'STRING' then
                        begin
                          row[7] := 7; // 7 for string
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = null;
                          if not row[8] then row[10] := vardef
                        end;
                    end
                  else
                  begin
                    (* If you specify NULL as the default value for a column, you cannot
                       specify a NOT NULL constraint as part of the column definition.
                       NULL is not a valid default value for a column that is part of a
                       primary key.
                    *)
                    row[7] := -1; // default not specify
                    row[8] := false;
                  end;
                end;

            DDtablecolumns.insertRow(row);
            DDtablecolumns.returnRow(DDtablecolumns.lastRow,row);
          end;
        {$ENDIF}

        {$IFDEF mongodb}
        for index1 := Low(constraintList) to high(constraintList) do
        begin

          // Constraint_metaData(constraint_name,column_name,...,column_name,constraint_kind,constraint_type)
          // column_name,...,column_name is ColumnsConstrain_metaData(column_name,table_name,database_name,constraint_name)
          // constraint_kind: column / table
          // constraint_type: FOREIGN KEY, PRIMARY KEY, UNIQUE, CHECK, REFERENCES

          // Table_metaData(table_name,database_name,numberOfAttributes)
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          DatabaseInstance.Put('kind','constraint');
          DatabaseInstance.Put('database_name',workingSchema.dbName);
          DatabaseInstance.Put('table_name',tblname);
          DatabaseInstance.Put('constraint_name',constraintList[index1].constraintname);
          DatabaseInstance.Put('constraint_kind',constraintList[index1].constraintkind); // constraint_kind: column / table
          DatabaseInstance.Put('constraint_type',constraintList[index1].constraintType);


          lcount := length(constraintList[index1].columnsName);
          DatabaseInstance.Put('column_count',lcount);

          for index2 := Low(constraintList[index1].columnsName) to high(constraintList[index1].columnsName) do
            DatabaseInstance.Put('column_name'+intToStr(index2),constraintList[index1].columnsName[index2]);
          lcount := length(constraintList[index1].checkInstructions);
          DatabaseInstance.Put('checkinstruction_count',lcount);
          if constraintList[index1].constraintType = 'CHECK' then
            for index2 := Low(constraintList[index1].checkInstructions) to high(constraintList[index1].checkInstructions) do
              begin
                DatabaseInstance.Put('mnemonic'+intToStr(index2),constraintList[index1].checkInstructions[index2].mnemonic);
                DatabaseInstance.Put('boolvalue'+intToStr(index2),constraintList[index1].checkInstructions[index2].boolvalue);
                DatabaseInstance.Put('value'+intToStr(index2),constraintList[index1].checkInstructions[index2].value);
                DatabaseInstance.Put('stvalue'+intToStr(index2),constraintList[index1].checkInstructions[index2].stvalue);
                DatabaseInstance.Put('printInstruction'+intToStr(index2),constraintList[index1].checkInstructions[index2].printInstruction);
              end;
          if constraintList[index1].constraintType = 'REFERENCES' then
            begin
              DatabaseInstance.Put('references_table',constraintList[index1].refTable);
              lcount := length(constraintList[index1].refcolumnsName);
              DatabaseInstance.Put('refcolumn_count',lcount);
              for index2 := Low(constraintList[index1].refcolumnsName) to high(constraintList[index1].refcolumnsName) do
                DatabaseInstance.Put('refcolumn_name'+intToStr(index2),constraintList[index1].refcolumnsName[index2]);
            end;
          DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
          DataDictionaryCollection.Insert(DatabaseInstance);
        end;
        {$ENDIF}

        {$IFDEF tablesdb}
        for index1 := Low(constraintList) to high(constraintList) do
          begin

            // Constraint_metaData(constraint_name,column_name,...,column_name,constraint_kind,constraint_type)
            // column_name,...,column_name is ColumnsConstrain_metaData(column_name,table_name,database_name,constraint_name)
            // constraint_kind: column / table
            // constraint_type: PRIMARY KEY, UNIQUE, CHECK, REFERENCES - FOREIGN KEY
            row := nil;
            setLength(row, 5);
            row[0] := constraintList[index1].constraintName;
            row[1] := tblName;
            row[2] := workingSchema.dbName;
            row[3] := constraintList[index1].constraintkind; // constraint_kind: column / table
            row[4] := constraintList[index1].constraintType;
            if constraintList[index1].constraintType = 'REFERENCES' then
              row[5] := constraintList[index1].refTable
             else row[5] := '';
            DDconstraints.insertRow(row);

            for index2 :=
                Low(constraintList[index1].columnsName)
                to high(constraintList[index1].columnsName) do
              begin
                row := nil;
                setLength(row, 2);
                row[0] := constraintList[index1].constraintName;
                row[1] := constraintList[index1].columnsName[index2];
                DDcolumnsconstraint.insertRow(row);
              end;

            if constraintList[index1].constraintType = 'REFERENCES' then
              for index2 :=
                  Low(constraintList[index1].refcolumnsName)
                  to high(constraintList[index1].refcolumnsName) do
                begin
                  row := nil;
                  setLength(row, 2);
                  row[0] := constraintList[index1].constraintName;
                  row[1] := constraintList[index1].refcolumnsName[index2];
                  DDrefcolumns.insertRow(row);
                end;

            if constraintList[index1].constraintType = 'CHECK' then
              for index2 :=
                  low(constraintList[index1].checkInstructions)
                  to high(constraintList[index1].checkInstructions) do
                begin
                  row := nil;
                  setLength(row, 6);
                  row[0] := constraintList[index1].constraintName;
                  row[1] := constraintList[index1].checkInstructions[Index2].mnemonic;
                  row[2] := constraintList[index1].checkInstructions[Index2].boolvalue;
                  row[3] := constraintList[index1].checkInstructions[Index2].value;
                  row[4] :=constraintList[index1].checkInstructions[index2].stvalue;
                  row[5] := constraintList[index1].checkInstructions[index2].printInstruction;
                  DDcheckinstructions.insertRow(row);
                end;
          end;
        {$ENDIF}

        (*** Loading into Working Schema ***)
        setLength(allowcolsNull, length(colsName));
        for index1 := low(allowcolsNull) to high(allowcolsNull) do
          allowcolsNull[index1] := True; // Null values are allowable by default

        setLength(workingSchema.tables, length(workingSchema.Tables) + 1);
        workingSchema.tables[high(workingSchema.tables)].tblName := tblName;

        with workingSchema.tables[high(workingSchema.tables)] do
          begin

            // timestamp := tbltimestamp; // View time should be less than any time of any table created from at creation time

            numCols := length(columnList);
            setLength(columns, numCols);
            for index1 := low(columnList) to high(columnList) do
              with columnList[index1]  do
                begin
                  columns[index1].colname := columnName;
                  if convertType(columnTypeName) = 'INTEGER' then
                    columns[index1].coltype := intType;
                  if convertType(columnTypeName) = 'SMALLINT' then
                    columns[index1].coltype := smallintType;
                  if convertType(columnTypeName) = 'INT64' then
                    columns[index1].coltype := int64Type;
                  if (convertType(columnTypeName) = 'SINGLE') or
                     (convertType(columnTypeName) = 'DOUBLE') or
                     (convertType(columnTypeName) = 'EXTENDED') then
                    begin
                      columns[index1].coltype := extendedType;
                      columns[index1].coltypescale.precision := numTypeSize[0];
                      columns[index1].coltypescale.scale := numTypeSize[1];
                    end;
                  if convertType(columnTypeName) = 'CURRENCY' then
                    begin
                      columns[index1].coltype := currencyType;
                      columns[index1].coltypescale.precision := numTypeSize[0];
                      columns[index1].coltypescale.scale := numTypeSize[1];
                    end;
                  if convertType(columnTypeName) = 'TDATETIME' then
                    columns[index1].coltype := tdatetimeType;
                  if convertType(columnTypeName) = 'TDATE' then
                    columns[index1].coltype := tdateType;
                  if convertType(columnTypeName) = 'TTIME' then
                    columns[index1].coltype := ttimeType;
                  if convertType(columnTypeName) = 'BOOLEAN' then
                    columns[index1].coltype := booleanType;
                  if convertType(columnTypeName) = 'STRING' then
                    begin
                      columns[index1].coltype := stringType;
                      columns[index1].coltypescale.size := charTypeSize;
                    end;
                  columns[index1].colHasAutoIncrement := hasColumnAutoIncrement;
                  columns[index1].colHasDefault := hasColumnDefault;
                  columns[index1].colSQLtypename := columnTypeName;
                  columns[index1].colDefaultValue := columnDefaultValue;
                end;

            for index := Low(constraintList) to high(constraintList) do
              with constraintList[index] do
                begin
                  // UNIQUE COULD BE NULL
                  // PRIMARY KEY IS NOT NULL COLUMN AND UNIQUE COLUMN
                  setLength(constraints, length(constraints) + 1);
                  with constraints[high(constraints)] do
                    begin
                      cnstrname := constraintName;
                      if (constraintType = 'NULL') then
                        begin
                          cnstrtype := 0;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if columnsName[index2] = columnName then
                                  begin
                                    nullCol := Index1;
                                    allowcolsNull[Index1] := True;
                                  end;
                        end;
                      if (constraintType = 'NOT NULL') then
                        with constraints[high(constraints)] do
                          begin
                            cnstrtype := 1;
                            for index2 := low(columnsName) to high(columnsName) do
                              for index1 := Low(columnList) to high(columnList) do
                                with columnList[index1] do
                                  if columnsName[index2] = columnName then
                                    begin
                                      nnullCol := Index1;
                                      allowcolsNull[Index1] := False;
                                    end;
                          end;
                      if (constraintType = 'UNIQUE') then
                        begin
                          cnstrtype := 2;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if columnsName[index2] = columnName then
                                  begin
                                    uqCols[0] := setbit(uqCols[0], Index1);
                                    allowcolsNull[Index1] := False;
                                  end;
                        end;
                      if (constraintType = 'PRIMARY KEY') then
                        begin
                          cnstrtype := 3;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if columnsName[index2] = columnName then
                                  begin
                                    pkCols[0] := setbit(pkCols[0], Index1);
                                    allowcolsNull[Index1] := False;
                                  end;
                        end;
                      if (constraintType = 'CHECK') then
                        begin
                          cnstrtype := 5;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if columnsName[index2] = columnName then
                                begin
                                  ckCols[0] := setbit(ckCols[0], Index1);
                                  checkCondition := constraintList[index].checkInstructions;
                                end;
                        end;
                      if (constraintType = 'REFERENCES') then
                        begin
                          cnstrtype := 4;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if columnsName[index2] = columnName then
                                  Cols[0] := setbit(Cols[0], Index1);
                          reftblName := refTable;
                          for index2 := low(columnsName) to high(columnsName) do
                            for index1 := Low(columnList) to high(columnList) do
                              with columnList[index1] do
                                if refcolumnsName[index2] = columnName then
                                  refCols[0] := setbit(refCols[0], Index1);
                        end;
                    end;
                end;
            idxdata := nil;

            {$IFDEF mongodb}
            Collection := GDB.GetCollection(tblName + '_' + dbName);
            {$ENDIF}

            {$IFDEF tablesdb}
            storage := TTableClass.Create(Path + tblName + '_' + dbName,
              False, colsName, colsType, allowcolsNull);
            storage.Free;
            storage := TTableClass.Create(Path + tblName + '_' + dbName,
              True, colsName, colsType, allowcolsNull);
            {$ENDIF}


            {
            IBSONInstance := TBSONObject.Create;

            IBSONInstance.Put('sys_RowId',1);
            IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
            Collection.CreateIndex(IBSONInstance);
            }

          end;

        colsName := nil;
        yyacceptmessage('Table ' + tblName + ' is created');
      end;

      4: // Opr TBN: POP tblName and insert it into a structure
      begin

        foundTable := True;
        // check if it is already exists or it is used as an alias
        if fromTableslen = 0 then
          begin
            found := false;
            for index1 := 0 to length(fromTables) - 1 do
              if fromTables[index1].Name = lowercase(stk[High(stk)].strValue) then
                begin
                  found := true;
                  fromindex := index1;
                  break;
                end;
            if not found then
              begin
                setLength(fromTables, Length(fromTables) + 1);
                fromindex := Length(fromTables) - 1;
                fromTables[high(fromTables)].Name := lowercase(stk[High(stk)].strValue);
                fromTables[high(fromTables)].aliasname := nil;
                fromTables[high(fromTables)].fromFields.columns := nil;
                fromTables[high(fromTables)].fromFields.constraints := nil;
              end;
            tblName := lowercase(stk[High(stk)].strValue);
          end else
          begin
            aliasName := '';
            for index1 := 0 to fromTableslen - 1 do
              begin
                found := false;
                for index2 := 0 to length(fromTables[index1].aliasname) - 1 do
                  begin
                    if fromTables[index1].aliasname[index2] = lowercase(stk[High(stk)].strValue) then
                      begin
                        aliasName := lowercase(stk[High(stk)].strValue);
                        tblName := fromTables[index1].Name;
                        found := true;
                        break;
                      end;
                    if not found then continue else break;
                  end;
                if not found then continue else break;
              end;
            if not found then
              tblName := lowercase(stk[High(stk)].strValue);
            tblcolName := tblName;
          end;
        setLength(stk, Length(stk) - 1);
      end;

      // A view is created every time with a timestamp
      // Every table when get updated its update timestamp change in data dictionary
      // if view timestamp >= timestamps of tables don't reload View
      226: // Opr CREATE View
      begin

        (*
        CREATE VIEW <table name> [(<VIEW column list>)]
        AS <query expression>
        [WITH [<levels clause>] CHECK OPTION]
        <levels clause> ::= CASCADED | LOCAL
        *)

        found := TableExists(viewName);
        if found then
          begin
            yyerror('Table ' + viewName + ' already exist ');
            Exit;
          end;

        found := ViewExists(viewName);
        if found then
          begin
            yyerror('View ' + viewName + ' already exist ');
            Exit;
          end;

        found := DatabaseExists(dbName);
        if not found then
          begin
            yyerror('Database ' + dbName + ' not found');
            Exit;
          end;

        setLength(workingSchema.views, length(workingSchema.views) + 1);
        workingSchema.views[high(workingSchema.views)].viewName := viewName;

        with workingSchema.views[high(workingSchema.views)] do
          begin
            ViewInstructions := nil;
            for index1 := low(sqlMemProg) + 2  to high(sqlMemProg) - 1 do
              begin
                setLength(ViewInstructions,length(ViewInstructions)+1);
                ViewInstructions[high(ViewInstructions)] := sqlMemProg[index1]
              end;

            numCols := length(selectColsInstructions);

            idxname := ''; // sequential
            { TODO : should be created as possible }

            columns := nil;

            for index1 := 0 to length(selectColsInstructions) - 1 do
              begin
                setlength(columns,length(columns) + 1);
                for index2 := 0 to length(selectColsInstructions[index1]) - 1 do
                  with selectColsInstructions[index1,index2] do
                    case mnemonic of
                      151:
                        begin
                          columns[high(columns)].tblname := copy(stvalue,1,pos('.',stvalue)-1);
                          columns[high(columns)].colname := copy(stvalue,pos('.',stvalue)+1,length(stValue));
                          columns[high(columns)].aliasname := '';
                        end;
                      179:
                        begin
                          columns[high(columns)].aliasname := stvalue;
                        end;
                    end
              end;


            {$IFDEF mongodb}
            DatabaseInstance := TBSONObject.Create;
            DatabaseInstance.Put('kind','view');
            DatabaseInstance.Put('database_name',workingSchema.dbName);
            DatabaseInstance.Put('view_name',viewname);
            lcount := length(selectColsInstructions);
            DatabaseInstance.Put('number_of_columns',lcount); // Number Of Attributes
            // DatabaseInstance.Put('number_of_documents',0); // Number Of Rows
            tbltimestamp := now;
            DatabaseInstance.Put('timestamp',tbltimestamp);
            DatabaseInstance.Put('index_name',idxname);
            lcount := length(sqlMemProg) - 3;
            DatabaseInstance.Put('number_of_instructions',lcount); // Number Of Instructions
            for index1 := low(sqlMemProg) + 2  to high(sqlMemProg) - 1 do
              begin
                DatabaseInstance.Put('mnemonic'+intToStr(index1-2),sqlMemProg[index1].mnemonic);
                DatabaseInstance.Put('boolvalue'+intToStr(index1-2),sqlMemProg[index1].boolvalue);
                DatabaseInstance.Put('value'+intToStr(index1-2),sqlMemProg[index1].value);
                DatabaseInstance.Put('stvalue'+intToStr(index1-2),sqlMemProg[index1].stvalue);
                DatabaseInstance.Put('printInstruction'+intToStr(index1-2),sqlMemProg[index1].printInstruction);
              end;
            DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
            DataDictionaryCollection.Insert(DatabaseInstance);

            for index1 := 0 to length(columns) - 1 do
              with columns[index1] do
                begin
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance.Put('kind','viewcolumn');
                  DatabaseInstance.Put('database_name',workingSchema.dbName);
                  DatabaseInstance.Put('view_name',viewname);
                  DatabaseInstance.Put('table_name',tblName);
                  DatabaseInstance.Put('column_name',colName);
                  DatabaseInstance.Put('alias_name',aliasName);
                  DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
                  DataDictionaryCollection.Insert(DatabaseInstance);
                end;
            {$ENDIF}


            {$IFDEF tablesdb}
            row := nil;
            setLength(row, 4);
            row[0] := viewName;
            row[1] := workingSchema.dbName;
            row[2] := idxname;
            row[3] := numCols; // number Of Attributes
            row[4] := length(sqlMemProg) - 3;  ; // Number Of Instructions
            DDviews.insertRow(row);

            for index1 := 0 to length(columns) - 1 do
              with columns[index1] do
                begin
                  row := nil;
                  setLength(row, 5);
                  row[0] := colName;
                  row[1] := tblName;
                  row[2] := viewName;
                  row[3] := workingSchema.dbName;
                  row[4] := aliasName;
                  DDviewColumns.insertRow(row);
                end;
            {$ENDIF}
          end;

        // Select column list will be the columnlist for a view unless specified
        yyacceptmessage('View ' + viewName + ' is created');

      end;

      227: // Opr VIEW NAME: POP VIEW NAME and insert it into a structure
      begin
        viewName := lowercase(stk[High(stk)].strValue);
        setLength(stk, Length(stk) - 1);
      end;

      5: // Opr NEW COLUMN
      begin
        // Check if the same column exist

        for index1 := low(columnList) to high(columnList) do
          if colsName[high(colsName)] =
            columnlist[index1].columnName then
          begin
            yyerror('Duplicate declaration of Column Name in the same table ' +
              colsName[high(colsName)]);
            Exit;
          end;

        setLength(columnList, length(columnList) + 1);
        with columnList[High(columnList)] do
          begin
            columnName := colsName[high(colsName)];
            columnTypeName := colTypeName;
            charTypeSize := charSize;
            numTypeSize[0] := numSize[0];
            numTypeSize[1] := numSize[1];
            hascolumnDefault := hasDefault;
            columnDefaultValue := dfltValue;
            hascolumnAutoIncrement := hasAutoIncrement;
          end;
        colsName := nil;
        colTypeName := '';
        charSize := 0;
        numSize[0] := 0;
        numSize[1] := 0;
        hasAutoIncrement := False;
        hasDefault := False;
        dfltValue := 0;
        expr := nil;
      end;

      6: // Opr CLN: POP colName and insert it into a structure
      begin
        setLength(expr, length(expr) + 1);
        expr[high(expr)].mnemonic := 151;
        expr[high(expr)].Value := 0.0;
        colname := lowercase(stk[High(stk)].strValue);
        if (tblcolName = '') and (fromTablesLen <> 0) then
          begin
            tblName := '';
            for index1 := 0 to fromTablesLen - 1 do
              begin
                tblFields := loadTableFields(fromtables[index1].Name);
                for index2 := 0 to length(tblFields.columns)- 1 do
                  begin
                    if colname = tblFields.columns[index2].colname then
                      begin
                        tblname := tblFields.tblName;
                        break;
                      end;
                  end;
                if tblName <> '' then break;
              end;
            if tblName = '' then
              begin
                yyerror('Column doesn''t belongs to any table');
                Exit;
              end;
          end;

        expr[high(expr)].stvalue := tblName + '.' + lowercase(stk[High(stk)].strValue);
        expr[high(expr)].printInstruction := 'PUSH COLUMN NAME';

        setLength(colsName, length(colsName) + 1);
        colsName[high(colsName)] := lowercase(stk[High(stk)].strValue);
        setLength(stk, Length(stk) - 1);
        //setLength(tblcolName, length(colsName));
        tblColName := '';
      end;

      7, 10: // Opr TYPE: POP colTypeSize and insert it with colTypeName into a structure
             // char(x): Where x is the number of characters to store. This data type is space padded to fill the number of characters specified.
      begin
        colTypeName := 'CHARACTER';
        charSize := StrToInt(stk[High(stk)].strValue);
        setLength(stk, Length(stk) - 1);
      end;

      8, 9, 11:
             // varchar(x): Where x is the number of characters to store. This data type does NOT space pad.
      begin
        colTypeName := 'CHARACTER VARYING';
        charSize := StrToInt(stk[High(stk)].strValue);
        setLength(stk, Length(stk) - 1);
      end;

      12: // Opr TYPE CLOB: insert CLOB into a structure
      begin
        yyerror('OOPS The CLOB TYPE is not yet implemented ');
        exit;
        colTypeName := 'CLOB';
        charSize := trunc(stk[High(stk)].extValue);
        setLength(stk, Length(stk) - 1);
      end;

      13:
      begin // Opr TYPE Date_Type: insert Date into a structure
        colTypeName := 'DATE';
        // The following code is added momentarily
      {  colTypeName := 'CHARACTER';
        charSize := 24;    }
      end;

      132:
      begin
        colTypeName := 'TIME';
      end;

      133:
      begin
        colTypeName := 'TIMESTAMP';
      end;

      223:
      begin
        colTypeName := 'DATETIME';
      end;

      136:
      begin // Opr TYPE Boolean: insert Boolean into a structure
        colTypeName := 'BOOLEAN';
      end;

      (*   Delphi types
              Type  Storage size                        Range

               Byte       1                             0 to 255
               ShortInt   1                          -127 to 127
               Word       2                             0 to 65,535
               SmallInt   2                       -32,768 to 32,767
               LongWord   4                             0 to 4,294,967,295
               Cardinal   4*                            0 to 4,294,967,295
               LongInt    4                -2,147,483,648 to 2,147,483,647
               Integer    4                -2,147,483,648 to 2,147,483,647
               Int64      8    -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807

               Single     4     7  significant digits, exponent   -38 to +38
               Currency   8    50+ significant digits, fixed 4 decimal places
               Double     8    15  significant digits, exponent  -308 to +308
               Extended  10    19  significant digits, exponent -4932 to +4932

      *)

      14: // NUMBER
      begin
        colTypeName := 'SINGLE';
        numSize[0] := Trunc(stk[High(stk)].extValue);
        if (numSize[0] > 53) then
          colTypeName := 'EXTENDED';
        if (numSize[0] > 24) and (numSize[0] <= 53) then
          colTypeName := 'DOUBLE';
        setLength(stk, Length(stk) - 1);
      end;

      15: //FLOAT
      // float(p): Where p is a precision value.
      begin
        colTypeName := 'SINGLE';
        numSize[0] := Trunc(stk[High(stk)].extValue);
        if (numSize[0] > 53) then
          colTypeName := 'EXTENDED';
        if (numSize[0] > 24) and (numSize[0] <= 53) then
          colTypeName := 'DOUBLE';
        setLength(stk, Length(stk) - 1);
      end;

      16: // REAL
      // real: Single-precision floating point number
      begin
        colTypeName := 'SINGLE';
      end;

      17: // DOUBLE PRECISION
      // double precision: Double-precision floating point number
      begin
        colTypeName := 'EXTENDED';
      end;

      18 .. 20: // NUMBER2, DECIMAL, DEC
      // decimal(p,s): Where p is a precision value; s is a scale value.
      begin
        colTypeName := 'EXTENDED';
        numSize[0] := Trunc(stk[High(stk) - 1].extValue);
        numSize[1] := Trunc(stk[High(stk)].extValue);
        setLength(stk, Length(stk) - 2);
      end;

      21: // NUMERIC
      // numeric(p,s): Where p is a precision value; s is a scale value. For example, numeric(6,2) is a number that has 4 digits before the decimal and 2 digits after the decimal.
      begin
        colTypeName := 'EXTENDED';
        numSize[0] := Trunc(stk[High(stk) - 1].extValue);
        numSize[1] := Trunc(stk[High(stk)].extValue);
        setLength(stk, Length(stk) - 2);
      end;

      22 .. 24: // NUMBER1, INTEGER, INT
      begin
        colTypeName := 'INTEGER';
      end;

      25: // SMALLINT
      begin
        colTypeName := 'SMALLINT';
      end;

      131: // BIGINT
      begin
        colTypeName := 'BIGINT';
      end;

      230: // PASSWORD
        begin
          lPassword := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

      231: // GRANT
        begin

          // check if dbuserid has the grant option and access to privileges that he will give
          // get the privileges of dbuserid
          // check if he has grant option and all then ok
          // if doesn't has grant then error
          // if doesn't has all option then check one by one the one that he has the ones he will grant
          // Owner of an object can grant access on the object to any
          // dbobject is empty when the grant is for Create, Alter or Drop View or Table
          // Grant Any refer to any schema (Super User) if Any is missing only for his schema

          // check if luserid exists
          found := UserIdExists(lUserID);
          if not found then
            begin
              yyerror('USER ID: ' +  lUserID + ' not exist exist ');
              Exit;
            end;

          {$IFDEF mongodb}
          NestedUserIdInstance := TBSONObject.Create;
          NestedPrivilegeInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedUserIdInstance.Put('$eq',dbUserId);
          NestedDatabaseInstance.Put('$eq','privilege');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('belongs_to',NestedUserIdInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance := DatabaseCursor.Next;
              //PrivilegeArray := TBSONArray.Create;
              PrivilegeArray  := DatabaseInstance.Items['privilege_array'].AsBSONArray;
              flagOptionGrant := false;
              flagAll := false;
              index3 := PrivilegeArray.Count;
              for index1 := 0 to PrivilegeArray.Count - 1 do
                begin
                  if PrivilegeArray[index1].AsString  = 'grant option' then
                    flagOptionGrant := true;
                  if PrivilegeArray[index1].AsString  = 'all' then
                    flagAll := true;

                end;
              if not flagOptionGrant then
                begin
                  yyerror('USER ID: ' +  dbUserID + ' cannot GRANT PRIVILEGES ');
                  Exit;
                end;

              if not flagAll then
                for index2 := 0 to length(lPrivilege) - 1 do
                  begin
                    found := false;
                    for index1 := 0 to PrivilegeArray.Count - 1 do
                      begin
                        if lPrivilege[index2] = PrivilegeArray[index1].AsString then
                          begin
                            found := true;
                            break
                          end;


                      end;
                    if found then continue else
                      begin
                        yyerror('USER ID: ' +  dbUserID + ' cannot GRANT PRIVILEGE ' + lPrivilege[index2]);
                        Exit;
                      end;
                  end;
              end;
          // if a privilege line for a luserid exist then update it
          privilegeArray := TBSONArray.Create;
          for index1 := 0 to length(lPrivilege) - 1 do
            privilegeArray.Put(lPrivilege[index1]);

          DataDictionaryInstance := TBSONObject.Create;
          DataDictionaryInstance.Put('kind','privilege');
          DataDictionaryInstance.Put('privilege_array',privilegeArray);
          DataDictionaryInstance.Put('dbobject',dbObject);
          DataDictionaryInstance.Put('belongs_to',luserId);
          DataDictionaryInstance.Put('_id', TBSONObjectId.NewFrom);
          DataDictionaryCollection.Insert(DataDictionaryInstance);
          {$ENDIF}

        end;

      233: // PRIVILEGE
        begin
          setlength(lPrivilege, length(lPrivilege) + 1);
          lPrivilege[high(lPrivilege)] := lowercase(stk[High(stk)].strValue);
          setLength(stk, Length(stk) - 1);
        end;

      240: // DATABASE OBJECT
        begin
          dbObject := stk[high(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

       112 .. 116: // AVG .. SUM
        begin
          grpkind := aggregateset(sqlMemProg[i].Mnemonic - 111);
          setlength(expr,length(expr) + 1);
          expr[high(expr)].mnemonic := sqlMemProg[i].Mnemonic;
        end;
       180: // ALL COLUMNS AGGREGATE
        begin
          {$IFDEF mongodb}
          if not flagAggregate then
            begin
              aggregateCollection := GDB.GetCollection('sys_aggregate');
              if aggregateCollection.count <> 0 then
                aggregateCollection.drop();
            end;
          {$ENDIF}
          flagAllColumnsAggregate := true;
          flagAggregate := true;

          setlength(expr,length(expr) + 1);
          expr[High(expr)].mnemonic := 180;
          expr[High(expr)].Value := 111 + ord(grpKind);
          expr[High(expr)].stvalue := '';
          expr[High(expr)].printInstruction := 'ALL COLUMNS AGGREGATE';

        end;

       181: // EXPRESSION AGGREGATE
        begin
          {$IFDEF mongodb}
          if not flagAggregate then
            begin
              aggregateCollection := GDB.GetCollection('sys_aggregate');
              if aggregateCollection.count <> 0 then
                aggregateCollection.drop();
            end;
          {$ENDIF}
          flagExpressionAggregate := true;
          flagAggregate := true;

          setlength(expr,length(expr) + 1);
          expr[High(expr)].mnemonic := 181;
          expr[High(expr)].Value := 111 + ord(grpKind);
          expr[High(expr)].printInstruction := 'EXPRESSION AGGREGATE';
        end;

       182: // DISTINCT AGGREGATE
        begin
          {$IFDEF mongodb}
          if not flagAggregate then
            begin
              aggregateCollection := GDB.GetCollection('sys_aggregate');
              if aggregateCollection.count <> 0 then
                aggregateCollection.drop();
            end;
          {$ENDIF}
          flagExpressionAggregate := true;
          flagAggregate := true;
          setlength(expr,length(expr) + 1);
          expr[High(expr)].mnemonic := 182;
          expr[High(expr)].Value := 111 + ord(grpKind);
          expr[High(expr)].printInstruction := 'DISTINCT AGGREGATE';
        end;

        36: //DISTINCT
        begin
          {$IFDEF mongodb}
          distinctcollection := GDB.GetCollection('sys_distinct');
          if distinctcollection.count <> 0 then
            distinctcollection.drop();
          {$ENDIF}
          flagDistinctClause := true;
        end;

        42 .. 47: // NOT .. BETWEEN
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
        end;

        48, 49: // IS NULL, IS NOT NULL
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
        end;

        50 .. 67: // EQ .. GE ANY
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];

        end;

        68: // EXISTS
        begin
          executeProgram(subqueryMemProg,dbUserId,dbName);
          flagSubquery := false;
          setLength(fromTables,fromTablesLen);
        end;

        244: // START EXISTS (SUBQUERY)
        begin
          flagSubquery := true;
        end;

        79, 83 .. 86, 95: // ADD .. DIV
        begin
          setLength(expr, length(expr) + 1);
          expr[high(expr)] := sqlMemProg[i];
        end;

        80: // UPDATE Rows in Table name;
        begin
          if workingSchema.dbName = '' then
            begin
              yyerror('No Database has been selected');
              exit;
            end;
          if not tableExists(tblName) then
            begin
              yyerror('Table don''t exists');
              Exit;
            end;

          tblFields := loadTableFields(tblName);

          resulttable.resultFields.numCols := 0;
          resulttable.resultFields.columns := nil;
          resulttable.ownerTable := nil;

          resulttable.resultFields.numCols :=
            resulttable.resultFields.numCols + tblFields.numCols;
          setLength(resulttable.resultFields.columns, length(
            resulttable.resultFields.columns) + tblFields.numCols);
          setLength(resulttable.ownerTable, length(resulttable.ownerTable) +
            tblFields.numCols);

          for resultindex := 0 to length(tblFields.columns) - 1 do
          begin
            resulttable.resultFields.columns[resultindex] :=
              tblFields.columns[resultindex];
            resulttable.ownertable[resultindex].colname :=
              tblFields.columns[resultindex].colname;
           resulttable.ownertable[resultindex].tblname :=
              tblFields.tblName;
            resulttable.ownertable[resultindex].aliasname := nil;
          end;

          colsname := nil;
          setlength(colsname, length(colsname) + tblFields.numCols);
          for resultindex := 0 to length(tblFields.columns) - 1 do
            colsname[resultindex] := tblFields.columns[resultindex].colname;

          colsType := nil;
          setlength(colstype, length(colsType) + tblFields.numCols);
          for resultindex := 0 to length(tblFields.columns) - 1 do
            begin
              case tblFields.columns[resultindex].coltype of
                inttype, smallinttype: colsType[resultindex] := 'INTEGER';
                int64type: colsType[resultindex] := 'INT64';
                extendedtype, tdatetimetype: colsType[resultindex] := 'EXTENDED';
                currencytype: colsType[resultindex] := 'CURRENCY';
                booleantype: colsType[resultindex] := 'BOOLEAN';
                stringtype: colsType[resultindex] :=
                    'STRING[' + IntToStr(
                    TBLFields.columns[resultindex].coltypescale.size) + ']'
              end;
            end;

          setLength(Row, tblFields.numCols);
          {$IFDEF tablesdb}
          resulttable.resultRow := nil;
          setLength(resulttable.resultRow, length(resulttable.resultRow) +
            tblFields.numCols);
          {$ENDIF}

          {$IFDEF mongodb}
          IBSONInstance :=  TBSONObject.Create;
          ACursor := tblFields.Collection.Find();
          while ACursor.HasNext do
            begin
              IBSONInstance := ACursor.Next;
              resultTable.resultIBSONInstance := TBSONObject.Create;

              for index := 0 to tblFields.numCols - 1 do
                begin
                  case tblFields.columns[index].coltype of
                    inttype, smallinttype:
                      begin
                        IntFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsInteger;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, IntFieldValue );
                      end;
                    int64type:
                      begin
                        Int64FieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsInt64;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, Int64FieldValue );
                      end;
                    extendedtype, tdatetimetype, TDateType, TTimeType:
                      begin
                        extendedFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, extendedFieldValue );
                      end;
                    currencytype:
                      begin
                        extendedFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, extendedFieldValue );
                      end;
                    booleantype:
                      begin
                        booleanFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsBoolean;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, booleanFieldValue );
                      end;
                    stringtype:
                      begin
                        stringFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsString;
                        resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                          tblFields.columns[index].colname, stringFieldValue );
                      end;
                  end;
                end;

              {$IFDEF tablesdb}
              for resultindex := 0 to length(row) - 1 do
                resulttable.resultRow[resultindex] := row[resultindex];
              {$ENDIF}

              if rowcondition( conditionInstructions, resultTable ) then
                begin
                  for index1 := 0 to length(setInstructions) - 1  do
                    begin
                      for index2 := low(setInstructions[index1]) + 1 to high(setInstructions[index1]) - 1 do
                        begin
                          runstack(setInstructions[index1,index2],setstk);
                        end;
                      updInstance := TBSONObject.Create;
                      updInstance := IBSONInstance;
                      colName := copy(setInstructions[index1,0].stvalue,pos('.',setInstructions[index1,0].stvalue)+1, length(setInstructions[index1,0].stvalue));
                      if setstk[0].caseValue = 6 then updInstance.Put(colName,setstk[0].strValue) else
                        if setstk[0].caseValue = 7 then updInstance.Put(colName,NULL) else
                          updInstance.Put(colName,setstk[0].extValue);
                      tblFields.Collection.Update(IBSONInstance,updInstance);
                    end;
                  UpdatedRows := UpdatedRows + 1;
                end;
            end;
          {$ENDIF}

          {$IFDEF tablesdb}
          resultTable.resultFields.storage := nil;

          for rowId1 :=
            tblFields.storage.firstRow to tblFields.storage.LastRow do
            begin
              if tblFields.storage.existRow(rowId1) then
                begin
                  tblFields.storage.returnRow(rowId1, row);
                  for resultindex := 0 to length(row) - 1 do
                    begin
                      resulttable.resultRow[resultindex] := row[resultindex];
                    end;
                  if rowcondition( conditionInstructions, resultTable ) then
                    begin
                      // We have to look if a column is on an index
                      for index1 := 0 to length(setInstructions) - 1  do
                        begin
                          for index2 := low(setInstructions[index1]) + 1 to high(setInstructions[index1]) - 1 do
                            begin
                              runstack(setInstructions[index1,index2],setstk);
                            end;
                          colName := copy(setInstructions[index1,0].stvalue,pos('.',setInstructions[index1,0].stvalue)+1, length(setInstructions[index1,0].stvalue));

                          // setstk has the values respectively for the columns change in update set clause
                          for j := 0 to tblFields.numCols - 1 do
                            begin
                              if tblFields.columns[j].colname = colName then
                                begin
                                  for index2 := 0 to length(tblFields.idxdata) - 1 do
                                    begin
                                      for index3 := 0 to length(tblFields.idxdata[index2].idxkeys) - 1 do
                                        begin
                                          if tblFields.idxdata[index2].idxkeys[index3].colName = colName then
                                            begin
                                              keys := nil;
                                              setlength(keys, length(keys) + 1);
                                              keys[high(Keys)] := row[j];
                                              //rowId := intToStr(rowid1);
                                              rowId := rowid1;
                                              tblFields.idxdata[index2].idxstorage.DeleteKey(keys,RowId);
                                            end
                                       end;
                                    end;
                                  if setstk[index1].caseValue = 7 then row[j] := null;
                                  if setstk[index1].caseValue = 0 then row[j] := setstk[index1].boolValue;
                                  if setstk[index1].caseValue = 4 then row[j] := setstk[index1].extValue;
                                  if setstk[index1].caseValue = 6 then row[j] := setstk[index1].strValue;
                                  if setstk[index1].caseValue = 8 then row[j] := setstk[index1].extValue;
                                  // check if the column belongs to an index for now only non join index
                                  if tblFields.columns[j].colname = colName then
                                    begin
                                      for index2 := 0 to length(tblFields.idxdata) - 1 do
                                        begin
                                          for index3 := 0 to length(tblFields.idxdata[index2].idxkeys) - 1 do
                                            begin
                                              if tblFields.idxdata[index2].idxkeys[index3].colName = colName then
                                                begin
                                                  keys := nil;
                                                  setlength(keys, length(keys) + 1);
                                                  keys[high(Keys)] := row[j];
                                                  //rowId := intToStr(rowid1);
                                                  rowId := rowid1;
                                                  tblFields.idxdata[index2].idxstorage.AddKey(keys,RowId);
                                                end
                                           end;
                                        end;
                                      end
                                end;
                            end;
                        end;
                      tblFields.storage.putRow(rowId1,row);


                      UpdatedRows := UpdatedRows + 1;
                    end;
                end;
            end;
          {$ENDIF}


             yyacceptmessage(intToStr(UpdatedRows) + ' ROWS UPDATED ');
        end;

        81:  // set new columns
        begin
          setlength(setInstructions, length(setInstructions) + 1);
          setlength(setInstructions[High(setInstructions)], length(expr)+1);
          for index := low(expr) to high(expr) do
            setInstructions[High(setInstructions), index] := expr[index];

          with setInstructions[high(setInstructions),index + 1] do
            begin
              mnemonic := 185;
              Value := 0;
              stvalue := '';
              printInstruction := 'SET COLUMN';
            end;

          expr := nil;
        end;

        //// bring the one for dmtables
        82: // DELETE Rows from Table name
        begin
          // check if the tablename exist
          tblFields := loadTableFields(tblName);

          if not TableExists(tblName) then
          begin
            yyerror('Table Name not exist: ' + tblName);
            exit;
          end;

          // In case of where ommited It is better to delete the table physically

          if conditionInstructions = nil then
          begin
            (*****
            // Better to delete phisycally and restore from DataDictionary as loading tables
            tblFields.storage.Free;
            DeleteFile(Path + dbUserId + workingschema.dbName + tblName);

            setLength(colsName, tblFields.numCols);
            for index := low(tblFields.columns) to high(tblFields.columns) do
              colsName[index] := tblFields.columns[index].colname;

            setLength(colsType, tblFields.numCols);
            for index := low(tblFields.columns) to high(tblFields.columns) do
            begin
              case tblFields.columns[index].coltype of
                intType:
                  type_Name := 'INTEGER';
                smallintType:
                  type_Name := 'SMALLINT';
                int64Type:
                  type_Name := 'INT64';
                extendedType:
                  type_Name := 'EXTENDED';
                TDateTimeType:
                  type_Name := 'TDATETIME';
                currencyType:
                  type_Name := 'CURRENCY';
                booleanType:
                  type_Name := 'BOOLEAN';
                stringType:
                begin
                  type_Name := 'STRING';
                  if tblFields.columns[index].coltypescale.size <> 0 then
                    type_Name :=
                      type_name + '[' +
                      IntToStr(tblFields.columns[index].coltypescale.size) + ']'
                  else
                    type_Name := type_name + '[255]';
                end;
              end;
              colstype[index] := type_Name;
            end;

            setLength(allowcolsNull, tblFields.numCols);
            for index := low(tblFields.columns) to high(tblFields.columns) do
            begin
              allowcolsNull[index] := True;
              for index2 :=
                low(tblFields.constraints) to high(tblFields.constraints) do
                with tblFields.constraints[index2] do
                begin
                  case cnstrtype of
                    1: if nnullcol = index then
                        allowcolsNull[index] := False;
                    2: if isbitset(uqCols[0], index) then
                        allowcolsNull[index] := False;
                    3: if isbitset(pkCols[0], index) then
                        allowcolsNull[index] := False;
                  end;
                end;

            end;
            tblFields.storage :=
              TTableClass.Create(Path + dbUserId + dbName + tblName,
              False, colsName, colsType, allowcolsNull);
            tblFields.storage.Free;
            tblFields.storage :=
              TTableClass.Create(Path + dbUserId + dbName + tblName,
              True, colsName, colsType, allowcolsNull);
            tblFields.Collection.Drop;
            *****)
            yyacceptmessage('ALL ROWS DELETED ');
            { TODO : Delete all values from indexes on table }
          end
          else
          begin


            resulttable.resultFields.numCols := 0;
            resulttable.resultFields.columns := nil;
            resulttable.ownerTable := nil;

            resulttable.resultFields.numCols :=
              resulttable.resultFields.numCols + tblFields.numCols;
            setLength(resulttable.resultFields.columns, length(
              resulttable.resultFields.columns) + tblFields.numCols);
            setLength(resulttable.ownerTable, length(resulttable.ownerTable) +
              tblFields.numCols);

            for resultindex := 0 to length(tblFields.columns) - 1 do
            begin
              resulttable.resultFields.columns[resultindex] :=
                tblFields.columns[resultindex];
              resulttable.ownertable[resultindex].colname :=
                tblFields.columns[resultindex].colname;
             resulttable.ownertable[resultindex].tblname :=
                tblFields.tblName;
              resulttable.ownertable[resultindex].aliasname := nil;
            end;

            colsname := nil;
            setlength(colsname, length(colsname) + tblFields.numCols);
            for resultindex := 0 to length(tblFields.columns) - 1 do
              colsname[resultindex] := tblFields.columns[resultindex].colname;

            colsType := nil;
            setlength(colstype, length(colsType) + tblFields.numCols);
            for resultindex := 0 to length(tblFields.columns) - 1 do
              begin
                case tblFields.columns[resultindex].coltype of
                  inttype, smallinttype: colsType[resultindex] := 'INTEGER';
                  int64type: colsType[resultindex] := 'INT64';
                  extendedtype, tdatetimetype: colsType[resultindex] := 'EXTENDED';
                  currencytype: colsType[resultindex] := 'CURRENCY';
                  booleantype: colsType[resultindex] := 'BOOLEAN';
                  stringtype: colsType[resultindex] :=
                      'STRING[' + IntToStr(
                      TBLFields.columns[resultindex].coltypescale.size) + ']'
                end;
              end;

            setLength(Row, tblFields.numCols);
            {$IFDEF tablesdb}
            resulttable.resultRow := nil;
            setLength(resulttable.resultRow, length(resulttable.resultRow) +
              tblFields.numCols);
            {$ENDIF}


            resultTable.resultFields.storage := nil;







            {$IFDEF mongodb}
            IBSONInstance :=  TBSONObject.Create;
            ACursor := tblFields.Collection.Find();
            while ACursor.HasNext do
              begin
                IBSONInstance := ACursor.Next;
                resultTable.resultIBSONInstance := TBSONObject.Create;;


                for index := 0 to tblFields.numCols - 1 do
                  begin
                    case tblFields.columns[index].coltype of
                      inttype, smallinttype:
                        begin
                          IntFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsInteger;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, IntFieldValue );
                        end;
                      int64type:
                        begin
                          Int64FieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsInt64;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, Int64FieldValue );
                        end;
                      extendedtype, tdatetimetype, TDateType, TTimeType:
                        begin
                          extendedFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, extendedFieldValue );
                        end;
                      currencytype:
                        begin
                          extendedFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, extendedFieldValue );
                        end;
                      booleantype:
                        begin
                          booleanFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsBoolean;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, booleanFieldValue );
                        end;
                      stringtype:
                        begin
                          stringFieldValue := IBSONInstance.Items[tblFields.columns[index].colname].AsString;
                          resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                            tblFields.columns[index].colname, stringFieldValue );
                        end;
                    end;
                  end;

                {$IFDEF tablesdb}
                for resultindex := 0 to length(row) - 1 do
                begin
                  resulttable.resultRow[resultindex] := row[resultindex];
                end;
                {$ENDIF}

                if rowcondition( conditionInstructions, resultTable ) then
                  begin
                    tblFields.Collection.Remove(IBSONInstance);
                    DeletedRows := DeletedRows + 1;
                  end;
              end;
            {$ENDIF}

            {$IFDEF tablesdb}
            for rowId1 :=
              tblFields.storage.firstRow to tblFields.storage.LastRow do
              begin
                if tblFields.storage.existRow(rowId1) then
                  begin
                    tblFields.storage.returnRow(rowId1, row);
                    for resultindex := 0 to length(row) - 1 do
                      begin
                        resulttable.resultRow[resultindex] := row[resultindex];
                      end;
                    if rowcondition(conditionInstructions,resultTable) then
                      begin
                        for j := 0 to tblFields.numCols - 1 do
                          begin
                            if tblFields.columns[j].colname = colName then
                              begin
                                for index2 := 0 to length(tblFields.idxdata) - 1 do
                                  begin
                                    for index3 := 0 to length(tblFields.idxdata[index2].idxkeys) - 1 do
                                      begin
                                        if tblFields.idxdata[index2].idxkeys[index3].colName = colName then
                                          begin
                                            keys := nil;
                                            setlength(keys, length(keys) + 1);
                                            keys[high(Keys)] := row[j];
                                            //rowId := intToStr(rowid1);
                                            rowId := rowid1;
                                            tblFields.idxdata[index2].idxstorage.DeleteKey(keys,RowId);
                                          end
                                     end;
                                  end;
                              end;
                          end;
                        tblFields.storage.deleteRow(rowId1);
                        DeletedRows := DeletedRows + 1;
                      end;
                  end;
              end;

            {$ENDIF}

            yyacceptmessage(inttostr(DeletedRows) + ' ROWS DELETED ');
          end;
        end;

        90: // Opr DEFAULT: POP dfltValue and insert it into a structure
        begin
          // st := colTypeName;
          // use stk to reduce to one value;
          Type_name:= convertType(colTypeName);

          hasDefault := True;
          case stk[high(stk)].caseValue of
            7: // if colname allow null then
              dfltValue := null;
            0: dfltValue := stk[High(stk)].boolValue;
            4, 8: dfltValue := stk[High(stk)].extValue;
            6: dfltValue := stk[High(stk)].strValue;
          end;
          setLength(stk, Length(stk) - 1);
        end;

        176: // AUTOINCREMENT
        begin
          hasAutoIncrement := True;
        end;

        26: // cnstrName
        begin
          cnstrName := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        27..31:
          // cnstrType: 'NULL','NOT NULL', 'UNIQUE', 'PRIMARY KEY', 'FOREIGN KEY / REFERENCES'
        begin
          cnstrType := Mnemonics[sqlMemProg[i].Mnemonic];
        end;

        32: // cnstrType: 'On Delete Cascade'
        begin
          setLength(stk, Length(stk) + 1);
          cnstrType := 'ON DELETE CASCADE';
        end;

        // A check constraint can NOT be defined on a SQL View.
        // The check constraint defined on a table must refer to only columns in that table. It can not refer to columns in other tables.
        // A check constraint can NOT include a SQL Subquery.
        // A check constraint can be defined in either a SQL CREATE TABLE statement or a SQL ALTER TABLE statement.

        130: // cnstrType Check condition to check on insert and update
        begin
          cnstrType := Mnemonics[sqlMemProg[i].Mnemonic];
        end;

        156: //REFERENCE TABLE NAME
        begin
          reftblname := lowercase(stk[High(stk)].strValue);
        end;

        91: // Opr COLUMN CONSTRAINT: Save Constraint in the List
        begin

          // Check for conflict NULL & NOT NULL

          (* cheking Data Dictionary *)
          if cnstrName <> '' then
            begin
              found := constraintNameExists(cnstrName + dbUserId + workingSchema.dbName);

              if found then
                begin
                  yyerror('Constraint Name already exist in ' + tblName);
                  Exit;
                end
               else
                cnstrName := cnstrName + dbUserId + workingSchema.dbName;
            end
            else
            begin
          (*  Constraint type	            Abbreviation
              references (foreign key)          fk
              unique                        	un
              primary key                       pk
              check	                        ck
              not null	                        nn
          *)

              // if there is no name the system will give it a name
              {$IFDEF mongodb}
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
              cnstrName :=
                'sys_CONSTRAINT_' +  DataBaseInstance.GetOid.GetOID ;
              //cnstrType + colsname[0] + Inttostr(DataDictionaryCollection.Count);
              {$ENDIF}

              {$IFDEF tablesdb}
              cnstrName :=
                'sys_CONSTRAINT' + cnstrType + colsname[0] + IntToStr((DDconstraints.lastRow+1));
              {$ENDIF}

              for index1 := low(constraintList) to high(constraintList) do
                if constraintList[index1].constraintname = cnstrName then
                begin
                  yyerror('Constraint Name already exist in ' + tblName);
                  Exit;
                end;
            end;
          setLength(constraintList, length(constraintList) + 1);
          with constraintList[High(constraintList)] do
          begin
            constraintName := cnstrName;
            constraintKind := 0; // Column constraint
            constraintType := cnstrType;
            setLength(columnsName,1);
            if cnstrType = 'REFERENCES' then
              begin
                columnsName[High(columnsName)] := colsName[low(colsName)];
                setLength(refcolumnsName,1);

                refcolumnsName[High(refcolumnsName)] := colsName[High(colsName)];
                refTable := reftblname;
                reftblname := '';
              end else
              columnsName[High(columnsName)] := colsName[High(colsName)];
            if cnstrType = 'CHECK' then
            begin
              checkInstructions := nil;
              for index := low(expr) + 1 to high(expr) do
              begin
                setLength(checkInstructions, length(checkInstructions) + 1);
                checkInstructions[high(checkInstructions)] := expr[index];
              end;
            end;
          end;
          expr := nil;
          cnstrName := '';
          cnstrType := '';
          // colsName := nil
        end;

        33: // Opr TABLE CONSTRAINT: Save Constraint in the List
        begin

          //  Check if the same constraint exist

          (* Check if the constraint name exist could be done by a linear scan into
             datadictionaty, next would be faster by using the b-join tree *)

          (* cheking Data Dictionary *)

          if cnstrName <> '' then
            begin
              found := constraintNameExists(cnstrName + dbUserId + workingSchema.dbName);

              if found then
                begin
                  yyerror('Constraint Name already exist in ' + tblName);
                  Exit;
                end
               else
                cnstrName := cnstrName + dbUserId + workingSchema.dbName;
            end
            else
            begin
              {$IFDEF mongodb}
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
              cnstrName :=
                'sys_CONSTRAINT_' +  DataBaseInstance.GetOid.GetOID ;
              //cnstrType + colsname[0] + Inttostr(DataDictionaryCollection.Count);
              {$ENDIF}

              {$IFDEF tablesdb}
              cnstrName :=
                'sys_CONSTRAINT' + cnstrType + colsname[0] + IntToStr((DDconstraints.lastRow+1));
              {$ENDIF}

              // if there is no name the system will give it a name

              {$IFDEF mongodb}
              // Use a sequence for the name in datadictionary
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
              cnstrName :=
                'sys_CONSTRAINT_' +  DataBaseInstance.GetOid.GetOID ;
              { cnstrName :=
                  'sys_CONSTRAINT' + cnstrType + Inttostr(DataDictionaryCollection.Count); }
              {$ENDIF}

              {$IFDEF tablesdb}
              cnstrName := 'sys_CONSTRAINT' + cnstrType;
              for index1 := 0 to high(colsname) do
                cnstrName := cnstrName + '_' + colsname[index1];
              cnstrName := cnstrName + IntToStr((DDconstraints.lastRow+1));
              {$ENDIF}

              for index1 := low(constraintList) to high(constraintList) do
                if constraintList[index1].constraintname = cnstrName then
                begin
                  yyerror('Constraint Name already exist in ' + tblName);
                  Exit;
                end;
            end;

          setLength(constraintList, length(constraintList) + 1);
          with constraintList[High(constraintList)] do
          begin
            constraintName := cnstrName;
            constraintType := cnstrType;
            constraintKind := 1; // Table constraint
            if cnstrType = 'REFERENCES' then
              begin
                columnsName:= nil;
                for index1 := low(colsName) to (length(colsName) div 2) - 1 do
                  begin
                    setLength(columnsName, length(columnsName) + 1);
                    columnsName[high(columnsName)] := colsName[index1];
                  end;
                refcolumnsName:= nil;

                for index1 := (length(colsName) div 2) to high(colsName) do
                  begin
                    setLength(refcolumnsName, length(refcolumnsName) + 1);
                    refcolumnsName[high(refcolumnsName)] := colsName[index1];
                  end;
                refTable := reftblname;
                reftblname := '';
              end else
              for index1 := low(colsName) to high(colsName) do
                begin
                  setLength(columnsName, length(columnsName) + 1);
                  columnsName[index1] := colsName[index1];
                end;
            if cnstrType = 'CHECK' then
            begin
              checkInstructions := nil;
              for index := low(expr) to high(expr) do
              begin
                setLength(checkInstructions, length(checkInstructions) + 1);
                checkInstructions[high(checkInstructions)] := expr[index];
              end;
            end;
          end;
          cnstrName := '';
          cnstrType := '';
          colsName := nil;
        end;

        69: // group by
            // It is not permissible to include column names in a SELECT clause that are not referenced in the GROUP BY clause.
        begin
          setLength(groupByColumns,length(colsName));
          groupByColumns := colsName;
          colsname := nil;
        end;

        70: // order by
        begin
          flagOrderClause := true;

        end;
        71:  // having
        begin
          setlength(havingconditionInstructions, length(expr));
          for index := low(expr) to high(expr) do
            havingconditionInstructions[index] := expr[index];
          expr := nil;
          colsname := nil;
        end;

        75: // opr ASC
        begin
          found := false;
          if tblName = '' then
            begin
              for index1 := 0 to fromTablesLen - 1 do
                begin
                  tblFields := loadTableFields(fromtables[index1].Name);
                  for index2 := 0 to length(tblFields.columns)- 1 do
                    begin
                      if colname = tblFields.columns[index2].colname then
                        begin
                          tblname := tblFields.tblName;
                          break;
                        end;
                    end;
                  if tblName <> '' then break;
                end;
              if tblName = '' then
                begin
                  yyerror('Column: ' + colname + ' Not belongs to any table in the From Clause ');
                  exit;
                end;
            end
           else
            begin
              found := false;
              for index1 := 0 to fromTablesLen - 1 do
                begin
                  tblFields := loadTableFields(fromtables[index1].Name);
                  if tblFields.tblName = tblName then
                    begin
                      found := true;
                      break;
                    end;
                end;
              if not found then
                begin
                  yyerror('Table Name: ' + tblname + ' Not belongs to any table in the From Clause ');
                  exit;
                end;
              tblFields := loadTableFields(tblName);
              tblName := '';
              for index2 := 0 to length(tblFields.columns) do
                begin
                  if colname = tblFields.columns[index2].colname then
                    begin
                      tblname := tblFields.tblName;
                      break;
                    end;
                end;
              if tblName = '' then
                begin
                  yyerror('Column: ' + colname + ' Not belongs to the table ');
                  exit;
                end;
            end;
          setLength(ordercolumnList,length(ordercolumnList)+1);
          ordercolumnList[high(ordercolumnList)].tblName := tblName;
          ordercolumnList[high(ordercolumnList)].colName := colName;
          ordercolumnList[high(ordercolumnList)].colOrder := true;
          tblName := '';
        end;

        76: // opr DESC
        begin
          found := false;
          if tblName = '' then
            begin
              for index1 := 0 to fromTablesLen - 1 do
                begin
                  tblFields := loadTableFields(fromtables[index1].Name);
                  for index2 := 0 to length(tblFields.columns)- 1 do
                    begin
                      if colname = tblFields.columns[index2].colname then
                        begin
                          tblname := tblFields.tblName;
                          break;
                        end;
                    end;
                  if tblName <> '' then break;
                end;
              if tblName = '' then
                begin
                  yyerror('Column: ' + colname + ' Not belongs to any table in the From Clause ');
                  exit;
                end;
            end
           else
            begin
              found := false;
              for index1 := 0 to fromTablesLen - 1 do
                begin
                  tblFields := loadTableFields(fromtables[index1].Name);
                  if tblFields.tblName = tblName then
                    begin
                      found := true;
                      break;
                    end;
                end;
              if not found then
                begin
                  yyerror('Table Name: ' + tblname + ' Not belongs to any table in the From Clause ');
                  exit;
                end;
              tblFields := loadTableFields(tblName);
              tblName := '';
              for index2 := 0 to length(tblFields.columns) do
                begin
                  if colname = tblFields.columns[index2].colname then
                    begin
                      tblname := tblFields.tblName;
                      break;
                    end;
                end;
              if tblName = '' then
                begin
                  yyerror('Column: ' + colname + ' Not belongs to the table ');
                  exit;
                end;
            end;
          setLength(ordercolumnList,length(ordercolumnList)+1);
          ordercolumnList[high(ordercolumnList)].tblName := tblName;
          ordercolumnList[high(ordercolumnList)].colName := colName;
          ordercolumnList[high(ordercolumnList)].colOrder := false;
          tblName := '';
        end;

        121: // Opr IDN: POP idxName and insert it into a structure
        begin
          indexName := lowercase(stk[High(stk)].strValue);
          setLength(stk, Length(stk) - 1);
        end;

        122: // opr ASC
        begin
          ordAsc := True;
        end;

        123: // opr DESC
        begin
          ordAsc := False;
        end;

        124: // Opr INDEX COLUMN
        begin
          // Check if the same column exist
          setLength(idxAsc, length(idxAsc) + 1);
          idxAsc[High(idxAsc)] := ordAsc;
          ordAsc := True; // return true by default
        end;

        120: // Opr CREATE Index
        begin
          // Check the Table Name
          tblFields := loadTableFields(tblName);
          if not TableExists(tblName) then
          begin
            yyerror('Table Name not exist: ' + tblName);
            exit;
          end;

          Thekeys := nil;
          // Check the Columns Name
          for index1 := low(colsName) to high(colsName) do
            begin
              found := False;
              for index2 := 0 to tblFields.numCols - 1 do
                if colsName[index1] = tblFields.columns[index2].colname then
                  begin
                    found := True;
                    break;
                  end;
              if not found then
                begin
                  yyerror('Column Name not exist: ' + colsName[high(colsName)]);
                  exit;
                end;

              setlength(Thekeys, length(Thekeys) + 1);
              case tblFields.columns[index2].coltype of
                intType, smallintType, int64Type:
                  Thekeys[High(Thekeys)] := colsName[index1] + ': INTEGER';
                extendedType:
                  Thekeys[High(Thekeys)] := colsName[index1] + ': EXTENDED';
                TDateTimeType, TDateType, TTimeType:
                  Thekeys[High(Thekeys)] := colsName[index1] + ': TDATETIME';
                currencyType:
                  Thekeys[High(Thekeys)] := colsName[index1] + ': CURRENCY';
                booleanType:
                  Thekeys[High(Thekeys)] := colsName[index1] + ': BOOLEAN';
                stringType:
                  Thekeys[High(Thekeys)] :=
                    colsName[index1] + ': STRING[' + IntToStr(
                    tblFields.columns[index2].coltypescale.size) + ']';
              end;
            end;

          found := IndexNameExists(IndexName + '_' + workingSchema.dbName);

          if found then
            begin
              yyerror('Index Name already exist in ' + workingSchema.dbname);
              Exit;
            end
           else
            indexName := indexName  + '_' + workingSchema.dbName;

          // Index_metaData(index_name,table_name,database_name,key_name,order_name...,key_name,order_name)
          // key_name,order...,key_name,order is KeysIndex_metaData(key_name,order,table_name,database_name,index_name)

          {$IFDEF mongodb}
          DatabaseInstance := TBSONObject.Create;
          DatabaseInstance.Put('kind','index');
          DatabaseInstance.Put('type','mono');
          DatabaseInstance.Put('database_name',workingSchema.dbName);
          DatabaseInstance.Put('index_name',indexName);
          DatabaseInstance.Put('table_name',tblname);
          lcount := length(colsName);
          DatabaseInstance.Put('columns_count',lcount);

          for index1 := low(colsName) to high(colsName) do
            begin
              DatabaseInstance.Put('column_name'+inttostr(index1),colsName[index1]);
              DatabaseInstance.Put('column_order'+inttostr(index1),idxASC[index1]);
            end;

          DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
          DataDictionaryCollection.Insert(DatabaseInstance);
          {$ENDIF}

          {$IFDEF tablesdb}
          row := nil;
          setLength(row, 3);
          row[0] := indexName;
          row[1] := tblName;
          row[2] := workingSchema.dbName;
          DDindexes.insertRow(row);

          // KeysIndex_metaData(key_name,order,table_name,database_name,index_name)
          for index1 := low(colsName) to high(colsName) do
            begin
              row := nil;
              setLength(row, 4);
              row[0] := indexName;
              row[1] := tblName;
              row[2] := colsName[index1];
              row[3] := idxASC[index1];
              DDkeysindexes.insertRow(row);
            end;
          {$ENDIF}

          for index2 := low(workingSchema.tables) to high(workingSchema.tables) do
            if workingSchema.tables[index2].tblName = tblName then
              begin
                setLength(workingSchema.tables[index2].idxdata,
                  length(workingSchema.Tables[index2].idxdata) + 1);
                with workingSchema.tables[index2] do
                  with idxData[High(idxData)] do
                    begin
                      idxname := indexname;
                      idxkeys := nil;
                      for index1 := low(colsName) to high(colsName) do
                        begin
                          setlength(idxkeys, length(idxkeys) + 1);
                          with idxkeys[high(idxkeys)] do
                            begin
                              colName := colsName[index1];
                              colOrder := idxASC[index1];
                            end;
                        end;

                      {$IFDEF mongodb}
                      IBSONInstance :=  TBSONObject.Create;
                      for index := 0 to length(idxkeys) - 1 do
                        begin
                          if idxkeys[index].colorder then
                              with idxkeys[index] do IBSONInstance.Put(colName,1) else
                              with idxkeys[index] do IBSONInstance.Put(colName,-1);
                        end;
                      tblFields.Collection.CreateIndex(IBSONInstance,indexname);
                      {$ENDIF}

                      {$IFDEF tablesdb}
                      idxstorage :=
                        BtrPlusClass.Create(Path + indexName, False, Thekeys, idxAsc);
                      idxstorage.Free;
                      idxstorage :=
                        BtrPlusClass.Create(Path + indexName, True, Thekeys, idxAsc);

                      // fill the index with tables data
                      (***********
                      colsNull := nil;
                      setlength(colsNull, numCols);
                      ************)
                      Row := nil;
                      setLength(Row, numCols);
                      for rowId1 :=
                            storage.firstRow to storage.LastRow do
                        begin
                          if storage.existRow(rowId1) then
                            begin
                              storage.returnRow(rowId1, Row);
                              ///// colsnull can be detected by row[index3] = null
                              keys := nil;
                              for index1 := low(colsName) to high(colsName) do
                                begin
                                  for index3 := 0 to tblFields.numCols - 1 do
                                    if (colsName[index1] =
                                      columns[index3].colname) then
                                      break;
                                  setlength(keys, length(keys) + 1);
                                  if (row[index3] = null) then
                                    begin
                                      keys[high(Keys)] := 'NULL';
                                    end
                                   else
                                    keys[high(Keys)] := Row[index3];
                                end;
                              //rowId := intToStr(rowid1);
                              rowId := rowid1;
                              workingSchema.tables[index2].IdxData[High(
                                workingSchema.tables[index2].IdxData)].idxstorage.AddKey(keys, rowId);
                            end;
                        end;
                      {$ENDIF}

                yyacceptmessage('Index created');

                TheKeys := nil;
                idxAsc := nil;
              end;
          end
        end;

        152: // CREATE JOIN INDEX
        begin

          if ordercolumnList <> nil then
            for index := 0 to length(ordercolumnList) - 1 do
              begin
                setlength(lidxkeys,length(lidxkeys) + 1);
                lidxkeys[high(lidxkeys)+1].tblName := ordercolumnList[index].tblname;
                lidxkeys[high(lidxkeys)+1].colName := ordercolumnList[index].colname;
                lidxkeys[high(lidxkeys)+1].colOrder := ordercolumnList[index].colorder
              end;
          // check the index name

          IndexName := lowercase(IndexName);
          found := IndexNameExists(IndexName + '_' + workingSchema.dbName);

          if found then
            begin
              yyerror('Index Name already exist in ' + workingSchema.dbname);
              Exit;
            end
           else
            indexName := indexName  + '_' + workingSchema.dbName;

           {$IFDEF mongodb}
          // Index_metaData(index_name,table_name,database_name,key_name,order_name...,key_name,order_name)
          // key_name,order...,key_name,order is KeysIndex_metaData(key_name,order,table_name,database_name,index_name)
          DatabaseInstance := TBSONObject.Create;
          DatabaseInstance.Put('kind','index');
          DatabaseInstance.Put('type','join');
          DatabaseInstance.Put('database_name',workingSchema.dbName);
          DatabaseInstance.Put('index_name',indexName);

          DatabaseInstance.Put('basetables_count', length(ljoinbaseTables));
          for index1 := low(ljoinbaseTables) to high(ljoinbaseTables) do
            DatabaseInstance.Put('basetable_name'+inttostr(index1),ljoinbaseTables[index1]);

          lcount := length(lidxkeys);
          DatabaseInstance.Put('columns_count',lcount);
          // KeysIndex_metaData(key_name,order,table_name,database_name,index_name)
          for index1 := low(lidxkeys) to high(lidxkeys) do
            begin
              DatabaseInstance.Put('table_name'+inttostr(index1),lidxkeys[index1].tblName);
              DatabaseInstance.Put('column_name'+inttostr(index1),lidxkeys[index1].colName);
              DatabaseInstance.Put('column_order'+inttostr(index1),lidxkeys[index1].colOrder);
            end;

          // joinKeysIndex_metaData(from_table_name,to_table_name,join_column_name,database_name,index_name)
          lcount := length(ljoincouples);
          DatabaseInstance.Put('join_couples',lcount);
          for index1 := low(ljoincouples) to high(ljoincouples) do
            begin
              DatabaseInstance.Put('from_table'+inttostr(index1),ljoincouples[index1].fromTable);
              DatabaseInstance.Put('to_table'+inttostr(index1),ljoincouples[index1].toTable);
              lcount := length(ljoincouples[index1].keyNames);
              DatabaseInstance.Put('keys_count'+inttostr(index1),lcount);
              for index2 :=
                low(ljoincouples[index1].keyNames)
                to high(ljoincouples[index1].keyNames) do
                begin
                  DatabaseInstance.Put('key_name'+inttostr(index1)+'-'+inttostr(index2),
                                       ljoincouples[index1].keyNames[Index2]);
                end;
            end;
          DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
          DataDictionaryCollection.Insert(DatabaseInstance);
          {$ENDIF}

          {$IFDEF tablesdb}
          // Index_metaData(index_name,relation_name,database_name,key_name,order_name...,key_name,order_name)
          // key_name,order...,key_name,order is KeysIndex_metaData(key_name,order,relation_name,database_name,index_name)
          row := nil;
          setLength(row, 3);
          row[0] := indexName;
          tblName := 'sys_JoinIndex'; // for join index
          row[1] := tblName;
          row[2] := workingSchema.dbName;
          DDindexes.insertRow(row);

          row := nil;
          setLength(row, 2);
          for index1 := low(ljoinbaseTables) to high(ljoinbaseTables) do
            begin
              row[0] := indexName;
              row[1] := ljoinbaseTables[index1];
              DDbasetables.insertRow(row);
            end;


          // KeysIndex_metaData(key_name,order,relation_name,database_name,index_name)
          row := nil;
          setLength(row, 4);
          for index1 := low(lidxkeys) to high(lidxkeys) do
            begin
              row[0] := indexName;
              row[1] := lidxkeys[index1].tblName;
              row[2] := lidxkeys[index1].colName;
              row[3] := lidxkeys[index1].colOrder;
              DDkeysindexes.insertRow(row);
            end;

          // joinkeysIndex_metaData(from_relation_name,to_relation_name,join_column_name,database_name,index_name)

          row := nil;
          setLength(row, 3);
          for index1 := low(ljoincouples) to high(ljoincouples) do
            begin
              row[0]:= indexName;

              row[1] := ljoincouples[index1].fromTable;

              row[2] := ljoincouples[index1].toTable;

              DDjoinindexes.insertRow(row);
            end;


          row := nil;
          setLength(row, 4);
          for index1 := low(ljoincouples) to high(ljoincouples) do
            for index2 := 0 to high(ljoincouples[index1].keyNames) do
              begin
                row[0]:= indexName;

                row[1] := ljoincouples[index1].fromTable;

                row[2] := ljoincouples[index1].toTable;

                row[3] := ljoincouples[index1].keyNames[Index2];

                DDkeysjointables.insertRow(row);
              end;
          {$ENDIF}

          (*** Loading into Working Schema ***)

          setLength(workingSchema.joinidxdata, length(workingSchema.joinidxdata) + 1);
          workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxname := indexname;
          workingSchema.joinidxdata[high(workingSchema.joinidxdata)].joincouples := nil;
          workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxkeys := nil;

          for index1 := low(ljoinBaseTables) to high(ljoinBaseTables) do
            with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
              begin
                setlength(joinBaseTables, length(joinBaseTables) + 1);
                joinBaseTables[high(joinBaseTables)] := ljoinBaseTables[index1];
              end;

          for index1 := low(lidxkeys) to high(lidxkeys) do
            begin
              with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                begin
                  setlength(idxkeys, length(idxkeys) + 1);
                  with idxkeys[high(idxkeys)] do
                    begin
                      idxkeys[high(idxkeys)].tblName := lidxkeys[index1].tblName;
                      idxkeys[high(idxkeys)].colName := lidxkeys[index1].colName;
                      idxkeys[high(idxkeys)].colOrder := lidxkeys[index1].colOrder;
                    end;
                end;
            end;

          for index1 := low(ljoincouples) to high(ljoincouples) do
            with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
              begin
                setlength(joincouples, length(joincouples) + 1);
                with joincouples[high(joincouples)] do
                  begin
                    fromTable := ljoincouples[index1].fromTable;
                    toTable := ljoincouples[index1].toTable;
                    for index2 :=
                      low(ljoincouples[index1].keyNames)
                      to high(ljoincouples[index1].keyNames) do
                      begin
                        setlength(keyNames, length(keyNames) + 1);
                        keyNames[high(KeyNames)] :=
                          ljoincouples[index1].keyNames[index2];
                      end;
                  end;
              end;

          //create the bjointree and open it
          with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
            begin
              TheBaseTables := nil;
              for index1 := 0 to length(joinbaseTables) - 1 do
                begin
                  setlength(TheBaseTables, length(TheBaseTables) + 1);
                  TheBaseTables[high(TheBaseTables)] := joinbaseTables[index1];
                end;
              idxstorage := BJoinTreeClass.Create(Path + idxName, TheBaseTables);
            end;

          for index2 := Low(TheBaseTables) to High(TheBaseTables) do
            with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
              begin
                idxstorage.AddTableToDictionary(TheBaseTables[index2]);
                // take all his columns and add them to Dictionary
                // check the null values
                tblFields := loadTableFields(TheBaseTables[index2]);
                for index3 := 0 to tblFields.numCols - 1 do
                  begin
                    colname := tblFields.columns[index3].colname;
                    case tblFields.columns[index3].coltype of
                      intType, smallintType, int64Type:
                        idxstorage.AddColumnToDictionary(
                          colname, 'Integer', TheBaseTables[index2]);
                      extendedType:
                        idxstorage.AddColumnToDictionary(
                          colname, 'Extended', TheBaseTables[index2]);
                      TDateTimeType, TDateType, TTimeType:
                        idxstorage.AddColumnToDictionary(
                          colname, 'TDateTime', TheBaseTables[index2]);
                      currencyType:
                        idxstorage.AddColumnToDictionary(
                          colname, 'Currency', TheBaseTables[index2]);
                      booleanType:
                        idxstorage.AddColumnToDictionary(
                          colname, 'Boolean', TheBaseTables[index2]);
                      stringType:
                        idxstorage.AddColumnToDictionary(colname, 'string[' +
                          IntToStr(tblFields.columns[index3].coltypescale.size) + ']', TheBaseTables[index2]);
                    end;
                  end;
              end;

          with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
            begin
              for index1 := low(joincouples) to high(joincouples) do
                with joincouples[index1] do
                  idxstorage.AddJoin(fromTable, toTable, keyNames[0]);
              TheKeys := nil;
              setlength(TheKeys, length(idxkeys));
              for index1 := low(idxkeys) to high(idxkeys) do
                TheKeys[index1] :=
                  idxkeys[index1].tblName + '.' + idxkeys[index1].colName;
              idxstorage.createBTrees(TheBaseTables, False, TheKeys);
              idxstorage.Free;

              idxstorage := BJoinTreeClass.Create(Path + idxName, TheBaseTables);
              for index2 := Low(TheBaseTables) to High(TheBaseTables) do
                with workingSchema.joinidxdata[high(workingSchema.joinidxdata)] do
                  begin
                    idxstorage.AddTableToDictionary(TheBaseTables[index2]);
                    // take all his columns and add them to Dictionary
                    // check the null values
                    tblFields := loadTableFields(TheBaseTables[index2]);
                    for index3 := 0 to tblFields.numCols - 1 do
                      begin
                        colname := tblFields.columns[index3].colname;
                        case tblFields.columns[index3].coltype of
                          intType, smallintType:
                            idxstorage.AddColumnToDictionary(
                              colname, 'Integer', TheBaseTables[index2]);
                          int64Type:
                            idxstorage.AddColumnToDictionary(
                              colname, 'Int64', TheBaseTables[index2]);
                          extendedType:
                            idxstorage.AddColumnToDictionary(
                              colname, 'Extended', TheBaseTables[index2]);
                          TDateTimeType, TDateType, TTimeType:
                            idxstorage.AddColumnToDictionary(
                              colname, 'TDateTime', TheBaseTables[index2]);
                          currencyType:
                            idxstorage.AddColumnToDictionary(
                              colname, 'Currency', TheBaseTables[index2]);
                          booleanType:
                            idxstorage.AddColumnToDictionary(
                              colname, 'Boolean', TheBaseTables[index2]);
                          stringType:
                            idxstorage.AddColumnToDictionary(colname, 'string[' +
                              IntToStr(tblFields.columns[index3].coltypescale.size) + ']', TheBaseTables[index2]);
                        end;
                      end;
                  end;
              for index1 := low(joincouples) to high(joincouples) do
                with joincouples[index1] do
                  idxstorage.AddJoin(fromTable, toTable, keyNames[0]);
              idxstorage.createBTrees(TheBaseTables, True, TheKeys);
            end;

          // case of base tables not empty
          {$IFDEF mongodb}
          for index := 0 to length(TheBaseTables) - 1 do
            begin
             // for every base table: load the rows and insert them in a join
             tblFields := loadTableFields(TheBaseTables[index]);
             BaseCursor := tblFields.Collection.Find();
             while BaseCursor.HasNext do
               begin
                 Row2 := nil;
                 BaseIBSONInstance := BaseCursor.Next;
                 for index1 := 0 to tblFields.numCols - 1 do
                   begin
                     setLength( Row2, length(Row2) + 1);
                     // setLength(colsNull, length(colsNull) + 1);
                     case BaseIBSONInstance.Items[tblFields.columns[index1].colname ].ValueType of
                       bvtNull:
                         begin
                           stringFieldValue := 'Null';
                           Row2[high(Row2)] := stringFieldvalue;
                         end;
                       bvtBoolean:
                         begin
                           Row2[high(Row2)] := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsBoolean;
                         end;
                       bvtInteger:
                         begin
                           Row2[high(Row2)] := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsInteger;
                         end;
                       bvtInt64:
                         begin
                           Row2[high(Row2)] := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsInt64;
                         end;
                       bvtDouble:
                         begin
                           Row2[high(Row2)] := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsFloat;
                         end;
                       bvtString:
                         begin
                           Row2[high(Row2)] := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsString;
                         end;
                     end;
                   end;
                 workingschema.joinidxdata[index1].idxstorage.AddKey(tblFields.tblName, Row2, BaseIBSONInstance.GetOid.GetOID);
               end;
           end;
          {$ENDIF}

          {$IFDEF tablesdb}
          for index := 0 to length(TheBaseTables) - 1 do
            begin
             // for every base table: load the rows and insert them in a join
             tblFields := loadTableFields(TheBaseTables[index]);
             for rowId1 := tblFields.storage.firstRow to tblFields.storage.LastRow do
               if tblFields.storage.existRow(rowId1) then
                 begin
                   tblFields.storage.returnRow(rowId1, row);
                   Row2 := nil;

                   for index1 := 0 to tblFields.numCols - 1 do
                     begin
                       setLength( Row2, length(Row2) + 1);
                       if row[index1] = null then
                         begin
                           stringFieldValue := 'Null';
                           Row2[high(Row2)] := stringFieldvalue;
                         end else
                         begin
                           case tblFields.columns[index1].coltype of
                             smallinttype, intType:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                             int64type:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                             extendedtype, tdatetype, ttimetype, tdatetimetype:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                             currencytype:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                             booleantype:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                             stringtype:
                               begin
                                 Row2[high(Row2)] := row[index1];
                               end;
                           end;
                         end;
                       workingschema.joinidxdata[index1].idxstorage.AddKey(tblFields.tblName, Row2, rowId1);
                     end;
                end;
            end;
          {$ENDIF}

          yyacceptmessage('Join Index Created ');

        end;

        153: // BASE TABLE
        begin
          // check if the table belongs to the schema

          if not TableExists(tblName) then
            begin
              yyerror('Table Name ' + tblName + ' doesn''t exist');
              exit;
            end;

          setlength(ljoinbaseTables,length(ljoinbaseTables) + 1);
          ljoinbaseTables[high(ljoinbaseTables)] := tblName;
        end;

        154: // JOIN TABLES CONDITION
        begin
          if length(expr) <> 3 then
            begin
              yyerror('Error in the join index');
              exit;
            end else
            begin
              if not ((expr[high(expr)-2].mnemonic = 151) and (expr[high(expr)-1].mnemonic = 151) and (expr[high(expr)].mnemonic = 50)) then
                begin
                  yyerror('Error in the join condition');
                  exit;
                end;
            end;

          setlength(ljoincouples,length(ljoincouples) + 1);
          with ljoinCouples[high(ljoinCouples)] do
            begin
              fromtblName := copy(expr[high(expr)-2].stvalue,1,pos('.',expr[high(expr)-2].stvalue)-1);
              totblName := copy(expr[high(expr)-1].stvalue,1,pos('.',expr[high(expr)-1].stvalue)-1);
              fromtocolName := copy(expr[high(expr)-2].stvalue,pos('.',expr[high(expr)-2].stvalue)+1,length(expr[high(expr)-2].stvalue));
              fromTable := fromtblName;
              toTable := totblName;
              setLength(KeyNames,1);
              keyNames[0] := fromtocolName
            end;

          found := false;
          for index := 0 to length(ljoinbaseTables) -1 do
            if fromtblName = ljoinbaseTables[index] then
              begin
                found := true;
                break
              end;
          if not found then
            yyerror('Table ' + fromtblName + ' don''t exists');

          found := false;
          for index := 0 to length(ljoinbaseTables) -1 do
            if totblName = ljoinbaseTables[index] then
              begin
                found := true;
                break
              end;
          if not found then
            yyerror('Table ' + totblName + ' don''t exists');

          setlength(ljoincouples,length(ljoincouples) + 1);
          with ljoinCouples[high(ljoinCouples)] do
            begin
              tofromcolName := copy(expr[high(expr)-1].stvalue,pos('.',expr[high(expr)-1].stvalue)+1,length(expr[high(expr)-1].stvalue));
              fromTable := totblName;
              toTable := fromtblName;
              setLength(KeyNames,1);
              keyNames[0] := tofromcolName
            end;
          fromTablesLen := length(fromTables);
          expr := nil;
          colsname := nil;

          // check fromtocolNames
          // check tofromcolNames

        end;

        166: // ALTER TABLE
        begin
          if ladd_column then
            begin
              if not TableExists(tblName) then
                begin
                  yyerror('Table ' + tblName + ' not found');
                  exit;
                end;

              tblFields := loadTableFields(tblName);
              for index1 := low(columnList) to high(columnList) do
                for index2 := 0 to high(tblFields.columns) do
                  if columnList[index1].columnName = tblFields.columns[index2].colname then
                    begin
                      yyerror('column ' + columnList[index1].columnName + ' already exists');
                      exit;
                    end;

              // ALTER TABLE only allows columns to be added that can contain nulls,
              // or have a DEFAULT definition specified, or the column being added
              // is an identity (autoincrement or timestamp) column, or alternatively if none of the
              // previous conditions are satisfied the table must be empty to allow addition of these columns

              for index1 := 0 to high(constraintList) do
                begin
                  if (constraintList[index1].constraintType = 'NOT NULL') or
                     (constraintList[index1].constraintType = 'PRIMARY KEY') then
                       for index2 := 0 to high(constraintList[index1].columnsName) do
                         begin
                           colname := constraintList[index1].columnsName[index2];
                           for index3 := 0 to high(columnList) do
                             if columnList[index3].columnName = colname then
                               if not ((columnList[index3].columnTypeName = 'TIMESTAMP') or
                                  columnList[index3].hascolumnDefault or
                                  columnList[index3].hascolumnAutoIncrement) then
                                 begin
                                   {$IFDEF mongodb}
                                   if tblFields.Collection.Count <> 0 then
                                   {$ENDIF}
                                   {$IFDEF tablesdb}
                                   if not DDTables.emptyTable then
                                   {$ENDIF}
                                     begin
                                       yyerror('Table could be empty in a NOT NULL column');
                                       exit;
                                     end;
                                 end;
                         end;
                 end;

              colsName := nil;
              colsType := nil;
              allowcolsNull := nil;

              {$IFDEF mongodb}
              for index1 := low(columnList) to high(columnList) do
                begin
                  // Column_metaData(column_name,table_name,database_name,position,type_name,Default)
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance.Put('kind','tablecolumn');
                  DatabaseInstance.Put('database_name',workingSchema.dbName);
                  DatabaseInstance.Put('table_name',tblname);
                  DatabaseInstance.Put('column_name',columnlist[index1].columnName);
                  setLength(colsName, length(colsName) + 1);
                  colsName[high(colsName)] := columnlist[index1].columnName;
                  DatabaseInstance.Put('position',index1);
                  DatabaseInstance.Put('type_name',columnlist[index1].columnTypeName);
                  setLength(colsType, length(colsType) + 1);
                  colsType[high(colsType)] := convertType(columnlist[index1].columnTypeName);
                  with columnlist[index1] do
                    if charTypeSize <> 0 then
                      begin
                        dimSize := charTypeSize;
                        colsType[high(colsType)] :=
                          colsType[high(colsType)] + '[' + IntToStr(charTypeSize) + ']';
                      end
                     else dimSize := numTypeSize[0];
                  DatabaseInstance.Put('dim1',dimSize); // 'dim1'
                  DatabaseInstance.Put('dim2',columnlist[index1].numTypeSize[1]); // 'dim2'

                  with columnlist[index1] do
                    if hascolumnAutoIncrement then
                      DatabaseInstance.Put('kind_default',8)  // AUTOINCREMENT
                     else
                      begin
                        if hascolumnDefault then
                          begin
                            if (convertType(columnTypeName) = 'INTEGER') or
                              (convertType(columnTypeName) = 'SMALLINT') then
                              begin
                                DatabaseInstance.Put('kind_default',0);  // INTEGER
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('int_default',varDef);
                              end;
                            if convertType(columnTypeName) = 'INT64' then
                              begin
                                DatabaseInstance.Put('kind_default',2);  // INT64
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('int64_default',varDef);
                              end;
                            if (convertType(columnTypeName) = 'SINGLE') or
                               (convertType(columnTypeName) = 'DOUBLE') or
                               (convertType(columnTypeName) = 'EXTENDED') then
                              begin
                                DatabaseInstance.Put('kind_default',3);  // EXTENDED
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('ext_default',varDef);
                              end;
                            if convertType(columnTypeName) = 'CURRENCY' then
                              begin
                                DatabaseInstance.Put('kind_default',4);  // CURRENCY
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('currency_default',varDef);
                              end;
                            if (convertType(columnTypeName) = 'TDATETIME') or
                               (convertType(columnTypeName) = 'TDATE') or
                               (convertType(columnTypeName) = 'TTIME') then
                              begin
                                DatabaseInstance.Put('kind_default',5);  // TDATETIME
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('ext_default',varDef);
                              end;
                            if convertType(columnTypeName) = 'BOOLEAN' then
                              begin
                                DatabaseInstance.Put('kind_default',6);  // BOOLEAN
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                                DatabaseInstance.Put('boolean_default',varDef);
                              end;
                            if convertType(columnTypeName) = 'STRING' then
                              begin
                                DatabaseInstance.Put('kind_default',7);  // STRING
                                with columnlist[index1] do
                                  vardef := IsCompatibleType(columnDefaultValue,columnTypeName,charTypeSize,numTypeSize[1]);
                                DatabaseInstance.Put('st_default',varDef);
                              end;
                          end
                         else
                          begin
                            (* If you specify NULL as the default value for a column, you cannot
                               specify a NOT NULL constraint as part of the column definition.
                               NULL is not a valid default value for a column that is part of a
                               primary key.
                            *)
                            DatabaseInstance.Put('kind_default',-1);  // DEFAULT NOT AVAILABLE
                          end;
                      end;

                    DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
                    DataDictionaryCollection.Insert(DatabaseInstance);

                end;
              {$ENDIF}

              {$IFDEF tablesdb}
              for index1 := low(columnList) to high(columnList) do
                begin
                  // Column_metaData(column_name,table_name,database_name,position,type_name,Default)
                  row := nil;
                  setLength(row, 14);
                  row[0] := columnlist[index1].columnName;
                  setLength(colsName, length(colsName) + 1);
                  colsName[high(colsName)] := columnlist[index1].columnName;
                  row[1] := tblName;
                  row[2] := workingSchema.dbName;
                  row[3] := index1; // position
                  row[4] := columnlist[index1].columnTypeName;
                  setLength(colsType, length(colsType) + 1);
                  colsType[high(colsType)] :=
                    convertType(columnlist[index1].columnTypeName);
                  with columnlist[index1] do
                    if charTypeSize <> 0 then
                    begin
                      dimSize := charTypeSize;
                      colsType[high(colsType)] :=
                        colsType[high(colsType)] + '[' + IntToStr(charTypeSize) + ']';
                    end
                    else
                      dimSize := numTypeSize[0];
                  row[5] := dimSize; // 'dim1'
                  row[6] := columnlist[index1].numTypeSize[1]; // 'dim2'

                  row[8] := 0;
                  row[9] := '';    // not valid
                  row[10] := 0;
                  row[11] := 0.0;
                  row[12] := 0.0;
                  row[13] := False;

                  with columnlist[index1] do
                    if hascolumnAutoIncrement then
                      begin
                        // could be used for Timestamp checking on data type
                        row[7] := 8; // AUTOINCREMENT
                      end
                     else
                      begin
                        if hascolumnDefault then
                          begin
                            if (convertType(columnTypeName) = 'INTEGER') or
                               (convertType(columnTypeName) = 'SMALLINT') then
                              begin
                                row[7] := 0;  // 0 for integer
                                row[8] := columnlist[index1].columnDefaultValue;
                              end;
                    ////        if columnlist[index1].columnDefaultValue = null then
                    ////          row[7] := 1; // null value
                            if convertType(columnTypeName) = 'INT64' then
                              begin
                                row[7] := 2;  // 2 for int64
                                row[10] := columnlist[index1].columnDefaultValue;
                              end;
                            if (convertType(columnTypeName) = 'SINGLE') or
                               (convertType(columnTypeName) = 'DOUBLE') or
                               (convertType(columnTypeName) = 'EXTENDED') then
                              begin
                                row[7] := 3;  // 3 for extended
                                row[11] := columnlist[index1].columnDefaultValue;
                              end;
                            if convertType(columnTypeName) = 'CURRENCY' then
                              begin
                                row[7] := 4;  // 4 for currency
                                row[12] := columnlist[index1].columnDefaultValue;
                              end;
                            if convertType(columnTypeName) = 'TDATETIME' then
                              begin
                                row[7] := 5;  // 5 for tdatetime
                                row[11] := @columnlist[index1].columnDefaultValue;
                              end;
                            if convertType(columnTypeName) = 'BOOLEAN' then
                              begin
                                row[7] := 6;  // 6 for boolean
                                row[13] := columnlist[index1].columnDefaultValue;
                              end;
                            if convertType(columnTypeName) = 'STRING' then
                              begin
                                row[7].VInteger := 7; // 7 for string
                                row[9]:= columnlist[index1].columnDefaultValue;
                              end;
                          end
                        else
                        begin
                          (* If you specify NULL as the default value for a column, you cannot
                             specify a NOT NULL constraint as part of the column definition.
                             NULL is not a valid default value for a column that is part of a
                             primary key.
                          *)
                          row[7] := -1; // default not specify
                        end;
                      end;

                  DDtablecolumns.insertRow(row);
                end;
              {$ENDIF}

              {$IFDEF mongodb}
              for index1 := Low(constraintList) to high(constraintList) do
                begin

                  // Constraint_metaData(constraint_name,column_name,...,column_name,constraint_kind,constraint_type)
                  // column_name,...,column_name is ColumnsConstrain_metaData(column_name,table_name,database_name,constraint_name)
                  // constraint_kind: column / table
                  // constraint_type: FOREIGN KEY, PRIMARY KEY, UNIQUE, CHECK, REFERENCES

                  // Table_metaData(table_name,database_name,numberOfAttributes)
                  NestedDatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance.Put('kind','constraint');
                  DatabaseInstance.Put('database_name',workingSchema.dbName);
                  DatabaseInstance.Put('table_name',tblname);
                  DatabaseInstance.Put('constraint_name',constraintList[index1].constraintname);
                  DatabaseInstance.Put('constraint_kind',constraintList[index1].constraintkind); // constraint_kind: column / table
                  DatabaseInstance.Put('constraint_type',constraintList[index1].constraintType);


                  lcount := length(constraintList[index1].columnsName);
                  DatabaseInstance.Put('column_count',lcount);

                  for index2 := Low(constraintList[index1].columnsName) to high(constraintList[index1].columnsName) do
                    DatabaseInstance.Put('column_name'+intToStr(index2),constraintList[index1].columnsName[index2]);
                  lcount := length(constraintList[index1].checkInstructions);
                  if constraintList[index1].constraintType = 'CHECK' then
                    begin
                      DatabaseInstance.Put('checkinstruction_count',lcount);
                      for index2 := Low(constraintList[index1].checkInstructions) to high(constraintList[index1].checkInstructions) do
                        begin
                          DatabaseInstance.Put('mnemonic'+intToStr(index2),constraintList[index1].checkInstructions[index2].mnemonic);
                          DatabaseInstance.Put('boolvalue'+intToStr(index2),constraintList[index1].checkInstructions[index2].boolvalue);
                          DatabaseInstance.Put('value'+intToStr(index2),constraintList[index1].checkInstructions[index2].value);
                          DatabaseInstance.Put('stvalue'+intToStr(index2),constraintList[index1].checkInstructions[index2].stvalue);
                          DatabaseInstance.Put('printInstruction'+intToStr(index2),constraintList[index1].checkInstructions[index2].printInstruction);
                        end;
                    end;
                  if constraintList[index1].constraintType = 'REFERENCES' then
                    begin
                      DatabaseInstance.Put('references_table',constraintList[index1].refTable);
                      lcount := length(constraintList[index1].refcolumnsName);
                      DatabaseInstance.Put('refcolumn_count',lcount);
                      for index2 := Low(constraintList[index1].refcolumnsName) to high(constraintList[index1].refcolumnsName) do
                        DatabaseInstance.Put('refcolumn_name'+intToStr(index2),constraintList[index1].refcolumnsName[index2]);
                    end;
                  DatabaseInstance.Put('_id', TBSONObjectId.NewFrom);
                  DataDictionaryCollection.Insert(DatabaseInstance);
                end;
                {$ENDIF}

                {$IFDEF tablesdb}
                for index1 := Low(constraintList) to high(constraintList) do
                  begin

                    // Constraint_metaData(constraint_name,column_name,...,column_name,constraint_kind,constraint_type)
                    // column_name,...,column_name is ColumnsConstrain_metaData(column_name,table_name,database_name,constraint_name)
                    // constraint_kind: column / table
                    // constraint_type: PRIMARY KEY, UNIQUE, CHECK, REFERENCES - FOREIGN KEY
                    row := nil;
                    setLength(row, 6);
                    row[0] := constraintList[index1].constraintName;
                    row[1] := tblName;
                    row[2] := workingSchema.dbName;
                    row[3] := constraintList[index1].constraintkind;
                    // constraint_kind: column / table
                    with constraintList[index1] do
                      if (constraintType = 'NULL') then
                        row[4] := 0
                       else
                        if (constraintType = 'NOT NULL') then
                          row[4] := 1
                         else
                          if (constraintType = 'PRIMARY KEY') then
                            row[4] := 3
                           else
                            if (constraintType = 'UNIQUE') then
                              row[4] := 2
                             else
                              if (constraintType = 'CHECK') then
                                row[4] := 5
                               else
                                if (constraintType = 'REFERENCES') then
                                  row[4] := 4;
                    if constraintList[index1].constraintType = 'REFERENCES' then
                      row[5] := constraintList[index1].refTable
                     else row[5] := '';
                    DDconstraints.insertRow(row);

                    for index2 :=
                        Low(constraintList[index1].columnsName)
                        to high(constraintList[index1].columnsName) do
                      begin
                        row := nil;
                        setLength(row, 8);
                        row[0] := constraintList[index1].constraintName;
                        row[1] := constraintList[index1].columnsName[index2];
                        row[2] := false;
                        row[3] := false;
                        row[4] := 0;
                        row[5] := false;
                        row[6] := 0;
                        row[7] := '';
                        DDcolumnsconstraint.insertRow(row);
                      end;

                    if constraintList[index1].constraintType = 'REFERENCES' then
                      for index2 :=
                          Low(constraintList[index1].refcolumnsName)
                          to high(constraintList[index1].refcolumnsName) do
                        begin
                          row := nil;
                          setLength(row, 8);
                          row[0] := constraintList[index1].constraintName;
                          row[1] := constraintList[index1].refcolumnsName[index2];
                          row[2] := true;
                          row[3] := false;
                          row[4] := 0;
                          row[5] := false;
                          row[6] := 0;
                          row[7] := '';
                          DDcolumnsconstraint.insertRow(row);
                        end;

                    if constraintList[index1].constraintType = 'CHECK' then
                      for index2 :=
                          low(constraintList[index1].checkInstructions)
                          to high(constraintList[index1].checkInstructions) do
                        begin
                          row := nil;
                          setLength(row, 8);
                          row[0] := constraintList[index1].constraintName;
                          row[1] := constraintList[index1].checkInstructions[index2].printInstruction;
                          row[2] := false;
                          row[3] := true;
                          row[4] := constraintList[index1].checkInstructions[Index2].mnemonic;
                          row[5] := constraintList[index1].checkInstructions[Index2].boolvalue;
                          row[6] := constraintList[index1].checkInstructions[Index2].value;
                          row[7] :=constraintList[index1].checkInstructions[index2].stvalue;
                          DDcolumnsconstraint.insertRow(row);
                        end;
                end;
              {$ENDIF}

              (*** Loading into Working Schema ***)
              setLength(allowcolsNull, length(colsName));
              for index1 := low(allowcolsNull) to high(allowcolsNull) do
                allowcolsNull[index1] := True; // Null values are allowable by default

              for index3 := 0 to high(workingSchema.tables) do
                if workingSchema.tables[index3].tblName = tblName then break;
              with workingSchema.tables[index3] do
                begin
                  //numCols := numCols + length(columnList);
                  setLength(columns, length(columns) + numCols);
                  for index1 := low(columnList) to high(columnList) do
                    with columnList[index1] do
                      begin
                        index2 := index1 + length(columnList);
                        columns[index2].colname := columnName;
                        columns[index2].colDefaultValue := columnDefaultValue;
                        columns[index2].coltypescale.size := 0;
                        columns[index2].colHasAutoIncrement := hasColumnAutoIncrement;
                        columns[index2].colHasDefault := hasColumnDefault;
                        if convertType(columnTypeName) = 'INTEGER' then
                          columns[index2].coltype := intType;
                        if convertType(columnTypeName) = 'SMALLINT' then
                          columns[index2].coltype := smallintType;
                        if convertType(columnTypeName) = 'INT64' then
                          columns[index2].coltype := int64Type;
                        if (convertType(columnTypeName) = 'SINGLE') or
                           (convertType(columnTypeName) = 'DOUBLE') or
                           (convertType(columnTypeName) = 'EXTENDED') then
                          begin
                            columns[index2].coltype := extendedType;
                            columns[index2].coltypescale.precision := numTypeSize[0];
                            columns[index2].coltypescale.scale := numTypeSize[1];
                          end;
                        if convertType(columnTypeName) = 'CURRENCY' then
                          begin
                            columns[index2].coltype := currencyType;
                            columns[index2].coltypescale.precision := numTypeSize[0];
                            columns[index2].coltypescale.scale := numTypeSize[1];
                          end;
                        if convertType(columnTypeName) = 'TDATETIME' then
                          columns[index2].coltype := tdatetimeType;
                        if convertType(columnTypeName) = 'TDATE' then
                          columns[index2].coltype := tdateType;
                        if convertType(columnTypeName) = 'TTIME' then
                          columns[index2].coltype := ttimeType;
                        if convertType(columnTypeName) = 'BOOLEAN' then
                          columns[index2].coltype := booleanType;
                        if convertType(columnTypeName) = 'STRING' then
                          begin
                            columns[index2].coltype := stringType;
                            columns[index2].coltypescale.size := charTypeSize;
                          end;
                      end;

                  numCols := numCols + length(columnList);


                  for index := Low(constraintList) to high(constraintList) do
                    with constraintList[index] do
                      begin
                        // UNIQUE COULD BE NULL
                        // PRIMARY KEY IS NOT NULL COLUMN AND UNIQUE COLUMN
                        setLength(constraints, length(constraints) + 1);
                        with constraints[high(constraints)] do
                          begin
                            cnstrname := constraintName;
                            if (constraintType = 'NULL') then
                              begin
                                cnstrtype := 0;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if columnsName[index2] = columnName then
                                        begin
                                          nullCol := Index1;
                                          allowcolsNull[Index1] := True;
                                        end;
                              end;
                            if (constraintType = 'NOT NULL') then
                              with constraints[high(constraints)] do
                                begin
                                  cnstrtype := 1;
                                  for index2 := low(columnsName) to high(columnsName) do
                                    for index1 := Low(columnList) to high(columnList) do
                                      with columnList[index1] do
                                        if columnsName[index2] = columnName then
                                          begin
                                            nnullCol := Index1;
                                            allowcolsNull[Index1] := False;
                                          end;
                                end;
                            if (constraintType = 'UNIQUE') then
                              begin
                                cnstrtype := 2;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if columnsName[index2] = columnName then
                                        begin
                                          uqCols[0] := setbit(uqCols[0], Index1);
                                          allowcolsNull[Index1] := False;
                                        end;
                              end;
                            if (constraintType = 'PRIMARY KEY') then
                              begin
                                cnstrtype := 3;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if columnsName[index2] = columnName then
                                        begin
                                          pkCols[0] := setbit(pkCols[0], Index1);
                                          allowcolsNull[Index1] := False;
                                        end;
                              end;
                            if (constraintType = 'CHECK') then
                              begin
                                cnstrtype := 5;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if columnsName[index2] = columnName then
                                      begin
                                        ckCols[0] := setbit(ckCols[0], Index1);
                                        checkCondition := constraintList[index].checkInstructions;
                                      end;
                              end;
                            if (constraintType = 'REFERENCES') then
                              begin
                                cnstrtype := 4;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if columnsName[index2] = columnName then
                                        Cols[0] := setbit(Cols[0], Index1);
                                reftblName := refTable;
                                for index2 := low(columnsName) to high(columnsName) do
                                  for index1 := Low(columnList) to high(columnList) do
                                    with columnList[index1] do
                                      if refcolumnsName[index2] = columnName then
                                        refCols[0] := setbit(refCols[0], Index1);
                              end;
                          end;
                      end;
                end;

              {$IFDEF mongodb}
              NestedtblNameInstance := TBSONObject.Create;
              NestedtblNameInstance.Put('$eq',tblName);
              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','table');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('table_name',NestedtblNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              if DatabaseCursor.HasNext then
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  lcount := DataBaseInstance.Items['number_of_columns'].AsInteger;
                  lcount := lcount + Length(columnList);
                  DatabaseInstance.Put('number_of_columns',lcount); // Number Of Attributes
                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;
              {$ENDIF}

              {$IFDEF tablesdb}
              for RowId1 := DDTables.firstRow to DDTables.lastRow do
                begin
                  table_name := DDTables.getValueByColumnName(RowId1,'table_name');
                  if tblname = table_name then break;
                end;
              DDTables.returnRow(RowId1,Row);
              lcount := DDTables.getValueByColumnName(RowId1,'numberofattributes');
              lcount := lcount + Length(columnList);
              Row[2] := lcount;
              DDTables.putRow(RowId1,Row);
              {$ENDIF}

              setlength(inscols, length(columnList));
              for index1 := 0 to high(columnList) do
                begin
                  inscols[index1].name := columnList[index1].columnName;
                  if columnList[index1].columnTypeName = 'TIMESTAMP' then
                    inscols[index1].value := now else
                    if columnList[index1].hascolumnAutoIncrement then
                      inscols[index1].value := 1 else
                      if columnList[index1].hascolumnDefault then
                        with columnList[index1] do
                          inscols[index1].value :=
                            isCompatibleType(columnDefaultValue,columnTypeName,
                              dim1,dim2) else
                        inscols[index1].value := null
                end;

              {$IFDEF mongodb}
              ACursor := tblFields.Collection.Find();
              while ACursor.HasNext do
                begin
                  oldIBSONInstance := ACursor.Next;
                  IBSONInstance := oldIBSONInstance;
                  for index1 := low(columnList) to high(columnList) do
                    begin
                      oldIBSONInstance.Put(inscols[index1].name,inscols[index1].value);
                      tblFields.Collection.Update(oldIBSONInstance,IBSONInstance);

                      if columnList[index1].hascolumnAutoIncrement then
                        inscols[index1].value := inscols[index1].value + 1;
                    end;
                end;

              for index1 := low(columnList) to high(columnList) do
                begin
                  if columnList[index1].hascolumnAutoIncrement then
                    begin
                      NesteddbNameInstance := TBSONObject.Create;
                      NesteddbNameInstance.Put('$eq',dbname);

                      NestedtblNameInstance := TBSONObject.Create;
                      NestedtblNameInstance.Put('$eq',tblFields.tblName);
                      NestedcolNameInstance := TBSONObject.Create;
                      NestedcolNameInstance.Put('$eq',columnList[index1].columnName);

                      sequencesInstance := TBSONObject.Create;
                      sequencesInstance.Put('database_name',NesteddbNameInstance);
                      sequencesInstance.Put('table_name',NestedtblNameInstance);
                      sequencesInstance.Put('column_name',NestedcolNameInstance);
                      sequencesCursor := SequencesCollection.Find(sequencesInstance);

                      if sequencesCursor.HasNext then
                        begin
                          oldsequencesInstance := sequencesCursor.Next;
                          sequencesInstance := oldsequencesInstance;
                          seq_counter := inscols[index1].value;
                          sequencesInstance.Put('counter',seq_counter);
                          sequencesCollection.Update(oldsequencesInstance,sequencesInstance);
                        end;
                    end;
                end;
              {$ENDIF}

              {$IFDEF tablesdb}
                // same as above
              {$ENDIF}

              colsName := nil;
              yyacceptmessage('Alter table succeed');
              ladd_column := false

            end;
          if ldrop_column then
            begin
              // You cannot delete a column that has a CHECK constraint. You must first delete the constraint.
              // You cannot delete a column that has PRIMARY KEY or FOREIGN KEY constraints or other dependencies.
              // You must first remove all dependencies on the column.

              // check all the constraint to see which one involve the column
              // take away the constraint
              // take away values from the columns
              // take away the indexes on column

              ldrop_column := false;
            end;
          if lrename_table then
            begin
              // fromTables[0] old name
              // fromTables[1] new name
              for index3 := 0 to high(workingSchema.tables) do
                if workingSchema.tables[index3].tblName = fromTables[1].Name then
                  begin
                    yyerror('Table name ' +  fromTables[1].Name + ' already exists');
                    exit;
                  end;

              found := false;
              for index3 := 0 to high(workingSchema.tables) do
                if workingSchema.tables[index3].tblName = fromTables[0].Name then
                  begin
                    found := true;
                    break;
                  end;
              if not found then
                begin
                  yyerror('Table name ' + fromTables[0].Name + ' not exists');
                  exit;
                end;

              workingSchema.tables[index3].tblName := tblName;
              // No need to check for table name in constraints as cols are referenced by position in the table
              for index2 := 0 to high(workingSchema.tables[index3].constraints) do
                begin
                  case workingSchema.tables[index3].constraints[index2].cnstrtype of
                    4:
                      begin
                        if workingSchema.tables[index3].constraints[index2].reftblName = fromTables[0].Name then
                          workingSchema.tables[index3].constraints[index2].reftblName := tblName
                      end;
                    5:
                      begin
                        for index1 := 0 to high(workingSchema.tables[index3].constraints[index2].checkCondition) do
                          with workingSchema.tables[index3].constraints[index2].checkCondition[index1] do
                            if mnemonic = 151 then
                              begin
                                table_name := copy(stValue,1,pos('.',stValue)-1);
                                if table_name = fromTables[0].Name then
                                  begin
                                    column_name := copy(stValue,pos('.',stValue)+1,length(stValue));
                                    stValue := tblName + '.' + column_name
                                  end;
                              end;
                      end;
                  end
                end;

              {$IFDEF mongodb}
              NestedtblNameInstance := TBSONObject.Create;
              NestedtblNameInstance.Put('$eq',fromTables[0].Name);
              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','table');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('table_name',NestedtblNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              if DatabaseCursor.HasNext then
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  DatabaseInstance.Put('table_name',tblname);
                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;


              NestedtblNameInstance := TBSONObject.Create;
              NestedtblNameInstance.Put('$eq',fromTables[0].Name);
              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','table');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('table_name',NestedtblNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              if DatabaseCursor.HasNext then
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  DatabaseInstance.Put('table_name',tblname);
                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;

              NestedtblNameInstance := TBSONObject.Create;
              NestedtblNameInstance.Put('$eq',fromTables[0].Name);
              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','tablecolumn');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('table_name',NestedtblNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              while DatabaseCursor.HasNext do
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  DatabaseInstance.Put('table_name',tblname);
                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;


              NestedtblNameInstance := TBSONObject.Create;
              NestedtblNameInstance.Put('$eq',fromTables[0].Name);
              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','constraint');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('table_name',NestedtblNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              while DatabaseCursor.HasNext do
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  DatabaseInstance.Put('table_name',tblname);
                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;

              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','constraint');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              while DatabaseCursor.HasNext do
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  constraint_type := oldDatabaseInstance.Items['constraint_type'].AsString;
                  if constraint_type = 'REFERENCES' then
                    if fromTables[0].Name = oldDatabaseInstance.Items['references_table'].AsString then
                      begin
                        DatabaseInstance.Put('references_table',tblname);
                        DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                      end;
                  if constraint_type = 'CHECK' then
                    begin
                      lcount :=  oldDatabaseInstance.Items['checkinstruction_count'].AsInteger;
                      for index2 := 0 to lcount - 1 do
                        begin
                          if oldDatabaseInstance.Items['mnemonic'+IntToStr(index2)].AsInteger = 151 then
                            begin
                              table_name := copy(oldDatabaseInstance.Items['stvalue'+IntToStr(index2)].AsString,
                                                 1, pos('.',oldDatabaseInstance.Items['stvalue'+IntToStr(index2)].AsString)-1);
                              if table_name = fromTables[0].Name then
                                begin
                                  column_name := copy(oldDatabaseInstance.Items['stvalue'+IntToStr(index2)].AsString,
                                                      pos('.',oldDatabaseInstance.Items['stvalue'+IntToStr(index2)].AsString)+1,
                                                      length(oldDatabaseInstance.Items['stvalue'+IntToStr(index2)].AsString));
                                  DatabaseInstance.Put('stvalue'+IntToStr(index2),tblName + '.' + column_name);
                                  DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                                end;
                            end;
                        end;
                    end;
                end;


              NesteddbNameInstance := TBSONObject.Create;
              NesteddbNameInstance.Put('$eq',dbName);
              NestedDatabaseInstance := TBSONObject.Create;
              NestedDatabaseInstance.Put('$eq','index');
              DatabaseInstance := TBSONObject.Create;
              DatabaseInstance.Put('database_name',NesteddbNameInstance);
              DatabaseInstance.Put('kind',NestedDatabaseInstance);

              DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);

              while DatabaseCursor.HasNext do
                begin
                  oldDatabaseInstance := DatabaseCursor.Next;
                  DatabaseInstance := TBSONObject.Create;
                  DatabaseInstance := oldDatabaseInstance;
                  index_type := oldDatabaseInstance.Items['type'].AsString;
                  if index_type = 'mono' then
                    begin
                      if fromTables[0].Name = oldDatabaseInstance.Items['table_name'].AsString then
                        begin
                          DatabaseInstance.Put('table_name',tblname);
                        end
                    end else
                    if index_type = 'join' then
                      begin
                        lcount := oldDatabaseInstance.Items['tbasetables_count'].AsInteger;
                        for index1 := 0 to lcount - 1 do
                          begin
                            fromtblName := oldDatabaseInstance.Items['from_table'+IntToStr(index1)].AsString;
                            if fromTables[0].Name = fromtblName then
                              DatabaseInstance.Put('from_table'+IntToStr(index1),tblname);
                            totblName := oldDatabaseInstance.Items['to_table'+IntToStr(index1)].AsString;
                            if fromTables[0].Name = totblName then
                              DatabaseInstance.Put('to_table'+IntToStr(index1),tblname);
                          end
                      end;
                    DataDictionaryCollection.Update(oldDatabaseInstance,DatabaseInstance);
                end;

              // check the views and triggers
              {$ENDIF}

              {$IFDEF tablesdb}

              {$ENDIF}

              lrename_table := false;
            end;
          if lrename_column then
            begin
              // change all column_name
              // change constraints
              // change indexes

              if not TableExists(tblName) then
                begin
                  yyerror('Table ' + tblName + ' not found');
                  exit;
                end;

              found := false;
              tblFields := loadTableFields(tblName);
              for index2 := 0 to high(tblFields.columns) do
                if colsName[0] = tblFields.columns[index2].colname then
                  begin
                    found := true;
                    break;
                  end;
              if not found then
                begin
                  yyerror('column ' + colsName[0] + ' not exists');
                  exit;
                end;

              tblFields := loadTableFields(tblName);
              for index2 := 0 to high(tblFields.columns) do
                if colsName[1] = tblFields.columns[index2].colname then
                  begin
                    yyerror('column ' + colsName[1] + ' already exists');
                    exit;
                  end;





              lrename_column := false;
            end
        end;

        167: // ALTER TABLE Add Column
        begin
          ladd_Column := true;
        end;

        168: // ALTER TABLE  DROP COLUMN
        begin
          ldrop_Column := true;
        end;

        239: // ALTER TABLE RENAME TABLE
        begin
          lrename_table := true;
        end;

        155: // ALTER TABLE RENAME COLUMN
        begin

          lrename_column := true;

        end;

        195:  //UPLOAD CSV
        begin
          if not TableExists(tblName) then
            begin
              yyerror('table: ' + tblName + ' not found');
              Exit;
            end;

          // get the number of rows in table registered in Data Dictionary
          // scan till this number
          // run add join index

          {$IFDEF mongodb}
          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblName);

          NesteddbNameInstance.Put('$eq',dbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);

          found := False;
          dbCounter := 0;
          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance :=  DatabaseCursor.Next;
              RowsInserted := DatabaseInstance.Items['number_of_documents'].AsInteger;
            end;

          counter := 0;

          tblFields := loadTableFields(tblName);
          BaseCursor := tblFields.Collection.Find();
          while BaseCursor.HasNext do
            begin
              BaseIBSONInstance := BaseCursor.Next;
              counter := counter + 1;
              if counter <= RowsInserted then continue else
                begin

                  {
                  sqlUnit.Form1.Edit6.Text := inttostr(counter);
                  sqlUnit.Form1.Edit6.Repaint;
                  if IsConsole then
                    writeln(counter);
                  }

                  Row2 := nil;
                  for index1 := 0 to tblFields.numCols - 1 do
                    begin
                      setLength( Row2, length(Row2) + 1);
                      if BaseIBSONInstance.Items[tblFields.columns[index1].colname ].ValueType = bvtNull then
                          begin
                            stringFieldValue := 'Null';
                            Row2[high(Row2)] := stringFieldvalue;
                          end else

                      case tblFields.columns[index1].coltype of
                        intType, smallintType:
                        begin
                          intFieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsInteger;
                          Row2[high(Row2)] := intFieldValue;
                        end;
                        int64Type:
                        begin
                          int64FieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsInt64;
                          Row2[high(Row2)] := Int64FieldValue;
                        end;
                        extendedType, TDateTimeType, TDateType, TTimeType:
                        begin
                          extendedFieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsFloat;
                          Row2[high(Row2)] := ExtendedFieldValue;
                        end;
                        currencyType:
                        begin
                          currencyFieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsFloat;
                          Row2[high(Row2)] := CurrencyFieldValue;
                        end;
                        booleanType:
                        begin
                          booleanFieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsBoolean;
                          Row2[high(Row2)] := booleanFieldValue;
                        end;
                        stringType:
                        begin
                          stringFieldValue := BaseIBSONInstance.Items[tblFields.columns[index1].colname ].AsString;
                          Row2[high(Row2)] := stringFieldValue;
                        end;
                      end;

                    end;
                  if BaseIBSONInstance.HasOid then
                    stID := BaseIBSONInstance.GetOid.GetOID else
                    stID := BaseIBSONInstance.Items['_id'].AsString;
                  workingschema.joinidxdata[0].idxstorage.AddKey(tblFields.tblName, Row2, stID);
                end;
            end;
          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblName);
          NesteddbNameInstance.Put('$eq',dbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance :=  DatabaseCursor.Next;
              RowsInserted := DatabaseInstance.Items['number_of_documents'].AsInteger;
              RowsInserted := counter;
              DatabaseInstance.Put('number_of_documents',RowsInserted);
              DataDictionaryCollection.Update(DatabaseInstance,DatabaseInstance);

            end;

          //////put the number of rows
          yyacceptmessage('Updated Join Index');
          {$ENDIF}
        end;

        78: // VALUES
        begin
          valuesList := nil;
          SetLength(valuesList, Length(stk));
          for j := low(stk) to high(stk) do
            case stk[j].caseValue of
              0: valuesList[j] := stk[j].boolValue;
              4,8: valuesList[j] := stk[j].extValue;
              6: valuesList[j] := stk[j].strValue;
              7: valuesList[j] := null;
            end;
        end;

        { TODO : check constraints before insert }
        77: // INSERT INTO *************** just first case with values list treated  *********************
        begin
          if not TableExists(tblName) then
            begin
              yyerror('Table Name ' + tblName + ' not belong to database ' + dbName);
              exit;
            end;

          tblFields := loadTableFields(tblname);

          // colnames = nil means that the columns have been omited
          if colsName = nil then
            begin
              if length(ValuesList) <> tblFields.numCols then // unless there is autoincrement
                begin
                  yyerror('Number of Values differs from number of Columns');
                  exit;
                end;
              setlength(inscols, tblFields.numCols);
              for index1 := 0 to tblFields.numCols - 1 do
                begin
                  inscols[index1].name := tblFields.columns[index1].colname;
                  with tblFields.columns[index1] do
                    inscols[index1].value :=
                      isCompatibleType(valuesList[index1],colSQLtypename,
                        colTypeScale.precision,colTypeScale.scale)
                end
            end else
            begin
              if length(colsName) <> length(ValuesList) then
                begin
                  yyerror('Number listed in Values differs from number of Columns');
                  exit;
                end;
              for index2 := low(colsName) to high(colsName) do
                begin
                  found := False;
                  for index1 := 0 to tblFields.numCols - 1 do
                    if tblFields.columns[index1].colname = colsName[index2] then
                      begin
                        found := True;
                        break;
                      end;
                  if not found then
                    begin
                      yyerror('Column Name ' + colsName[index2] +
                        ' not found in the table ' + tblFields.tblName);
                      exit;
                    end;
                end;
              setlength(inscols, tblFields.numCols);
              for index1 := 0 to tblFields.numCols - 1 do
                begin
                  found := false;
                  for index2 := low(colsName) to high(colsName) do
                    begin
                      if tblFields.columns[index1].colname = colsName[index2] then
                        begin
                          found := True;
                          break;
                        end;
                    end;
                  inscols[index1].name := tblFields.columns[index1].colname;
                  if found then
                    with tblFields.columns[index1] do
                      inscols[index1].value :=
                        isCompatibleType(valuesList[index2],colSQLtypename,
                          colTypeScale.precision,colTypeScale.scale)
                   else
                    if tblFields.columns[index1].colHasDefault then
                      begin
                        inscols[index1].value := tblFields.columns[index1].colDefaultValue
                      end else
                      if tblFields.columns[index1].colhasAutoIncrement then
                        begin

                          {$IFDEF mongodb}
                          // make sure when you drop a table or alter the table, you delete the row from sequences table
                          // when you create an autoincrement field, create the sequence row
                          NesteddbNameInstance := TBSONObject.Create;
                          NesteddbNameInstance.Put('$eq',dbname);

                          NestedtblNameInstance := TBSONObject.Create;
                          NestedtblNameInstance.Put('$eq',tblFields.tblName);
                          NestedcolNameInstance := TBSONObject.Create;
                          NestedcolNameInstance.Put('$eq',tblFields.columns[index1].colname);
                          sequencesInstance := TBSONObject.Create;
                          sequencesInstance.Put('database_name',NesteddbNameInstance);
                          sequencesInstance.Put('table_name',NestedtblNameInstance);
                          sequencesInstance.Put('column_name',NestedcolNameInstance);
                          sequencesCursor := SequencesCollection.Find(sequencesInstance);

                          if sequencesCursor.HasNext then
                            begin
                              oldsequencesInstance := sequencesCursor.Next;
                              sequencesInstance := oldsequencesInstance;
                              seq_counter := oldsequencesInstance.Items['counter'].AsInt64;
                              inscols[index1].value := seq_counter;
                              seq_counter := seq_counter + 1;
                              sequencesInstance.Put('counter',seq_counter);
                              sequencesCollection.Update(oldsequencesInstance,sequencesInstance);
                            end;
                          {$ENDIF}

                          {$IFDEF tablesdb}
                          rowId1 := DDtablecolumns.firstRow;
                          repeat
                            if DDtablecolumns.existRow(rowId1) then
                              begin
                                if DDtablecolumns.getValueByColumnName(rowId1, 'database_name') = dbName then
                                  begin
                                    table_name := DDtablecolumns.getValueByColumnName(rowId1, 'table_name');
                                    if table_name = tblFields.tblName then
                                      begin
                                        column_name := DDtablecolumns.getValueByColumnName(rowId1, 'column_name');
                                        if column_name = tblFields.columns[index1].colname then
                                          begin
                                            row := nil;
                                            setlength(row,16);
                                            DDTableColumns.returnRow(rowId1,row);
                                            seq_counter := DDtablecolumns.getValueByColumnName(rowId1, 'sequencecounter');
                                            inscols[index1].value := seq_counter;
                                            seq_counter := seq_counter + 1;
                                            row[15] := seq_counter;
                                            DDTableColumns.putRow(rowId1,row);
                                            break
                                          end
                                      end
                                  end
                              end;
                            rowId1 := rowId1 + 1;
                          until rowId1 > DDtablecolumns.lastRow;
                          {$ENDIF}
                        end else
                        if tblFields.columns[index1].colSQLtypename = 'TIMESTAMP' then
                          begin
                            inscols[index1].value := double(now);
                          end else
                          begin
                            nnullFieldValue := False;
                            (* check for constraints come later
                               If you specify no default value for a column, the default is NULL
                                 unless you place a NOT NULL constraint on the column. In this case,
                                 no default exists.
                            *)
                              inscols[index1].value := null
                          end;
               end;

              {$IFDEF tablesdb}
              row := nil;
              for index1 := 0 to tblFields.numCols - 1 do
                begin
                  setlength(row,length(row)+1);
                  row[High(row)] := inscols[index1].value;
                end;
              {$ENDIF}

              for index2 := 0 to length(tblFields.constraints) - 1 do
                begin
                  case tblFields.constraints[index2].cnstrtype of
                    1: // Not Null constraint
                      begin
                        if (inscols[tblFields.constraints[index2].nnullcol].value = null) then
                          begin
                            yyerror('Column ' + tblFields.columns[tblFields.constraints[index2].nnullcol].colname + ' not allow null values');
                            Exit;
                          end;
                      end;
                    2: // when unique key is declared, automatically the system create an index for the keys
                      begin
                        // check by the index if the value exists
                      end;
                    3: // when primary key is declared, automatically the system create an index for the keys
                      begin
                        // check by the index if the value exists
                        for index1 := 0 to tblFields.numCols - 1 do
                          if isbitset(tblFields.constraints[index2].pkCols[0], index1) then
                            if inscols[index1].value = null then
                              begin
                                yyerror('Column ' + tblFields.columns[index1].colname + ' not allow null values');
                                Exit;
                              end;
                      end;

                    5: //Check constraint
                      begin
                        resultTable.ownertable := nil;
                        for index1 := 0 to tblFields.numCols - 1 do
                          begin
                            setlength(resultTable.ownertable,length(resultTable.ownertable)+1);
                            resultTable.ownertable[index1].tblname := tblName;
                            resultTable.ownertable[index1].colname := tblFields.columns[index1].colname;
                            resultTable.ownertable[index1].aliasname := nil;
                          end;
                        resultTable.resultFields := tblFields;

                        {$IFDEF mongodb}
                        resultTable.resultIBSONInstance := TBSONObject.Create;
                        for index := 0 to tblFields.numCols - 1 do
                          begin
                            case tblFields.columns[index].coltype of
                              inttype, smallinttype:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                              int64type:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                              extendedtype, tdatetimetype, TDateType, TTimeType:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                              currencytype:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                              booleantype:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                              stringtype:
                                begin
                                  resultTable.resultIBSONInstance.Put( tblFields.tblName + tableColumnSeperator +
                                    tblFields.columns[index].colname, inscols[index].value );
                                end;
                            end;
                          end;
                        {$ENDIF}

                        {$IFDEF tablesdb}
                        with resultTable do
                          begin
                            resultRow := nil;
                            for index1 := 0 to tblFields.numCols - 1 do
                              begin
                                setlength(resultRow,length(resultRow)+1);
                                resultRow[High(resultRow)] := inscols[index1].value;
                              end;
                          end;
                        {$ENDIF}

                        if not rowcondition(tblFields.constraints[index2].checkCondition ,resulttable) then
                          begin
                            yyerror('Check condition not satisfied');
                            Exit;
                          end
                      end;
                  end;
                end;
            end;

          {$IFDEF mongodb}
          IBSONInstance :=  TBSONObject.Create;
          for index1 := 0 to tblFields.numCols - 1 do
            begin
              IBSONInstance.Put(inscols[index1].name, inscols[index1].Value);
            end;
          IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
          tblFields.Collection.Insert(IBSONInstance);
          {$ENDIF}

          {$IFDEF tablesdb}
          tblFields.storage.insertRow(row);
          {$ENDIF}


          //// search for indexes and fill them

          for index1 := low(tblFields.idxdata) to high(tblFields.idxdata) do
            begin
              keys := nil;
              for index2 := low(tblFields.idxdata[index1].idxkeys) to high(tblFields.idxdata[index1].idxkeys) do
              begin
                for index3 := 0 to tblFields.numCols - 1 do
                  if (tblFields.idxdata[index1].idxkeys[index2].colName =
                    tblFields.columns[index3].colname) then
                    break;
                setlength(keys, length(keys) + 1);
                if (row[index3] = null) then
                begin
                  keys[high(Keys)] := 'NULL';
                end
                else
                keys[high(Keys)] := Row[index3];
              end;
              rowId := {intToStr(}tblFields.storage.lastRow{)};

              tblFields.idxdata[index1].idxstorage.AddKey(keys,{strToint(}rowId{)});
            end;


          {$IFDEF mongodb}
          NestedtblNameInstance := TBSONObject.Create;
          NesteddbNameInstance := TBSONObject.Create;
          NestedDatabaseInstance := TBSONObject.Create;
          DatabaseInstance := TBSONObject.Create;
          NestedtblNameInstance.Put('$eq',tblName);
          NesteddbNameInstance.Put('$eq',dbname);
          NestedDatabaseInstance.Put('$eq','table');
          DatabaseInstance.Put('kind',NestedDatabaseInstance);
          DatabaseInstance.Put('database_name',NesteddbNameInstance);
          DatabaseInstance.Put('table_name',NestedtblNameInstance);

          DatabaseCursor := DataDictionaryCollection.Find(DatabaseInstance);
          if DatabaseCursor.HasNext then
            begin
              DatabaseInstance :=  DatabaseCursor.Next;
              RowsInserted := DatabaseInstance.Items['number_of_documents'].AsInteger;
              RowsInserted := RowsInserted + 1;
              DatabaseInstance.Put('number_of_documents',RowsInserted);
              DataDictionaryCollection.Update(DatabaseInstance,DatabaseInstance);

            end;
          {$ENDIF}



          yyacceptmessage('Row inserted ');
        end;

        40: // FROM ALIAS NAME
        begin
          aliasname := stk[High(stk)].strValue;
          setlength(fromTables[fromindex].aliasname, length(
          fromTables[fromindex].aliasname) + 1);
          fromTables[fromindex].aliasname[high(
            fromTables[fromindex].aliasname)] := aliasname;
          setLength(stk, Length(stk) - 1);
        end;

        178: // SELECT ALIAS NAME
        begin
          setLength(Expr,length(Expr) + 1);
          Expr[high(Expr)].mnemonic := 179;
          Expr[high(Expr)].value := 0;
          Expr[high(Expr)].stvalue := stk[High(stk)].strValue;
          Expr[high(Expr)].printInstruction := 'EXPRESSION ALIAS';
          setLength(stk, Length(stk) - 1);
        end;

        87: // FROM CLAUSE
        begin
          // fromTables structure has tables and eventual aliasname
          // check the tables are in the database and an alias name is not a name of a table
          // check the column names

          for index1 := low(fromTables) to high(fromTables) do
          begin
            found := False;
            for index2 :=
              low(workingSchema.tables) to high(workingSchema.tables) do
            begin
              for aliasindex :=
                low(fromTables[index1].aliasname)
                to high(fromTables[index1].aliasname) do
                if fromTables[index1].aliasname[aliasindex] <> '' then
                  if fromTables[index1].aliasname[aliasindex] = workingSchema.tables[index2].tblName then
                  begin
                    yyerror(
                      'an alias name found as a name for a table in the database ');
                    exit;
                  end;
              if fromTables[index1].Name = workingSchema.tables[index2].tblName then
                found := True;
            end;
            if not found then
            begin
              yyerror('Table not found in database ');
              exit;
            end;
          end;
          fromTableslen := length(fromTables);
          tblName := '';
          foundTable := False;
          // This is to know if any table found after form table is done for the columns
        end;

        37: // ALL COLUMNS
        begin
          // selectColsInstructions := nil;
          for index1 := 0 to fromTableslen - 1 do
          begin
            tblFields := loadTableFields(fromTables[index1].Name);
            with workingSchema.tables[index1] do
              for index2 := 0 to tblFields.numCols - 1 do
              begin
                setLength(expr, length(expr) + 1);
                expr[high(expr)].mnemonic := 151;
                expr[high(expr)].Value := 0.0;
                expr[high(expr)].stvalue :=
                  tblFields.tblName + '.' + tblFields.columns[index2].colname;
                expr[high(expr)].printInstruction := 'PUSH COLUMN NAME';
                setlength(selectColsInstructions, length(selectColsInstructions) + 1);
                setlength(selectColsInstructions[high(selectColsInstructions)],
                  length(expr) + 1);
                for index := low(expr) to high(expr) do
                  selectColsInstructions[high(selectColsInstructions), index] :=
                    expr[index];
                expr := nil;
                with selectColsInstructions[high(selectColsInstructions), index + 1] do
                  begin
                    mnemonic := 177;
                    Value := 0;
                    stvalue := '';
                    printInstruction := 'Show Column';
                  end;
              end;
          end;
        end;

        38: // ALL TABLE COLUMNS
        begin
          // check the table is in the from clause as tablename or aliasname
          // get from dictionary all columns from the table
          found := False;
          for index1 := 0 to fromTableslen - 1 do
          begin
            if (tblName = fromTables[index1].Name) then
            begin
              found := True;
              tblFields := loadTableFields(fromTables[index1].Name);
              break;
            end;
            for aliasindex :=
              low(fromTables[index1].aliasname) to high(fromTables[index1].aliasname) do
              if (tblName = fromTables[index1].aliasname[index]) then
              begin
                found := True;
                tblFields := loadTableFields(fromTables[index1].Name);
                break;
              end;
          end;
          if not found then
          begin
            yyerror('The table not found in database ');
            exit;
          end;
          for index2 := 0 to tblFields.numCols - 1 do
            begin
              setLength(expr, length(expr) + 1);
              expr[high(expr)].mnemonic := 151;
              expr[high(expr)].Value := 0.0;
              expr[high(expr)].stvalue :=
                tblFields.tblName + '.' + tblFields.columns[index2].colname;
              expr[high(expr)].printInstruction := 'PUSH COLUMN NAME';
              setlength(selectColsInstructions, length(selectColsInstructions) + 1);
              setlength(selectColsInstructions[high(selectColsInstructions)],
                length(expr) + 1);
              for index := low(expr) to high(expr) do
                selectColsInstructions[high(selectColsInstructions), index] :=
                  expr[index];
              expr := nil;
              with selectColsInstructions[high(selectColsInstructions), index + 1] do
                begin
                  mnemonic := 177;
                  Value := 0;
                  stvalue := '';
                  printInstruction := 'Show Column';
                end;
            end;
        end;

        39: // COLUMN WITIHIN EXPRESSION
        begin
          // check the table is in the from clause as tablename or aliasname
          if flagAllColumnsAggregate then
            begin
              flagAllColumnsAggregate := false;
              setlength(selectColsInstructions, length(selectColsInstructions) + 1);
              setlength(selectColsInstructions[high(selectColsInstructions)],
                length(expr) + 1);
              for index := low(expr) to high(expr) do
                selectColsInstructions[high(selectColsInstructions), index] :=
                  expr[index];
              if expr = nil then index := -1;
              with selectColsInstructions[high(selectColsInstructions), index + 1] do
                begin
                  mnemonic := 177;
                  Value := 0;
                  stvalue := '';
                  printInstruction := 'Show Column';
                end;
              expr := nil;
              {
              for index1 := 0 to fromTableslen - 1 do
                begin
                  tblFields := loadTableFields(fromTables[index1].Name);
                  with workingSchema.tables[index1] do
                    for index2 := 0 to tblFields.numCols - 1 do
                    begin
                      setLength(expr, length(expr) + 1);
                      expr[high(expr)].mnemonic := 151;
                      expr[high(expr)].Value := 0.0;
                      expr[high(expr)].stvalue :=
                        tblFields.tblName + '.' + tblFields.columns[index2].colname;
                      expr[high(expr)].printInstruction := 'PUSH COLUMN NAME';
                      setlength(selectColsInstructions, length(
                        selectColsInstructions) + 1);
                      setlength(selectColsInstructions[high(selectColsInstructions)],
                        length(expr) + 1);
                      for index := low(expr) to high(expr) do
                        selectColsInstructions[high(selectColsInstructions), index] :=
                          expr[index];
                      expr := nil;
                      with selectColsInstructions[high(selectColsInstructions),
                          index + 1] do
                      begin
                        mnemonic := 177;
                        Value := 0;
                        stvalue := '';
                        printInstruction := 'Show Column';
                      end;
                    end;
                end;
             }
            end else
            begin
              for index := low(expr) to high(expr) do
                begin
                  if (expr[index].mnemonic = 151) then
                    begin
                      found := False;
                      if foundTable then
                        begin
                          for index1 := 0 to fromTableslen - 1 do
                            begin
                              if (tblName = fromTables[index1].Name) then
                                begin
                                  found := True;
                                  tblFields := loadTableFields(fromTables[index1].Name);
                                  break;
                                end;
                              for aliasindex := low(fromTables[index1].aliasname)
                                to high(fromTables[index1].aliasname) do
                                  if (tblName = fromTables[index1].aliasname[index]) then
                                    begin
                                      tblFields :=
                                        loadTableFields(fromTables[index1].Name);
                                      found := True;
                                      break;
                                    end;
                            end;

                          if not found then
                          begin
                            yyerror('the table/alias: ' + tblName + ' not found in database');
                            exit;
                          end;
                          found := False;
                          for index2 := 0 to tblFields.numCols - 1 do
                            if (copy(expr[index].stvalue,
                              pos('.', expr[index].stvalue) + 1, length(expr[index].stvalue)) =
                              tblFields.columns[index2].colname) then
                            begin
                              found := True;
                              break;
                            end;
                          if not found then
                          begin
                            yyerror('the column: '+ copy(expr[index].stvalue,
                              pos('.', expr[index].stvalue) + 1, length(expr[index].stvalue)) +' doesn''t belong to the table: ' +
                              tblFields.tblName);
                            exit;
                          end;
                        end
                       else
                        begin
                          for index1 := 0 to fromTableslen - 1 do
                            begin
                              tblFields := loadTableFields(fromTables[index1].Name);
                              for index2 := 0 to tblFields.numCols - 1 do
                                if (
                                  copy(expr[index].stvalue, pos('.', expr[index].stvalue) + 1,
                                  length(expr[index].stvalue)) =
                                  tblFields.columns[index2].colname) then
                                begin
                                  found := True;
                                  break;
                                end;
                              if found then
                                break;
                            end;
                          if not found then
                            begin
                              yyerror('the column doesn''t belong to the table ');
                              exit;
                            end;
                        end;
                      expr[index].stvalue :=
                        tblFields.tblName + '.' + tblFields.columns[index2].colname;
                    end;
                end;
                setlength(selectColsInstructions, length(selectColsInstructions) + 1);
                setlength(selectColsInstructions[high(selectColsInstructions)],
                  length(expr) + 1);
                for index := low(expr) to high(expr) do
                  selectColsInstructions[high(selectColsInstructions), index] :=
                    expr[index];
                if expr = nil then index := -1;
                with selectColsInstructions[high(selectColsInstructions), index + 1] do
                  begin

                    mnemonic := 177;
                    Value := 0;
                    stvalue := '';
                    printInstruction := 'Show Column';


                  end;
                {
                setlength(selectColsInstructions[high(selectColsInstructions)],
                  length(expr) + 2);
                with selectColsInstructions[high(selectColsInstructions), index + 2] do
                  begin

                    mnemonic := 179;
                    Value := 0;
                    if aliasName <> '' then
                    stvalue := aliasName else stValue := tblName;
                    printInstruction := 'EXPRESSION ALIAS';




                  end;

                 }




                if flagExpressionAggregate then
                  flagExpressionAggregate := false;
                expr := nil;
            end
        end;

        150: // COLUMNS PROJECTION
        begin
          // End of columns clause: selectColsInstructions has a stack to process the columns
          aggregateColsInstructions := selectColsInstructions;
          if flagAggregate then
            begin
              selectColsInstructions := nil;
              for index1 := 0 to fromTableslen - 1 do
              begin
                expr := nil;
                tblFields := loadTableFields(fromTables[index1].Name);
                with workingSchema.tables[index1] do
                  for index2 := 0 to tblFields.numCols - 1 do
                  begin
                    setLength(expr, length(expr) + 1);
                    expr[high(expr)].mnemonic := 151;
                    expr[high(expr)].Value := 0.0;
                    expr[high(expr)].stvalue :=
                      tblFields.tblName + '.' + tblFields.columns[index2].colname;
                    expr[high(expr)].printInstruction := 'PUSH COLUMN NAME';
                    setlength(selectColsInstructions, length(
                      selectColsInstructions) + 1);
                    setlength(selectColsInstructions[high(selectColsInstructions)],
                      length(expr) + 1);
                    for index := low(expr) to high(expr) do
                      selectColsInstructions[high(selectColsInstructions), index] :=
                        expr[index];
                    expr := nil;
                    with selectColsInstructions[high(selectColsInstructions),
                        index + 1] do
                    begin
                      mnemonic := 177;
                      Value := 0;
                      stvalue := '';
                      printInstruction := 'Show Column';
                    end;
                  end;
              end;
            end;
          colsName := nil;
          expr := nil;
          tblname := '';
        end;

        41: // where
        begin
          // save them as instructions in the stack
          setlength(conditionInstructions, length(expr));
          for index := low(expr) to high(expr) do
            conditionInstructions[index] := expr[index];
          expr := nil;
        end;

        184: // SHOW HEADER SELECT STATEMENT
        begin

          for fromIndex := low(fromTables) to high(fromTables) do
            fromTables[fromIndex].fromFields :=
              loadTableFields(fromTables[fromIndex].Name);

          resulttable.resultFields.numCols := 0;
          resulttable.resultFields.columns := nil;
          resulttable.ownerTable := nil;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            resulttable.resultFields.numCols :=
              resulttable.resultFields.numCols +
              fromTables[fromIndex].fromFields.numCols;
            setLength(resulttable.resultFields.columns, length(
              resulttable.resultFields.columns) +
              length(fromTables[fromindex].fromFields.Columns));
            setLength(resulttable.ownerTable, length(resulttable.ownerTable) +
              length(fromTables[fromindex].fromFields.Columns));
          end;

          fromlengthLimit := 0;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            if fromindex > 0 then
              fromlengthLimit :=
                fromlengthLimit + length(
                fromTables[fromindex - 1].fromFields.columns);
            for resultindex :=
              0 to length(fromTables[fromIndex].fromFields.columns) - 1 do
            begin
              resulttable.resultFields.columns[resultindex + fromlengthlimit] :=
                fromTables[fromindex].fromFields.columns[resultindex];
              resulttable.ownertable[resultindex + fromlengthlimit].colname :=
                fromTables[fromIndex].fromFields.columns[resultindex].colname;
              resulttable.ownertable[resultindex + fromlengthlimit].tblname :=
                fromTables[fromIndex].Name;

              setlength(resulttable.ownertable[resultindex +
                fromlengthlimit].aliasname, length(fromTables[fromIndex].aliasname));
              for index2 := low(fromTables[fromIndex].aliasname) to high(fromTables[fromIndex].aliasname) do
                resulttable.ownertable[resultindex + fromlengthlimit].aliasname[index2] :=
                  fromTables[fromIndex].aliasname[index2];
            end;
          end;

          colsname := nil;
          fromlengthLimit := 0;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            setlength(colsname, length(colsname) +
              fromTables[fromindex].fromFields.numCols);
            if fromindex > 0 then
              fromlengthLimit :=
                fromlengthLimit + length(
                fromTables[fromindex - 1].fromFields.columns);
            for resultindex :=
              0 to length(fromTables[fromindex].fromFields.columns) - 1 do
              colsname[resultindex + fromlengthlimit] :=
                fromTables[fromindex].fromFields.columns[resultindex].colname;
          end;

          rescolname  := '';
  //        rescolname := rescolname + 'sys_queryId,sys_user,rowId,';
          rescolname := rescolname + 'rowId,';
          sqlResults := nil;
          setLength(sqlResults, length(sqlResults) + 1);
          sqlResults[High(sqlResults)] := '{ "query" : [   ';
          setLength(sqlResults, length(sqlResults) + 1);
          sqlresults[High(SQLResults)] := '{';

          if selectColsInstructions <> nil then
            begin
              for index2 := low(selectColsInstructions) to high(selectColsInstructions) do
              begin
                isexpr := false;
                exprname := 'Expr';
                for index3 := low(selectColsInstructions[index2]) to high(selectColsInstructions[index2]) do
                begin
                  if (selectColsInstructions[index2,index3].mnemonic <> 151) and
                     (selectColsInstructions[index2,index3].mnemonic <> 177) then isExpr := true;
                  case selectColsInstructions[index2,index3].mnemonic of

                    151: // COLUMN NAME
                       begin
                         tblName := copy(selectColsInstructions[index2,index3].stvalue,1,pos('.',selectColsInstructions[index2,index3].stvalue)-1);
                         colName := copy(selectColsInstructions[index2,index3].stvalue,pos('.',selectColsInstructions[index2,index3].stvalue)+1,length(selectColsInstructions[index2,index3].stvalue));
                         found := false;
                         for index1 := low(resulttable.ownertable) to high(resulttable.ownertable) do
                           begin
                             if (resulttable.ownertable[index1].tblname = tblName) and
                                (resulttable.ownertable[index1].colname = colName) then
                               break;
                             for aliasindex := low(resulttable.ownertable[index1].aliasname) to high(resulttable.ownertable[index1].aliasname) do
                               begin
                                 if (resulttable.ownertable[index1].aliasname[aliasindex] = tblName) and
                                    (resulttable.ownertable[index1].colname = colName) then found := true;
                                 if found then break;
                               end;
                           end;
                         end;

                     177: // SHOW COLUMN
                        begin
                          if not isexpr then
                            with resulttable.ownertable[index1] do
                              exprname := tblname + tableColumnSeperator + colname;
                            //sqlresults[High(SQLResults)] := sqlresults[High(SQLResults)] + '"COLUMN NAME": '+ ' ' + ':' + '"' + exprname + '",  ';
                            rescolname := rescolname + exprname +  ',';
                        end;

                     179:
                        begin
                          exprname := selectColsInstructions[index2,index3].stValue;
                        end;

                  end;
                end;
              end;
            end;

          rescolname := copy(rescolname,1,length(rescolname) -1);
          sqlResults[High(sqlResults)] :=
            copy(sqlResults[High(sqlResults)], 1,
            length(sqlResults[High(sqlResults)]) - 2);
          setLength(sqlResults, length(sqlResults) + 1);
          sqlResults[High(sqlResults)] := sqlResults[High(sqlResults)] + '  ] }';
        end;

        34: // SELECT STATEMENT
        begin

          if optionCreateViewCommand then continue;
          for fromIndex := low(fromTables) to high(fromTables) do
            fromTables[fromIndex].fromFields :=
              loadTableFields(fromTables[fromIndex].Name);

          resulttable.resultFields.numCols := 0;
          resulttable.resultFields.columns := nil;
          resulttable.ownerTable := nil;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            resulttable.resultFields.numCols :=
              resulttable.resultFields.numCols +
              fromTables[fromIndex].fromFields.numCols;
            setLength(resulttable.resultFields.columns, length(
              resulttable.resultFields.columns) +
              length(fromTables[fromindex].fromFields.Columns));
            setLength(resulttable.ownerTable, length(resulttable.ownerTable) +
              length(fromTables[fromindex].fromFields.Columns));
          end;

          fromlengthLimit := 0;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            if fromindex > 0 then
              fromlengthLimit := fromlengthLimit +
                length(fromTables[fromindex - 1].fromFields.columns);
            for resultindex :=
              0 to length(fromTables[fromIndex].fromFields.columns) - 1 do
            begin
              resulttable.resultFields.columns[resultindex + fromlengthlimit] :=
                fromTables[fromindex].fromFields.columns[resultindex];
              resulttable.ownertable[resultindex + fromlengthlimit].colname :=
                fromTables[fromIndex].fromFields.columns[resultindex].colname;
              resulttable.ownertable[resultindex + fromlengthlimit].tblname :=
                fromTables[fromIndex].Name;

              setlength(resulttable.ownertable[resultindex +
                fromlengthlimit].aliasname, length(fromTables[fromIndex].aliasname));
              for index2 := low(fromTables[fromIndex].aliasname) to high(fromTables[fromIndex].aliasname) do
                resulttable.ownertable[resultindex + fromlengthlimit].aliasname[index2] :=
                  fromTables[fromIndex].aliasname[index2];
            end;
          end;

          {$IFDEF tablesdb}
          resulttable.resultRow := nil;
          {$ENDIF}
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            setLength(fromTables[fromindex].fromRow,
              fromTables[fromindex].fromFields.numCols);
            {$IFDEF tablesdb}
            setLength(resulttable.resultRow, length(resulttable.resultRow) +
              fromTables[fromindex].fromFields.numCols);
            {$ENDIF}
          end;




          // ****** EXECUTE PLAN ******
          //
          // if any of the column condition on the where has an index
          // load the table with the index
          // exprList shold be uploaded before joinFlag
          // aliases make fromTbleslen <> 1
          // check in exprList for tables involved


          // look for index on the columns
          executeplan.useIndex := false;
          executeplan.Index := nil;
          executeplan.joinFlag := false;

          if conditionInstructions = nil then
            begin
              // read sequentially the data
              executeplan.useIndex := false;
              if flagOrderClause then
                begin
                  // read sequentially by ordering
                end
            end else
            begin
              // check the column names that are used
              // get the list of expressions, every expression hold an equality ... between column name and a value ...
              // check for an existing index on the column
              exprList := nil;
              setLength(exprList,length(exprList) + 1);
              exprList[high(exprList)] := nil;
              for index := low(conditionInstructions) to high(conditionInstructions) do
                begin
                  // save all instructions till find 50..55 and in the future between, like and In
                  // for 50..55 there is check for index and not on 42..44 later should be done
                  if not (conditionInstructions[index].mnemonic in [42..44]) then // 'NOT', 'OR', 'AND'
                    begin
                      setlength(exprList[high(exprList)],length(exprList[high(exprList)])+1);
                      exprList[high(exprList),high(exprList[high(exprList)])] := conditionInstructions[index];
                    end;
                  if conditionInstructions[index].mnemonic in [50..55] then // 'EQ', 'LT', 'GT', 'NE', 'LE', 'GE'
                    setLength(exprList,length(exprList) + 1);
                  // check the exprList for join
                end;
              setLength(exprList,length(exprList) - 1);
              executePlan.joinFlag := fromTableslen <> 1;
              if not executePlan.joinFlag then
                begin
                  // go through exprlist and get one by one to see the index
                  // Order By translated into find().sort

                  for index5 := 0 to length(exprList) - 1 do
                    begin
                      for index6 := 0 to length(exprList[index5])  - 1 do
                        begin
                          if exprList[index5,index6].mnemonic = 151 then
                            begin

                              // check the indexes to see if there is one on the column
                              with fromtables[0].fromFields do
                                for index2 := low(idxData) to high(idxData) do
                                  begin
                                    with idxData[index2] do
                                      for index3 := low(idxkeys) to high(idxkeys) do
                                        if tblFields.tblName + '.' + idxkeys[index3].colName = exprList[index5,index6].stvalue then
                                          begin
                                            executePlan.useIndex := true;
                                            setlength(executeplan.Index,length(executeplan.Index)+1);
                                            executeplan.Index[high(executePlan.Index)].Name := idxData[index2].idxname;
                                            executeplan.Index[high(executePlan.Index)].Number := index2;
                                            executeplan.Index[high(executePlan.Index)].colName := idxkeys[index3].colName;
                                          end;
                                  end;
                              break;
                            end;
                        end;
                      //executeplan.useIndex := false;
                      if executeplan.useIndex then
                        begin
                          runstk := nil;
                          for index6 := low(exprList[index5]) + 1 to high(exprList[index5]) - 1 do
                            runStack(exprList[index5,index6],runstk);
                          case runstk[high(runstk)].caseValue of
                            0: executeplan.Index[high(executePlan.Index)].Value := runstk[high(runstk)].boolValue;
                            4: executeplan.Index[high(executePlan.Index)].Value := runstk[high(runstk)].extValue;
                            6: executeplan.Index[high(executePlan.Index)].Value := runstk[high(runstk)].strValue;
                          end;
                          executeplan.Index[high(executePlan.Index)].mnemonic := exprList[index5,high(exprList[index5])].mnemonic;
                        end;
                     { TODO : check if the type of the column math the value in runstk }
                    end;
                end else
                begin
                  TheBaseTables := nil;
                  if workingSchema.joinidxdata <> nil then
                    for index1 := 0 to length(workingSchema.joinidxdata) - 1 do
                      begin
                        for index2 := 0 to length(workingSchema.joinidxdata[index1].joincouples) - 1 do
                          begin
                            found := false;
                            for index3 := 0 to length(TheBaseTables) - 1 do
                              begin
                                if workingSchema.joinidxdata[index1].joincouples[index2].fromTable = TheBaseTables[index3] then
                                  begin
                                    found := true;
                                    break;
                                  end;
                              end;
                            if not found then
                              begin
                                setlength(TheBaseTables, length(TheBaseTables) + 1);
                                TheBaseTables [High(TheBaseTables)] :=
                                  workingSchema.joinidxdata[index1].joincouples[index2].fromTable;
                              end;
                            found := false;
                            if workingSchema.joinidxdata[index1].joincouples[index2].toTable = TheBaseTables[index3] then
                              begin
                                found := true;
                                break;
                              end;
                            if not found then
                              begin
                                setlength(TheBaseTables, length(TheBaseTables) + 1);
                                TheBaseTables [High(TheBaseTables)] :=
                                  workingSchema.joinidxdata[index1].joincouples[index2].toTable;
                              end;
                            // Take the common keys between them
                          end;
                        for index2 := 0 to length(workingSchema.joinidxdata[index1].idxkeys) - 1 do
                          begin
                            // here to find the key in the index to take care
                          end;
                        // if the base tables are the same as in where condition
                        whTheBaseTables := nil;
                        for index := low( conditionInstructions) to high( conditionInstructions) do
                          begin
                            if conditionInstructions[index].mnemonic = 151 then
                              begin
                                // take the column
                                st := copy(conditionInstructions[index].stvalue,1,pos('.',conditionInstructions[index].stvalue)-1);
                                found := false;
                                for index4 := 0 to length(whThebaseTables) - 1 do
                                  if whTheBaseTables[index4] = st then found := true;
                                if not found then
                                  begin
                                    setlength(whTheBaseTables,length(whTheBaseTables)+1);
                                    whTheBaseTables[high(whTheBaseTables)] := st
                                  end;

                              end;

                            // length of base tables are equal
                            // every table name exist

                            // check for the common keys
                            // if common keys are the same and the base tables are the same in the join in the where condition
                            // it's the same jon problem
                            {
                               Executeplan.joinFlag := true;
                               Executeplan.useIndex := true;
                               Executeplan.Index.Number := 0; // momenterly the index on the joinidxdata
                             }
                          end;
                        found := length(whThebaseTables) = length(ThebaseTables);
                        for index2 := 0 to length(whThebaseTables) - 1 do
                          begin
                            if found then
                              begin
                                found := false;
                                for index3 := 0 to length(ThebaseTables) - 1 do
                                  begin
                                    if TheBaseTables[index3] = whThebaseTables[index2] then
                                      begin
                                        found := true;
                                      end;
                                  end
                              end
                          end;

                        if found then
                          begin
                            Executeplan.joinFlag := true;
                            Executeplan.useIndex := true;
                            setlength(Executeplan.Index,length(Executeplan.Index)+1);
                            Executeplan.Index[High(Executeplan.Index)].Number := index1;
                            Executeplan.Index[High(Executeplan.Index)].Name:= workingSchema.joinidxdata[index1].idxname;
                          end;
                      end;

                end;
            end;

          for index := 0 to fromTableslen - 1 do
            begin
              //setLength(executePlan,Length(executePlan)+1);

              //executePlan[high(executePlan)] := 'use table: ' + fromTables[index].name;
            end;

          // ****** EXECUTE PLAN ******

          ResultRows := 0;
          {$IFDEF mongodb}
          resultTable.resultIBSONInstance := TBSONObject.Create;
          {$ENDIF}

          executePlan.useIndex := false;

          setlength(keys,1);
          setlength(dataRef,1);
          setlength(InheritedKeys,1);
          flag := false; //momentarily
          if analyzeQuery(conditionInstructions, idxstorage) then
            begin
              flag := true;
              idxStorage.ClearKey;
              repeat
                idxStorage.NextKey(Keys,InheritedKeys,DataRef);
                if DataRef[0] <> nullDataValue then
                  begin
                    rowId1 := DataRef[0];
                    if not fromTables[0].fromFields.storage.existRow(rowId1) then
                      continue;

                    fromTables[0].fromFields.storage.returnRow(
                      rowId1, fromTables[0].fromrow);
                    for resultindex := 0 to length(fromTables[0].fromrow) - 1 do
                      begin
                        resulttable.resultRow[resultindex] :=
                          fromTables[0].fromrow[resultindex];
                      end;
                    if rowcondition(conditionInstructions,resultTable) then
                      begin
                        resultRows := resultRows + 1;
                        extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                      end
                  end;
              until dataref[0] = nullDataValue;
              idxstorage.Free;
            end;


          if not flag then //momentarily
          // departments employees job_history jobs locations countries
          if executePlan.useIndex then // momenteraly
            begin
              if not executePlan.joinFlag then
                begin
                  {$IFDEF mongodb}
                  IBSONInstance := TBSONObject.Create;
                  IBSONInstance2 := TBSONObject.Create;
                  for index := 0 to length(executeplan.Index) - 1 do
                    begin
                      varFieldvalue := executeplan.Index[index].Value;
                      case executeplan.Index[index].mnemonic of
                        50: stcondition := '$eq';
                        51: stcondition := '$lt';
                        52: stcondition := '$gt';
                        53: stcondition := '$ne';
                        54: stcondition := '$lte';
                        55: stcondition := '$gte';
                      end;
                      IBSONInstance2.Put(stcondition,varFieldValue);
                      IBSONInstance.Put(executeplan.Index[index].colName,IBSONInstance2);
                    end;

                  fromTables[0].Cursor := tblFields.Collection.Find(IBSONInstance);

                  while fromTables[0].Cursor.HasNext do
                    begin
                      fromTables[0].fromIBSONInstance := fromTables[0].Cursor.Next;

                      for index := 0 to fromTables[0].fromFields.numCols - 1 do
                        begin
                          setlength( colNames, length(colNames) + 1 );
                          colNames[index] := fromTables[0].fromFields.columns[index].colname;
                          setlength( colTypes, length(colTypes) + 1 );
                          colTypes[index] := fromTables[0].fromFields.columns[index].coltype;
                          case fromTables[0].fromFields.columns[index].coltype of
                            inttype, smallinttype:
                              begin
                                IntFieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsInteger;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, IntFieldValue );
                              end;
                            int64type:
                              begin
                                Int64FieldValue := fromTables[0].fromIBSONInstance.Items[tblFields.columns[index].colname].AsInt64;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, Int64FieldValue );
                              end;
                            extendedtype, tdatetimetype, TDateType, TTimeType:
                              begin
                                extendedFieldValue := fromTables[0].fromIBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, extendedFieldValue );
                              end;
                            currencytype:
                              begin
                                extendedFieldValue := fromTables[0].fromIBSONInstance.Items[tblFields.columns[index].colname].AsFloat;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, extendedFieldValue );
                              end;
                            booleantype:
                              begin
                                booleanFieldValue := fromTables[0].fromIBSONInstance.Items[tblFields.columns[index].colname].AsBoolean;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, booleanFieldValue );
                              end;
                            stringtype:
                              begin
                                stringFieldValue := fromTables[0].fromIBSONInstance.Items[tblFields.columns[index].colname].AsString;
                                resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                  fromTables[0].fromFields.columns[index].colname, stringFieldValue );
                              end;
                          end;
                        end;
                      if rowcondition(conditionInstructions,resultTable) then
                        begin
                          resultRows := ResultRows + 1;
                          extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection,flagAggregate,resultRows);
                        end;
                    end;
                  {$ENDIF}
                  {$IFDEF tablesdb}
                  if executeplan.Index[index].mnemonic = 50 then // eq
                    begin
                      if (fromTables[0].fromFields.idxdata[0].idxname =  executeplan.Index[index].Name) and
                         (fromTables[0].fromFields.idxdata[0].idxkeys[0].colName = executeplan.Index[index].colName) then
                        begin
                          // Momentarely
                          setlength(keys,1);
                          setlength(dataRef,1);
                          Keys[0] := conditionInstructions[1].stvalue;
                          // Momentarely

                          // A better way for the delete to not have to use nextkey
                          fromTables[0].fromFields.idxdata[0].idxstorage.FindKey(Keys,DataRef[0]);
                          if DataRef[0] <> nullDataValue then
                             begin
                               flag := true;
                               repeat

                                 rowId1 := {StrToInt(}DataRef[0]{)};
                                 while not(fromTables[0].fromFields.storage.existRow(rowId1)) do
                                   begin
                                     fromTables[0].fromFields.idxdata[0].idxstorage.nextKey(Keys,DataRef[0]);
                                     rowId1 := {StrToInt(}DataRef[0]{)};
                                     if keys[0] <> conditionInstructions[1].stvalue then
                                       begin
                                         flag := false;
                                         break;
                                       end;
                                   end;

                                 rowId1 := {StrToInt(}DataRef[0]{)};
                                 if (fromTables[0].fromFields.storage.existRow(rowId1)) and flag then
                                   begin
                                     fromTables[0].fromFields.storage.returnRow(
                                       rowId1, fromTables[0].fromrow);
                                     for resultindex := 0 to length(fromTables[0].fromrow) - 1 do
                                       begin
                                         resulttable.resultRow[resultindex] :=
                                           fromTables[0].fromrow[resultindex];
                                       end;
                                     if rowcondition(conditionInstructions,resultTable) then
                                       begin
                                         resultRows := resultRows + 1;
                                         extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                                       end
                                   end;


                                 fromTables[0].fromFields.idxdata[0].idxstorage.nextKey(Keys,DataRef[0]);

                               until keys[0] <> conditionInstructions[1].stvalue;
                             end;
                        end;
                    end;

                  {$ENDIF}
                end
              else
                begin
                  {$IFDEF mongodb}
                  // get all the data and put them
                  // BTree_Collection := workingSchema.joinidxdata[Executeplan.Index[0].Number].idxstorage.Collection;

                  resultTable.resultIBSONInstance := TBSONObject.create;
                  setlength(Keys,length(theBaseTables));
                  setlength(keys,2);
                  /////BTree_Cursor := BTree_Collection.Find();
                  TheBaseTables := workingSchema.joinidxdata[Executeplan.Index[0].Number].idxstorage.BaseTables;
                  setlength(dataref,length(theBaseTables));
                  setlength(Maxdataref,length(theBaseTables));
                  workingSchema.joinidxdata[Executeplan.Index[0].Number].idxstorage.MaxKey(Keys,MaxDataRef);
                  // if MaxDataRef[0] = '' then exit;
                  workingSchema.joinidxdata[Executeplan.Index[0].Number].idxstorage.ClearKey;

                  repeat
                    workingSchema.joinidxdata[Executeplan.Index[0].Number].idxstorage.NextKey(keys,dataref);
                    found := true;
                    for index1 := 0 to length(dataref) - 1 do
                      found := found and (MaxDataRef[index1] = DataRef[index1]);

                    for index := 0 to length(TheBaseTables) - 1 do
                      begin

                        for index5 := 0 to length(TheBaseTables) - 1 do
                          if fromTables[index5].Name = TheBaseTables[index] then
                            begin
                              break
                            end;

                        fromTables[index5].fromFields := LoadTableFields(TheBaseTables[index5]);

                       stringFieldValue := Dataref[index5];

                        //lvalueType := BTREE_IBSONInstance.Items['DATA_REFERENCE' + intToStr(index5)].ValueType;
                        if true{lvalueType = bvtstring} then
                            begin
                              //stringFieldValue := BTREE_IBSONInstance.Items['DATA_REFERENCE' + intToStr(index5)].AsString;

                              fromTables[index5].fromIBSONInstance := TBSONObject.Create;

                              {InnerIBSONInstance := TBSONObject.create;
                              InnerIBSONInstance.Put('$eq',stringFieldValue);}

                              fromTables[index5].fromIBSONInstance.Put('_id',stringFieldValue);

                              nextfound := false;
                              fromTables[index5].Cursor :=
                                fromTables[index5].fromFields.Collection.Find(fromTables[index5].fromIBSONInstance);
                              if fromTables[index5].Cursor.hasNext then
                                begin
                                  nextfound := true;

                                fromTables[index5].fromIBSONInstance := fromTables[index5].Cursor.Next;

                              for index3 := 0 to fromTables[index5].fromFields.numCols - 1 do
                                begin
                                  lvalueType := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].ValueType;

                                  case lvaluetype of
                                    bvtNull:
                                      begin
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, null );
                                      end;
                                    bvtBoolean:
                                      begin
                                        booleanFieldValue := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].AsBoolean;
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, BooleanFieldValue );
                                      end;
                                    bvtInteger:
                                      begin
                                        IntFieldValue := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].AsInteger;
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, IntFieldValue );
                                      end;
                                    bvtInt64:
                                      begin
                                        Int64FieldValue := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].AsInt64;
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, Int64FieldValue );
                                      end;
                                    bvtDouble:
                                      begin
                                        extendedFieldValue := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].AsFloat;
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, extendedFieldValue );
                                      end;
                                    bvtString:
                                      begin
                                        stringFieldValue := fromTables[index5].fromIBSONInstance.Items[fromTables[index5].fromFields.columns[index3].colname].AsString;
                                        resultTable.resultIBSONInstance.Put( fromTables[index5].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index5].fromFields.columns[index3].colname, stringFieldValue );
                                      end;
                                  end;
                                end;

                                end;
                            end;
                      end;
                    if nextfound then
                    if rowcondition(conditionInstructions,resultTable) then
                    begin
                      resultRows := ResultRows + 1;
                      extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection, flagAggregate, resultRows);
                    end;

                  until found;
                  {$ENDIF}
                end

            end
           else
            case fromTablesLen of
              1:
              begin
                {$IFDEF mongodb}
                fromTables[0].fromIBSONInstance :=  TBSONObject.Create;
                fromTables[0].Cursor := fromTables[0].fromFields.Collection.Find;
                if flagOrderClause then
                  begin
                    SortIBSONInstance := TBSONObject.Create;
                    for index1 := 0 to length(ordercolumnList) - 1 do
                      if ordercolumnList[index1].colorder then
                        SortIBSONInstance.Put(ordercolumnList[index1].colname,1) else
                        SortIBSONInstance.Put(ordercolumnList[index1].colname,-1);
                    fromTables[0].Cursor.Sort(SortIBSONInstance);
                end;
                while fromTables[0].Cursor.HasNext do
                  begin
                    fromTables[0].fromIBSONInstance := fromTables[0].Cursor.Next;
                    for index := 0 to fromTables[0].fromFields.numCols - 1 do
                      begin
                        lvalueType := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].ValueType;
                        case lvaluetype of
                          bvtNull:
                            begin
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, NULL );
                            end;
                          bvtBoolean:
                            begin
                              booleanFieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsBoolean;
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, BooleanFieldValue );
                            end;
                          bvtInteger:
                            begin
                              IntFieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsInteger;
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, IntFieldValue );
                            end;
                          bvtInt64:
                            begin
                              Int64FieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsInt64;
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, Int64FieldValue );
                            end;
                          bvtDouble:
                            begin
                              extendedFieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsFloat;
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, extendedFieldValue );
                            end;
                          bvtString:
                            begin
                              stringFieldValue := fromTables[0].fromIBSONInstance.Items[fromTables[0].fromFields.columns[index].colname].AsString;
                              resultTable.resultIBSONInstance.Put( fromTables[0].fromFields.tblName + tableColumnSeperator +
                                                                   fromTables[0].fromFields.columns[index].colname, stringFieldValue );
                            end;
                        end;
                      end;
                    if rowcondition(conditionInstructions,resultTable) then
                    begin
                      resultRows := ResultRows + 1;
                      extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection, flagAggregate, resultRows);
                      end;
                  end;
                {$ENDIF}

                {$IFDEF tablesdb}



                if flagOrderClause then
                  begin
                    // try it for one column now ascending
                    // for index1 := 0 to length(ordercolumnList) - 1 do
                    index1 := 0;
                    if (fromTables[0].fromFields.idxdata[0].idxkeys[0].colName = ordercolumnList[index1].colname) and
                       (fromTables[0].fromFields.idxdata[0].idxkeys[0].colorder = ordercolumnList[index1].colorder) then
                      begin
                        // use the index to find the table
                        // to check well
                        setlength(keys,1);
                        setlength(dataRef,1);
                        setlength(MaxdataRef,1);
                        // to check well

                        fromTables[0].fromFields.idxdata[0].idxstorage.MaxKey(Keys,MaxDataRef[0]);
                        fromTables[0].fromFields.idxdata[0].idxstorage.ClearKey;
                        if MaxDataRef[0] <> nullDataValue then
                          repeat
                            fromTables[0].fromFields.idxdata[0].idxstorage.NextKey(keys,DataRef[0]);
                            rowId1 := {StrToInt(}DataRef[0]{)};
                            if fromTables[0].fromFields.storage.existRow(rowId1) then
                              begin
                                fromTables[0].fromFields.storage.returnRow(
                                  rowId1, fromTables[0].fromrow);
                                for resultindex := 0 to length(fromTables[0].fromrow) - 1 do
                                  begin
                                    resulttable.resultRow[resultindex] :=
                                      fromTables[0].fromrow[resultindex];
                                  end;
                                if rowcondition(conditionInstructions,resultTable) then
                                  begin
                                    resultRows := resultRows + 1;
                                    extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                                  end
                              end ;
                            found := DataRef[0] = MaxDataRef[0];
                          until found;


                      end

                  end else
                  begin
                    for rowId1 :=
                       fromTables[0].fromFields.storage.firstRow to
                       fromTables[0].fromFields.storage.LastRow do
                      begin
                        if fromTables[0].fromFields.storage.existRow(rowId1) then
                          begin
                            fromTables[0].fromFields.storage.returnRow(
                              rowId1, fromTables[0].fromrow);
                            for resultindex := 0 to length(fromTables[0].fromrow) - 1 do
                              begin
                                resulttable.resultRow[resultindex] :=
                                  fromTables[0].fromrow[resultindex];
                              end;
                            if rowcondition(conditionInstructions,resultTable) then
                              begin
                                resultRows := resultRows + 1;
                                extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                              end
                          end
                      end
                  end;
                {$ENDIF}

              end;

              2..254:
              begin
                {$IFDEF mongodb}
                resultRows := 0;
                for index := 0 to fromTablesLen - 1 do
                  begin
                    fromTables[Index].fromIBSONInstance :=  TBSONObject.Create;
                    fromTables[Index].Cursor := fromTables[Index].fromFields.Collection.Find;

                    if flagOrderClause then
                      begin
                        SortIBSONInstance := TBSONObject.Create;
                        for index1 := 0 to length(ordercolumnList) - 1 do
                          if ordercolumnList[index1].tblName = fromTables[Index].Name then
                            if ordercolumnList[index1].colorder then
                              SortIBSONInstance.Put(ordercolumnList[index1].colname,1) else
                              SortIBSONInstance.Put(ordercolumnList[index1].colname,-1);
                        fromTables[index].Cursor.Sort(SortIBSONInstance);
                      end;

                    RowIndex[Index] := 0;
                    if fromTables[index].Cursor.HasNext then
                      fromTables[index].fromIBSONInstance := fromTables[index].Cursor.Next
                  end;

                flagdone := false;
                repeat
                  flag := false;
                  currentTableIndex := FromTablesLen - 1;
                  while not flag do
                    begin
                      repeat
                        if RowIndex[currenttableindex] >= fromTables[currenttableIndex].Cursor.Count then
                          begin
                            if currenttableindex = 0 then
                              begin
                                flag := true;
                                flagdone := true;
                                break;
                              end;
                            RowIndex[CurrentTableIndex] := 0;
                            fromTables[CurrentTableIndex].Cursor := fromTables[CurrentTableIndex].fromFields.Collection.Find;

                            if flagOrderClause then
                              begin
                                SortIBSONInstance := TBSONObject.Create;
                                for index1 := 0 to length(ordercolumnList) - 1 do
                                  if ordercolumnList[index1].tblName = fromTables[CurrentTableIndex].Name then
                                    if ordercolumnList[index1].colorder then
                                      SortIBSONInstance.Put(ordercolumnList[index1].colname,1) else
                                      SortIBSONInstance.Put(ordercolumnList[index1].colname,-1);
                                fromTables[CurrentTableIndex].Cursor.Sort(SortIBSONInstance);
                              end;

                            if fromTables[currentTableIndex].Cursor.HasNext then
                              fromTables[currentTableIndex].fromIBSONInstance := fromTables[currentTableIndex].Cursor.Next;

                            CurrentTableIndex := CurrentTableIndex - 1;
                            RowIndex[currentTableIndex] := RowIndex[currentTableIndex] + 1;

                            if fromTables[currentTableIndex].Cursor.HasNext then
                              fromTables[currentTableIndex].fromIBSONInstance := fromTables[currentTableIndex].Cursor.Next;

                          end else flag := true;
                      until flag or flagdone;
                      if not flagdone then
                        begin
                          for index1 := 0 to fromtableslen -1 do
                            begin

                              for index := 0 to fromTables[index1].fromFields.numCols - 1 do
                                begin

                                  lvalueType := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].ValueType;
                                  case lvaluetype of
                                    bvtNull:
                                      begin
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, null );
                                      end;
                                    bvtBoolean:
                                      begin
                                        booleanFieldValue := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].AsBoolean;
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, BooleanFieldValue );
                                      end;
                                    bvtInteger:
                                      begin
                                        IntFieldValue := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].AsInteger;
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, IntFieldValue );
                                      end;
                                    bvtInt64:
                                      begin
                                        Int64FieldValue := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].AsInt64;
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, Int64FieldValue );
                                      end;
                                    bvtDouble:
                                      begin
                                        extendedFieldValue := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].AsFloat;
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, extendedFieldValue );
                                      end;
                                    bvtString:
                                      begin
                                        stringFieldValue := fromTables[index1].fromIBSONInstance.Items[fromTables[index1].fromFields.columns[index].colname].AsString;
                                        resultTable.resultIBSONInstance.Put( fromTables[index1].fromFields.tblName + tableColumnSeperator +
                                                                             fromTables[index1].fromFields.columns[index].colname, stringFieldValue );
                                      end;
                                  end;
                                end;
                            end;
                          if rowcondition(conditionInstructions,resultTable) then
                            begin
                              resultRows := ResultRows + 1;
                              extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection, flagAggregate, resultRows);
                            end;
                          rowindex[fromTablesLen - 1] := rowindex[fromTablesLen - 1] + 1;
                          if fromTables[fromTablesLen - 1].Cursor.HasNext then
                            begin
                              fromTables[fromTablesLen - 1].fromIBSONInstance := fromTables[fromTablesLen - 1].Cursor.Next;
                            end

                        end;
                    end;


                  until flagdone;

                  {$ENDIF}
              end;
            end;

           {
           SortIBSONInstance := TBSONObject.Create;
           //SortIBSONInstance.Put('t_a', -1);
           SortIBSONInstance.Put('$natural', -1);
           distinctCursor := distinctCollection.find();
           distinctCursor.Sort(SortIBSONInstance);
           while distinctCursor.HasNext do
             begin
               distinctIBSONInstance := distinctCursor.Next;
               st := distinctIBSONInstance.Items['t_a'].AsString;
             end;
           }

           {$IFDEF mongodb}
           GroupBy_Instance := TBSONObject.create;
           oldGroupBy_Instance := TBSONObject.create;

           if flagAggregate then
             begin
               conditionstk := nil;
               // if GroupByColumns <> nil create the collection below
               GroupBY_Collection :=  GDB.GetCollection('sys_group_by');
               if GroupBy_Collection.Count <> 0 then
                 GroupBy_Collection.drop();



               flagAggregate := false;
               AggregateCursor := aggregateCollection.Find();
               while AggregateCursor.HasNext do
                 begin
                   GroupBy_Instance := TBSONObject.create;
                   oldGroupBy_Instance := TBSONObject.create;
                   AggregateInstance := AggregateCursor.Next;
                   aggregateRowId := AggregateInstance.Items['rowId'].AsInteger;
                   AggregateCol := 0; // length(GroupByColumns);
                   for index1 := low(aggregateColsInstructions) to high(aggregateColsInstructions) do
                     for index2 := low(aggregateColsInstructions[index1]) to high(aggregateColsInstructions[index1]) do
                       begin
                         if flagAggregate then
                           aggregateConditionInstruction := aggregateColsInstructions[index1,index2];
                         case aggregateColsInstructions[index1,index2].mnemonic of
                           112..116:
                              begin
                                grpkind := aggregateset(aggregateColsInstructions[index1,index2].Mnemonic - 111);
                                aggregatestk := nil;
                                flagAggregate := true;
                             end;

                           151:
                             begin
                               tblName := copy(aggregateColsInstructions[index1,index2].stValue,1,pos('.',aggregateColsInstructions[index1,index2].stValue)-1);
                               colName := copy(aggregateColsInstructions[index1,index2].stValue,pos('.',aggregateColsInstructions[index1,index2].stValue)+1,length(aggregateColsInstructions[index1,index2].stValue));
                               st := AggregateInstance.Items[tblName + tableColumnSeperator + colName].AsString;
                               tblFields := loadTableFields(tblName);
                               for index3 := 0 to tblFields.numCols - 1 do
                                 if tblFields.columns[index3].colname = colname then
                                   begin
                                     colType := tblFields.columns[index3].coltype;
                                     if flagAggregate then     //// check for the boolean type. DateTime should be implemented as push date to put the right case value
                                       if colType = stringType then
                                         begin
                                           AggregateConditionInstruction.mnemonic := 89;
                                           AggregateConditionInstruction.value := 0;
                                           AggregateConditionInstruction.stvalue := st;
                                           AggregateConditionInstruction.printInstruction := 'PUSH LITERAL ' + st;
                                         end else
                                         begin
                                           AggregateConditionInstruction.mnemonic := 88;
                                           AggregateConditionInstruction.value := strtoFloatDef(st,0);
                                           AggregateConditionInstruction.stvalue := '';
                                           AggregateConditionInstruction.printInstruction := 'PUSH  ' + st;
                                         end
                                      else
                                       begin
                                         if GroupByColumns <> nil then
                                           begin
                                             aggregateColsInstructions[index1,index2].mnemonic := 129;
                                             aggregateColsInstructions[index1,index2].value := 0;
                                             aggregateColsInstructions[index1,index2].stvalue := tblName + tableColumnSeperator + colName;
                                             aggregateColsInstructions[index1,index2].printInstruction := 'COLUMN NAME';
                                           end else
                                           if colType = stringType then
                                             begin
                                               aggregateColsInstructions[index1,index2].mnemonic := 89;
                                               aggregateColsInstructions[index1,index2].value := 0;
                                               aggregateColsInstructions[index1,index2].stvalue := st;
                                               aggregateColsInstructions[index1,index2].printInstruction := 'PUSH LITERAL ' + st;
                                             end else
                                             begin
                                               aggregateColsInstructions[index1,index2].mnemonic := 88;
                                               aggregateColsInstructions[index1,index2].value := strtoFloatDef(st,0);
                                               aggregateColsInstructions[index1,index2].stvalue := '';
                                               aggregateColsInstructions[index1,index2].printInstruction := 'PUSH  ' + st;
                                             end;
                                           if length(aggregateColsInstructions[index1]) = 2 then
                                             begin
                                               setlength(aggregateColsInstructions[index1],length(aggregateColsInstructions[index1])+1);
                                               aggregateColsInstructions[index1,high(aggregateColsInstructions[index1])] :=
                                                 aggregateColsInstructions[index1,high(aggregateColsInstructions[index1])-1];
                                               with aggregateColsInstructions[index1,high(aggregateColsInstructions[index1])-1] do
                                                 begin
                                                   mnemonic := 179;
                                                   value := 0;
                                                   stvalue := tblName + tableColumnSeperator + colName;
                                                   printInstruction := 'EXPRESSION ALIAS';
                                                 end
                                             end;
                                       end;
                                     break;
                                   end;
                               if flagAggregate then
                                 runstack(aggregateConditionInstruction,aggregatestk)
                             end;

                           180, 181:
                             begin
                               if aggregateConditionInstruction.mnemonic = 180 then
                                 if aggregateConditionInstruction.value <> 113 then
                                   begin
                                     yyError('syntax error');
                                     exit;
                                   end;
                               ConditionInstruction.mnemonic := 88;
                               ConditionInstruction.stvalue := '';
                               case grpKind of
                                 countKind:
                                   begin
                                     ConditionInstruction.value := 1;
                                     ConditionInstruction.printInstruction := 'PUSH 1';
                                   end;
                                 sumKind:
                                   begin
                                     ConditionInstruction.value := aggregatestk[high(aggregatestk)].extvalue;
                                     ConditionInstruction.printInstruction := 'PUSH ' + floatToStr(ConditionInstruction.value);
                                   end;
                               end;

                               runstack(ConditionInstruction,conditionstk);

                               if GroupByColumns <> nil then
                                 begin
                                   GroupByInstance := TBSONObject.create;
                                   for index := 0 to length(GroupByColumns) - 1 do
                                     begin
                                       NestedGroupByInstance := TBSONObject.create;
                                       st := AggregateInstance.Items[tblName + tableColumnSeperator + GroupByColumns[index]].AsString;
                                       NestedGroupByInstance.Put('$eq',st);
                                       GroupByInstance.Put(tblName + tableColumnSeperator + GroupByColumns[index],NestedGroupByInstance);
                                     end;
                                   if AggregateCol = 0 then
                                      begin
                                        GroupByCursor := aggregateCollection.Find(GroupByInstance);
                                        if GroupByCursor.HasNext then
                                          begin
                                            GroupByInstance :=  GroupByCursor.Next;
                                            GroupByRowId := GroupByInstance.Items['rowId'].AsInteger;
                                          end;

                                        if GroupByRowId = AggregateRowId then
                                          begin
                                            aggregateColumns := nil;
                                          end else
                                          begin
                                            GroupBy_Instance := TBSONObject.create;
                                            for index := 0 to length(GroupByColumns) - 1 do
                                              begin
                                                st := AggregateInstance.Items[tblName + tableColumnSeperator + GroupByColumns[index]].AsString;
                                                NestedGroupByInstance := TBSONObject.create;
                                                NestedGroupByInstance.Put('$eq',st);
                                                GroupBy_Instance.Put(tblName + tableColumnSeperator + GroupByColumns[index],NestedGroupByInstance);
                                              end;
                                            GroupBy_Cursor := GroupBy_Collection.Find(GroupBy_Instance);
                                            if GroupBy_Cursor.HasNext then
                                              begin
                                                GroupBy_Instance :=  GroupBy_Cursor.Next;
                                                for index := 0 to GroupBy_instance.count - 1 - length(GroupByColumns) - 1  do
                                                  if GroupBy_Instance.Contain('c'+intToStr(index)) then
                                                    aggregateColumns[index] := GroupBy_Instance.Items['c'+intToStr(index)].AsString
                                                   else break
                                              end;
                                          end;

                                      end;
                                 end;

                               AggregateValue := 0;
                               if AggregateCol > length(aggregateColumns) - 1 then
                                 setlength(aggregateColumns,length(aggregateColumns)+1) else
                                 AggregateValue := StrToInt(aggregateColumns[aggregateCol]);
                               AggregateValue :=  AggregateValue + conditionStk[High(conditionStk)].extValue;
                               aggregateColumns[aggregateCol] := floatToStr(aggregateValue);
                               aggregateCol := AggregateCol + 1;

                               conditionstk := nil;

                               aggregatestk := nil;
                               flagAggregate := False;
                             end;


                           182:
                             begin
                               //find the one with the same value for the column
                               NestedDistinctAggregateInstance := TBSONObject.create;
                               DistinctAggregateInstance := TBSONObject.create;
                               NestedDistinctAggregateInstance.Put('$eq',st);
                               DistinctAggregateInstance.Put(tblName + tableColumnSeperator + colName,NestedDistinctAggregateInstance);
                               DistinctAggregateCursor := aggregateCollection.Find(DistinctAggregateInstance);
                               if DistinctAggregateCursor.HasNext then
                                 begin
                                   DistinctAggregateInstance :=  DistinctAggregateCursor.Next;
                                   DistinctAggregateRowId := DistinctAggregateInstance.Items['rowId'].AsInteger;
                                   if DistinctAggregateRowId = AggregateRowId then
                                     begin
                                       ConditionInstruction.mnemonic := 88;
                                       ConditionInstruction.stvalue := '';
                                       case grpKind of
                                         countKind:
                                           begin
                                             ConditionInstruction.value := 1;
                                             ConditionInstruction.printInstruction := 'PUSH 1';
                                           end;
                                         sumKind:
                                           begin
                                             ConditionInstruction.value := aggregatestk[high(aggregatestk)].extvalue;
                                             ConditionInstruction.printInstruction := 'PUSH ' + floatToStr(ConditionInstruction.value);
                                           end;
                                       end;
                                       runstack(ConditionInstruction,conditionstk);
                                       AggregateValue := 0;
                                       if AggregateCol > length(aggregateColumns) - 1 then
                                         setlength(aggregateColumns,length(aggregateColumns)+1) else
                                         AggregateValue := StrToInt(aggregateColumns[aggregateCol]);
                                       AggregateValue :=  AggregateValue + conditionStk[High(conditionStk)].extValue;
                                       aggregateColumns[aggregateCol] := floatToStr(aggregateValue);
                                     end;
                                 end;
                               aggregateCol := AggregateCol + 1;
                               conditionstk := nil;
                               aggregatestk := nil;
                               flagAggregate := False;
                             end
                           else
                            if flagAggregate then
                              runstack(aggregateConditionInstruction,aggregatestk)
                          end;
                       end;

                   if GroupByColumns <> nil then
                     if (GroupByRowId = AggregateRowId) then
                       begin
                         GroupBy_Instance := AggregateInstance;
                         for index := 0 to length(aggregateColumns) - 1 do
                           begin
                             GroupBy_Instance.Put('c'+intToStr(index),aggregateColumns[index]);
                           end;
                         GroupBy_Collection.Insert(GroupBy_Instance);

                       end
                      else
                       begin
                         oldGroupBy_Instance := GroupBy_Instance;
                         for index := 0 to length(aggregateColumns) - 1 do
                           begin
                             GroupBy_Instance.Put('c'+intToStr(index),aggregateColumns[index]);
                           end;
                         GroupBy_Collection.Update(oldGroupBy_Instance,GroupBy_Instance);
                       end
                 end;

               if GroupByColumns <> nil then
                 begin
                   GroupBy_Cursor := GroupBy_Collection.Find();
                   resultrows := 0;
                   while GroupBy_Cursor.HasNext do
                     begin
                       GroupBy_Instance :=  GroupBy_Cursor.Next;
                       for index := 0 to GroupBy_instance.count - 1 - length(GroupByColumns) - 1  do
                         if GroupBy_Instance.Contain('c'+intToStr(index)) then
                           aggregateColumns[index] := GroupBy_Instance.Items['c'+intToStr(index)].AsString
                          else break;
                       aggregateCol := 0;
                       selectColsInstructions := nil;
                       flagAggregate := false;
                       for index1 := low(aggregateColsInstructions) to high(aggregateColsInstructions) do
                         begin
                           setlength(selectColsInstructions,length(selectColsInstructions)+1);
                           selectColsInstructions[high(selectColsInstructions)] := nil;
                           for index2 := low(aggregateColsInstructions[index1]) to high(aggregateColsInstructions[index1]) do
                             case aggregateColsInstructions[index1,index2].mnemonic of
                               112..116:
                                 begin
                                    flagAggregate := true;
                                 end;
                               129:
                                 begin
                                   st := GroupBy_Instance.Items[aggregateColsInstructions[index1,index2].stValue].AsString;
                                   for index3 := 0 to tblFields.numCols - 1 do
                                     if tblFields.columns[index3].colname = colname then
                                       begin

                                         setLength(selectColsInstructions[index1], length(selectColsInstructions[index1])+1);
                                         colType := tblFields.columns[index3].coltype;
                                         if colType = stringType then
                                           begin
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].mnemonic := 89;
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].value := 0;
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].stvalue := st;
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].printInstruction := 'PUSH LITERAL ' + st;
                                           end else     //// check for bool and datetime
                                           begin
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].mnemonic := 88;
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].value := strtoFloatDef(st,0);
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].stvalue := '';
                                             selectColsInstructions[index1,high(selectColsInstructions[index1])].printInstruction := 'PUSH  ' + st;
                                           end;

                                       end;
                                 end;
                               180 .. 182:
                                 begin
                                   setLength(selectColsInstructions[index1], length(selectColsInstructions[index1])+1);
                                   with selectColsInstructions[index1,high(selectColsInstructions[index1])] do
                                     begin
                                       mnemonic := 88;
                                       stvalue := '';
                                       value := strToFloat(aggregateColumns[aggregateCol]);
                                       printInstruction := 'PUSH ' + aggregateColumns[aggregateCol];
                                     end;
                                   aggregateCol := AggregateCol + 1;
                                   flagAggregate := false;
                                 end;
                               else
                                 begin
                                   if not flagAggregate then
                                     begin
                                       setLength(selectColsInstructions[index1], length(selectColsInstructions[index1])+1);
                                       selectColsInstructions[index1,high(selectColsInstructions[index1])] := aggregateColsInstructions[index1,index2];
                                     end;

                                 end
                             end;
                         end;
                       if rowcondition(havingconditionInstructions,resultTable) then
                         begin
                           resultRows := ResultRows + 1;
                           extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection, flagAggregate, resultRows);
                         end;
                     end;
                 end else
                 begin
                   aggregateCol := 0;
                   selectColsInstructions := nil;
                   flagAggregate := false;
                   for index1 := low(aggregateColsInstructions) to high(aggregateColsInstructions) do
                     begin
                       setlength(selectColsInstructions,length(selectColsInstructions)+1);
                       selectColsInstructions[high(selectColsInstructions)] := nil;
                       for index2 := low(aggregateColsInstructions[index1]) to high(aggregateColsInstructions[index1]) do
                         case aggregateColsInstructions[index1,index2].mnemonic of
                           112..116:
                              begin
                                flagAggregate := true;
                             end;
                           180 .. 182:
                             begin
                               setLength(selectColsInstructions[index1], length(selectColsInstructions[index1])+1);
                               with selectColsInstructions[index1,high(selectColsInstructions[index1])] do
                                 begin
                                   mnemonic := 88;
                                   stvalue := '';
                                   value := strToFloat(aggregateColumns[aggregateCol]);
                                   printInstruction := 'PUSH ' + aggregateColumns[aggregateCol];
                                 end;
                               aggregateCol := AggregateCol + 1;
                               flagAggregate := false;
                             end;
                           else
                             begin
                               if not flagAggregate then
                                 begin
                                   setLength(selectColsInstructions[index1], length(selectColsInstructions[index1])+1);
                                   selectColsInstructions[index1,high(selectColsInstructions[index1])] := aggregateColsInstructions[index1,index2];
                                 end;

                             end
                         end;
                     end;
                   resultrows := 1;
                   extractselect(dbUserId, selectColsInstructions, resultTable, distinctCollection, flagDistinctClause, AggregateCollection, flagAggregate, resultRows);
                 end;
             end;

          if useSelect then
            begin
              ACursor := GCollection.Find();
              while ACursor.HasNext do
                begin
                  IBSONInstance := ACursor.Next;
                  Cursors[high(Cursors)].Collection.Insert(IBSoNInstance);
                end;
            end else
            begin
              resParams := nil;
              for index := 0 to resultTable.resultIBSONInstance.Count - 1 do
                begin
                  setLength(resParams,length(resParams)+1);
                  resParams[High(resParams)] := resultTable.resultIBSONInstance.Item[index].AsString
                end;
            end;
            {$ENDIF}
          yyacceptmessage('SELECT STATEMENT: ' + intToStr(ResultRows));
        end;
      end;
    end;
end;

procedure createTables;
begin

  {$IFDEF tablesdb}
  DDusers := TTableClass.Create(Path + 'DD_USERS_DD', False,
    ['user_id', 'password', 'created_by'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  // PUBLIC means all users
  // Privileges are pre-defined by sys_Privilege_name or obj_Privilege_name and no need to insert them in database

  DDroles := TTableClass.Create(Path + 'DD_ROLES_DD', False,
    ['role_name'], ['STRING[lendbOjects]']);

  DDuser_roleprivilege_object := TTableClass.Create(Path + 'DD_USER_ROLEPRIVILEGE_DD', False,
    ['role_or_privilege_name', 'user_id', 'dbobject'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);
    // kind: role      --> user on dbobject
    //       privilege --> user on dbobject
    // kind not necessary can be deducted by name

  DDrole_privilege := TTableClass.Create(Path + 'DD_ROLE_PRIVILEGE_DD', False,
    ['role_name', 'privilege_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);
  // privilege --> role

  DDdatabases := TTableClass.Create(Path + 'DD_DATABASES_DD', False,
    ['database_name'], ['STRING[lendbOjects]']);

  DDtables := TTableClass.Create(Path + 'DD_TABLES_DD', False,
    ['table_name', 'database_name', 'number_of_columns', 'number_of_documents'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER']);

  DDtablecolumns := TTableClass.Create(Path + 'DD_TABLECOLS_DD', False,
    ['column_name', 'table_name', 'database_name',
     'position', 'type_name', 'dim1', 'dim2', 'kinddefault', 'nulldefault',
     'intdefault', 'stdefault', 'int64default', 'extdefault', 'currencydefault',
     'booleandefault', 'sequencecounter'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'INTEGER', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER', 'INTEGER', 'BOOLEAN',
     'INTEGER',  'STRING[lendbOjects]', 'INT64', 'EXTENDED', 'CURRENCY',
     'BOOLEAN', 'INT64']);

  // constraint_name unique
  DDconstraints := TTableClass.Create(Path + 'DD_CONSTRAINTS_DD', False,
    ['constraint_name', 'table_name', 'database_name',
     'constraint_kind', 'constraint_type', 'references_table'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'INTEGER', 'STRING[24]', 'STRING[lendbOjects]']);
  // just in case there is a references constraint_type

  DDcolumnsconstraint := TTableClass.Create(
    Path + 'DD_COLUMNSCONSTRAINT_DD', False,
    ['constraint_name', 'column_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDcheckinstructions := TTableClass.Create(Path + 'DD_CHECKINSTRUCTIONS_DD', False,
    ['constraint_name', 'mnemonic', 'boolvalue', 'value', 'stvalue', 'printInstruction'],
    ['STRING[lendbOjects]', 'INTEGER','BOOLEAN','EXTENDED','STRING[255]','STRING[255]']);

  DDrefcolumns := TTableClass.Create(Path + 'DD_REFCOLUMNS_DD', False,
    ['constraint_name', 'refcolumn_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDviews := TTableClass.Create(Path + 'DD_VIEWS_DD', False,
    ['view_name', 'database_name', 'index_name', 'number_of_columns', 'number_of_instructions'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER']);

  DDviewcolumns := TTableClass.Create(Path + 'DD_VIEWCOLS_DD', False,
    ['column_name', 'table_name','view_name', 'database_name',
     'alias_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'STRING[lendbOjects]']);

  DDviewinstructions := TTableClass.Create(Path + 'DD_VIEWINSTRUCTIONS_DD', False,
    ['view_name', 'database_name', 'mnemonic', 'boolvalue', 'value', 'stvalue', 'printInstruction'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER','BOOLEAN','EXTENDED','STRING[255]','STRING[255]']);

  // index_name unique
  // table_name = 'sys_joinindex' mean join index
  DDindexes := TTableClass.Create(Path + 'DD_INDEXES_DD', False,
    ['index_name', 'table_name', 'database_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDbasetables := TTableClass.Create(Path + 'DD_BASETABLES_DD', False,
  ['index_name', 'basetable_name'],
  ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDkeysindexes := TTableClass.Create(Path + 'DD_KEYSINDEXES_DD', False,
    ['index_name', 'table_name', 'column_name', 'column_order'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'BOOLEAN']);

  DDjoinindexes := TTableClass.Create(Path + 'DD_JOININDEXES_DD', False,
    ['index_name', 'from_table', 'to_table'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDkeysjointables := TTableClass.Create(Path + 'DD_KEYSJOINTABLES_DD', False,
    ['index_name', 'from_table', 'to_table', 'key_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  //createDefaultSchema('sys_suid','sample');
  {$ENDIF}


end;

procedure openTables;
begin

  {$IFDEF tablesdb}
  //loadSchema('sys_suid','sample');

  DDusers := TTableClass.Create(Path + 'DD_USERS_DD', True,
    ['user_id', 'password', 'created_by'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  // PUBLIC means all users
  // Privileges are pre-defined by sys_Privilege_name or obj_Privilege_name and no need to insert them in database

  DDroles := TTableClass.Create(Path + 'DD_ROLES_DD', True,
    ['role_name'], ['STRING[lendbOjects]']);

  DDuser_roleprivilege_object := TTableClass.Create(Path + 'DD_USER_ROLEPRIVILEGE_DD', True,
    ['role_or_privilege_name', 'user_id', 'dbobject'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);
    // kind: role      --> user on dbobject
    //       privilege --> user on dbobject
    // kind not necessary can be deducted by name

  DDrole_privilege := TTableClass.Create(Path + 'DD_ROLE_PRIVILEGE_DD', True,
    ['role_name', 'privilege_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);
  // privilege --> role

  DDdatabases := TTableClass.Create(Path + 'DD_DATABASES_DD', True,
    ['database_name'], ['STRING[lendbOjects]']);

  DDtables := TTableClass.Create(Path + 'DD_TABLES_DD', True,
    ['table_name', 'database_name', 'number_of_columns', 'number_of_documents'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER']);

  DDtablecolumns := TTableClass.Create(Path + 'DD_TABLECOLS_DD', True,
    ['column_name', 'table_name', 'database_name',
     'position', 'type_name', 'dim1', 'dim2', 'kinddefault', 'nulldefault',
     'intdefault', 'stdefault', 'int64default', 'extdefault', 'currencydefault',
     'booleandefault', 'sequencecounter'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'INTEGER', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER', 'INTEGER', 'BOOLEAN',
     'INTEGER',  'STRING[lendbOjects]', 'INT64', 'EXTENDED', 'CURRENCY',
     'BOOLEAN', 'INT64']);

  // constraint_name unique
  DDconstraints := TTableClass.Create(Path + 'DD_CONSTRAINTS_DD', True,
    ['constraint_name', 'table_name', 'database_name',
     'constraint_kind', 'constraint_type', 'references_table'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'INTEGER', 'STRING[24]', 'STRING[lendbOjects]']);
  // just in case there is a references constraint_type

  DDcolumnsconstraint := TTableClass.Create(
    Path + 'DD_COLUMNSCONSTRAINT_DD', True,
    ['constraint_name', 'column_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDcheckinstructions := TTableClass.Create(Path + 'DD_CHECKINSTRUCTIONS_DD', True,
    ['constraint_name', 'mnemonic', 'boolvalue', 'value', 'stvalue', 'printInstruction'],
    ['STRING[lendbOjects]', 'INTEGER','BOOLEAN','EXTENDED','STRING[255]','STRING[255]']);

  DDrefcolumns := TTableClass.Create(Path + 'DD_REFCOLUMNS_DD', True,
    ['constraint_name', 'refcolumn_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDviews := TTableClass.Create(Path + 'DD_VIEWS_DD', True,
    ['view_name', 'database_name', 'index_name', 'number_of_columns', 'number_of_instructions'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER', 'INTEGER']);

  DDviewcolumns := TTableClass.Create(Path + 'DD_VIEWCOLS_DD', True,
    ['column_name', 'table_name','view_name', 'database_name',
     'alias_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]',
     'STRING[lendbOjects]']);

  DDviewinstructions := TTableClass.Create(Path + 'DD_VIEWINSTRUCTIONS_DD', True,
    ['view_name', 'database_name', 'mnemonic', 'boolvalue', 'value', 'stvalue', 'printInstruction'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'INTEGER','BOOLEAN','EXTENDED','STRING[255]','STRING[255]']);

  // index_name unique
  // table_name = 'sys_joinindex' mean join index
  DDindexes := TTableClass.Create(Path + 'DD_INDEXES_DD', True,
    ['index_name', 'table_name', 'database_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDbasetables := TTableClass.Create(Path + 'DD_BASETABLES_DD', True,
  ['index_name', 'basetable_name'],
  ['STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDkeysindexes := TTableClass.Create(Path + 'DD_KEYSINDEXES_DD', True,
    ['index_name', 'table_name', 'column_name', 'column_order'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'BOOLEAN']);

  DDjoinindexes := TTableClass.Create(Path + 'DD_JOININDEXES_DD', True,
    ['index_name', 'from_table', 'to_table'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);

  DDkeysjointables := TTableClass.Create(Path + 'DD_KEYSJOINTABLES_DD', True,
    ['index_name', 'from_table', 'to_table', 'key_name'],
    ['STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]', 'STRING[lendbOjects]']);
  {$ENDIF}

end;


procedure closeTables;
begin

  {$IFDEF tablesdb}
  DDusers.Free;
  DDroles.Free;
  DDuser_roleprivilege_object.Free;
  DDrole_privilege.Free;
  DDdatabases.Free;
  DDviews.Free;
  DDviewinstructions.Free;
  DDtables.Free;
  DDtablecolumns.Free;
  DDviewcolumns.Free;
  DDconstraints.Free;
  DDcolumnsconstraint.Free;
  DDcheckinstructions.Free;
  DDrefcolumns.Free;
  DDindexes.Free;
  DDbasetables.Free;
  DDkeysindexes.Free;
  DDjoinindexes.Free;
  DDkeysjointables.Free;
  {$ENDIF}

end;


initialization


parser := TParser.Create();
lexer := TLexer.Create();
parser.lexer := lexer;

// userid := 'sys_suid';



finalization

lexer.free;
parser.free;



end.
