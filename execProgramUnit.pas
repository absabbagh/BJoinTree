unit execProgramUnit;
// usr/local/bin/


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
  BPlusTreeU, TtableU,
  BJoinTreeU,
  variants;

const
  lendbOjects = 48;



type
  typeset = (inttype, smallinttype, int64type, extendedtype, currencytype,
    tdatetype, ttimetype, tdatetimetype, booleantype, stringtype);

  tblStructure = record
    tblName: string;
    // timestamp: TDateTime;
    storage: TTableClass;
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
      idxstorage: BtrPlusClass;
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

var
  Path: string = '';

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

  { TODO : check all the fields in BSON, if someone is messing put it as default or yyerror }
  { DONE : To put boolean values as a new kind in sqlLex and sql.y }

procedure closeSchemaTables;
var
  index: integer;
  index1: integer;
begin
  for index := low(workingSchema.tables) to High(workingSchema.tables) do
    begin
      workingSchema.tables[index].storage.Free;
      for index1 := low(workingSchema.tables[index].idxdata) to high(workingSchema.tables[index].idxdata) do
        workingSchema.tables[index].idxdata[index1].idxstorage.Free
    end;
  for index := low(workingSchema.joinidxdata) to high(workingSchema.joinidxdata) do
    workingSchema.joinidxdata[index].idxstorage.Free
  // close indexes
end;

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
      row[2] := 'Null';
      DDusers.insertRow(row);
      closeTables;
    end;
  outData := outData + '.txt';
  AssignFile(outText,Path + outData);
  rewrite(outText);
  openTables;
end;
procedure disconnectFromTablesDB;
begin
  closeFile(outText);
  closeSchemaTables;
  closeTables;
end;

procedure connectToDB(DBName: string; outData: string);
begin
  connectToTablesDB(DBName,outData);
end;

procedure disconnectFromDB;
begin
  disconnectFromTablesDB;
end;

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

function loadSchema(dbName: string): boolean;
var
  rowId1, rowId2, rowId3, rowId4: int64;
  joinindex: boolean;
  idxAsc: array of boolean;
  view_Name: string;
  colsname, colstype: array of string;
  allowcolsNull: array of boolean;
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

  // load all the tables

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

  // load all the columns

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
                       colDefaultValue := Null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'intdefault');
                  1: colDefaultValue := Null;
                  2: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := Null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'int64default');
                  3, 5: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                          colDefaultValue := Null else
                          colDefaultValue :=
                            DDtablecolumns.getValueByColumnName(rowId1, 'extdefault');
                  4: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := Null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'currencydefault');
                  6: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := Null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'booleandefault');
                  7: if DDtablecolumns.getValueByColumnName(rowId1, 'nulldefault') then
                       colDefaultValue := Null else
                       colDefaultValue :=
                         DDtablecolumns.getValueByColumnName(rowId1, 'stdefault');
                end;
              end;
          end;
      end;
    rowId1 := rowId1 + 1;
  until rowId1 > DDtablecolumns.lastRow;

  // load all constraints for the database

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


  { TODO : check for null and call load with null also for indexes }


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
                    if workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxname =
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
                              if DDkeysjointables.getValueByColumnName(rowId3, 'index_name') = idxname then
                                for index1 := low(joincouples) to high(joincouples) do
                                  if (DDkeysjointables.getValueByColumnName(rowId3, 'from_table') = joincouples[index1].fromTable) and
                                     (DDkeysjointables.getValueByColumnName(rowId3, 'to_table') = joincouples[index1].toTable) then
                                    begin
                                      setLength(joincouples[index1].keyNames,
                                        length(joincouples[index1].keyNames) + 1);
                                      joincouples[index1].keyNames[high(joincouples[index1].keyNames)] :=
                                        DDkeysjointables.getValueByColumnName(rowId3, 'key_name');
                                    end
                            end;
                          rowId3 := rowId3 + 1;
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
                  if not joinIndex then
                    begin
                      with workingSchema.tables[index].idxdata[high(workingSchema.tables[index].idxdata)] do
                        if idxname = DDkeysindexes.getValueByColumnName(rowId2, 'index_name') then
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
     resultRow: array of variant;
   end;

 type
   conditionInstructionstype = array of singleInstructionType;


   runstacksingletype = record // extvalue for all numbers
     caseValue: integer;       // 7 for Null Value
                               // 8 for dates
                               // 9 for options
                               // 10 for column name
                               //   boolValue: false no index
                               //              true use index -->
                               //                   intvalue: 1 use index on condition
                               //                             0 use full scan on index
                               //   int64Value: 1 next exist to seperate left expression from right expression
     boolValue: boolean;       // 0
     intValue: integer;        // 4
     int64Value: integer;      // 4
     dblValue: double;         // 4
     extValue: extended;       // 4
     curValue: currency;       // 4
     strValue: string;         // 6
   end;

   runstacktype = array of runstacksingletype;

 type
   executePlanType = record
     useIndex: boolean;
     joinFlag: boolean;
     Index: array of record
       Number: Integer;
       Name: string;
       tblName: string;
       colName : string;
       Value: variant;
       mnemonic: integer;
     end
   end;


{$INCLUDE functionselectextractrow.inc}
{$INCLUDE functioncomprow.inc}

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
                result := Null;
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
                result := Null;
                yyerror('Value is not in the range of SMALLINT');
                exit
              end;
          end;
        if (sqltype_name = 'INTEGER') then
          begin
            if (varVar > high(integer)) or (varVar < low(integer)) then
              begin
                result := Null;
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
                result := Null;
                yyerror('Value is not in the range of SMALLINT');
                exit
              end;
            result := intValue
          end;
        if (sqltype_name = 'INTEGER') then
          begin
            if (intvalue > High(integer)) or (intvalue < Low(integer)) then
              begin
                result := Null;
                yyerror('Value is not in the range of INTEGER');
                exit
              end;
            result := intValue
         end;
        if (sqltype_name = 'BIGINT') then
          begin
            if (intvalue > High(int64)) or (intvalue < Low(int64)) then
              begin
                result := Null;
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
                    result := Null;
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
        result := Null;
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
            result := Null;
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
                  result := Null;
                  yyerror('Value is not in the range of SMALLINT');
                  Exit;
                end
             else
              begin
                result := Null;
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
                  result := Null;
                  yyerror('Value is not in the range of INTEGER');
                  Exit;
                end
             else
              begin
                result := Null;
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
                  result := Null;
                  yyerror('Value is not in the range of INT64');
                  Exit;
                end
             else
              begin
                result := Null;
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
                result := Null;
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
                extendedFieldValue := EncodeDate(year, month, day);
                result := extendedFieldValue
              end;
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
  end = nil;



// check for DOUBLE PRECISION -- EXTENDED not working properly saving the right number

function isEquivalentGraph(JoinGraph, idxJoinGraph: GraphStructure): boolean;
var
  i, j, k, l: Integer;
  found: boolean;
begin
  result := false;
  if not(length(JoinGraph) = length(idxJoinGraph)) then exit;
  for i := 0 to length(JoinGraph) - 1 do
    begin
      found := false;
      for j := 0 to length(idxJoinGraph) - 1 do
        begin
          if (JoinGraph[i].node = idxJoinGraph[j].node) then
            begin
              found := true;
              break
            end;
        end;
      if not found then exit
    end;

  for i := 0 to length(JoinGraph) - 1 do
    begin
      for j := 0 to length(idxJoinGraph) - 1 do
        begin
          if (JoinGraph[i].node = idxJoinGraph[j].node) then
            begin
              if not(length(JoinGraph[i].adjacents) = length(idxJoinGraph[j].adjacents)) then exit;
              for k := 0 to length(JoinGraph[i].adjacents) - 1 do
                begin
                  found := false;
                  for l := 0 to length(idxJoinGraph[j].adjacents) - 1 do
                    begin
                      if ((JoinGraph[i].adjacents[k].link = idxJoinGraph[j].adjacents[l].link) and
                          (JoinGraph[i].adjacents[k].CommonKey = idxJoinGraph[j].adjacents[l].CommonKey)) then
                        begin
                          found := true;
                          break
                        end;
                    end;
                  if not found then exit
                end
            end
        end
    end;
  result := true
end;

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
  end = nil;

  ldbName: string = '';
  lUserId: string = '';
  stk: runstacktype = nil;
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
  file_name: string = '';
  row: array of variant;
  dimSize: integer;
  index: integer;
  hasDefault: boolean = false;
  hasAutoincrement: boolean = false;
  colsType: array of string = nil;
  tblFields: tblStructure;
  indexName: string = '';
  ordAsc: boolean = True;
  idxAsc: array of boolean = nil;
  Thekeys: array of string;
  expr: progInstrunctionsType = nil;
  fromTableslen: byte = 0;
  container: array of int64 = nil;
  shiftresultindex: Integer = 0;
  aliasindex: integer;
  tblcolName: string = '';
  conditionInstructions: conditionInstructionstype = nil;
  allowcolsNull: array of boolean = nil;
  tbltimestamp: double;
  executePlan: executePlanType;

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
  colDefaultValue: variant;
  resultindex: integer;
  resulttable: resultTableType;
  resultshifting: Integer;
  fromindex: integer;
  fromlengthLimit: integer;
  dbCounter: integer = 0;
  table_name: string = '';
  Keys: array of variant;
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
  mapdataref: array of Integer = nil;
  stringFieldValue: string;
  Row2: array of variant;
  lcount: Integer;
  exprList: array of array of singleInstructionType = nil;
  index5, index6: integer;
  ladd_column: boolean = false;
  ldrop_column: boolean = false;
  lrename_table: boolean = false;
  lrename_column: boolean = false;

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
  setstk: runstacktype = nil;
  reftblName: string = '';
  vardef: variant;
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
  rowid: DataPointerType;
  rowId1, rowId2: Int64;
  colsTxt: string = '';
  JoinGraph: GraphStructure;
  idxJoinGraph: GraphStructure;
begin
  if sqlMemProg = nil then exit;
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
  dfltValue := Null;

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



          // Go back and return to undo all the staff done by Getprocessid
          // search all transactions  previous transaction

        end;

        209: // ROLLBACK TO
        begin
          // check for the process id
          // check for the name if exist
          // go back  to savepoint

          // Go back and return to undo all the staff done by Getprocessid till
          // savepoint, delete all saveoints that come after it
        end;

        210: // COMMIT TRANSACTION
        begin
          // check for the process id
          // check for the name if exist
          // COMMIT

        end;

        211: // TRANSACTION NAME
        begin
          TransactionName := stk[High(stk)].strValue;
          setLength(stk, Length(stk) - 1);
        end;

        212: // HOLD SAVEPOINT
        begin
          // CREATE A SAVEPOINT
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
          stk[High(stk)].strValue  := 'Null';
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

          for index1 := 0 to high(workingschema.tables) do
            if (workingschema.tables[index1].tblName = tblName) then break;
          for index2 := index1+1 to high(workingschema.tables) do
            workingschema.tables[index2-1] := workingschema.tables[index2];
          setLength(workingSchema.tables,length(workingSchema.tables)-1) ;

          yyacceptmessage('Table ' + tblName + ' dropped');
        end;

        191: // DROP INDEX
        begin
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
        end;

        192: // DROP JOIN INDEX
        begin

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
                for index2 := 0 to high(workingSchema.tables[index].idxdata) do
                  workingSchema.tables[index].idxdata[index2].idxstorage.Free;
                workingSchema.tables[index].storage.Free;
              end;
            for index := low(workingSchema.joinidxdata) to High(workingSchema.joinidxdata) do
              workingSchema.joinidxdata[index].idxstorage.Free;
          end;

        workingSchema.dbName := ldbName;
        workingSchema.tables := nil; // New schema with no tables
        workingSchema.joinidxdata := nil;
        yyacceptmessage('Found New Schema ' + ldbName);

        // DATABASE_metaData(DBNAME)
        row := nil;
        setLength(row, 1);
        row[0] := ldbName;
        DDdatabases.insertRow(row);

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

        row := nil;
        setLength(row, 4);
        row[0] := tblName;
        row[1] := workingSchema.dbName;
        row[2] := Length(columnList); // number Of Attributes
        row[3] := Integer(0); // Number Of Rows
        DDtables.insertRow(row);
        DDtables.returnRow(DDtables.lastRow,row);

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
                          row[8] := vardef = Null;
                          if not row[8] then row[9] := vardef
                        end;
                      if convertType(columnTypeName) = 'INT64' then
                        begin
                          row[7] := 2;  // 2 for int64
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then row[11] := vardef
                        end;
                      if (convertType(columnTypeName) = 'SINGLE') or
                         (convertType(columnTypeName) = 'DOUBLE') or
                         (convertType(columnTypeName) = 'EXTENDED') then
                        begin
                          row[7] := 3;  // 3 for extended
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then row[12] := vardef
                        end;
                      if convertType(columnTypeName) = 'CURRENCY' then
                        begin
                          row[7] := 4;  // 4 for currency
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then row[13] := vardef
                        end;
                      if convertType(columnTypeName) = 'TDATETIME' then
                        begin
                          row[7] := 5;  // 5 for tdatetime
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then
                            row[12] := vardef else
                            row[12] := now;
                        end;
                      if convertType(columnTypeName) = 'TDATE' then
                        begin
                          row[7] := 5;  // 5 for tdatetime
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then
                            row[12] := vardef else
                            row[12] := now;
                        end;
                      if convertType(columnTypeName) = 'TTIME' then
                        begin
                          row[7] := 5;  // 5 for tdatetime
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then
                            row[12] := vardef else
                            row[12] := now;
                        end;
                      if convertType(columnTypeName) = 'BOOLEAN' then
                        begin
                          row[7] := 6;  // 6 for boolean
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
                          if not row[8] then row[14] := vardef
                        end;
                      if convertType(columnTypeName) = 'STRING' then
                        begin
                          row[7] := 7; // 7 for string
                          with columnlist[index1] do
                            vardef := IsCompatibleType(columnDefaultValue,columnTypeName,numTypeSize[0],numTypeSize[1]);
                          row[8] := vardef = Null;
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

            storage := TTableClass.Create(Path + tblName + '_' + dbName,
              False, colsName, colsType, allowcolsNull);
            storage.Free;
            storage := TTableClass.Create(Path + tblName + '_' + dbName,
              True, colsName, colsType, allowcolsNull);


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
          if colsName[low(colsName)] =
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
          flagExpressionAggregate := true;
          flagAggregate := true;

          setlength(expr,length(expr) + 1);
          expr[High(expr)].mnemonic := 181;
          expr[High(expr)].Value := 111 + ord(grpKind);
          expr[High(expr)].printInstruction := 'EXPRESSION AGGREGATE';
        end;

       182: // DISTINCT AGGREGATE
        begin
          flagExpressionAggregate := true;
          flagAggregate := true;
          setlength(expr,length(expr) + 1);
          expr[High(expr)].mnemonic := 182;
          expr[High(expr)].Value := 111 + ord(grpKind);
          expr[High(expr)].printInstruction := 'DISTINCT AGGREGATE';
        end;

        36: //DISTINCT
        begin
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
                extendedtype, tdatetimetype, tdatetype, ttimetype:
                  colsType[resultindex] := 'EXTENDED';
                currencytype: colsType[resultindex] := 'CURRENCY';
                booleantype: colsType[resultindex] := 'BOOLEAN';
                stringtype: colsType[resultindex] :=
                    'STRING[' + IntToStr(
                    TBLFields.columns[resultindex].coltypescale.size) + ']'
              end;
            end;

          setLength(Row, tblFields.numCols);
          resulttable.resultRow := nil;
          setLength(resulttable.resultRow, length(resulttable.resultRow) +
            tblFields.numCols);
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
                                  if setstk[index1].caseValue = 7 then row[j] := Null;
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
                  extendedtype, tdatetimetype, tdatetype, ttimetype:
                    colsType[resultindex] := 'EXTENDED';
                  currencytype: colsType[resultindex] := 'CURRENCY';
                  booleantype: colsType[resultindex] := 'BOOLEAN';
                  stringtype: colsType[resultindex] :=
                      'STRING[' + IntToStr(
                      TBLFields.columns[resultindex].coltypescale.size) + ']'
                end;
              end;

            setLength(Row, tblFields.numCols);
            resulttable.resultRow := nil;
            setLength(resulttable.resultRow, length(resulttable.resultRow) +
              tblFields.numCols);

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
              dfltValue := Null;
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
              cnstrName :=
                'sys_CONSTRAINT' + cnstrType + colsname[0] + IntToStr((DDconstraints.lastRow+1));

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
              cnstrName :=
                'sys_CONSTRAINT' + cnstrType + colsname[0] + IntToStr((DDconstraints.lastRow+1));

              // if there is no name the system will give it a name

              cnstrName := 'sys_CONSTRAINT' + cnstrType;
              for index1 := 0 to high(colsname) do
                cnstrName := cnstrName + '_' + colsname[index1];
              cnstrName := cnstrName + IntToStr((DDconstraints.lastRow+1));

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
          if not TableExists(tblName) then
            begin
              yyerror('Table Name not exist: ' + tblName);
              exit;
            end;
          tblFields := loadTableFields(tblName);

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
                                  if (row[index3] = Null) then
                                    begin
                                      keys[high(Keys)] := 'Null';
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

          (*** Loading into Working Schema ***)

          setLength(workingSchema.joinidxdata, length(workingSchema.joinidxdata) + 1);
          workingSchema.joinidxdata[high(workingSchema.joinidxdata)].idxname := indexname;
          workingSchema.joinidxdata[high(workingSchema.joinidxdata)].joinBaseTables := nil;
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
            end;

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
              idxstorage.createBTrees(TheBaseTables, True, TheKeys);
            end;

          // case of base tables not empty

          for index := 0 to length(TheBaseTables) - 1 do
            begin
             // for every base table: load the rows and insert them in a join
             tblFields := loadTableFields(TheBaseTables[index]);
             for rowId1 := tblFields.storage.firstRow to tblFields.storage.LastRow do
               if tblFields.storage.existRow(rowId1) then
                 begin
                   tblFields.storage.returnRow(rowId1, row);
                   Row2 := nil;
                   setLength(row,tblFields.numCols);
                   for index1 := 0 to tblFields.numCols - 1 do
                     begin
                       setLength( Row2, length(Row2) + 1);
                       if row[index1] = Null then
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
                     end;
                   workingschema.joinidxdata[high(workingSchema.joinidxdata)].idxstorage.AddKey(tblFields.tblName, Row2, rowId1);
                end;
            end;

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
          tblFields := loadTableFields(tblName);


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
            end;

          for index1 := low(colsName) to high(colsName) do
            begin
              setlength(lidxKeys,length(lidxKeys) + 1);
              lidxKeys[high(lidxKeys)].colName := colsName[index1];
              lidxKeys[high(lidxKeys)].colOrder := idxASC[index1];
              lidxKeys[high(lidxKeys)].tblName  := tblName
            end;

          colsName := nil;
          idxASC := nil;
          expr := nil;

          // check if the table belongs to the schema

          found := false;
          for index := Low(ljoinbaseTables) to high(ljoinbaseTables) do
            if tblName = ljoinbaseTables[index] then
              begin
                found := true;
                break;
              end;
          if not found then
            begin
              setlength(ljoinbaseTables,length(ljoinbaseTables) + 1);
              ljoinbaseTables[high(ljoinbaseTables)] := tblName
            end
        end;

        154: // JOIN TABLES CONDITION
        begin
{          if (length(expr) < (length(ljoinBaseTables) * 3)) or ((length(expr) div 3) <> 0) then
            begin;
              yyerror('Error in the join condition');
              exit;
            end;
 }
          while expr <> nil do
            begin
              if expr[high(expr)].mnemonic = 44 then
                setlength(expr,length(expr)-1);
              index := length(expr);
              if not ((expr[high(expr)-2].mnemonic = 151) and (expr[high(expr)-1].mnemonic = 151) and (expr[high(expr)].mnemonic = 50)) then
                begin
                  yyerror('Error in the join condition');
                  exit;
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
              // check fromtocolNames
              // check tofromcolNames
              setlength(expr,length(expr)-3);

            end;
         fromTablesLen := length(fromTables);
         colsname := nil;
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
                                   if not DDTables.emptyTable then
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
                            if convertType(columnTypeName) = 'TDATE' then
                              begin
                                row[7] := 5;  // 5 for tdatetime
                                row[11] := @columnlist[index1].columnDefaultValue;
                              end;
                            if convertType(columnTypeName) = 'TTIME' then
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
                        inscols[index1].value := Null
                end;


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
              7: valuesList[j] := Null;
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

                          // make sure when you drop a table or alter the table, you delete the row from sequences table
                          // when you create an autoincrement field, create the sequence row
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
                        end else
                        if tblFields.columns[index1].colSQLtypename = 'TIMESTAMP' then
                          begin
                            inscols[index1].value := double(now);
                          end else
                          begin
                            (* check for constraints come later
                               If you specify no default value for a column, the default is NULL
                                 unless you place a NOT NULL constraint on the column. In this case,
                                 no default exists.
                            *)
                              inscols[index1].value := Null
                          end;
               end;

              row := nil;
              for index1 := 0 to tblFields.numCols - 1 do
                begin
                  setlength(row,length(row)+1);
                  row[High(row)] := inscols[index1].value;
                end;

              for index2 := 0 to length(tblFields.constraints) - 1 do
                begin
                  case tblFields.constraints[index2].cnstrtype of
                    1: // Not Null constraint
                      begin
                        if (inscols[tblFields.constraints[index2].nnullcol].value = Null) then
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
                            if inscols[index1].value = Null then
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

                        with resultTable do
                          begin
                            resultRow := nil;
                            for index1 := 0 to tblFields.numCols - 1 do
                              begin
                                setlength(resultRow,length(resultRow)+1);
                                resultRow[High(resultRow)] := inscols[index1].value;
                              end;
                          end;

                        if not rowcondition(tblFields.constraints[index2].checkCondition ,resulttable) then
                          begin
                            yyerror('Check condition not satisfied');
                            Exit;
                          end
                      end;
                  end;
                end;
            end;

          tblFields.storage.insertRow(row);


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
                if (row[index3] = Null) then
                begin
                  keys[high(Keys)] := 'Null';
                end
                else
                keys[high(Keys)] := Row[index3];
              end;
              rowId := {intToStr(}tblFields.storage.lastRow{)};

              tblFields.idxdata[index1].idxstorage.AddKey(keys,{strToint(}rowId{)});
            end;

          for index1 := 0 to length(workingSchema.joinidxdata) - 1  do
            for index2 := 0 to high(workingSchema.joinidxdata[index1].joinBaseTables) do
              if workingSchema.joinidxdata[index1].joinBaseTables[index2] = tblFields.tblName then
                begin
                  rowId := tblFields.storage.lastRow;
                  workingSchema.joinidxdata[index1].idxstorage.AddKey(tblFields.tblName,row,rowId);
                end;
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

          resulttable.resultRow := nil;
          for fromindex := 0 to fromTablesLen - 1 do
          begin
            setLength(fromTables[fromindex].fromRow,
              fromTables[fromindex].fromFields.numCols);
            setLength(resulttable.resultRow, length(resulttable.resultRow) +
              fromTables[fromindex].fromFields.numCols);
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
          executeplan.joinFlag := false;
          executeplan.Index := nil;

          if conditionInstructions = nil then
            begin
              // read sequentially the data
              // executeplan.useIndex := false;
              if flagOrderClause then
                begin
                  // read sequentially by ordering
                end
            end else
            begin
              analyzeQuery(conditionInstructions,executePlan);

              executePlan.joinFlag := fromTableslen <> 1;
              if executePlan.joinFlag then
                begin
                  // create the join graph for the query
                  SetLength(JoinGraph,Length(fromTables));
                  for index5 := 0 to length(fromTables) - 1 do
                    begin
                      JoinGraph[index5].node := fromTables[index5].Name;
                      JoinGraph[index5].adjacents := nil;
                    end;
                  index6 := high(conditionInstructions);
                  while index6 >=0 do
                    begin
                      if not ((conditionInstructions[index6-2].mnemonic = 151) and
                              (conditionInstructions[index6-1].mnemonic = 151) and
                              (conditionInstructions[index6].mnemonic = 50)) then
                        begin
                          index6 -= 1;
                        end else
                        begin
                          fromtblName := copy(conditionInstructions[index6-2].stvalue,1,
                                              pos('.',conditionInstructions[index6-2].stvalue)-1);
                          for index5 := 0 to length(JoinGraph)-1 do
                            if JoinGraph[index5].node = fromtblName then
                              begin
                                totblName := copy(conditionInstructions[index6-1].stvalue,1,
                                                  pos('.',conditionInstructions[index6-1].stvalue)-1);
                                setlength(JoinGraph[index5].adjacents,length(JoinGraph[index5].adjacents)+1);
                                JoinGraph[index5].adjacents[high(JoinGraph[index5].adjacents)].link := totblName;
                                JoinGraph[index5].adjacents[high(JoinGraph[index5].adjacents)].CommonKey :=
                                   copy(conditionInstructions[index6-2].stvalue,
                                        pos('.',conditionInstructions[index6-2].stvalue)+1,
                                        length(conditionInstructions[index6-2].stvalue));
                                break;
                              end;
                          for index5 := 0 to length(JoinGraph)-1 do
                            if JoinGraph[index5].node = totblName then
                              begin
                                setlength(JoinGraph[index5].adjacents,length(JoinGraph[index5].adjacents)+1);
                                JoinGraph[index5].adjacents[high(JoinGraph[index5].adjacents)].link := fromtblName;
                                JoinGraph[index5].adjacents[high(JoinGraph[index5].adjacents)].CommonKey :=
                                  copy(conditionInstructions[index6-1].stvalue,
                                       pos('.',conditionInstructions[index6-1].stvalue)+1,
                                       length(conditionInstructions[index6-1].stvalue));
                              end;
                          index6 -= 3;
                        end;
                    end;

                  // compare the join graph from all the join index with the one for the query
                  if workingSchema.joinidxdata <> nil then
                    for index1 := 0 to length(workingSchema.joinidxdata) - 1 do
                      begin
                        idxJoinGraph :=  workingSchema.joinidxdata[index1].idxstorage.JoinGraph;
                        found := isEquivalentGraph(JoinGraph,idxJoinGraph);
                        if found then break
                      end;

                  if found then
                    begin
                      Executeplan.joinFlag := true;
                      Executeplan.useIndex := true;
                      setlength(Executeplan.Index,length(Executeplan.Index)+1);
                      Executeplan.Index[High(Executeplan.Index)].Number := index1;
                      Executeplan.Index[High(Executeplan.Index)].Name:= workingSchema.joinidxdata[index1].idxname;

                      setlength(keys,length(workingSchema.joinidxdata[index1].idxkeys));
                      setlength(dataref,length(workingSchema.joinidxdata[index1].joinBaseTables));

                      for index2 := 0 to length(fromTables) - 1 do
                        for index3 := 0 to length(workingSchema.joinidxdata[index1].joinBaseTables) - 1 do
                          if fromTables[index2].Name = workingSchema.joinidxdata[index1].joinBaseTables[index3] then
                            begin
                               setlength(mapdataref,length(mapdataref)+1);
                               mapdataref[index2] := index3;
                               break
                            end;
                    end;
                end
            end;


          // ****** EXECUTE PLAN ******

          ResultRows := 0;


          // departments employees job_history jobs locations countries
          if executePlan.useIndex then // momenteraly
            begin
              if executePlan.joinFlag then
                begin
                  workingSchema.joinidxdata[Executeplan.Index[High(Executeplan.Index)].Number].idxstorage.ClearKey;
                  workingSchema.joinidxdata[Executeplan.Index[High(Executeplan.Index)].Number].idxstorage.NextKey(keys,dataref);

                  resultshifting := 0;
                  for index := 0 to length(fromTables) - 1 do
                    begin
                      fromTables[index].fromFields.storage.returnRow(
                        dataref[mapdataref[index]], fromTables[index].fromrow);
                      for resultindex :=
                       0 to length(fromTables[index].fromrow) - 1 do
                        begin
                          resulttable.resultRow[resultindex +
                            resultshifting] :=
                            fromTables[index].fromrow[resultindex];
                        end;
                      resultshifting += length(fromTables[index].fromrow)
                    end;

                  if rowcondition(conditionInstructions,resultTable) then
                  begin
                    resultRows := ResultRows + 1;
                    extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                  end;

                end else
                begin
                  // use executeplan to get the rows
                end
            end
           else

            begin
              setlength(container,fromTablesLen);
              for index := 0 to fromTablesLen - 1 do
                container[index] := 1;
              repeat
                shiftresultindex := 0;
                for index := 0 to fromTablesLen - 1 do
                  begin
                    if fromTables[index].fromFields.storage.existRow(container[index]) then
                      begin
                        fromTables[index].fromFields.storage.returnRow(
                          container[index], fromTables[index].fromrow);
                        if index > 0 then shiftresultindex := shiftresultindex + length(fromTables[index -1].fromrow);
                        for resultindex := 0 to length(fromTables[index].fromrow) - 1 do
                          begin
                            resulttable.resultRow[resultindex + shiftresultindex] :=
                              fromTables[index].fromrow[resultindex];
                          end;
                      end
                  end;
                if rowcondition(conditionInstructions,resultTable) then
                  begin
                    resultRows := ResultRows + 1;
                    extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                  end;
                index2 := fromTablesLen - 1;
                flag := false;
                repeat
                  container[index2] += 1;
                  if flag then container[index2] -= 1;
                  found := false;
                  flag := false;
                  if container[index2] = fromTables[index2].fromFields.storage.lastRow + 1 then
                    begin
                      if index2 <> 0 then
                        begin
                          container[index2] := 1;
                          container[index2-1] += 1;
                          index2 -= 1;
                          flag := true
                        end
                    end else found := true;
                    if index2 = 0 then found := true
                until found;
              until container[0] = fromTables[0].fromFields.storage.lastRow + 1;
            end;

            (*
            case fromTablesLen of
              1:
              begin
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
              end;

              2..254:
              begin
                for rowId1 :=
                  fromTables[0].fromFields.storage.firstRow to
                  fromTables[0].fromFields.storage.LastRow do
                  if fromTables[0].fromFields.storage.existRow(rowId1) then
                    for rowId2 :=
                      fromTables[1].fromFields.storage.firstRow to
                      fromTables[1].fromFields.storage.LastRow do
                      if fromTables[1].fromFields.storage.existRow(rowId2) then
                      begin
                        fromTables[0].fromFields.storage.returnRow(
                          rowId1, fromTables[0].fromrow);
                        fromTables[1].fromFields.storage.returnRow(
                          rowId2, fromTables[1].fromrow);
                        for resultindex :=
                          0 to length(fromTables[0].fromrow) - 1 do
                        begin
                          resulttable.resultRow[resultindex] :=
                            fromTables[0].fromrow[resultindex];
                        end;
                        for resultindex :=
                          0 to length(fromTables[1].fromrow) - 1 do
                        begin
                          resulttable.resultRow[resultindex +
                            length(fromTables[0].fromrow)] :=
                            fromTables[1].fromrow[resultindex];
                        end;
                        if rowcondition(conditionInstructions,resultTable) then
                        begin
                          resultRows := ResultRows + 1;
                          extractselect(dbUserId, selectColsInstructions, outText, resultTable, resultRows);
                        end;
                      end;
              end;
            end;
            *)
          yyacceptmessage('SELECT STATEMENT: ' + intToStr(ResultRows));
        end;
      end;
    end;
end;

procedure createTables;
begin

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

end;

procedure openTables;
begin
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
end;


procedure closeTables;
begin
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
