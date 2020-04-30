function rowCondition(conditionInstructions: array of singleInstructionType;
                      resultTable: resulttabletype
                     ): boolean;
var
  index1, index2: integer;
  runstk: runstacktype = nil;
  ltblName: string;
  lcolname: string;
  found: boolean;
  aliasindex: integer;
  lconditionInstructions: array of singleInstructionType;
  chEscape: char = ' ';
  stPattern: string;
  RPattern: string;
  RegexObj: TRegExpr;
  lValueType: string;
begin
  { TODO :  use executeplan or indexes directly to be sure about it }
  setlength(lconditionInstructions, length(conditionInstructions));
  for index1 := low(conditionInstructions) to high(conditionInstructions) do
    lconditionInstructions[index1] := conditionInstructions[index1];

  if lconditionInstructions = nil then
    begin
      // Fill up with dummy values to return true
      setlength(lconditionInstructions, 3);
      lconditionInstructions[0].mnemonic := 88;
      lconditionInstructions[0].Value := 0;
      lconditionInstructions[0].stvalue := '';
      lconditionInstructions[0].printInstruction := 'PUSH 0';
      lconditionInstructions[1].mnemonic := 88;
      lconditionInstructions[1].Value := 0;
      lconditionInstructions[1].stvalue := '';
      lconditionInstructions[1].printInstruction := 'PUSH 0';
      lconditionInstructions[2].mnemonic := 50;
      lconditionInstructions[2].Value := 0;
      lconditionInstructions[2].stvalue := '';
      lconditionInstructions[2].printInstruction := 'EQ';
    end;

  // Check the Where Condition
  for index2 := low(lconditionInstructions) to high(lconditionInstructions) do
    begin
      if lconditionInstructions[index2].mnemonic in [42..44,50..55,79,83..86,88,89,221] then
        runStack(lconditionInstructions[index2],runstk);

      case lconditionInstructions[index2].mnemonic of
       151: // COLUMN NAME
          begin
            ltblName := copy(lconditionInstructions[index2].stvalue,1,pos('.',lconditionInstructions[index2].stvalue)-1);
            lcolName := copy(lconditionInstructions[index2].stvalue,pos('.',lconditionInstructions[index2].stvalue)+1,length(lconditionInstructions[index2].stvalue));
            found := false;
            for index1 := low(resulttable.ownertable) to high(resulttable.ownertable) do
              begin
                if (resulttable.ownertable[index1].tblname = ltblName) and
                   (resulttable.ownertable[index1].colname = lcolName) then
                  break;
                for aliasindex := low(resulttable.ownertable[index1].aliasname) to high(resulttable.ownertable[index1].aliasname) do
                  begin
                    if (resulttable.ownertable[index1].aliasname[aliasindex] = ltblName) and
                       (resulttable.ownertable[index1].colname = lcolName) then found := true;
                    if found then break;
                  end;
              end;

            { TODO : check for column_name don't exists in where clause. group by clause and having }

            setlength(runstk,length(runstk) + 1);

              if resultTable.resultrow[index1] = null then
                begin
                  runstk[High(runstk)].caseValue := 7; // Null Value
                  runstk[High(runstk)].strValue := 'NULL';
                end else
                case resultTable.resultFields.columns[index1].coltype of
                  booleanType:
                    begin
                      runstk[High(runstk)].caseValue := 0;
                      runstk[High(runstk)].boolValue := resultTable.resultrow[index1];
                    end;
                  intType, smallintType:
                    begin
                      runstk[High(runstk)].caseValue := 4;
                      runstk[High(runstk)].extValue := resultTable.resultrow[index1];
                    end;
                  int64Type:
                    begin
                      runstk[High(runstk)].caseValue := 4;
                      runstk[High(runstk)].extValue := resultTable.resultrow[index1];
                    end;
                  TDateTimeType, TDateType, TTimeType:
                    begin
                      runstk[High(runstk)].caseValue := 8;
                      runstk[High(runstk)].extValue := resultTable.resultrow[index1];
                    end;
                  extendedType:
                    begin
                      runstk[High(runstk)].caseValue := 4;
                      runstk[High(runstk)].extValue := resultTable.resultrow[index1];
                    end;
                  currencyType:
                    begin
                      runstk[High(runstk)].caseValue := 4;
                      runstk[High(runstk)].extValue :=  resultTable.resultrow[index1];
                    end;
                  stringType:
                    begin
                      runstk[High(runstk)].caseValue := 6;
                      runstk[High(runstk)].strValue :=  resultTable.resultrow[index1];
                    end;
                end;
          end;

       45: // IN
         begin
           // Check all the cases
           runstk[0].boolValue := false;
           for index1 := 1 to length(runstk) - 1 do
             begin
               if runstk[index1].strvalue = runstk[0].strValue then
                 begin
                   runstk[0].boolValue := true;
                   break;
                 end;
             end;
           runstk[0].caseValue := 0;
           setlength(runstk,1)
         end;

      46: // LIKE
         begin
           // runstk[0] is the value
           // runstk[1] is the pattern
           // runstk[2] is the escape
           if length(runstk) = 3 then
             if runstk[2].strValue[1] <> '' then
               chEscape := runstk[2].strValue[1];
           stPattern := runstk[1].strValue;
           setlength(runstk,1);
           RPattern := '^';
           for index1 := 1 to length(stPattern) do
             begin
               if stPattern[index1] = '%' then
                 begin
                   if index1 > 1 then
                     if stPattern[index1-1] = chEscape then
                       RPattern := RPattern + stPattern[index1] else
                       RPattern := RPattern + '.*?' else
                       RPattern := RPattern + '.*?'
                 end else
                 if stPattern[index1] = '_' then
                   begin
                     if index1 > 1 then
                       if stPattern[index1-1] = chEscape then
                         RPattern := RPattern + stPattern[index1] else
                         RPattern := RPattern + '.' else
                       RPattern := RPattern + '.'
                   end else
                   if stPattern[index1] = '\' then RPattern := RPattern + '\\' else
                     if stPattern[index1] = '[' then RPattern := RPattern + '\[' else
                       if stPattern[index1] = ']' then RPattern := RPattern + '\]' else
                         if stPattern[index1] = '^' then RPattern := RPattern + '\^' else
                           if stPattern[index1] = '.' then RPattern := RPattern + '\.' else
                             if stPattern[index1] = '*' then RPattern := RPattern + '\*' else
                               if stPattern[index1] = '+' then RPattern := RPattern + '\+' else
                                 if stPattern[index1] = '{' then RPattern := RPattern + '\{' else
                                   if stPattern[index1] = '}' then RPattern := RPattern + '\}' else
                                     RPattern := RPattern + stPattern[index1];
             end;
           RPattern := RPattern + '$';
           RegexObj := TRegExpr.Create;
           RegexObj.Expression := RPattern;
           case runstk[0].caseValue of
             0 : // boolean
                begin
                  if lowercase(stPattern) = 'true' then
                    runstk[0].boolValue := runstk[0].boolValue else
                    if lowercase(stPattern) = 'false' then
                      runstk[0].boolValue := not runstk[0].boolValue else
                      if runstk[0].boolValue then
                        runstk[0].boolValue := RegexObj.Exec('TRUE') else
                        runstk[0].boolValue := RegexObj.Exec('FALSE')
                end;
             4: // extended
                begin
                  runstk[0].boolValue := RegexObj.Exec(floatToStr(runstk[0].extValue));
                end;
             6: // string
                begin
                  runstk[0].boolValue := RegexObj.Exec(runstk[0].strValue);
                end;
             7: // null
                begin
                  if lowercase(stPattern) = 'null' then
                    runstk[0].boolValue := true else
                    runstk[0].boolValue := RegexObj.Exec('NULL')
                end;
             8: // dates
                begin
                  runstk[0].boolValue := RegexObj.Exec(dateTimeToStr(runstk[0].extValue));
                end;
           end;
           runstk[0].caseValue := 0;
           RegexObj.Free;
         end;


      48: // IS NULL
         begin
           // Check all the cases
           runstk[High(runstk)].boolValue := runstk[High(runstk)].caseValue = 7;
           runstk[High(runstk)].caseValue := 0;
         end;

      49: // IS NOT NULL
         begin
           // Check all the cases
           runstk[High(runstk)].boolValue := runstk[High(runstk)].caseValue <> 7;
           runstk[High(runstk)].caseValue := 0;
         end;

      end;
    end;

  result := false;
  if length(runstk) = 1 then
    if (runstk[High(runstk)].caseValue = 0) then
      if runstk[High(runstk)].boolValue then
        begin
          result := true;
        end;
  runstk := nil;

end;

function analyzeQuery(conditionInstructions: array of singleInstructionType;
                      var idxstorage: BtrPlusClass
                     ): boolean;
const
  nullDataValue = -1; //'';
var
  index1, index2: integer;
  index3, index4: integer;
  index5: Integer;
  runstk: runstacktype = nil;
  ltblName: string;
  lcolname: string;
  found: boolean;
  aliasindex: integer;
  lconditionInstructions: array of singleInstructionType;
  Keys: array of variant;
  InheritedKeys: array of variant;
  dataref: array of dataPointerType = nil;
  st: string;
  i: Integer;
  keysStorage: array of BtrPlusClass = nil;
  NumberOfInheritedKeys: Integer = 0;
  Thekeys: array of string;
  TheInheritedkeys: array of string;
  indexName: string;
  idxKeys: array of variant;
  idxInheritedKeys: array of variant;
  IKShift: Integer = 0;
begin
  result := false;
  setlength(lconditionInstructions, length(conditionInstructions));
  for index1 := low(conditionInstructions) to high(conditionInstructions) do
    lconditionInstructions[index1] := conditionInstructions[index1];

  if lconditionInstructions = nil then Exit;

  // Check for mono indexes
  // Use join graph to search for join indexes, if the graphs are identicals use the join indexes
  // In case of mono indexes, keep track of idxstorage
  // when it's time to go out, use a BPlusTree to store datapointer, datapointer as key, inherited keys for the different keys
  // keep track what have an index to check if already exist for the key
  for index2 := low(lconditionInstructions) to high(lconditionInstructions) do
    begin
      case lconditionInstructions[index2].mnemonic of
       151: // COLUMN NAME
          begin
            ltblName := copy(lconditionInstructions[index2].stvalue,1,pos('.',lconditionInstructions[index2].stvalue)-1);
            lcolName := copy(lconditionInstructions[index2].stvalue,pos('.',lconditionInstructions[index2].stvalue)+1,length(lconditionInstructions[index2].stvalue));


            // check here if there is an index for the column
            // check the indexes to see if there is one on the column
            found := false;
            for index1 := low(workingSchema.tables) to high(workingSchema.tables) do
              if workingSchema.tables[index1].tblName = ltblName then
                begin
                  found := true;
                  break;
                end;
            if not found then exit; // No index on the column, so scan the table
            found := false;
            for index3 := low(workingSchema.tables[index1].idxdata) to high(workingSchema.tables[index1].idxdata) do
              if (workingSchema.tables[index1].idxdata[index3].idxkeys[0].colName = lcolName) then
                begin
                  // Check if already exists the index
                  for index4 := 0 to length(KeysStorage) - 1 do
                    begin
                      for index5 := 0 to keysStorage[index4].NumberOfKeys - 1 do
                        if KeysStorage[index4].KeycolName(index5) = lcolname then
                          begin
                            found := true;
                            break;
                          end;
                      if found then break
                    end;
                  if found then break;
                  found := true;
                  setlength(keysStorage,length(keysStorage)+1);
                  keysStorage[High(KeysStorage)] := workingSchema.tables[index1].idxdata[index3].idxstorage;
                end;
            if not found then exit; // No index on the column, so scan the table
          end;
      end;
    end;

  result := true;

  // Save all the indexes in a new index with datapointer as key and keys as inherited keys

  for index4 := 0 to length(KeysStorage) - 1 do
    NumberOfInheritedKeys := NumberOfInheritedKeys + keysStorage[index4].NumberOfKeys;


  setlength(TheKeys,1);
  Thekeys[0] := 'DPKey0: INTEGER';

  TheInheritedKeys := nil;
  for index4 := 0 to length(KeysStorage) - 1 do
    begin
      for index5 := 0 to keysStorage[index4].NumberOfKeys - 1 do
        begin
          setlength(TheInheritedKeys,length(TheInheritedKeys)+1);
          case KeysStorage[index4].KeycolType(index5) of
            0, -5:  TheInheritedkeys[High(TheInheritedkeys)] :=
                      KeysStorage[index4].KeycolName(index5) + ': INTEGER';
            -1: TheInheritedkeys[High(TheInheritedkeys)] :=
                  KeysStorage[index4].KeycolName(index5) + ': BOOLEAN';
            -2: TheInheritedkeys[High(TheInheritedkeys)] :=
                  KeysStorage[index4].KeycolName(index5) + ': STRING[1]';
            -3: TheInheritedkeys[High(TheInheritedkeys)] :=
                  KeysStorage[index4].KeycolName(index5) + ': EXTENDED';
            -4: TheInheritedkeys[High(TheInheritedkeys)] :=
                  KeysStorage[index4].KeycolName(index5) + ': CURRENCY'
          else
            TheInheritedkeys[High(TheInheritedkeys)] :=
              KeysStorage[index4].KeycolName(index5) + ': STRING[' +
              intToStr(KeysStorage[index4].KeycolType(index5)) + ']';
          end;
        end;
      end;

  indexName := 'idxtmpKeyCols'  + '_' + workingSchema.dbName;

  if FileExists(Path + indexName + '.idx') then DeleteFile(Path + indexName + '.idx');
(*
  idxstorage :=
    BtrPlusClass.Create(Path + indexName, False, Thekeys, TheInheritedKeys, 1);
  idxstorage.Free;

  idxstorage :=
    BtrPlusClass.Create(Path + indexName, True, Thekeys, TheInheritedKeys, 1);

  setlength(idxkeys,1);
  setlength(idxInheritedKeys,NumberOfInheritedKeys);
  IKShift := 0;
  for index5 := 0 to length(keysStorage) - 1 do
    begin
      setlength(keys,keysStorage[index5].NumberOfKeys);
      IKShift += length(Keys);
      setlength(Inheritedkeys,keysStorage[index5].NumberOfInheritedKeys);
      setlength(dataRef,keysStorage[index5].NumberOfDataRef);
      // insert dataref as key with inherited keys as null
      keysStorage[index5].ClearKey;
      repeat
        keysStorage[index5].NextKey(Keys,InheritedKeys,DataRef);
        if DataRef[0] <> nullDataValue then
          begin
            idxKeys[0] := DataRef[0]; //StrToInt(DataRef[0]);
            if index5 = 0 then
              begin
                for index4 := 0 to length(Keys) - 1 do
                  idxInheritedKeys[index4] := keys[index4];
                for index4 := length(Keys) to NumberOfInheritedKeys - 1 do
                  idxInheritedKeys[index4] := null;
                idxstorage.AddKey(idxKeys,idxInheritedKeys,DataRef);


              end else
              begin
                // find the column and put the appropriate key
                for index4 := 0 to length(Keys) - 1 do
                  idxstorage.AddInheritedKey(idxkeys,idxstorage.InheritedKeycolName(index4+IKShift-1),keys[index4]);
              end
          end
      until DataRef[0] = nullDataValue;
    end;
*)
end;

