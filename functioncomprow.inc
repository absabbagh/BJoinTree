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
begin
  { #todo:  use executeplan or indexes directly to be sure about it }
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
      if lconditionInstructions[index2].mnemonic in
           [42..44,50..55,79,83..86,88,89,221,241,47,92..111,38,171..175] then
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

            { #todo : check for column_name don't exists in where clause. group by clause and having }

            setlength(runstk,length(runstk) + 1);
            runstk[High(runstk)].boolValue := false;
            runstk[High(runstk)].intValue := 0;
            runstk[High(runstk)].int64Value := 0;

              if resultTable.resultrow[index1] = null then
                begin
                  runstk[High(runstk)].caseValue := 7; // Null Value
                  runstk[High(runstk)].strValue := 'NULL';
                end else
                case resultTable.columns[index1].coltype of
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
           { #note : runstk[0] is the value
                     runstk[1] is the pattern
                     runstk[2] is the escape
           }
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
    begin
      if (runstk[High(runstk)].caseValue = 0) then
        begin
          if runstk[High(runstk)].boolValue then
            begin
              result := true;
            end;
        end else Parser.yyerror('syntax error ');
    end else Parser.yyerror('syntax error ');
  runstk := nil;

end;

{ #note : Analyze query make the condition postfix as in this case
          no index always true
          index should be verified
}

function analyzeQuery(conditionInstructions: array of singleInstructionType;
                      var executePlan: executePlanType): boolean;
var
  index1, index2: integer;
  runstk: runstacktype = nil;
  ltblName: string;
  lcolname: string;
  lconditionInstructions: array of singleInstructionType;
  tblFields: tblStructure;
  chEscape: char = ' ';
  stPattern: string;
  RPattern: string;
  RegexObj: TRegExpr;
  startFrom: Integer = 0;
begin
  setlength(lconditionInstructions, length(conditionInstructions));
  for index1 := low(conditionInstructions) to high(conditionInstructions) do
    lconditionInstructions[index1] := conditionInstructions[index1];

  result := false; // return false in case of scan tables

  if lconditionInstructions = nil then exit;

  // Check the Where Condition
  for index2 := low(lconditionInstructions) to high(lconditionInstructions) do
    begin
      if lconditionInstructions[index2].mnemonic in [42..44,50..55,79,83..86,88,89,221] then
        begin
          runStack(lconditionInstructions[index2],startFrom,runstk,executePlan);
        end;

      case lconditionInstructions[index2].mnemonic of
       151: // COLUMN NAME
          begin
            ltblName := copy(lconditionInstructions[index2].stvalue,1,pos('.',lconditionInstructions[index2].stvalue)-1);
            lcolName := copy(lconditionInstructions[index2].stvalue,pos('.',lconditionInstructions[index2].stvalue)+1,length(lconditionInstructions[index2].stvalue));

            //// search for indexes and fill them

            tblFields := loadTableFields(ltblName);

            setlength(runstk,length(runstk) + 1);
            runstk[High(runstk)].boolValue := false; // no index
            runstk[High(runstk)].intValue := -1; // index number
            // if index number exist but boolValue is false, so scan the table
            runstk[High(runstk)].int64Value := 0;
            runstk[high(runstk)].caseValue := 10; // column name
            runstk[high(runstk)].extValue := 1; // First occurrence of column name
            runstk[high(runstk)].strValue := ltblName + '.' + lcolName;

            for index1 := low(tblFields.idxdata) to high(tblFields.idxdata) do
              if tblFields.idxdata[index1].idxkeys[0].colName = lcolName then
                begin
                  executePlan.useIndex := true;
                  setlength(executePlan.Index,length(executePlan.Index) + 1);
                  executePlan.Index[High(executePlan.Index)].colName := lcolName;
                  executePlan.Index[High(executePlan.Index)].tblName  := ltblName;
                  executePlan.Index[High(executePlan.Index)].Number := index1;
                  executePlan.Index[High(executePlan.Index)].Name := tblFields.idxdata[index1].idxname;
                  runstk[high(runstk)].boolValue := true; // index found and in use
                  break
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
           { #note : runstk[0] is the value
                     runstk[1] is the pattern
                     runstk[2] is the escape
           }
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


