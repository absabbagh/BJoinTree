function compareBooleans(comparaison_op: Integer;
                         var runstk: runstacktype): boolean;
begin
  result := true;
  case comparaison_op of

    50: runstk[High(runstk)-1].boolValue :=
          runstk[High(runstk)-1].boolValue = runstk[High(runstk)].boolValue;

    51: runstk[High(runstk)-1].boolValue := not runstk[High(runstk)-1].boolValue and
                                                 runstk[High(runstk)].boolValue;

    52: runstk[High(runstk)-1].boolValue := runstk[High(runstk)-1].boolValue and
                                                 not runstk[High(runstk)].boolValue;

    53: runstk[High(runstk)-1].boolValue :=
          runstk[High(runstk)-1].boolValue <> runstk[High(runstk)].boolValue;

    54:  runstk[High(runstk)-1].boolValue := not runstk[High(runstk)-1].boolValue or
                                                   (runstk[High(runstk)-1].boolValue and
                                                   runstk[High(runstk)].boolValue);

    55:  runstk[High(runstk)-1].boolValue := runstk[High(runstk)-1].boolValue or
                                                   (not runstk[High(runstk)-1].boolValue and
                                                   not runstk[High(runstk)].boolValue);

  end;
  runstk[High(runstk)-1].caseValue := 0;
  setLength(runstk,Length(runstk)-1)
end;

function runStack(conditionInstruction: singleInstructiontype;
                  var runstk: runstacktype): boolean;
var
  stDate: string;
begin
  result := true;
  case conditionInstruction.mnemonic of
    83: // ADD
       begin
         case runstk[high(runstk)].caseValue of // can be seen false as 0 and true as 1
             0: case runstk[High(runstk)-1].caseValue of
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end
                  else
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end
                end;
             4: case runstk[High(runstk)-1].caseValue of
                  0:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  4:
                    begin
                      runstk[High(runstk)-1].caseValue := 4;
                      runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue +
                                                               runstk[High(runstk)].extValue;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  6:
                    begin
                      try
                        runstk[High(runstk)-1].caseValue := 4;
                        runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) +
                                                                 runstk[High(runstk)].extValue;
                      except
                        runstk[High(runstk)-1].caseValue := 6;
                        runstk[High(runstk)-1].strValue := runstk[High(runstk)-1].strValue +
                                                                 floatToStr(runstk[High(runstk)].extValue);
                      end;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8:
                    begin
                      runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue +
                                   runstk[High(runstk)].extValue;
                      runstk[High(runstk)-1].caseValue := 8;
                      setLength(runstk,Length(runstk)-1);
                    end;
                end;
             6:
                case runstk[High(runstk)-1].caseValue of
                  0:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  4:
                    begin
                      try
                        runstk[High(runstk)-1].caseValue := 4;
                        runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue +
                                                                 strToFloat(runstk[High(runstk)].strValue);
                      except
                        runstk[High(runstk)-1].caseValue := 6;
                        runstk[High(runstk)-1].strValue := floatToStr(runstk[High(runstk)-1].extValue) +
                                                                 runstk[High(runstk)].strValue;
                      end;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  6:
                    begin
                      runstk[High(runstk)-1].caseValue := 6;
                      runstk[High(runstk)-1].strValue := runstk[High(runstk)-1].strValue +
                                                               runstk[High(runstk)].strValue;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8:
                    begin
                      try
                        runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue +
                                                                 strToFloat(runstk[High(runstk)].strValue);
                        runstk[High(runstk)-1].caseValue := 8;
                      except
                        runstk[High(runstk)-1].caseValue := 6; // use decodedate better than datetimetostr
                        runstk[High(runstk)-1].strValue := DateTimeToStr(runstk[High(runstk)-1].extValue) +
                                                                 runstk[High(runstk)].strValue;
                      end;
                      setLength(runstk,Length(runstk)-1)

                    end;
                end;
             7:
                begin
                  runstk[High(runstk)-1].caseValue := 7;
                  setLength(runstk,Length(runstk)-1);
                end;
             8:
                case runstk[High(runstk)-1].caseValue of
                  0:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  4:
                    begin
                      runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue +
                                   runstk[High(runstk)].extValue;
                      runstk[High(runstk)-1].caseValue := 8;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  6:
                    begin
                      try
                        runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) +
                                                                 runstk[High(runstk)].extValue;
                        runstk[High(runstk)-1].caseValue := 8;
                      except
                        runstk[High(runstk)-1].caseValue := 6; // use decodedate better than datetimetostr
                        runstk[High(runstk)-1].strValue := runstk[High(runstk)-1].strValue +
                                                                 DateTimeToStr(runstk[High(runstk)].extValue);
                      end;
                      setLength(runstk,Length(runstk)-1)

                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                end;
         end;
       end;

    84: // SUB
       begin
         case runstk[high(runstk)].caseValue of
            0: case runstk[High(runstk)-1].caseValue of
                 7:
                   begin
                     runstk[High(runstk)-1].caseValue := 7;
                     setLength(runstk,Length(runstk)-1);
                   end
                 else
                   begin
                     yyerror('Type error in select clause ');
                     runstk[High(runstk)-1].caseValue := 7;
                     setLength(runstk,Length(runstk)-1);
                     result := false;
                   end
               end;
             4: case runstk[High(runstk)-1].caseValue of
                 0:
                   begin
                     yyerror('Type error in select clause ');
                     runstk[High(runstk)-1].caseValue := 7;
                     setLength(runstk,Length(runstk)-1);
                     result := false;
                   end;
                  4:
                    begin
                      runstk[High(runstk)-1].caseValue := 4;
                      runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue -
                                                               runstk[High(runstk)].extValue;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  6:
                    begin
                      try
                        runstk[High(runstk)-1].caseValue := 4;
                        runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) -
                                                                 runstk[High(runstk)].extValue;
                      except
                        runstk[High(runstk)-1].caseValue := 6;
                        runstk[High(runstk)].strValue := floatToStr(runstk[High(runstk)].extValue);
                        if pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) <> 0 then
                          runstk[High(runstk)-1].strValue :=
                            copy (runstk[High(runstk)-1].strValue,1,
                              pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue)-1) +
                            copy (runstk[High(runstk)-1].strValue,
                              pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) +
                              length(runstk[High(runstk)].strValue),length(runstk[High(runstk)-1].strValue))  ;
                      end;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8:
                    begin
                      runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue -
                                   runstk[High(runstk)].extValue;
                      runstk[High(runstk)-1].caseValue := 8;
                      setLength(runstk,Length(runstk)-1);
                    end;
                end;
             6:
                case runstk[High(runstk)-1].caseValue of
                  0:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  4:
                    begin
                      try
                        runstk[High(runstk)-1].caseValue := 4;
                        runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue -
                                                                 strToFloat(runstk[High(runstk)].strValue);
                      except
                        runstk[High(runstk)-1].caseValue := 6;
                        runstk[High(runstk)-1].strValue := floatToStr(runstk[High(runstk)-1].extValue);
                        if pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) <> 0 then
                          runstk[High(runstk)-1].strValue :=
                            copy (runstk[High(runstk)-1].strValue,1,
                              pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue)-1) +
                            copy (runstk[High(runstk)-1].strValue,
                              pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) +
                              length(runstk[High(runstk)].strValue),length(runstk[High(runstk)-1].strValue))  ;
                      end;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  6:
                    begin
                      runstk[High(runstk)-1].caseValue := 6;
                      if pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) <> 0 then
                        runstk[High(runstk)-1].strValue :=
                          copy (runstk[High(runstk)-1].strValue,1,
                            pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue)-1) +
                          copy (runstk[High(runstk)-1].strValue,
                            pos(runstk[High(runstk)].strValue, runstk[High(runstk)-1].strValue) +
                            length(runstk[High(runstk)].strValue),length(runstk[High(runstk)-1].strValue))  ;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8:
                    begin
                      try
                        runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue -
                                                                 strToFloat(runstk[High(runstk)].strValue);
                        runstk[High(runstk)-1].caseValue := 8;
                      except
                        runstk[High(runstk)-1].caseValue := 6; // use decodedate better than datetimetostr
                        stdate := DateTimeToStr(runstk[High(runstk)-1].extValue);

                        if pos(runstk[High(runstk)].strValue, stdate) <> 0 then
                          runstk[High(runstk)-1].strValue :=
                            copy (stdate,1,
                              pos(runstk[High(runstk)].strValue, stdate)-1) +
                            copy (stdate,
                              pos(runstk[High(runstk)].strValue, stdate) +
                              length(runstk[High(runstk)].strValue),length(stdate))
                          else runstk[High(runstk)-1].strValue := stdate
                      end;
                      setLength(runstk,Length(runstk)-1)
                    end;
                end;
             7:
                begin
                  runstk[High(runstk)-1].caseValue := 7;
                  setLength(runstk,Length(runstk)-1);
                end;
             8:
                case runstk[High(runstk)-1].caseValue of
                  0:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  4:
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                  6:
                    begin
                      runstk[High(runstk)-1].caseValue := 6; // use decodedate better than datetimetostr
                      stdate := DateTimeToStr(runstk[High(runstk)].extValue);

                      if pos(stDate, runstk[High(runstk)-1].strValue) <> 0 then
                        runstk[High(runstk)-1].strValue :=
                          copy (runstk[High(runstk)-1].strValue,1,
                            pos(stDate, runstk[High(runstk)-1].strValue)-1) +
                          copy (runstk[High(runstk)-1].strValue,
                            pos(stDate, runstk[High(runstk)-1].strValue) +
                            length(stDate),length(runstk[High(runstk)-1].strValue))
                        else runstk[High(runstk)-1].strValue := runstk[High(runstk)-1].strValue;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7:
                    begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                    end;
                  8: // You can subtract two DATE values, and the result is a FLOAT 
                     // which is the number of days between the two DATE values.
                     // In general, the result may contain a fraction because DATE also has a time component.
                    begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                end;
         end;
       end;

    85: // MUL
       begin
        case runstk[high(runstk)].caseValue of
            0: begin
                 yyerror('Type error in select clause ');
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;
            4: case runstk[High(runstk)-1].caseValue of
                0: begin
                     yyerror('Type error in select clause ');
                     runstk[High(runstk)-1].caseValue := 7;
                     setLength(runstk,Length(runstk)-1);
                     result := false;
                   end;
                 4:
                   begin
                     runstk[High(runstk)-1].caseValue := 4;
                     runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue *
                                                              runstk[High(runstk)].extValue;
                     setLength(runstk,Length(runstk)-1)
                   end;
                 6:
                   begin
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) *
                                                                runstk[High(runstk)].extValue;
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
               end;
            6:
               case runstk[High(runstk)-1].caseValue of
                 0: begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
                 4:
                   begin
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue *
                                                                strToFloat(runstk[High(runstk)].strValue);
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
                 6:
                   begin
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) *
                                                                strToFloat(runstk[High(runstk)].strValue);
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end
               end;
            7:
              begin
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
              end;
            8:
              begin
                yyerror('Type error in select clause ');
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
                result := false;
              end;
        end;
      end;

    86: // DIV
      begin
        case runstk[high(runstk)].caseValue of
            0: begin
                 yyerror('Type error in select clause ');
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;
            4: case runstk[High(runstk)-1].caseValue of
                 4:
                   begin
                     if (runstk[High(runstk)].extValue = 0.0) then // Infiniti
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end else
                       begin
                         runstk[High(runstk)-1].caseValue := 4;
                         runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue /
                                                                  runstk[High(runstk)].extValue;
                         setLength(runstk,Length(runstk)-1)
                       end;
                   end;
                 6:
                   begin
                     if (runstk[High(runstk)].extValue = 0.0) then // Infiniti
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) /
                                                                runstk[High(runstk)].extValue;
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
               end;
            6:
               case runstk[High(runstk)-1].caseValue of
                 4:
                   begin
                     if strToFloatDef(runstk[High(runstk)].strValue,0.0) = 0.0 then // Infiniti
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := runstk[High(runstk)-1].extValue /
                                                                strToFloat(runstk[High(runstk)].strValue);
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
                 6:
                   begin
                     if strToFloatDef(runstk[High(runstk)].strValue,0.0) = 0.0 then // Infiniti
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := strToFloat(runstk[High(runstk)-1].strValue) /
                                                                strToFloat(runstk[High(runstk)].strValue);
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end
               end;
            7:
              begin
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
              end;
            8:
              begin
                yyerror('Type error in select clause ');
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
                result := false;
              end;
        end;
      end;

    95: // MOD
      begin
        case runstk[high(runstk)].caseValue of
            0: begin
                 yyerror('Type error in select clause ');
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;
            4: case runstk[High(runstk)-1].caseValue of
                 4:
                   begin
                     if (runstk[High(runstk)].extValue = 0.0) then
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end else
                       begin
                         runstk[High(runstk)-1].caseValue := 4;
                         runstk[High(runstk)-1].extValue := trunc(runstk[High(runstk)-1].extValue) MOD
                                                                  trunc(runstk[High(runstk)].extValue);
                         setLength(runstk,Length(runstk)-1)
                       end;
                   end;
                 6:
                   begin
                     if (runstk[High(runstk)].extValue = 0.0) then
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := trunc(strToFloat(runstk[High(runstk)-1].strValue)) MOD
                                                                trunc(runstk[High(runstk)].extValue);
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
               end;
            6:
               case runstk[High(runstk)-1].caseValue of
                 4:
                   begin
                     if strToFloatDef(runstk[High(runstk)].strValue,0.0) = 0.0 then
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := trunc(runstk[High(runstk)-1].extValue) MOD
                                                                trunc(strToFloat(runstk[High(runstk)].strValue));
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end;
                 6:
                   begin
                     if strToFloatDef(runstk[High(runstk)].strValue,0.0) = 0.0 then
                       begin
                         yyerror('Type error in select clause ');
                         runstk[High(runstk)-1].caseValue := 7;
                         setLength(runstk,Length(runstk)-1);
                         result := false;
                       end;
                     try
                       runstk[High(runstk)-1].caseValue := 4;
                       runstk[High(runstk)-1].extValue := trunc(strToFloat(runstk[High(runstk)-1].strValue)) MOD
                                                                trunc(strToFloat(runstk[High(runstk)].strValue));
                       setLength(runstk,Length(runstk)-1)
                     except
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                   end
               end;
            7:
              begin
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
              end;
            8:
              begin
                yyerror('Type error in select clause ');
                runstk[High(runstk)-1].caseValue := 7;
                setLength(runstk,Length(runstk)-1);
                result := false;
              end;
        end;
      end;

    79: // UMINUS
      begin
        case runstk[high(runstk)].caseValue of
            0: begin
                 yyerror('Type error in select clause ');
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;

            4: runstk[High(runstk)].extValue := -runstk[High(runstk)].extValue;

            6: begin
                 try
                   runstk[High(runstk)-1].caseValue := 4;
                   runstk[High(runstk)-1].extValue := - strToFloat(runstk[High(runstk)-1].strValue);
                 except
                   yyerror('Type error in select clause ');
                   runstk[High(runstk)-1].caseValue := 7;
                   setLength(runstk,Length(runstk)-1);
                   result := false;
                 end;
               end;

            7: begin
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;

            8: begin
                 yyerror('Type error in select clause ');
                 runstk[High(runstk)-1].caseValue := 7;
                 setLength(runstk,Length(runstk)-1);
                 result := false;
               end;
        end;
      end;

    88: // PUSH: value Extended Value
      begin
      setLength(runstk,Length(runstk)+1);
      runstk[High(runstk)].caseValue := 4;
      runstk[High(runstk)].extValue := conditionInstruction.value
    end;

    89: // PUSH: literal
      begin
      setLength(runstk,Length(runstk)+1);
      runstk[High(runstk)].caseValue := 6;
      runstk[High(runstk)].strvalue := conditionInstruction.stvalue;
    end;

   221: // PUSH: boolean
      begin
        setLength(runstk,Length(runstk)+1);
        runstk[High(runstk)].caseValue := 0;
        runstk[High(runstk)].boolvalue := conditionInstruction.boolvalue;
      end;

    50, 51, 52, 53, 54, 55: // EQ, LT, GT, NE, LE, GE
      begin
        case runstk[high(runstk)].caseValue of
          0: begin
               case runstk[high(runstk)-1].caseValue of
                 0: begin
                      compareBooleans(conditionInstruction.mnemonic,runstk);
                    end;
                 4: begin
                       if runstk[High(runstk)-1].extValue < 0  then
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end else
                         begin
                           runstk[High(runstk)-1].caseValue := 0;
                           runstk[High(runstk)-1].boolValue := runstk[High(runstk)-1].extValue > 0;

                           compareBooleans(conditionInstruction.mnemonic,runstk);
                         end;
                    end;
                 6: begin
                      try
                        if trim(uppercase(runstk[High(runstk)-1].strValue)) = 'TRUE' then
                          begin
                            runstk[High(runstk)-1].caseValue := 0;
                            runstk[High(runstk)-1].boolValue := true
                          end else
                          if trim(uppercase(runstk[High(runstk)-1].strValue)) = 'FALSE' then
                            begin
                              runstk[High(runstk)-1].caseValue := 0;
                              runstk[High(runstk)-1].boolValue := false
                            end else
                            begin
                              if strTofloat(runstk[High(runstk)-1].strValue) > 0 then
                                begin
                                  runstk[High(runstk)-1].caseValue := 0;
                                  runstk[High(runstk)-1].boolValue := true
                                end else
                                if strTofloat(runstk[High(runstk)-1].strValue) = 0 then
                                begin
                                  runstk[High(runstk)-1].caseValue := 0;
                                  runstk[High(runstk)-1].boolValue := false
                                end else
                                begin
                                  yyerror('Type error in select clause ');
                                  result := false;
                                  exit;
                                end;
                            end;
                        compareBooleans(conditionInstruction.mnemonic,runstk);
                      except
                        yyerror('Type error in select clause ');
                        result := false;
                        exit;
                      end;
                    end;
                 7: begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;

                 8: begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
               end;
             end;
          4: begin
               case runstk[high(runstk)-1].caseValue of
                 0: begin
                     if runstk[High(runstk)].extValue < 0  then
                       begin
                         yyerror('Type error in select clause ');
                         result := false;
                         exit;
                       end else
                       begin
                         runstk[High(runstk)].caseValue := 0;
                         runstk[High(runstk)].boolValue := runstk[High(runstk)].extValue > 0;
                         compareBooleans(conditionInstruction.mnemonic,runstk);
                       end;
                    end;
                 4: begin
                      case conditionInstruction.mnemonic of
                        50: runstk[High(runstk)-1].boolValue :=
                              runstk[High(runstk)-1].extValue = runstk[High(runstk)].extValue;

                        51: runstk[High(runstk)-1].boolValue :=
                              runstk[High(runstk)-1].extValue < runstk[High(runstk)].extValue;

                        52: runstk[High(runstk)-1].boolValue :=
                              runstk[High(runstk)-1].extValue > runstk[High(runstk)].extValue;

                        53: runstk[High(runstk)-1].boolValue :=
                              runstk[High(runstk)-1].extValue <> runstk[High(runstk)].extValue;

                        54: runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].extValue <= runstk[High(runstk)].extValue;

                        55:  runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].extValue >= runstk[High(runstk)].extValue;
                      end;
                      runstk[High(runstk)-1].caseValue := 0;
                      setLength(runstk,Length(runstk)-1)
                    end;
                 6: begin
                      try
                        runstk[High(runstk)-1].extValue := strTofloat(runstk[High(runstk)-1].strValue);
                        runstk[High(runstk)-1].caseValue := 4;
                        case conditionInstruction.mnemonic of
                          50: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue = runstk[High(runstk)].extValue;

                          51: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue < runstk[High(runstk)].extValue;

                          52: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue > runstk[High(runstk)].extValue;

                          53: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue <> runstk[High(runstk)].extValue;

                          54: runstk[High(runstk)-1].boolValue :=
                                 runstk[High(runstk)-1].extValue <= runstk[High(runstk)].extValue;

                          55:  runstk[High(runstk)-1].boolValue :=
                                 runstk[High(runstk)-1].extValue >= runstk[High(runstk)].extValue;
                        end;
                        runstk[High(runstk)-1].caseValue := 0;
                        setLength(runstk,Length(runstk)-1)
                      except
                        yyerror('Type error in select clause ');
                        result := false;
                        exit;
                      end
                    end;
                 7: begin
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;

                 8: begin
                      yyerror('Type error in select clause ');
                      runstk[High(runstk)-1].caseValue := 7;
                      setLength(runstk,Length(runstk)-1);
                      result := false;
                    end;
               end;
             end;
          6: begin
               case runstk[high(runstk)-1].caseValue of
                 0: begin
                      try
                        if strTofloat(runstk[High(runstk)].strValue) > 0 then
                          begin
                            runstk[High(runstk)].caseValue := 0;
                            runstk[High(runstk)].boolValue := true
                          end else
                          if strTofloat(runstk[High(runstk)].strValue) = 0 then
                          begin
                            runstk[High(runstk)].caseValue := 0;
                            runstk[High(runstk)].boolValue := false
                          end else
                          begin
                            yyerror('Type error in select clause ');
                            result := false;
                            exit;
                          end;
                        compareBooleans(conditionInstruction.mnemonic,runstk);
                      except
                        yyerror('Type error in select clause ');
                        result := false;
                        exit;
                      end;
                    end;
                 4: begin
                      try
                        runstk[High(runstk)].caseValue := 4;
                        runstk[High(runstk)].extValue := strTofloat(runstk[High(runstk)].strValue);

                        case conditionInstruction.mnemonic of
                          50: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue = runstk[High(runstk)].extValue;

                          51: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue < runstk[High(runstk)].extValue;

                          52: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue > runstk[High(runstk)].extValue;

                          53: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].extValue <> runstk[High(runstk)].extValue;

                          54: runstk[High(runstk)-1].boolValue :=
                                 runstk[High(runstk)-1].extValue <= runstk[High(runstk)].extValue;

                          55:  runstk[High(runstk)-1].boolValue :=
                                 runstk[High(runstk)-1].extValue >= runstk[High(runstk)].extValue;
                        end;
                        runstk[High(runstk)-1].caseValue := 0;
                        setLength(runstk,Length(runstk)-1)
                      except
                        yyerror('Type error in select clause ');
                        result := false;
                        exit;
                      end;
                    end;
                  6: begin
                       case conditionInstruction.mnemonic of
                         50: runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].strValue = runstk[High(runstk)].strValue;

                         51: runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].strValue < runstk[High(runstk)].strValue;

                         52: runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].strValue > runstk[High(runstk)].strValue;

                         53: runstk[High(runstk)-1].boolValue :=
                               runstk[High(runstk)-1].strValue <> runstk[High(runstk)].strValue;

                         54: runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].strValue <= runstk[High(runstk)].strValue;

                         55:  runstk[High(runstk)-1].boolValue :=
                                runstk[High(runstk)-1].strValue >= runstk[High(runstk)].strValue;
                       end;
                      runstk[High(runstk)-1].caseValue := 0;
                      setLength(runstk,Length(runstk)-1)
                    end;
                  7: begin
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
                  8: begin
                       // try to see if the string is a date if not we take the date as a string
                       yyerror('Type error in select clause ');
                       runstk[High(runstk)-1].caseValue := 7;
                       setLength(runstk,Length(runstk)-1);
                       result := false;
                     end;
               end;
             end;
        end;
      end;

    42: // NOT
      begin
        case runstk[High(runstk)].caseValue of
          0: runstk[high(runstk)].boolValue := not runstk[high(runstk)].boolValue;
          4: if runstk[high(runstk)].extValue > 0 then
               begin
                 runstk[high(runstk)].boolValue := false;
                 runstk[High(runstk)].caseValue := 0;
               end else
               if runstk[high(runstk)].extValue = 0 then
                 begin
                   runstk[high(runstk)].boolValue := true;
                   runstk[High(runstk)].caseValue := 0;
                 end else
                 begin
                   yyerror('Type error in select clause ');
                   result := false;
                   exit;
                 end;
          6: try
               if (strToFloat(runstk[high(runstk)].strValue) > 0) then
                 begin
                   runstk[high(runstk)].boolValue := false;
                   runstk[High(runstk)].caseValue := 0;
                 end else
                 if (strToFloat(runstk[high(runstk)].strValue) = 0) then
                   begin
                     runstk[high(runstk)].boolValue := true;
                     runstk[High(runstk)].caseValue := 0;
                   end else
                   begin
                     yyerror('Type error in select clause ');
                     result := false;
                     exit;
                   end;
             except
               yyerror('Type error in select clause ');
               result := false;
               exit;
             end
        end
      end;

    43: // OR
      begin
        case runstk[High(runstk)].caseValue of
          0: case runstk[High(runstk)-1].caseValue of
                0: begin
                     runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue or
                                                       runstk[high(runstk)].boolValue;
                     setlength(runstk,length(runstk)-1);
                   end;
                4: begin
                     if runstk[high(runstk)-1].extValue > 0 then
                       begin
                         runstk[high(runstk)-1].boolValue := true;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if runstk[high(runstk)-1].extValue = 0 then
                         begin
                           runstk[high(runstk)-1].boolValue := runstk[high(runstk)].boolValue;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                    end;
                6: begin
                     try
                       if (strToFloat(runstk[high(runstk)-1].strValue) > 0) then
                          begin
                            runstk[high(runstk)-1].boolValue := true;
                            runstk[High(runstk)-1].caseValue := 0;
                            setlength(runstk,length(runstk)-1);
                          end else
                          if (strToFloat(runstk[high(runstk)-1].strValue) = 0) then
                             begin
                               runstk[high(runstk)-1].boolValue := runstk[high(runstk)].boolValue;
                               runstk[High(runstk)-1].caseValue := 0;
                               setlength(runstk,length(runstk)-1);
                             end else
                             begin
                               yyerror('Type error in select clause ');
                               result := false;
                               exit;
                             end;
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
             end;
          4: case runstk[High(runstk)-1].caseValue of
                0: begin
                     if runstk[high(runstk)].extValue > 0 then
                       begin
                         runstk[high(runstk)].boolValue := true;
                         runstk[High(runstk)].caseValue := 0;
                         runstk[high(runstk)-1].boolValue := true;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if runstk[high(runstk)].extValue = 0 then
                         begin
                           runstk[high(runstk)].boolValue := false;
                           runstk[High(runstk)].caseValue := 0;
                           runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                     setlength(runstk,length(runstk)-1);
                   end;
                4: begin
                     if ((runstk[high(runstk)-1].extValue > 0) and
                         (runstk[high(runstk)].extValue >= 0)) or
                        ((runstk[high(runstk)].extValue > 0) and
                         (runstk[high(runstk)-1].extValue >= 0)) then
                       begin
                         runstk[high(runstk)-1].boolValue := true;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if (runstk[high(runstk)].extValue = 0) and (runstk[high(runstk)-1].extValue = 0) then
                         begin
                           runstk[high(runstk)-1].boolValue := false;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                   end;
                6: begin
                     try
                       if ((strToFloat(runstk[high(runstk)-1].strValue) > 0) and
                           (runstk[high(runstk)].extValue >= 0)) or
                          ((runstk[high(runstk)].extValue > 0) and
                           (strToFloat(runstk[high(runstk)-1].strValue) >= 0)) then
                         begin
                           runstk[high(runstk)-1].boolValue := true;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (runstk[high(runstk)].extValue = 0) and (strToFloat(runstk[high(runstk)-1].strValue) = 0) then
                           begin
                             runstk[high(runstk)-1].boolValue := false;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;

                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
             end;

          6: case runstk[High(runstk)-1].caseValue of
                0: begin
                     try
                       if (strToFloat(runstk[high(runstk)].strValue) > 0) then
                         begin
                           runstk[high(runstk)].boolValue := true;
                           runstk[High(runstk)].caseValue := 0;
                           runstk[high(runstk)-1].boolValue := true;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (strToFloat(runstk[high(runstk)].strValue) = 0) then
                           begin
                             runstk[high(runstk)].boolValue := false;
                             runstk[High(runstk)].caseValue := 0;
                             runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;
                       setlength(runstk,length(runstk)-1);
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
                4: begin
                     try
                       if ((runstk[high(runstk)-1].extValue > 0) and
                           ((strToFloat(runstk[high(runstk)].strValue) >= 0))) or
                          (((strToFloat(runstk[high(runstk)].strValue) > 0) and
                           (runstk[high(runstk)-1].extValue >= 0))) then
                         begin
                           runstk[high(runstk)-1].boolValue := true;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (strToFloat(runstk[high(runstk)].strValue) = 0) and (runstk[high(runstk)-1].extValue = 0) then
                           begin
                             runstk[high(runstk)-1].boolValue := false;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
                6: begin
                    try
                      if ((strToFloat(runstk[high(runstk)-1].strValue) > 0) and
                          ((strToFloat(runstk[high(runstk)].strValue) >= 0))) or
                         (((strToFloat(runstk[high(runstk)].strValue) > 0) and
                          (strToFloat(runstk[high(runstk)-1].strValue) >= 0))) then
                        begin
                          runstk[high(runstk)-1].boolValue := true;
                          runstk[High(runstk)-1].caseValue := 0;
                          setlength(runstk,length(runstk)-1);
                        end else
                        if (strToFloat(runstk[high(runstk)].strValue) = 0) and (strToFloat(runstk[high(runstk)-1].strValue) = 0) then
                          begin
                            runstk[high(runstk)-1].boolValue := false;
                            runstk[High(runstk)-1].caseValue := 0;
                            setlength(runstk,length(runstk)-1);
                          end else
                          begin
                            yyerror('Type error in select clause ');
                            result := false;
                            exit;
                          end;
                    except
                      yyerror('Type error in select clause ');
                      result := false;
                      exit;
                    end
                end;
             end;
        end;
      end;

    44: // AND
      begin
        case runstk[High(runstk)].caseValue of
          0: case runstk[High(runstk)-1].caseValue of
                0: begin
                     runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue and
                                                       runstk[high(runstk)].boolValue;
                     setlength(runstk,length(runstk)-1);
                   end;
                4: begin
                     if runstk[high(runstk)-1].extValue > 0 then
                       begin
                         runstk[high(runstk)-1].boolValue := runstk[high(runstk)].boolValue;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if runstk[high(runstk)-1].extValue = 0 then
                         begin
                           runstk[high(runstk)-1].boolValue := false;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                    end;
                6: begin
                     try
                       if (strToFloat(runstk[high(runstk)-1].strValue) > 0) then
                          begin
                            runstk[high(runstk)-1].boolValue := runstk[high(runstk)].boolValue;
                            runstk[High(runstk)-1].caseValue := 0;
                            setlength(runstk,length(runstk)-1);
                          end else
                          if (strToFloat(runstk[high(runstk)-1].strValue) = 0) then
                             begin
                               runstk[high(runstk)-1].boolValue := false;
                               runstk[High(runstk)-1].caseValue := 0;
                               setlength(runstk,length(runstk)-1);
                             end else
                             begin
                               yyerror('Type error in select clause ');
                               result := false;
                               exit;
                             end;
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
             end;
          4: case runstk[High(runstk)-1].caseValue of
                0: begin
                     if runstk[high(runstk)].extValue > 0 then
                       begin
                         runstk[high(runstk)].boolValue := true;
                         runstk[High(runstk)].caseValue := 0;
                         runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if runstk[high(runstk)].extValue = 0 then
                         begin
                           runstk[high(runstk)].boolValue := false;
                           runstk[High(runstk)].caseValue := 0;
                           runstk[high(runstk)-1].boolValue := false;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                     setlength(runstk,length(runstk)-1);
                   end;
                4: begin
                     if ((runstk[high(runstk)-1].extValue = 0) and
                         (runstk[high(runstk)].extValue >= 0)) or
                        ((runstk[high(runstk)].extValue = 0) and
                         (runstk[high(runstk)-1].extValue >= 0)) then
                       begin
                         runstk[high(runstk)-1].boolValue := false;
                         runstk[High(runstk)-1].caseValue := 0;
                         setlength(runstk,length(runstk)-1);
                       end else
                       if (runstk[high(runstk)].extValue > 0) and (runstk[high(runstk)-1].extValue > 0) then
                         begin
                           runstk[high(runstk)-1].boolValue := true;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         begin
                           yyerror('Type error in select clause ');
                           result := false;
                           exit;
                         end;
                   end;
                6: begin
                     try
                       if ((strToFloat(runstk[high(runstk)-1].strValue) = 0) and
                           (runstk[high(runstk)].extValue >= 0)) or
                          ((runstk[high(runstk)].extValue = 0) and
                           (strToFloat(runstk[high(runstk)-1].strValue) >= 0)) then
                         begin
                           runstk[high(runstk)-1].boolValue := false;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (runstk[high(runstk)].extValue = 0) and (strToFloat(runstk[high(runstk)-1].strValue) = 0) then
                           begin
                             runstk[high(runstk)-1].boolValue := true;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;

                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
             end;

          6: case runstk[High(runstk)-1].caseValue of
                0: begin
                     try
                       if (strToFloat(runstk[high(runstk)].strValue) > 0) then
                         begin
                           runstk[high(runstk)].boolValue := true;
                           runstk[High(runstk)].caseValue := 0;
                           runstk[high(runstk)-1].boolValue := runstk[high(runstk)-1].boolValue;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (strToFloat(runstk[high(runstk)].strValue) = 0) then
                           begin
                             runstk[high(runstk)].boolValue := false;
                             runstk[High(runstk)].caseValue := 0;
                             runstk[high(runstk)-1].boolValue := false;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;
                       setlength(runstk,length(runstk)-1);
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
                4: begin
                     try
                       if ((runstk[high(runstk)-1].extValue = 0) and
                           ((strToFloat(runstk[high(runstk)].strValue) >= 0))) or
                          (((strToFloat(runstk[high(runstk)].strValue) = 0) and
                           (runstk[high(runstk)-1].extValue >= 0))) then
                         begin
                           runstk[high(runstk)-1].boolValue := false;
                           runstk[High(runstk)-1].caseValue := 0;
                           setlength(runstk,length(runstk)-1);
                         end else
                         if (strToFloat(runstk[high(runstk)].strValue) > 0) and (runstk[high(runstk)-1].extValue > 0) then
                           begin
                             runstk[high(runstk)-1].boolValue := true;
                             runstk[High(runstk)-1].caseValue := 0;
                             setlength(runstk,length(runstk)-1);
                           end else
                           begin
                             yyerror('Type error in select clause ');
                             result := false;
                             exit;
                           end;
                     except
                       yyerror('Type error in select clause ');
                       result := false;
                       exit;
                     end
                   end;
                6: begin
                    try
                      if ((strToFloat(runstk[high(runstk)-1].strValue) = 0) and
                          ((strToFloat(runstk[high(runstk)].strValue) >= 0))) or
                         (((strToFloat(runstk[high(runstk)].strValue) = 0) and
                          (strToFloat(runstk[high(runstk)-1].strValue) >= 0))) then
                        begin
                          runstk[high(runstk)-1].boolValue := true;
                          runstk[High(runstk)-1].caseValue := 0;
                          setlength(runstk,length(runstk)-1);
                        end else
                        if (strToFloat(runstk[high(runstk)].strValue) > 0) and (strToFloat(runstk[high(runstk)-1].strValue) > 0) then
                          begin
                            runstk[high(runstk)-1].boolValue := false;
                            runstk[High(runstk)-1].caseValue := 0;
                            setlength(runstk,length(runstk)-1);
                          end else
                          begin
                            yyerror('Type error in select clause ');
                            result := false;
                            exit;
                          end;
                    except
                      yyerror('Type error in select clause ');
                      result := false;
                      exit;
                    end
                end;
             end;
        end;
      end;
  end;

end;

{$IFDEF mongodb}
function extractselect(dbUserId: string;
                       selectColsInstructions: selectColsInstructionstype;
                       resultTable: resulttabletype;
                       distinctCollection: TMongoCollection;
                       flagDistinctClause: boolean;
                       AggregateCollection: TMongoCollection;
                       flagAggregate: boolean;
                       var resultRows: Integer): Boolean;
var
  IBSONInstance: IBSONObject;
  DistinctIBSONInstance: IBSONObject;
  index1, index2, index3: Integer;
  isexpr: boolean;
  exprname: string;
  selectstk: runstacktype;
  ltblName: string;
  lcolname: string;
  found: boolean;
  aliasIndex: Integer;
  ObjId: string;
  ObjVal: string;
  lValueType: TBSONValueType;
  distinctCursor: IMongoDBCursor;
  index: Integer;
  colType :  inttype .. stringtype;
  flagTime, flagDate: Boolean;
  Year, Month, Day: Word;
  Hour, Minute, Second, Millisecond: Word;
begin
  result := true;
  selectstk := nil;
  if selectColsInstructions <> nil then
    begin
      if not  flagAggregate then
        if flagDistinctClause then
          DistinctIBSONInstance :=  TBSONObject.Create;
      IBSONInstance :=  TBSONObject.Create;
      IBSONInstance.Put('sys_queryId', lqueryId);
      IBSONInstance.Put('sys_user', dbuserId);
      IBSONInstance.Put('rowId', resultRows);
      for index2 := low(selectColsInstructions) to high(selectColsInstructions) do
      begin
        isExpr := false;
        exprname := 'Expr' + intToStr(index2+1);
        for index3 := low(selectColsInstructions[index2]) to high(selectColsInstructions[index2]) do
        begin
          if (selectColsInstructions[index2,index3].mnemonic in [42..44,50..55,79,83..86,88,89,221]) then
            begin
              runstack(selectColsInstructions[index2,index3],selectstk)
            end;
          if (selectColsInstructions[index2,index3].mnemonic <> 151) and
             (selectColsInstructions[index2,index3].mnemonic <> 177) then isExpr := true;
          case selectColsInstructions[index2,index3].mnemonic of

           179:
              begin
                exprname := selectColsInstructions[index2,index3].stValue;
              end;


           151: // COLUMN NAME
              begin
                ltblName := copy(selectColsInstructions[index2,index3].stvalue,1,pos('.',selectColsInstructions[index2,index3].stvalue)-1);
                lcolName := copy(selectColsInstructions[index2,index3].stvalue,pos('.',selectColsInstructions[index2,index3].stvalue)+1,length(selectColsInstructions[index2,index3].stvalue));

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
                colType :=  resultTable.resultFields.columns[index1].coltype;

                setlength(selectstk,length(selectstk) + 1);
                selectstk[High(selectstk)].caseValue := 4;
                if (colType = tdatetimetype) or
                   (colType = tdatetype) or
                   (colType = ttimetype) then
                  begin
                    selectstk[High(selectstk)].caseValue := 8;
                    flagTime := true;
                    flagDate := true;
                    if (colType = tdatetype) then
                      flagTime := false;
                    if (colType = ttimetype) then
                      flagDate := false;
                  end;

                lvalueType := resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                              resultTable.ownertable[index1].colname].ValueType;
                case lvalueType of
                  bvtNull:
                    begin
                      selectstk[High(selectstk)].caseValue := 7; // Null Value
                      selectstk[High(selectstk)].strValue := 'NULL';
                    end;
                  bvtBoolean:
                    begin
                      selectstk[High(selectstk)].caseValue := 0;
                      selectstk[High(selectstk)].boolValue :=
                        resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                                                              resultTable.ownertable[index1].colname].AsBoolean;
                    end;
                  bvtInteger:
                    begin
                      selectstk[High(selectstk)].extValue :=
                      resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                                                            resultTable.ownertable[index1].colname].AsInteger;
                    end;
                  bvtInt64:
                    begin
                      selectstk[High(selectstk)].extValue :=
                        resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                                                              resultTable.ownertable[index1].colname].AsInt64;
                    end;
                  bvtDouble:
                    begin
                      selectstk[High(selectstk)].extValue :=
                      resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                                                            resultTable.ownertable[index1].colname].AsFloat;
                    end;
                  bvtString:
                    begin
                      selectstk[High(selectstk)].caseValue := 6;
                      selectstk[High(selectstk)].strValue :=
                      resultTable.resultIBSONInstance.Items[resultTable.ownertable[index1].tblname + tableColumnSeperator +
                                                            resultTable.ownertable[index1].colname].AsString;
                    end;
                end;


              end;

             177: // SHOW COLUMN
                begin
                  if not isexpr then
                    with resulttable.ownertable[index1] do
                      exprname := tblname + tableColumnSeperator + colname;
                  // check for dulpicate

                  ObjId := exprName;

                  case selectstk[High(selectstk)].caseValue of
                    0: if selectstk[0].boolValue then
                          ObjVal := 'TRUE'
                        else
                          ObjVal := 'FALSE';
                    4: ObjVal := floatToStr( selectstk[0].extValue );
                    6: ObjVal := selectstk[0].strValue;
                    7: ObjVal := 'NULL';
                    8: begin
                         if flagDate then
                           begin
                             DecodeDate(selectstk[0].extValue,Year,Month,Day);
                             ObjVal := IntToStr(Year) + '/';
                             if Month < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(Month) + '/';
                             if day < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(day) + ' ';
                           end;
                         if flagTime then
                           begin
                             DecodeTime(selectstk[0].extValue,hour,minute,second,millisecond);
                             if hour < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + IntToStr(hour) + ':';
                             if minute < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(minute) + ':';
                             if second < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(second) + ':';
                             if millisecond < 10 then ObjVal := ObjVal + '00'
                              else
                               if millisecond < 100 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(millisecond);
                           end;
                         ObjVal := trim(ObjVal);
                       end;
                  end;
                  if not  flagAggregate then
                    if flagDistinctClause then
                      begin
                        DistinctIBSONInstance.Put(ObjId, ObjVal);
                      end;
                  IBSONInstance.Put(ObjId, ObjVal);
                  selectstk := nil;
                end;
          end;
        end;
      end;
    end;
  if flagDistinctClause and not flagAggregate then
    begin
      distinctCursor := distinctCollection.find(DistinctIBSONInstance);
      if distinctCursor.Count <> 0 then
        resultRows := resultRows - 1
       else
        begin
          DistinctIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
          distinctCollection.Insert(DistinctIBSONInstance);
          IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
          GCollection.Insert(IBSONInstance);
        end
    end else
    begin
      IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
      if flagAggregate then AggregateCollection.Insert(IBSONInstance) else GCollection.Insert(IBSONInstance);
    end;
end;
{$ENDIF}

{$IFDEF tablesdb}
function extractselect(dbUserId: string;
                       selectColsInstructions: selectColsInstructionstype;
                       var outText: TextFile;
                       resultTable: resulttabletype;
                       var resultRows: Integer): Boolean;
var
  selectstk: runstacktype;
  index1, index2, index3: Integer;
  isexpr: boolean;
  exprname: string;
  ltblName: string;
  lcolname: string;
  found: boolean;
  aliasIndex: Integer;
  colType :  inttype .. stringtype;
  flagTime, flagDate: Boolean;
  ObjId: string;
  ObjVal: string;
  Year, Month, Day: Word;
  Hour, Minute, Second, Millisecond: Word;
  colsIdtxt: string = '';
  colsValtxt: string = '';
  colstxt: string = '';
  SDFInstance: SDFClass;
  i: Integer;
  IdSDFInstance: SDFClass;
  ValSDFInstance: SDFClass;
begin
  result := true;
  selectstk := nil;
  if selectColsInstructions <> nil then
    begin
      {
      if not  flagAggregate then
        if flagDistinctClause then
          DistinctIBSONInstance :=  TBSONObject.Create;
      }
      // colsIdTxt := 'sys_queryId' + #8 + 'sys_user' + #8 + 'rowId' + #8;
      // colsValTxt := '"' + lqueryId + '"' + #8 + '"' + dbuserId + '"' + #8 + intToStr(resultRows) + #8;
      colsTxt := 'sys_queryId: ' + lqueryId + ' ' + 'sys_user: ' + dbUserId + ' ' + 'rowId: ' + intToStr(resultRows) + ' ';
      for index2 := low(selectColsInstructions) to high(selectColsInstructions) do
      begin
        isExpr := false;
        exprname := 'Expr' + intToStr(index2+1);
        for index3 := low(selectColsInstructions[index2]) to high(selectColsInstructions[index2]) do
        begin
          if (selectColsInstructions[index2,index3].mnemonic in [42..44,50..55,79,83..86,88,89,221]) then
            begin
              runstack(selectColsInstructions[index2,index3],selectstk)
            end;
          if (selectColsInstructions[index2,index3].mnemonic <> 151) and
             (selectColsInstructions[index2,index3].mnemonic <> 177) then isExpr := true;
          case selectColsInstructions[index2,index3].mnemonic of

           179:
              begin
                exprname := selectColsInstructions[index2,index3].stValue;
              end;


           151: // COLUMN NAME
              begin
                ltblName := copy(selectColsInstructions[index2,index3].stvalue,1,pos('.',selectColsInstructions[index2,index3].stvalue)-1);
                lcolName := copy(selectColsInstructions[index2,index3].stvalue,pos('.',selectColsInstructions[index2,index3].stvalue)+1,length(selectColsInstructions[index2,index3].stvalue));

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
                colType :=  resultTable.resultFields.columns[index1].coltype;

                setlength(selectstk,length(selectstk) + 1);
                selectstk[High(selectstk)].caseValue := 4;
                if (colType = tdatetimetype) or
                   (colType = tdatetype) or
                   (colType = ttimetype) then
                  begin
                    selectstk[High(selectstk)].caseValue := 8;
                    flagTime := true;
                    flagDate := true;
                    if (colType = tdatetype) then
                      flagTime := false;
                    if (colType = ttimetype) then
                      flagDate := false;
                  end;

                case colType of
                  inttype, smallinttype, int64type,
                  extendedtype, currencytype,
                  tdatetype, ttimetype, tdatetimetype:
                     begin
                       if resultTable.resultRow[index1]= null then
                         begin
                           selectstk[High(selectstk)].caseValue := 7; // Null Value
                           selectstk[High(selectstk)].strValue := 'NULL';
                         end else
                         selectstk[High(selectstk)].extValue := resultTable.resultRow[index1];
                     end;
                  booleantype:
                     begin
                       if resultTable.resultRow[index1]= null then
                         begin
                           selectstk[High(selectstk)].caseValue := 7; // Null Value
                           selectstk[High(selectstk)].strValue := 'NULL';
                         end else
                         begin
                           selectstk[High(selectstk)].caseValue := 0;
                           selectstk[High(selectstk)].boolValue := resultTable.resultRow[index1];
                         end
                     end;
                  stringtype:
                     begin
                       if resultTable.resultRow[index1]= null then
                         begin
                           selectstk[High(selectstk)].caseValue := 7; // Null Value
                           selectstk[High(selectstk)].strValue := 'NULL';
                         end else
                         begin
                           selectstk[High(selectstk)].caseValue := 6;
                           selectstk[High(selectstk)].strValue := resultTable.resultRow[index1];
                         end
                     end;
                end;

              end;

             177: // SHOW COLUMN
                begin
                  if not isexpr then
                    with resulttable.ownertable[index1] do
                      exprname := tblname + '.' {tableColumnSeperator} + colname;
                  // check for dulpicate

                  ObjId := exprName;

                  case selectstk[High(selectstk)].caseValue of
                    0: if selectstk[0].boolValue then
                          ObjVal := 'TRUE'
                        else
                          ObjVal := 'FALSE';
                    4: ObjVal := floatToStr( selectstk[0].extValue );
                    6: ObjVal := selectstk[0].strValue;
                    7: ObjVal := 'NULL';
                    8: begin
                         if flagDate then
                           begin
                             DecodeDate(selectstk[0].extValue,Year,Month,Day);
                             ObjVal := IntToStr(Year) + '/';
                             if Month < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(Month) + '/';
                             if day < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(day) + ' ';
                           end;
                         if flagTime then
                           begin
                             DecodeTime(selectstk[0].extValue,hour,minute,second,millisecond);
                             if hour < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + IntToStr(hour) + ':';
                             if minute < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(minute) + ':';
                             if second < 10 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(second) + ':';
                             if millisecond < 10 then ObjVal := ObjVal + '00'
                              else
                               if millisecond < 100 then ObjVal := ObjVal + '0';
                             ObjVal := ObjVal + intToStr(millisecond);
                           end;
                         ObjVal := trim(ObjVal);
                       end;
                  end;
                  {if not  flagAggregate then
                    if flagDistinctClause then
                      begin
                        DistinctIBSONInstance.Put(ObjId, ObjVal);
                      end;}
                  (*IBSONInstance.Put(ObjId, ObjVal);*)
                  colsIdTxt := colsIdTxt + objId + ',';
                  colsValTxt := colsValTxt + objVal + ',';

                  colsTxt := colsTxt + objId + ':' + ' ' + objVal + ' ';
                  selectstk := nil;
                end;
          end;
        end;
      end;
    end;
  {
  if flagDistinctClause and not flagAggregate then
    begin
      distinctCursor := distinctCollection.find(DistinctIBSONInstance);
      if distinctCursor.Count <> 0 then
        resultRows := resultRows - 1
       else
        begin
          DistinctIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
          distinctCollection.Insert(DistinctIBSONInstance);
          IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
          GCollection.Insert(IBSONInstance);
        end
    end else
    begin
      IBSONInstance.Put('_id', TBSONObjectId.NewFrom);
      if flagAggregate then AggregateCollection.Insert(IBSONInstance) else GCollection.Insert(IBSONInstance);
    end;
  }

  writeln(outText,colsTxt);

  SDFInstance := SDFClass.Create;
  if (resultRows = 1) then
    begin
      setLength(sqlResults, length(sqlResults) + 1);
      sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],'ROW ID',True);
      IdSDFInstance := SDFClass.Create;
      IdSDFInstance.GetLexemes(colsIdTxt);
      for i := 0 to IdSDFInstance.CountLexemes - 2 do
        sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],IdSDFInstance.Lexemes[i],True);
      IdSDFInstance.Free;
    end;

  setLength(sqlResults, length(sqlResults) + 1);
  sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],IntToStr(resultRows),True);

  ValSDFInstance := SDFClass.Create;
  ValSDFInstance.GetLexemes(colsValTxt);
  for i := 0 to ValSDFInstance.CountLexemes - 2 do
    sqlResults[high(sqlResults)] := SDFInstance.AddString(sqlResults[high(sqlResults)],ValSDFInstance.Lexemes[i],True);
  ValSDFInstance.Free;

  SDFInstance.Free;

end;
{$ENDIF}

