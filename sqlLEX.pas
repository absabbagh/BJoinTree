


function TLexer.parse() : integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

  var result : integer;

begin
  (* actions: *)
  case yyruleno of
  1:
                                begin
                                  val(yytext, yylval.yyExtended, result);
                                  if result = 0 then
                                    if (yylval.yyExtended >= low(Integer)) and (yylval.yyExtended <= high(Integer)) then
                                      begin
                                        yylval.yyInteger := Trunc(yylval.yyExtended);
                                        yylval.yyInt64 := yylval.yyInteger;
                                        return(NUM)
                                      end else
                                      if (yylval.yyExtended >= low(Int64)) and(yylval.yyExtended <= high(Int64)) then
                                        begin
                                          yylval.yyInt64 := Trunc(yylval.yyExtended);
                                          return(NUMBER)
                                        end
                                     else
                                      return(NUMBER)
				  else
				    return(ILLEGAL)
				end;

  2:
                             	begin
				  val(yytext, yylval.yyExtended, result);
				  if result=0 then
				    return(NUMBER)
				  else
				    return(ILLEGAL)
				end;

  3:
                                return(tknEQ);

  4:
                                return(tknLT);

  5:
                                return(tknGT);

  6:
                                return(tknNE);

  7:
                                return(tknLE);

  8:
                                return(tknGE);

  9:
                                                                     
                                begin
				  yylval.yyString := copy(yytext,2,length(yytext)-2);
                                  return(QUOTED_STRING);
                                end;

  10:
                                                                              
                                begin
				  yylval.yyString := copy(yytext,2,length(yytext)-2);
                                  return(QUOTED_STRING);
                                end;

  11:
                         
                                begin
				  yylval.yyString := yytext;
				  return(ID);
                                  if Uppercase(yytext) = 'FALSE' then
                                    begin
                                      yylval.yyBoolean := false;
                                      return(BOOL)
                                    end;
                                  if Uppercase(yytext) = 'TRUE' then
                                    begin
                                      yylval.yyBoolean := True;
                                      return(BOOL)
                                    end;
                                  if Uppercase(yytext) = 'CREATE' then return( tknCREATE );
                                  if Uppercase(yytext) = 'DROP' then return( tknDROP );
                                  if Uppercase(yytext) = 'DATABASE' then return( tknDATABASE );
                                  if Uppercase(yytext) = 'TABLE' then return( tknTABLE );
                                  if Uppercase(yytext) = 'INDEX' then return( tknINDEX );
                                  if Uppercase(yytext) = 'CHAR' then return( tknCHAR );
                                  if Uppercase(yytext) = 'VARCHAR' then return( tknVARCHAR );
                                  if Uppercase(yytext) = 'CHARACTER' then return( tknCHARACTER );
                                  if Uppercase(yytext) = 'VARYING' then return( tknVARYING );
                                  if Uppercase(yytext) = 'CLOB' then return( tknCLOB );
                                  if Uppercase(yytext) = 'DATE' then return( tknDATE );
                                  if Uppercase(yytext) = 'TIME' then return( tknTIME );
                                  if Uppercase(yytext) = 'TIMESTAMP' then return( tknTIMESTAMP );
                                  if Uppercase(yytext) = 'DATETIME' then return( tknDATETIME );
                                  if Uppercase(yytext) = 'WITH' then return( tknWITH );
                                  if Uppercase(yytext) = 'WITHOUT' then return( tknWITHOUT );
                                  if Uppercase(yytext) = 'ZONE' then return( tknZONE );
                                  if Uppercase(yytext) = 'NUMBER' then return( tknNUMBER );
                                  if Uppercase(yytext) = 'FLOAT' then return( tknFLOAT );
                                  if Uppercase(yytext) = 'REAL' then return( tknREAL );
                                  if Uppercase(yytext) = 'DOUBLE' then return( tknDOUBLE );
                                  if Uppercase(yytext) = 'PRECISION' then return( tknPRECISION );
                                  if Uppercase(yytext) = 'DECIMAL' then return( tknDECIMAL );
                                  if Uppercase(yytext) = 'DEC' then return( tknDEC );
                                  if Uppercase(yytext) = 'NUMERIC' then return( tknNUMERIC );
                                  if Uppercase(yytext) = 'INTEGER' then return( tknINTEGER );
                                  if Uppercase(yytext) = 'INT' then return( tknINT );
                                  if Uppercase(yytext) = 'SMALLINT' then return( tknSMALLINT );
                                  if Uppercase(yytext) = 'BIGINT' then return( tknBIGINT );
                                  if Uppercase(yytext) = 'BOOLEAN' then return( tknBOOLEAN );
                                  if Uppercase(yytext) = 'DEFAULT' then return( tknDEFAULT );
                                  if Uppercase(yytext) = 'CONSTRAINT' then return( tknCONSTRAINT );
                                  if Uppercase(yytext) = 'NULL' then return( tknNULL );
                                  if Uppercase(yytext) = 'NOT' then return( tknNOT );
                                  if Uppercase(yytext) = 'UNIQUE' then return( tknUNIQUE );
                                  if Uppercase(yytext) = 'PRIMARY' then return( tknPRIMARY );
                                  if Uppercase(yytext) = 'FOREIGN' then return( tknFOREIGN );
                                  if Uppercase(yytext) = 'KEY' then return( tknKEY );
                                  if Uppercase(yytext) = 'REFERENCES' then return( tknREFERENCES);
                                  if Uppercase(yytext) = 'CHECK' then return( tknCHECK);
                                  if Uppercase(yytext) = 'ON' then return( tknON);
                                  if Uppercase(yytext) = 'DELETE' then return( tknDELETE);
                                  if Uppercase(yytext) = 'CASCADE' then return( tknCASCADE);
                                  if Uppercase(yytext) = 'ASC' then return( tknASC );
                                  if Uppercase(yytext) = 'DESC' then return( tknDESC );
                                  if Uppercase(yytext) = 'COMMENT' then return( tknCOMMENT );
                                  if Uppercase(yytext) = 'COLUMN' then return( tknCOLUMN );
                                  if Uppercase(yytext) = 'SELECT' then return( tknSELECT );
                                  if Uppercase(yytext) = 'FROM' then return( tknFROM );
                                  if Uppercase(yytext) = 'ALL' then return( tknALL );
                                  if Uppercase(yytext) = 'DISTINCT' then return( tknDISTINCT );
                                  if Uppercase(yytext) = 'WHERE' then return( tknWHERE );
                                  if Uppercase(yytext) = 'OR' then return( tknOR );
                                  if Uppercase(yytext) = 'AND' then return( tknAND );
                                  if Uppercase(yytext) = 'IN' then return( tknIN );
                                  if Uppercase(yytext) = 'LIKE' then return( tknLIKE );
                                  if Uppercase(yytext) = 'BETWEEN' then return( tknBETWEEN );
                                  if Uppercase(yytext) = 'IS' then return( tknIS );
                                  if Uppercase(yytext) = 'EXISTS' then return( tknEXISTS );
                                  if Uppercase(yytext) = 'ANY' then return( tknANY );
                                  if Uppercase(yytext) = 'GROUP' then return( tknGROUP );
                                  if Uppercase(yytext) = 'BY' then return( tknBY );
                                  if Uppercase(yytext) = 'HAVING' then return( tknHAVING );
                                  if Uppercase(yytext) = 'UNION' then return( tknUNION );
                                  if Uppercase(yytext) = 'INTERSECT' then return( tknINTERSECT );
                                  if Uppercase(yytext) = 'MINUS' then return( tknMINUS );
                                  if Uppercase(yytext) = 'ORDER' then return( tknORDER );
                                  if Uppercase(yytext) = 'INSERT' then return( tknINSERT );
                                  if Uppercase(yytext) = 'INTO' then return( tknINTO );
                                  if Uppercase(yytext) = 'VALUES' then return( tknVALUES );
                                  if Uppercase(yytext) = 'UPDATE' then return( tknUPDATE );
                                  if Uppercase(yytext) = 'SET' then return( tknSET );
                                  if Uppercase(yytext) = 'ABS' then return( tknABS );
                                  if Uppercase(yytext) = 'CEIL' then return( tknCEIL );
                                  if Uppercase(yytext) = 'FLOOR' then return( tknFLOOR );
                                  if Uppercase(yytext) = 'MOD' then return( tknMOD );
                                  if Uppercase(yytext) = 'POWER' then return( tknPOWER );
                                  if Uppercase(yytext) = 'ROUND' then return( tknROUND );
                                  if Uppercase(yytext) = 'SIGN' then return( tknSIGN );
                                  if Uppercase(yytext) = 'SQRT' then return( tknSQRT );
                                  if Uppercase(yytext) = 'TRUNC' then return( tknTRUNC );
                                  if Uppercase(yytext) = 'CHR' then return( tknCHR );
                                  if Uppercase(yytext) = 'LPAD' then return( tknLPAD );
                                  if Uppercase(yytext) = 'LTRIM' then return( tknLTRIM );
                                  if Uppercase(yytext) = 'RPAD' then return( tknRPAD );
                                  if Uppercase(yytext) = 'RTRIM' then return( tknRTRIM );
                                  if Uppercase(yytext) = 'SOUNDEX' then return( tknSOUNDEX );
                                  if Uppercase(yytext) = 'SUBSTR' then return( tknSUBSTR );
                                  if Uppercase(yytext) = 'LENGTH' then return( tknLENGTH );
                                  if Uppercase(yytext) = 'TO_CHAR' then return( tknTO_CHAR );
                                  if Uppercase(yytext) = 'TO_DATE' then return( tknTO_DATE );
                                  if Uppercase(yytext) = 'TO_NUMBER' then return( tknTO_NUMBER );
                                  if Uppercase(yytext) = 'AVG' then return( tknAVG );
                                  if Uppercase(yytext) = 'COUNT' then return( tknCOUNT );
                                  if Uppercase(yytext) = 'MAX' then return( tknMAX );
                                  if Uppercase(yytext) = 'MIN' then return( tknMIN );
                                  if Uppercase(yytext) = 'STDDEV' then return( tknSTDDEV );
                                  if Uppercase(yytext) = 'SUM' then return( tknSUM );
                                  if Uppercase(yytext) = 'VARIANCE' then return( tknVARIANCE );
                                  if Uppercase(yytext) = 'TRIGGER' then return( tknTRIGGER );
                                  if Uppercase(yytext) = 'BEFORE' then return( tknBEFORE );
                                  if Uppercase(yytext) = 'AFTER' then return( tknAFTER );
                                  if Uppercase(yytext) = 'OF' then return( tknOF );
                                  if Uppercase(yytext) = 'FOR' then return( tknFOR );
                                  if Uppercase(yytext) = 'EACH' then return( tknEACH );
                                  if Uppercase(yytext) = 'ROW' then return( tknROW );
                                  if Uppercase(yytext) = 'WHEN' then return( tknWHEN );
                                  if Uppercase(yytext) = 'BEGIN' then return( tknBEGIN );
                                  if Uppercase(yytext) = 'END' then return( tknEND );
                                  if Uppercase(yytext) = 'JOIN' then return( tknJOIN );
                                  if Uppercase(yytext) = 'SYSTEM' then return( tknSYSTEM );
                                  if Uppercase(yytext) = 'DATABASES' then return( tknDATABASES );
                                  if Uppercase(yytext) = 'TABLES' then return( tknTABLES );
                                  if Uppercase(yytext) = 'INDEXES' then return( tknINDEXES );
                                  if Uppercase(yytext) = 'JOININDEXES' then return( tknJOININDEXES );
                                  if Uppercase(yytext) = 'COLUMNS' then return( tknCOLUMNS );
                                  if Uppercase(yytext) = 'CONSTRAINTS' then return( tknCONSTRAINTS );
                                  if Uppercase(yytext) = 'ALTER' then return( tknALTER );
                                  if Uppercase(yytext) = 'ADD' then return( tknADD );
                                  if Uppercase(yytext) = 'MODIFY' then return( tknMODIFY );
                                  if Uppercase(yytext) = 'LEN' then return( tknLEN );
                                  if Uppercase(yytext) = 'UCASE' then return( tknUCASE );
                                  if Uppercase(yytext) = 'LCASE' then return( tknLCASE );
                                  if Uppercase(yytext) = 'MID' then return( tknMID );
                                  if Uppercase(yytext) = 'NOW' then return( tknNOW );
                                  if Uppercase(yytext) = 'FORMAT' then return( tknFORMAT );
                                  if Uppercase(yytext) = 'AUTOINCREMENT' then return( tknAUTOINCREMENT );
                                  if Uppercase(yytext) = 'AS' then return( tknAS );
                                  if Uppercase(yytext) = 'SHOW' then return( tknSHOW );
                                  if Uppercase(yytext) = 'HEADER' then return( tknHEADER );
                                  if Uppercase(yytext) = 'LOAD_CSV' then return( tknLOAD_CSV );
                                  if Uppercase(yytext) = 'UPLOAD_CSV' then return( tknUPLOAD_CSV );
                                  if Uppercase(yytext) = 'LOAD_SQL' then return( tknLOAD_SQL );
                                  if Uppercase(yytext) = 'PARSE' then return( tknPARSE );
                                  if Uppercase(yytext) = 'USE' then return( tknUSE );
                                  if Uppercase(yytext) = 'SET' then return( tknSET );
                                  if Uppercase(yytext) = 'TRANSACTION' then return( tknTRANSACTION );
                                  if Uppercase(yytext) = 'ROLLBACK' then return( tknROLLBACK );
                                  if Uppercase(yytext) = 'COMMIT' then return( tknCOMMIT );
                                  if Uppercase(yytext) = 'SAVEPOINT' then return( tknSAVEPOINT );
                                  if Uppercase(yytext) = 'TO' then return( tknTO );
                                  if Uppercase(yytext) = 'RELEASE' then return( tknRELEASE );
                                  if Uppercase(yytext) = 'DECLARE' then return( tknDECLARE );
                                  if Uppercase(yytext) = 'CURSOR' then return( tknCURSOR );
                                  if Uppercase(yytext) = 'OPEN' then return( tknOPEN );
                                  if Uppercase(yytext) = 'FETCH' then return( tknFETCH );
                                  if Uppercase(yytext) = 'CLOSE' then return( tknCLOSE );
                                  if Uppercase(yytext) = 'TRUNCATE' then return( tknTRUNCATE );
                                  if Uppercase(yytext) = 'VIEW' then return( tknVIEW );
                                  if Uppercase(yytext) = 'USER' then return( tknUSER );
                                  if Uppercase(yytext) = 'IDENTIFIED' then return( tknIDENTIFIED );
                                  if Uppercase(yytext) = 'PASSWORD' then return( tknPASSWORD );
                                  if Uppercase(yytext) = 'GRANT' then return( tknGRANT );
                                  if Uppercase(yytext) = 'REVOKE' then return( tknREVOKE );
                                  if Uppercase(yytext) = 'OPTION' then return( tknOPTION );
                                  if Uppercase(yytext) = 'IF' then return( tknIF );
                                  if Uppercase(yytext) = 'RENAME' then return( tknRENAME );
                                  if Uppercase(yytext) = 'USERS' then return( tknUSERS );
                                  if Uppercase(yytext) = 'ESCAPE' then return( tknESCAPE );
				end;

  12:
                		;
  13:
                                ;


  14,
  15:
  				returnc(yytext[1]);
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 33;
yynmatches = 33;
yyntrans   = 57;
yynstates  = 30;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  1,
  2,
  14,
  { 3: }
  3,
  14,
  { 4: }
  4,
  14,
  { 5: }
  5,
  14,
  { 6: }
  14,
  { 7: }
  14,
  { 8: }
  14,
  { 9: }
  14,
  { 10: }
  11,
  14,
  { 11: }
  12,
  13,
  14,
  { 12: }
  13,
  14,
  { 13: }
  14,
  { 14: }
  15,
  { 15: }
  1,
  2,
  { 16: }
  { 17: }
  { 18: }
  6,
  { 19: }
  7,
  { 20: }
  8,
  { 21: }
  { 22: }
  9,
  { 23: }
  { 24: }
  10,
  { 25: }
  11,
  { 26: }
  13,
  { 27: }
  2,
  { 28: }
  { 29: }
  2
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  1,
  2,
  14,
{ 3: }
  3,
  14,
{ 4: }
  4,
  14,
{ 5: }
  5,
  14,
{ 6: }
  14,
{ 7: }
  14,
{ 8: }
  14,
{ 9: }
  14,
{ 10: }
  11,
  14,
{ 11: }
  12,
  13,
  14,
{ 12: }
  13,
  14,
{ 13: }
  14,
{ 14: }
  15,
{ 15: }
  1,
  2,
{ 16: }
{ 17: }
{ 18: }
  6,
{ 19: }
  7,
{ 20: }
  8,
{ 21: }
{ 22: }
  9,
{ 23: }
{ 24: }
  10,
{ 25: }
  11,
{ 26: }
  13,
{ 27: }
  2,
{ 28: }
{ 29: }
  2
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11..#31,'#'..'&','('..'/',':',';',
            '?','@','['..']','_','`','{'..#255 ]; s: 13),
  ( cc: [ #9 ]; s: 12),
  ( cc: [ #10 ]; s: 14),
  ( cc: [ ' ' ]; s: 11),
  ( cc: [ '!' ]; s: 6),
  ( cc: [ '"' ]; s: 9),
  ( cc: [ '''' ]; s: 8),
  ( cc: [ '0'..'9' ]; s: 2),
  ( cc: [ '<' ]; s: 4),
  ( cc: [ '=' ]; s: 3),
  ( cc: [ '>' ]; s: 5),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 10),
  ( cc: [ '^' ]; s: 7),
{ 1: }
  ( cc: [ #1..#8,#11..#31,'#'..'&','('..'/',':',';',
            '?','@','['..']','_','`','{'..#255 ]; s: 13),
  ( cc: [ #9 ]; s: 12),
  ( cc: [ #10 ]; s: 14),
  ( cc: [ ' ' ]; s: 11),
  ( cc: [ '!' ]; s: 6),
  ( cc: [ '"' ]; s: 9),
  ( cc: [ '''' ]; s: 8),
  ( cc: [ '0'..'9' ]; s: 2),
  ( cc: [ '<' ]; s: 4),
  ( cc: [ '=' ]; s: 3),
  ( cc: [ '>' ]; s: 5),
  ( cc: [ 'A'..'Z','a'..'z' ]; s: 10),
  ( cc: [ '^' ]; s: 7),
{ 2: }
  ( cc: [ '.' ]; s: 16),
  ( cc: [ '0'..'9' ]; s: 15),
  ( cc: [ 'E','e' ]; s: 17),
{ 3: }
{ 4: }
  ( cc: [ '=' ]; s: 19),
  ( cc: [ '>' ]; s: 18),
{ 5: }
  ( cc: [ '=' ]; s: 20),
{ 6: }
  ( cc: [ '=' ]; s: 18),
{ 7: }
  ( cc: [ '=' ]; s: 18),
{ 8: }
  ( cc: [ ' ','!','$','%','('..'*','-'..':','@'..'Z',
            '_','a'..'z' ]; s: 21),
  ( cc: [ '''' ]; s: 22),
{ 9: }
  ( cc: [ ' ','!','#'..')','-'..':','@'..'Z','_','a'..'z' ]; s: 23),
  ( cc: [ '"' ]; s: 24),
{ 10: }
  ( cc: [ '$','0'..'9','@'..'Z','_','a'..'z' ]; s: 25),
{ 11: }
  ( cc: [ #9,' ' ]; s: 26),
{ 12: }
  ( cc: [ #9,' ' ]; s: 26),
{ 13: }
{ 14: }
{ 15: }
  ( cc: [ '.' ]; s: 16),
  ( cc: [ '0'..'9' ]; s: 15),
  ( cc: [ 'E','e' ]; s: 17),
{ 16: }
  ( cc: [ '0'..'9' ]; s: 27),
{ 17: }
  ( cc: [ '+','-' ]; s: 28),
  ( cc: [ '0'..'9' ]; s: 29),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
  ( cc: [ ' ','!','$','%','('..'*','-'..':','@'..'Z',
            '_','a'..'z' ]; s: 21),
  ( cc: [ '''' ]; s: 22),
{ 22: }
{ 23: }
  ( cc: [ ' ','!','#'..')','-'..':','@'..'Z','_','a'..'z' ]; s: 23),
  ( cc: [ '"' ]; s: 24),
{ 24: }
{ 25: }
  ( cc: [ '$','0'..'9','@'..'Z','_','a'..'z' ]; s: 25),
{ 26: }
  ( cc: [ #9,' ' ]; s: 26),
{ 27: }
  ( cc: [ '0'..'9' ]; s: 27),
  ( cc: [ 'E','e' ]; s: 17),
{ 28: }
  ( cc: [ '0'..'9' ]; s: 29),
{ 29: }
  ( cc: [ '0'..'9' ]; s: 29)
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 11,
{ 8: } 12,
{ 9: } 13,
{ 10: } 14,
{ 11: } 16,
{ 12: } 19,
{ 13: } 21,
{ 14: } 22,
{ 15: } 23,
{ 16: } 25,
{ 17: } 25,
{ 18: } 25,
{ 19: } 26,
{ 20: } 27,
{ 21: } 28,
{ 22: } 28,
{ 23: } 29,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 33
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 3,
{ 3: } 5,
{ 4: } 7,
{ 5: } 9,
{ 6: } 10,
{ 7: } 11,
{ 8: } 12,
{ 9: } 13,
{ 10: } 15,
{ 11: } 18,
{ 12: } 20,
{ 13: } 21,
{ 14: } 22,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 25,
{ 19: } 26,
{ 20: } 27,
{ 21: } 27,
{ 22: } 28,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 32,
{ 29: } 33
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 11,
{ 8: } 12,
{ 9: } 13,
{ 10: } 14,
{ 11: } 16,
{ 12: } 19,
{ 13: } 21,
{ 14: } 22,
{ 15: } 23,
{ 16: } 25,
{ 17: } 25,
{ 18: } 25,
{ 19: } 26,
{ 20: } 27,
{ 21: } 28,
{ 22: } 28,
{ 23: } 29,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 33,
{ 29: } 33
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 3,
{ 3: } 5,
{ 4: } 7,
{ 5: } 9,
{ 6: } 10,
{ 7: } 11,
{ 8: } 12,
{ 9: } 13,
{ 10: } 15,
{ 11: } 18,
{ 12: } 20,
{ 13: } 21,
{ 14: } 22,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 25,
{ 19: } 26,
{ 20: } 27,
{ 21: } 27,
{ 22: } 28,
{ 23: } 28,
{ 24: } 29,
{ 25: } 30,
{ 26: } 31,
{ 27: } 32,
{ 28: } 32,
{ 29: } 33
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 14,
{ 2: } 27,
{ 3: } 30,
{ 4: } 30,
{ 5: } 32,
{ 6: } 33,
{ 7: } 34,
{ 8: } 35,
{ 9: } 37,
{ 10: } 39,
{ 11: } 40,
{ 12: } 41,
{ 13: } 42,
{ 14: } 42,
{ 15: } 42,
{ 16: } 45,
{ 17: } 46,
{ 18: } 48,
{ 19: } 48,
{ 20: } 48,
{ 21: } 48,
{ 22: } 50,
{ 23: } 50,
{ 24: } 52,
{ 25: } 52,
{ 26: } 53,
{ 27: } 54,
{ 28: } 56,
{ 29: } 57
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 13,
{ 1: } 26,
{ 2: } 29,
{ 3: } 29,
{ 4: } 31,
{ 5: } 32,
{ 6: } 33,
{ 7: } 34,
{ 8: } 36,
{ 9: } 38,
{ 10: } 39,
{ 11: } 40,
{ 12: } 41,
{ 13: } 41,
{ 14: } 41,
{ 15: } 44,
{ 16: } 45,
{ 17: } 47,
{ 18: } 47,
{ 19: } 47,
{ 20: } 47,
{ 21: } 49,
{ 22: } 49,
{ 23: } 51,
{ 24: } 51,
{ 25: } 52,
{ 26: } 53,
{ 27: } 55,
{ 28: } 56,
{ 29: } 57
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap then
    begin
      yyclear;
      return(0);
    end;

  if not yydone then goto start;

  Result := yyretval;

end(*yylex*);

