{$I-}

unit yacclib;

{$MODE Delphi}

interface

var
  yyerrmsgs: array of string; (* error messages *)
  yymiscmsgs: array of string; (* miscellenous messages *)

const
  yymaxdepth = 1024;
(* default stack size of parser *)

type
  YYSType = integer;
(* default value type, may be redefined in Yacc output file *)

var

  yychar:  integer; (* current lookahead character *)
  yynerrs: integer;

(* current number of syntax errors reported by the
                       parser *)
procedure yyerror(msg: string);
(* error message printing routine used by the parser *)

procedure yyacceptmessage(msg: string);
(* error message printing routine used by the parser *)

procedure yyclearin;
(* delete the current lookahead token *)

procedure yyaccept;
  (* trigger accept action of the parser; yyparse accepts returning 0, as if
     it reached end of input *)

procedure yyabort;
  (* like yyaccept, but causes parser to return with value 1, as if an
     unrecoverable syntax error had been encountered *)

procedure yyerrlab;
  (* causes error recovery to be started, as if a syntax error had been
     encountered *)

procedure yyerrok;
  (* when in error mode, resets the parser to its normal mode of
     operation *)

(* Flags used internally by the parser routine: *)

var

  yyflag:    (yyfnone, yyfaccept, yyfabort, yyferror);
  yyerrflag: integer;

implementation

procedure yyerror(msg: string);
begin
  msg := 'ERROR: '+ msg;
  setLength(yyerrmsgs,length(yyerrmsgs) + 1);
  yyerrmsgs[High(yyerrmsgs)] := msg;
  (********************) //Halt(1)
end(*yyerrmsg*);

procedure yyacceptmessage(msg: string);
begin
  msg := 'ACCEPT: '+ msg;
  setLength(yymiscmsgs,length(yymiscmsgs) + 1);
  yymiscmsgs[High(yymiscmsgs)] := msg;
  (********************) //Halt(1)
end(*yyerrmsg*);

procedure yyclearin;
begin
  yychar := -1;
end(*yyclearin*);

procedure yyaccept;
begin
  yyflag := yyfaccept;
end(*yyaccept*);

procedure yyabort;
begin
  yyflag := yyfabort;
end(*yyabort*);

procedure yyerrlab;
begin
  yyflag := yyferror;
end(*yyerrlab*);

procedure yyerrok;
begin
  yyerrflag := 0;
end(*yyerrork*);

end(*YaccLib*).
