%{
unit expr;

interface

uses
	SysUtils,
	Classes,
	yacclib,
	lexlib,
        uStreamLexer;

type
  TExprLexer = class(TStreamLexer)
  public
    function parse() : integer; override;
  end;

  TWriteCallback = procedure(Value: Real) of object;
  EExprParserException = class(Exception);
  TExprParser = class(TCustomParser)
  private
    x: array [1..26] of Real;
    writecallback: TWriteCallback;
  public
    constructor Create;
    function parse(AStream: TStream; AWriteCB: TWriteCallback) : integer; reintroduce;
  end;


%}

%token <Real> NUM       /* constants */
%token <Integer> ID     /* variables */
%type <Real> expr	/* expressions */

%left '+' '-'      	/* operators */
%left '*' '/'
%right UMINUS

%token ILLEGAL 		/* illegal token */

%%

input	: /* empty */
	| input '\n'		 { yyaccept; }
	| input expr '\n'	 { writecallback($2); }
	| input ID '=' expr '\n' { x[$2] := $4; writecallback($4); }
	| error '\n'             { yyerrok; }
	;

expr    :  expr '+' expr	 { $$ := $1 + $3; }
	|  expr '-' expr	 { $$ := $1 - $3; }
	|  expr '*' expr	 { $$ := $1 * $3; }
	|  expr '/' expr	 { $$ := $1 / $3; }
	|  '(' expr ')'		 { $$ := $2; }
	|  '-' expr              { $$ := -$2; }
           %prec UMINUS
	|  NUM                   { $$ := $1; }
        |  ID                    { $$ := x[$1]; }
	;

%%

{$I exprlex.pas}

end.
