%{
unit sql;

interface

uses
  sysUtils,
  dlib,
  yacclib,
  lexlib;


type

  NodePointer = ^NodeType;

  conNodePointer = ^conNodeType;
  conNodeType = record
    value: Real;
  end;

  stNodePointer = ^stNodeType;
  stNodeType = record
    value: string;
  end;

  dbNodePointer = ^dbNodeType;
  dbNodeType = record
    value: string;
  end;

  oprNodePointer = ^oprNodeType;
  oprNodeType = record
    Mnemonic: Integer; (* operator mnemonic *)
    stMnemonic: string;
    op: array of NodePointer
  end;

  NodeType = record
    case kind: byte of
      0: (conNode: conNodePointer);
      1: (stNode: stNodePointer);
      2: (oprNode: oprNodePointer);
      3: (dbNode: dbNodePointer);
    end;

type
  TLexer = class(TLexerParserBase)
  public
    function parse() : integer; override;
  end;

  TParser = class(TLexerParserBase)
  public
    lexer : TLexer;

    function parse() : integer; override;
  end;

const
  Mnemonics: array [0..131] of string =
    ('REPEAT', 'CREATE DATABASE', 'DATABASE NAME', 'CREATE TABLE', 'TABLE NAME',
     'NEW COLUMN', 'COLUMN NAME', 'CHAR', 'VARCHAR', 'CHAR VARYING',           //9
     'CHARACTER', 'CHARACTER VARYING', 'CLONB', 'DATE', 'NUMBER',
     'FLOAT', 'REAL', 'DOUBLE PRECISION', 'NUMBER2','DECIMAL',
     'DEC', 'NUMERIC', 'NUMBER1','INTEGER', 'INT',
     'SMALLINT', 'CONSTRAINT NAME','NULL','NOT NULL', 'UNIQUE',            //29
     'PRIMARY KEY', 'REFERENCES', 'ON DELETE CASCADE', 'TABLE CONSTRAINT', 'SELECT',
     'ALL', 'DISTINCT', 'ALL COLUMNS', 'ALL TABLE COLUMNS', 'COLUMNS WITHIN EXPRESSION',
     'ALIAS NAME','WHERE', 'NOT', 'OR', 'AND',
     'IN', 'LIKE', 'BETWEEN', 'IS NULL', 'IS NOT NULL',                    //49
     'EQ', 'LT', 'GT', 'NE', 'LE',
     'GE', 'EQ ALL', 'LT ALL', 'GT ALL', 'NE ALL',
     'LE ALL', 'GE ALL', 'EQ ANY', 'LT ANY', 'GT ANY',
     'NE ANY', 'LE ANY', 'GE ANY', 'EXISTS', 'GROUP BY',                   //69
     'ORDER BY', 'HAVING', 'UNION ALL', 'INTERSECT', 'MINUS',
     'ASC', 'DESC', 'INSERT INTO', 'VALUES', 'UMINUS',
     'UPDATE', 'SET', 'DELETE FROM', 'ADD', 'SUB',
     'MUL', 'DIV', 'FROM', 'PUSH', 'PUSH LITERAL',                         //89
     'DEFAULT', 'COLUMN CONSTRAINT', 'ABS', 'CEIL', 'FLOOR',
     'MOD', 'POWER', 'ROUND', 'SIGN', 'SQRT',
     'TRUNC', 'CHR', 'LPAD', 'LTRIM', 'RPAD',
     'RTRIM', 'SOUNDEX', 'SUBSTR', 'LENGTH', 'TO_CHAR',                    //109
     'TO_DATE', 'TO_NUMBER', 'AVG', 'COUNT', 'MAX',
     'MIN', 'STDDEV', 'SUM', 'VARIANCE', 'PUSH NAME',                      //119
     'CREATE INDEX', 'INDEX NAME', 'ASC', 'DESC','INDEX COLUMN',
     'TABLE COMMENT', 'COLUMN COMMENT', 'COMMENT', 'PUSH COMMENT', 'VOID', //129
     'CHECK', 'BIGINT');

type
  singleInstructionType = record
    mnemonic: Integer;
    value: real;
    stvalue: string;
    printInstruction: string // just for printing
  end;

var
  sqlMemProg: array of singleInstructionType;

implementation

function con(value: Real): NodePointer;
begin
  new(result);
  result^.kind := 0;
  New(result^.conNode);
  result^.conNode^.value := value
end;

function stcon(value: string): NodePointer;
begin
  new(result);
  result^.kind := 1;
  New(result^.stNode);
  result^.stNode^.value := value
end;

function DBName(Name: string): NodePointer;
begin
  new(result);
  result^.kind := 3;
  New(result^.stNode);
  result^.stNode^.value := Name
end;

function DBcomment(comment: string): NodePointer;
begin
  new(result);
  result^.kind := 4;
  New(result^.stNode);
  result^.stNode^.value := comment
end;

function opr(Mnemonic: Integer; stMnemonic: string): NodePointer; overload;
begin
  new(result);
  result^.kind := 2;
  New(result^.oprNode);
  result^.oprNode^.Mnemonic := Mnemonic;
  result^.oprNode^.stMnemonic := stMnemonic;
  result^.oprNode^.op := nil
end;

function opr(Mnemonic: Integer; stMnemonic: string; op: array of NodePointer): NodePointer; overload;
var
  i: Integer;
begin
  new(result);
  result^.kind := 2;
  New(result^.oprNode);
  result^.oprNode^.Mnemonic := Mnemonic;
  result^.oprNode^.stMnemonic := stMnemonic;
  SetLength(result^.oprNode^.op,length(op));
  for i := Low(op) to High(op) do
    result^.oprNode^.op[i] := op[i]
end;

procedure ex(p: NodePointer); forward;

%}

%token <Real> NUMBER                    /* constants */
%token <Integer> NUM                    /* constants */
%token <string> ID                      /* variables */
%type <Pointer> expr                    /* expressions */
%token <string> QUOTED_STRING

%token tknCREATE                        /* keyword */
%token tknDATABASE                      /* keyword */
%token tknTABLE                         /* keyword */
%token tknCHAR                          /* keyword */
%token tknVARCHAR                       /* keyword */
%token tknCHARACTER                     /* keyword */
%token tknVARYING                       /* keyword */
%token tknCLOB                          /* keyword */
%token tknDATE                          /* keyword */
%token tknNUMBER                        /* keyword */
%token tknFLOAT                         /* keyword */
%token tknREAL                          /* keyword */
%token tknDOUBLE                        /* keyword */
%token tknPRECISION                     /* keyword */
%token tknDECIMAL                       /* keyword */
%token tknDEC                           /* keyword */
%token tknNUMERIC                       /* keyword */
%token tknINTEGER                       /* keyword */
%token tknINT                           /* keyword */
%token tknSMALLINT                      /* keyword */
%token tknBIGINT                      /* keyword */
%token tknDEFAULT                       /* keyword */
%token tknCONSTRAINT                    /* keyword */
%token tknNULL                          /* keyword */
%token tknNOT                           /* keyword */
%token tknUNIQUE                        /* keyword */
%token tknPRIMARY                       /* keyword */
%token tknKEY                           /* keyword */
%token tknREFERENCES                    /* keyword */
%token tknCHECK                         /* keyword */
%token tknON                            /* keyword */
%token tknDELETE                        /* keyword */
%token tknCASCADE                       /* keyword */
%token tknINDEX                         /* keyword */
%token tknSELECT                        /* keyword */
%token tknASC                           /* keyword */
%token tknDESC                          /* keyword */
%token tknCOMMENT                        /* keyword */
%token tknCOLUMN                        /* keyword */
%token tknFROM                          /* keyword */
%token tknALL                           /* keyword */
%token tknDISTINCT                      /* keyword */
%token tknWHERE                         /* keyword */
%token tknOR                            /* keyword */
%token tknAND                           /* keyword */
%token tknIN                            /* keyword */
%token tknLIKE                          /* keyword */
%token tknBETWEEN                       /* keyword */
%token tknIS                            /* keyword */
%token tknEXISTS                        /* keyword */
%token tknANY                           /* keyword */
%token tknGROUP                         /* keyword */
%token tknBY                            /* keyword */
%token tknHAVING                        /* keyword */
%token tknUNION                         /* keyword */
%token tknINTERSECT                     /* keyword */
%token tknMINUS                         /* keyword */
%token tknORDER                         /* keyword */
%token tknASC                           /* keyword */
%token tknDESC                          /* keyword */
%token tknINSERT                        /* keyword */
%token tknINTO                          /* keyword */
%token tknVALUES                        /* keyword */
%token tknUPDATE                        /* keyword */
%token tknSET                           /* keyword */
%token tknABS                           /* keyword */
%token tknCEIL                          /* keyword */
%token tknFLOOR                         /* keyword */
%token tknMOD                           /* keyword */
%token tknPOWER                         /* keyword */
%token tknROUND                         /* keyword */
%token tknSIGN                          /* keyword */
%token tknSQRT                          /* keyword */
%token tknTRUNC                         /* keyword */
%token tknCHR                           /* keyword */
%token tknLPAD                          /* keyword */
%token tknLTRIM                         /* keyword */
%token tknRPAD                          /* keyword */
%token tknRTRIM                         /* keyword */
%token tknSOUNDEX                       /* keyword */
%token tknSUBSTR                        /* keyword */
%token tknLENGTH                        /* keyword */
%token tknTO_CHAR                       /* keyword */
%token tknTO_DATE                       /* keyword */
%token tknTO_NUMBER                     /* keyword */
%token tknAVG                           /* keyword */
%token tknCOUNT                         /* keyword */
%token tknMAX                           /* keyword */
%token tknMIN                           /* keyword */
%token tknSTDDEV                        /* keyword */
%token tknSUM                           /* keyword */
%token tknVARIANCE                      /* keyword */

%token tknEQ
%token tknLT
%token tknGT
%token tknNE
%token tknLE
%token tknGE

%type <Pointer> sql_command
%type <Pointer> to_define_data
%type <Pointer> to_modify_data
%type <Pointer> create_command
%type <Pointer> comment_command
%type <Pointer> create_database
%type <Pointer> create_table
%type <Pointer> database_name
%type <Pointer> table_name
%type <Pointer> new_column_list
%type <Pointer> option_new_constraint_list
%type <Pointer> new_column
%type <Pointer> new_constraint_list
%type <Pointer> column
%type <Pointer> column_name
%type <Pointer> datatype
%type <Pointer> option_default_expression
%type <Pointer> rpt_column_constraint
%type <Pointer> column_constraint
%type <Pointer> option_constraint_name
%type <Pointer> def_column_constraint
%type <Pointer> constraint_name
%type <Pointer> table_constraint
%type <Pointer> def_table_constraint
%type <Pointer> column_list
%type <Pointer> character_type
%type <Pointer> date_type
%type <Pointer> float_type
%type <Pointer> decimal_type
%type <Pointer> integer_type
%type <Pointer> create_index
%type <Pointer> index_name
%type <Pointer> indexed_column_list
%type <Pointer> indexed_column
%type <Pointer> option_sort
%type <Pointer> comment
%type <Pointer> select_command
%type <Pointer> option_duplicate
%type <Pointer> option_columns
%type <Pointer> selected_tables
%type <Pointer> rptlogical_term
%type <Pointer> logical_term
%type <Pointer> rptlogical_factor
%type <Pointer> logical_factor
%type <Pointer> option_where_condition
%type <Pointer> rptdisplayed_column
%type <Pointer> displayed_column
%type <Pointer> alias
%type <Pointer> selected_table
%type <Pointer> condition
%type <Integer> comparaison_op
%type <Pointer> exp_set
%type <Pointer> match_string
%type <Pointer> quantified_factor
%type <Pointer> rptnumber_quoted_string
%type <Pointer> subquery
%type <Pointer> number_quoted_string
%type <Pointer> group_order_clause
%type <Pointer> expr_list
%type <Pointer> option_having
%type <Pointer> set_clause
%type <Pointer> rptsorted_def
%type <Pointer> sorted_def
%type <Pointer> rptgroup_order_clause
%type <Pointer> insert_command
%type <Pointer> value_list
%type <Pointer> rptvalue
%type <Pointer> value
%type <Pointer> query
%type <Pointer> update_command
%type <Pointer> simple_update
%type <Pointer> subquery_update
%type <Pointer> rptset_column
%type <Pointer> set_column
%type <Pointer> rptcolumn_name
%type <Pointer> delete_command
%type <Pointer> delete_command
%type <Pointer> delete_command
%type <Pointer> option_column_name
%type <Pointer> option_delete_cascade
%type <string>  match_string
%type <Pointer> variable
%type <Pointer> constant
%type <Pointer> function
%type <Pointer> option_expression
%type <Pointer> group_function
%type <Pointer> option_group
%type <Pointer> number_function
%type <Pointer> char_function
%type <Pointer> conversion_function


%left '+' '-'                           /* operators */
%left '*' '/'
%right UMINUS

%token ILLEGAL                          /* illegal token */

%%

input	: /* empty */
	| input '\n'
                { yyaccept; }
        | input sql_command '\n'
                { ex($2); yyaccept; }
        | input condition '\n'
                { ex($2); yyaccept; }
	| error '\n'
                { yyerrok; }
	;

sql_command : to_define_data
                { $$ := $1; }
            | to_modify_data
                { $$ := $1; }
            ;

/* User Name --> Directory
   DataBase  --> Sub-Directory
   Use Database command */

to_define_data : create_command
                { $$ := $1; }
               | comment_command
                { $$ := $1; }
               ;
create_command : create_database
                { $$ := $1; }
               | create_table
                { $$ := $1; }
               | create_index
                { $$ := $1; }
               ;

create_database : tknCREATE tknDATABASE database_name
                { $$ := opr(1,'CREATE DATABASE',[$3]); }
                ;

database_name : ID
                { $$ := opr(2,'DATABASE NAME',[DBName($1)]); }
              ;

create_table : tknCREATE tknTABLE table_name '(' new_column_list option_new_constraint_list ')'
                { $$ := opr(3,'CREATE TABLE',[$3,$5,$6]); }
             ;

table_name : ID
                { $$ := opr(4,'TABLE NAME',[DBName($1)]); }
           ;

new_column_list :  new_column
                { $$ := $1; }
                |   new_column_list ',' new_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                ;

new_column : column_name datatype option_default_expression rpt_column_constraint
                { $$ := opr(5,'NEW COLUMN',[$1,$2,$3,$4]); }
                ;

column_name : ID
                { $$ := opr(6,'COLUMN NAME',[DBName($1)]); }
              ;

datatype : character_type
                { $$ := $1; }
         | date_type
                { $$ := $1; }
         | float_type
                { $$ := $1; }
         | decimal_type
                { $$ := $1; }
         | integer_type
                { $$ := $1; }
         ;

character_type : tknCHAR '(' NUM ')'
                { $$ := opr(7,'CHAR',[con($3)]); }
               | tknVARCHAR '(' NUM ')'
                { $$ := opr(8,'VARCHAR',[con($3)]); }
               | tknCHAR tknVARYING '(' NUM ')'
                { $$ := opr(9,'CHAR VARYING',[con($4)]); }
               | tknCHARACTER '(' NUM ')'
                { $$ := opr(10,'CHARACTER',[con($3)]); }
               | tknCHARACTER VARYING '(' NUM ')'
                { $$ := opr(11,'CHARACTER VARYING',[con($4)]); }
               | tknCLOB '(' NUM ')'
                { $$ := opr(12,'CLOB',[con($3)]); }
               ;

date_type : tknDATE
                { $$ := opr(13,'DATE'); }
          ;

float_type : tknNUMBER
                { $$ := opr(14,'NUMBER'); } /* not in SQL-92 */
           | tknFLOAT
                { $$ := opr(15,'FLOAT'); }
           | tknREAL
                { $$ := opr(16,'REAL'); }
           | tknDOUBLE tknPRECISION
                { $$ := opr(17,'DOUBLE PRECISION'); }
           ;

decimal_type : tknNUMBER '(' NUM ',' NUM ')'
                { $$ := opr(18,'NUMBER2',[con($3),con($5)]); } /* not in SQL-92 */
             | tknDECIMAL '(' NUM ',' NUM ')'
                { $$ := opr(19,'DECIMAL',[con($3),con($5)]); }
             | tknDEC '(' NUM ',' NUM ')'
                { $$ := opr(20,'DEC',[con($3),con($5)]); }
             | tknNUMERIC '(' NUM ',' NUM ')'
                { $$ := opr(21,'NUMERIC',[con($3),con($5)]); }
             ;
integer_type : tknNUMBER '(' NUM ')'
                { $$ := opr(22,'NUMBER1',[con($3)]); } /* not in SQL-92 */
             | tknINTEGER
                { $$ := opr(23,'INTEGER'); }
             | tknINT
                { $$ := opr(24,'INT'); }
             | tknSMALLINT
                { $$ := opr(25,'SMALLINT'); }
             | tknBIGINT
                { $$ := opr(131,'BIGINT'); }
             ;

option_default_expression : /* empty */
                { $$ := nil; }
                          | tknDEFAULT expr
                { $$ := opr(90,'DEFAULT',[$2]); }
                          ;

rpt_column_constraint : /* empty */
                { $$ := nil; }
                      | rpt_column_constraint column_constraint
                { $$ := opr(0,'REPEAT',[$1,$2]); }
                      ;

column_constraint : option_constraint_name def_column_constraint
                { $$ := opr(91,'CONSTRAINT',[$1,$2]); }
                  ;

option_constraint_name : /* empty */
                { $$ := nil; }
                       | tknCONSTRAINT constraint_name
                { $$ := opr(129,'VOID',[$2]); }
                       ;

constraint_name : ID
                { $$ := opr(26,'CONSTRAINT NAME',[DBName($1)]); }
              ;

def_column_constraint : tknNULL
                { $$ := opr(27,'NULL'); }
                      | tknNOT tknNULL
                { $$ := opr(28,'NOT NULL'); }
                      | tknUNIQUE
                { $$ := opr(29,'UNIQUE'); }
                      | tknPRIMARY tknKEY
                { $$ := opr(30,'PRIMARY KEY'); }
                      | tknREFERENCES table_name option_column_name option_delete_cascade
                { $$ := opr(31,'REFERENCES',[$2,$3,$4]); }
                      | tknCHECK '(' condition ')'
                { $$ := opr(130,'CHECK',[$3]); }
                      ;

option_column_name :  /* empty */
                { $$ := nil; }
                   | '(' column_name ')'
                { $$ := $2; }
                   ;

option_delete_cascade :  /* empty */
                { $$ := nil; }
                      | tknON tknDELETE tknCASCADE
                { $$ := opr(32,'ON DELETE CASCADE'); }
                      ;

option_new_constraint_list : /* empty */
                { $$ := nil; }
                           | ',' new_constraint_list
                { $$ := $2; }
                           ;

new_constraint_list :  table_constraint
                { $$ := $1; }
                    |   new_constraint_list ',' table_constraint
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                ;
table_constraint : option_constraint_name def_table_constraint
                { $$ := opr(33,'TABLE CONSTRAINT',[$2,$1]); }
                 ;

def_table_constraint : tknUNIQUE '(' column_list ')'
                { $$ := opr(29,'UNIQUE',[$3]); }
                     | tknPRIMARY tknKEY '(' column_list ')'
                { $$ := opr(30,'PRIMARY KEY',[$4]); }
                     | tknCHECK '(' condition ')'
                { $$ := opr(130,'CHECK',[$3]); }
                     ;

column_list : column
                { $$ := $1; }
            | column_list ',' column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
            ;

column : column_name
                { $$ := opr(129,'VOID',[$1]); }
       ;

create_index : tknCREATE tknINDEX index_name tknON table_name '(' indexed_column_list ')'
                { $$ := opr(120,'CREATE INDEX',[$3,$5,$7]); }
             ;

index_name : ID
                { $$ := opr(121,'INDEX NAME',[DBName($1)]); }
           ;

indexed_column_list : indexed_column
                { $$ := $1; }
                    | indexed_column_list ',' indexed_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                    ;
indexed_column : column option_sort
                { $$ := opr(124,'INDEX COLUMN',[$1,$2]); }

option_sort : /* empty */
                { $$ := nil; }
            | tknASC
                { $$ := opr(122,'ASC'); }
            | tknDESC
                { $$ := opr(123,'DESC'); }
            ;


comment_command : tknCOMMENT tknON tknTABLE table_name tknIS comment
                 { $$ := opr(125,'TABLE COMMENT',[$4,$6]); }
                | tknCOMMENT tknON tknCOLUMN table_name '.' column_name tknIS comment
                 { $$ := opr(126,'COLUMN COMMENT',[$4,$6,$8]); }
                ;

comment : QUOTED_STRING
                { $$ := opr(129,'VOID',[DBcomment($1)]); }
        ;

to_modify_data : select_command
                { $$ := $1; }
               | insert_command
                { $$ := $1; }
               | update_command
                { $$ := $1; }
               | delete_command
                { $$ := $1; }
               ;

select_command : tknSELECT option_duplicate option_columns
                 tknFROM selected_tables
                 option_where_condition
                 rptgroup_order_clause
                 set_clause
                { $$ := opr(34,'SELECT',[$2,$3,opr(87,'FROM',[$5]),$6,$7,$8]); }
               ;

option_duplicate : /* empty */
                { $$ := nil; }
                 | tknALL
                { $$ := opr(35,'ALL'); }
                 | tknDISTINCT
                { $$ := opr(36,'DISTINCT'); }
                 ;

option_columns : '*'
                { $$ := opr(37,'ALL COLUMNS'); }
               | rptdisplayed_column
                { $$ := $1; }
               ;

rptdisplayed_column : displayed_column
                { $$ := $1; }
                    | rptdisplayed_column ',' displayed_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                    ;

displayed_column : table_name '.' '*'
                { $$ := opr(38,'ALL TABLE COLUMNS',[$1]); }
                 | expr
                { $$ := opr(39,'COLUMNS WITHIN EXPRESSION',[$1]); }
                 | expr alias
                { $$ := opr(39,'COLUMNS WITHIN EXPRESSION',[$1,$2]); }
                 ;

alias : ID
                { $$ := opr(40,'ALIAS NAME',[DBName($1)]); }
      ;

selected_tables : selected_table
                { $$ := $1; }
                | selected_tables ',' selected_table
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                ;

selected_table : table_name
                { $$ := $1; }
               | table_name alias
                { $$ := opr(129,'VOID',[$1,$2]); }
               ;

option_where_condition : /* empty */
                { $$ := nil; }
                       | tknWHERE condition
                { $$ := opr(41,'WHERE',[$2]); }

condition : tknNOT rptlogical_term
                { $$ := opr(42,'NOT',[$2]); }
          | rptlogical_term
                { $$ := $1; }
          ;

rptlogical_term : logical_term
                { $$ := $1; }
                | rptlogical_term tknOR logical_term
                { $$ := opr(43,'OR',[$1,$3]); }
                ;

logical_term : rptlogical_factor
                { $$ := $1; }
             ;

rptlogical_factor : logical_factor
                { $$ := $1; }
                  | rptlogical_factor tknAND logical_factor
                { $$ := opr(44,'AND',[$1,$3]); }
                  ;

logical_factor : expr comparaison_op expr
                { $$ := opr(50+$2,'',[$1,$3]); }
               | tknIN exp_set
                { $$ := opr(45,'IN',[$2]); }
               | tknLIKE match_string
                { $$ := opr(46,'LIKE',[stcon($2)]); }
               | tknBETWEEN expr tknAND expr
                { $$ := opr(47,'BETWEEN',[$2,$4]); }
               | tknIS tknNULL
                { $$ := opr(48,'IS NULL'); }
               | tknIS tknNOT tknNULL
                { $$ := opr(49,'IS NOT NULL'); }
               | quantified_factor
                { $$ := $1; }
               | '(' condition ')'
                { $$ := $2; }
               ;


match_string : QUOTED_STRING
                { $$ := $1; }
             ;

comparaison_op : tknEQ
                { $$ := 0; }
               | tknLT
                { $$ := 1; }
               | tknGT
                { $$ := 2; }
               | tknNE
                { $$ := 3; }
               | tknLE
                { $$ := 4; }
               | tknGE
                { $$ := 5; }
               ;

exp_set : rptnumber_quoted_string
                { $$ := $1; }
        | subquery
                { $$ := $1; }
        ;

rptnumber_quoted_string : number_quoted_string
                { $$ := $1; }
                        | rptnumber_quoted_string ',' number_quoted_string
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                        ;

number_quoted_string : NUM
                { $$ := con($1); }
                     | NUMBER
                { $$ := con($1); }
                     | QUOTED_STRING
                { $$ := stcon($1); }
                     ;

subquery : '(' select_command ')'
                { $$ := $2; }
         ;

quantified_factor : expr comparaison_op subquery
                { $$ := opr(50+$2,'',[$1,$3]); }
                  | expr comparaison_op tknALL subquery
                { $$ := opr(56+$2,'' + ' ALL',[$1,$4]); }
                  | expr comparaison_op tknANY subquery
                { $$ := opr(62+$2,'' + ' ANY',[$1,$4]); }
                  | tknExists subquery
                { $$ := opr(68,'EXISTS',[$2]); }
                  ;

rptgroup_order_clause : /* empty */
                { $$ := nil; }
                      | group_order_clause
                { $$ := $1; }
                   ;

group_order_clause : tknGROUP tknBY expr_list option_having
                { $$ := opr(69,'GROUP BY',[$3,$4]); }
                   |  group_order_clause tknGROUP tknBY expr_list option_having
                { $$ := opr(69,'GROUP BY',[$1,$4,$5]); }
                   |  tknORDER tknBY rptsorted_def
                { $$ := opr(70,'ORDER BY',[$3]); }
                   |  group_order_clause tknORDER tknBY rptsorted_def
                { $$ := opr(70,'ORDER BY',[$1,$4]); }
             ;             ;

expr_list : expr
                { $$ := $1; }
          | expr_list ',' expr
                { $$ := opr(0,'REPEAT',[$1,$3]); }
          ;

option_having : /* empty */
                { $$ := nil; }
              | tknHAVING condition
                { $$ := opr(71,'HAVING',[$2]); }
              ;

set_clause : /* empty */
                { $$ := nil; }
           | set_clause tknUNION tknALL select_command
                { $$ := opr(72,'UNION ALL',[$1,$4]); }
           | set_clause tknINTERSECT select_command
                { $$ := opr(73,'INTERSECT',[$1,$3]); }
           | set_clause tknMINUS select_command
                { $$ := opr(74,'MINUS',[$1,$3]); }
           ;

rptsorted_def : sorted_def
                { $$ := $1; }
              | rptsorted_def ',' sorted_def
                { $$ := opr(0,'REPEAT',[$1,$3]); }
              ;

sorted_def : expr tknASC
                { $$ := opr(75,'ASC',[$1]); }
           | expr tknDESC
                { $$ := opr(76,'DESC',[$1]); }
           ;

insert_command : tknINSERT tknINTO  table_name '(' column_list ')' value_list
                { $$ := opr(77,'INSERT INTO',[$3,$5,$7]); }
               | tknINSERT tknINTO table_name  '(' column_list ')' query
                { $$ := opr(77,'INSERT INTO',[$3,$5,opr(78,'VALUE',[$7])]); }
               ;

value_list : tknVALUES '(' rptvalue ')'
                { $$ := opr(78,'VALUE',[$3]); }
           ;

rptvalue : value
                { $$ := $1; }
         | rptvalue ',' value
                { $$ := opr(0,'REPEAT',[$1,$3]); }
         ;

value : ID
                { $$ := stcon($1); }
      | '+' ID %prec UMINUS
                { $$ := stcon($2); }
      | '-' ID %prec UMINUS
                { $$ := opr(79,'UMINUS',[stcon($2)]); }
      | NUM
                { $$ := con($1); }
      | '+' NUM %prec UMINUS
                { $$ := opr(79,'UMINUS',[con($2)]); }
      | '-' NUM %prec UMINUS
                { $$ := con($2); }
      | NUMBER
                { $$ := con($1); }
      | '+' NUMBER %prec UMINUS
                { $$ := con($2); }
      | '-' NUMBER %prec UMINUS
                { $$ := opr(79,'UMINUS',[con($2)]); }
      | QUOTED_STRING
                { $$ := stcon($1); }
      ;

query : select_command
                { $$ := $1; }
      ;


update_command : simple_update
                { $$ := $1; }
               | subquery_update
                { $$ := $1; }
               ;

simple_update : tknUPDATE table_name rptset_column tknWHERE  condition
                { $$ := opr(80,'UPDATE',[$2,$3,$5]); }
              | tknUPDATE table_name alias rptset_column tknWHERE condition
                { $$ := opr(80,'UPDATE',[$2,$3,$4,$6]); }
              ;

rptset_column : set_column
                { $$ := $1; }
              | rptset_column ',' set_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
              ;

set_column : tknSET column tknEQ expr %prec UMINUS
                { $$ := opr(81,'SET',[$2,$4]); }
           | tknSET column tknEQ  subquery %prec UMINUS
                { $$ := opr(81,'SET',[$2,$4]); }
           ;

subquery_update : tknUPDATE table_name tknSET '(' rptcolumn_name ')' '='
                    subquery tknWHERE condition %prec UMINUS
                { $$ := opr(80,'UPDATE',[$2,$5,$8,$10]); }
                | tknUPDATE table_name alias tknSET '(' rptcolumn_name ')' '='
                    subquery tknWHERE condition %prec UMINUS
                { $$ := opr(80,'UPDATE',[$2,$3,$6,$9,$11]); }
                ;

rptcolumn_name : column_name
                { $$ := $1; }
               | rptcolumn_name ',' column_name
                { $$ := opr(0,'REPEAT',[$1,$3]); }
               ;

delete_command : tknDELETE tknFROM table_name
                { $$ := opr(82,'DELETE FROM',[$3]); }
               | tknDELETE tknFROM table_name tknWHERE condition
                { $$ := opr(82,'DELETE FROM',[$3,$5]); }
               ;

expr    :  expr '+' expr
                { $$ := opr(83,'ADD',[$1,$3]); }
	|  expr '-' expr
                { $$ := opr(84,'SUB',[$1,$3]); }
	|  expr '*' expr
                { $$ := opr(85,'MUL',[$1,$3]); }
	|  expr '/' expr
                { $$ := opr(86,'DIV',[$1,$3]); }
	|  '(' expr ')'
                { $$ := $2; }
	|  '+' expr %prec UMINUS
                { $$ := $2; }
	|  '-' expr %prec UMINUS
                { $$ := opr(79,'UMINUS',[$2]); }
        |  constant
                { $$ := $1; }
        |  variable
                { $$ := $1; }
        | function
                { $$ := $1; }
        | function option_expression
                { $$ := opr(129,'VOID',[$1,$2]); }
        | group_function
                { $$ := $1; }
        | group_function '(' option_group expr ')'
                { $$ := opr(129,'VOID',[$1,$3,$4]); }
	;

option_expression : '(' expr_list ')'
                  ;

option_group : '*'
                { $$ := opr(37,'ALL COLUMNS'); }
             | tknALL
                { $$ := opr(35,'ALL'); }
             | tknDISTINCT
                { $$ := opr(36,'DISTINCT'); }
             ;

variable : column_name
                { $$ := $1; }
         | table_name '.' column_name
                { $$ := opr(129,'VOID',[$1,$3]); }
         ;

constant :  NUM
                { $$ := con($1); }
	 |  NUMBER
                { $$ := con($1); }
         | QUOTED_STRING
                { $$ := stcon($1); }
         ;

function : number_function
                { $$ := $1; }
         | char_function
                { $$ := $1; }
         | conversion_function
                { $$ := $1; }
         ;

number_function : tknABS
                { $$ := opr(92,'ABS'); }
                | tknCEIL
                { $$ := opr(93,'CEIL'); }
                | tknFLOOR
                { $$ := opr(94,'FLOOR'); }
                | tknMOD
                { $$ := opr(95,'MOD'); }
                | tknPOWER
                { $$ := opr(96,'POWER'); }
                | tknROUND
                { $$ := opr(97,'ROUND'); }
                | tknSIGN
                { $$ := opr(98,'SIGN'); }
                | tknSQRT
                { $$ := opr(99,'SQRT'); }
                | tknTRUNC
                { $$ := opr(100,'TRUNC'); }
                ;

char_function : tknCHR
                { $$ := opr(101,'CHR'); }
              | tknLPAD
                { $$ := opr(102,'LPAD'); }
              | tknLTRIM
                { $$ := opr(103,'LTRIM'); }
              | tknRPAD
                { $$ := opr(104,'RPAD'); }
              | tknRTRIM
                { $$ := opr(105,'RTRIM'); }
              | tknSOUNDEX
                { $$ := opr(106,'SOUNDEX'); }
              | tknSUBSTR
                { $$ := opr(107,'SUBSTR'); }
              | tknLENGTH
                { $$ := opr(108,'LENGTH'); }
              ;


conversion_function : tknTO_CHAR
                { $$ := opr(109,'TO_CHAR'); }
                    | tknTO_DATE
                { $$ := opr(110,'TO_DATE'); }
                    | tknTO_NUMBER
                { $$ := opr(111,'TO_NUMBER'); }
                    ;

group_function : tknAVG
                { $$ := opr(112,'AVG'); }
               | tknCOUNT
                { $$ := opr(113,'COUNT'); }
               | tknMAX
                { $$ := opr(114,'MAX'); }
               | tknMIN
                { $$ := opr(115,'MIN'); }
               | tknSTDDEV
                { $$ := opr(116,'STDDEV'); }
               | tknSUM
                { $$ := opr(117,'SUM'); }
               | tknVARIANCE
                { $$ := opr(118,'VARIANCE'); }
               ;

/*
create_trigger
      ::=
      "create" [ "or" "replace" ] "trigger"
      [ schema_name "." ] trigger_name
      trigger_sync
      trigger_ref
      trigger_when
      plsql_block

trigger_name
      ::= identifier


trigger_sync
      ::=
      ( "before" | "after" )
      ( "delete" | "insert" | "update" [ "of" column_list ] )
      { "or" ( "delete" | "insert" | "update" [ "of" column_list ] ) }
      "on" [ schema_name "." ] table_name

trigger_ref
      ::=
      "referencing"
      [ "old" [ "as" ] identifier ]
      [ "new" [ "as" ] identifier ]

trigger_when
      ::=
      "for" "each" "row" [ "when" "(" condition ")" ]
*/

%%

{$I sqlLEX.pas}

procedure ex(p: NodePointer);
var
  i: Integer;
begin
  if (p = nil) then exit;
  case p^.Kind  of
    0:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 88;
        sqlMemProg[High(sqlMemProg)].value := p^.conNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic] + ' ' + FloatToStr(value)
      end;
    1:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 89;
        sqlMemProg[High(sqlMemProg)].stvalue := p^.stNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic] + ' ' + stvalue
      end;
    3:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 119;
        sqlMemProg[High(sqlMemProg)].stvalue := p^.dbNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic] + ' ' + stvalue
      end;
    4:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 128;
        sqlMemProg[High(sqlMemProg)].stvalue := p^.dbNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic] + ' ' + stvalue
      end;
    2:
      begin
        for i := 0 to Length(p^.oprnode^.op) - 1 do ex(p^.oprNode^.op[i]);
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := p^.oprNode^.Mnemonic;
        if (p^.oprNode^.Mnemonic <> 0) and (p^.oprNode^.Mnemonic <> 129) then
          with sqlMemProg[High(sqlMemProg)] do
            printInstruction := Mnemonics[mnemonic]
      end
  end
end;

begin
end.

