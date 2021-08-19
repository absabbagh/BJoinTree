%{
unit sql;

interface

uses
  sysUtils,
  yacclib,
  lexlib;

type

  NodePointer = ^NodeType;

  conNodePointer = ^conNodeType;
  conNodeType = record
    value: extended;
  end;

  stNodePointer = ^stNodeType;
  stNodeType = record
    value: string;
  end;

  boolNodePointer = ^boolNodeType;
  boolNodeType = record
    value: boolean;
  end;

  dbNodePointer = ^dbNodeType;
  dbNodeType = record
    value: string;
  end;

  optionNodePointer = ^optionNodeType;
  optionNodeType = record
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
      5: (boolNode: boolNodePointer);
      6: (optionNode: optionNodePointer);
    end;

type
  TLexer = class(TCustomLexer)
  private
    bufptr: integer;
    buf: array [1 .. max_chars] of AnsiChar;
  protected
    function get_char: AnsiChar; override;
    procedure unget_char(c: AnsiChar); override;
    procedure put_char(c: AnsiChar); override;
    procedure yyclear; override;
    function yywrap: boolean; override;
  public
    yyInputText: string;

    function parse() : integer; override;
  end;

  TParser = class(TCustomParser)
  public
    lexer : TLexer;

    procedure yyacceptmessage(msg: string);
    (* accept message printing routine used by the parser *)

    procedure yyerror(msg: string); reintroduce;
    (* error message printing routine used by the parser *)

    function parse() : integer; override;
  end;

const
  Mnemonics: array [0..255] of string =
    ('REPEAT', 'CREATE DATABASE', 'DATABASE NAME', 'CREATE TABLE', 'TABLE NAME',
     'NEW COLUMN', 'COLUMN NAME', 'CHAR', 'VARCHAR', 'CHAR VARYING',       //9
     'CHARACTER', 'CHARACTER VARYING', 'CLOB', 'DATE', 'NUMBER',
     'FLOAT', 'REAL', 'DOUBLE PRECISION', 'NUMBER2','DECIMAL',             //19
     'DEC', 'NUMERIC', 'NUMBER1','INTEGER', 'INT',
     'SMALLINT', 'CONSTRAINT NAME','NULL','NOT NULL', 'UNIQUE',            //29
     'PRIMARY KEY', 'REFERENCES', 'ON DELETE CASCADE', 'TABLE CONSTRAINT', 'SELECT',
     'ALL', 'DISTINCT', 'ALL COLUMNS', 'ALL TABLE COLUMNS', 'COLUMNS WITHIN EXPRESSION',
     'FROM ALIAS NAME','WHERE', 'NOT', 'OR', 'AND',
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
     'MIN', 'SUM', 'STDDEV', 'VARIANCE', 'PUSH NAME',                      //119
     'CREATE INDEX', 'INDEX NAME', 'ASC', 'DESC','INDEX COLUMN',
     'TABLE COMMENT', 'COLUMN COMMENT', 'COMMENT', 'PUSH COMMENT', 'VOID', //129
     'CHECK', 'BIGINT', 'TIME', 'TIMESTAMP', 'WITH TIME ZONE',
     'WITHOUT TIME ZONE', 'BOOLEAN', 'FOREIGN KEY', 'CREATE TRIGGER', 'TRIGGER NAME', //139
     'TRIGGER SYNC', 'BEFORE', 'AFTER', 'TRIGGER DELETE', 'TRIGGER INSERT',
     'TRIGGER UPDATE', 'FOR EACH ROW', 'WHEN CONDITION','TRIGGER STEP', 'BLOCK',      //149
     'COLUMNS PROJECTION', 'PUSH COLUMN NAME', 'CREATE JOIN INDEX', 'BASE TABLE', 'JOIN TABLES CONDITION',
     'RENAME COLUMN ', 'REFERENCE TABLE NAME', 'SHOW ALL DATABASES', 'USER_ID', 'SWITCH DATABASE', //159
     'SHOW ALL TABLES', 'SHOW ALL COLUMNS', 'SHOW ALL JOIN INDEXES', 'SHOW ALL INDEXES', 'SHOW INDEXES',
     'DROP DATABASE', 'ALTER TABLE', 'ADD', 'DROP', 'DROP CONSTRAINT', //169
     'MODIFY', 'UCASE', 'LCASE', 'MID', 'NOW',
     'FORMAT', 'AUTOINCREMENT', 'SHOW COLUMN', 'COLUMN ALIAS NAME', 'EXPRESSION ALIAS', //179
     'ALL COLUMNS AGGREGATE', 'EXPRESSION AGGREGATE', 'DISTINCT AGGREGATE', 'COUNT COLUMN NAME', 'SHOW SELECT STATEMENT HEADER',
     'SET COLUMN', 'LOAD CSV', 'LOAD SQL', 'FILE NAME', 'PARSE',
     'DROP TABLE', 'DROP INDEX', 'DROP JOIN INDEX', 'COLUMNS SPECIFIED', 'TABLES COLUMNS SPECIFIED',
     'UPLOAD CSV', 'EMPTY JSON OBJECT', 'MEMBERS OBJECT', 'EMPTY JSON ARRAY', 'ELEMENTS ARRAY', //199
     'JSON MEMBER', 'JSON ELEMENT', 'JSON PAIR', 'JSON STRING VALUE', 'JSON OBJECT VALUE',
     'JSON ARRAY VALUE', 'JSON STRING', 'START TRANSACTION', 'ROLLBACK TRANSACTION', 'ROLLBACK TO', //209
     'COMMIT TRANSACTION', 'TRANSACTION NAME', 'HOLD SAVEPOINT', 'SAVEPOINT NAME', 'RELEASE SAVEPOINT',
     'CURSOR_NAME', 'START CURSOR DECLARATION', 'OPEN CURSOR', 'FETCH CURSOR', 'CLOSE CURSOR', //219
     'END CURSOR DECLARATION', 'PUSH BOOLEAN', 'UNION', 'DATETIME', 'COLUMN FROM SUBQUERY',
     'TABLE FROM SUBQUERY', 'CREATE VIEW', 'VIEW NAME', 'CREATE USER', 'DOT', // 229
     'PASSWORD', 'GRANT', 'REVOKE', 'PRIVILEGE', 'PUSH OPTION',
     'DROP VIEW', 'RENAME USER', 'DROP USER', 'DROP TRIGGER', 'RENAME TABLE', // 239
     'DATABASE OBJECT', 'PUSH NULL', 'ADD CONSTRAINT', 'ESCAPE', 'START EXISTS',
     'SHOW ALL CONSTRAINTS', 'CREATE ROLE', 'ROLE_NAME', 'ALTER USER', 'NEW PASSWORD', //249
     'USER_ID OR ROLE_NAME', 'LOCK TABLES', 'READ', 'WRITE', 'UNLOCK TABLES',
     'LOCK TABLE ALIAS NAME');

type
  singleInstructionType = record
    mnemonic: Integer;
    boolvalue: boolean;
    value: extended;
    stvalue: string;
    printInstruction: string // just for printing
  end;
  progInstrunctionsType = array of singleInstructionType;

  selectColsInstructionstype = array of array of singleInstructionType;

var
  selectColsInstructions: selectColsInstructionstype = nil;
  aggregateColsInstructions: selectColsInstructionstype = nil;

  lQueryId: string = '';
  rescolname: string = '';

  lCoupleJason: array of record
    keyName: string;
    value: variant
  end = nil;

  sqlMemProg: progInstrunctionsType;

  sqlResults: array of string;

  resParams: array of string = nil;

  sqlpopulation: integer;

  sqlerror: integer;

  procedure ex(p: NodePointer);

%}

%union {
type YYSType = record
                 yyBoolean: Boolean;
                 yyInteger : Integer;
                 yyInt64 : Int64;
                 yyPointer : Pointer;
                 yyExtended : Extended;
                 yyReal : Real;
                 yystring : string;
               end(*YYSType*);

}


%token <Boolean> BOOL
%token <Extended> NUMBER                /* constants */
%token <Integer> NUM                    /* constants */
%token <string> ID                      /* variables */
%type <Pointer> expr                    /* expressions */
%token <string> QUOTED_STRING

%token tknCREATE                        /* keyword */
%token tknDROP                          /* keyword */
%token tknDATABASE                      /* keyword */
%token tknTABLE                         /* keyword */
%token tknCHAR                          /* keyword */
%token tknVARCHAR                       /* keyword */
%token tknCHARACTER                     /* keyword */
%token tknVARYING                       /* keyword */
%token tknCLOB                          /* keyword */
%token tknDATE                          /* keyword */
%token tknTIME                          /* keyword */
%token tknTIMESTAMP                     /* keyword */
%token tknDATETIME                      /* keyword */
%token tknWITH                          /* keyword */
%token tknWITHOUT                       /* keyword */
%token tknZONE                          /* keyword */
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
%token tknBIGINT                        /* keyword */
%token tknBOOLEAN                       /* keyword */
%token tknDEFAULT                       /* keyword */
%token tknCONSTRAINT                    /* keyword */
%token tknNULL                          /* keyword */
%token tknNOT                           /* keyword */
%token tknUNIQUE                        /* keyword */
%token tknPRIMARY                       /* keyword */
%token tknFOREIGN                       /* keyword */
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
%token tknCOMMENT                       /* keyword */
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
%token tknTRIGGER                       /* keyword */
%token tknBEFORE                        /* keyword */
%token tknAFTER                         /* keyword */
%token tknOF                            /* keyword */
%token tknFOR                           /* keyword */
%token tknEACH                          /* keyword */
%token tknROW                           /* keyword */
%token tknWHEN                          /* keyword */
%token tknBEGIN                         /* keyword */
%token tknEND                           /* keyword */
%token tknJOIN                          /* keyword */
%token tknSYSTEM                        /* KEYWORD */
%token tknDATABASES                     /* keyword */
%token tknTABLES                        /* keyword */
%token tknJOININDEXES                   /* keyword */
%token tknINDEXES                       /* keyword */
%token tknCOLUMNS                       /* keyword */
%token tknCONSTRAINTS                   /* keyword */
%token tknALTER                         /* keyword */
%token tknADD                           /* keyword */
%token tknDROP                          /* keyword */
%token tknMODIFY                        /* keyword */
%token tknLEN                           /* keyword */
%token tknUCASE                         /* keyword */
%token tknLCASE                         /* keyword */
%token tknMID                           /* keyword */
%token tknNOW                           /* keyword */
%token tknFORMAT                        /* keyword */
%token tknAUTOINCREMENT                 /* keyword */
%token tknAS                            /* keyword */
%token tknSHOW                          /* keyword */
%token tknHEADER                        /* keyword */
%token tknLOAD_CSV                      /* keyword */
%token tknUPLOAD_CSV                    /* keyword */
%token tknLOAD_SQL                      /* keyword */
%token tknPARSE                         /* keyword */
%token tknUSE                           /* keyword */
%token tknSTART                         /* keyword */
%token tknTRANSACTION                   /* keyword */
%token tknROLLBACK                      /* keyword */
%token tknCOMMIT                        /* keyword */
%token tknSAVEPOINT                     /* keyword */
%token tknLOCK                          /* keyword */
%token tknUNLOCK                          /* keyword */
%token tknREAD                          /* keyword */
%token tknWRITE                         /* keyword */
%token tknTO                            /* keyword */
%token tknRELEASE                       /* keyword */
%token tknDECLARE                       /* keyword */
%token tknCURSOR                        /* keyword */
%token tknOPEN                          /* keyword */
%token tknFETCH                         /* keyword */
%token tknCLOSE                         /* keyword */
%token tknDEALLOCATE                    /* keyword */
%token tknTRUNCATE                      /* keyword */
%token tknVIEW                          /* keyword */
%token tknUSER                          /* keyword */
%token tknIDENTIFIED                    /* keyword */
%token tknPASSWORD                      /* keyword */
%token tknGRANT                         /* keyword */
%token tknREVOKE                        /* keyword */
%token tknOPTION                        /* keyword */
%token tknIF                            /* keyword */
%token tknRENAME                        /* keyword */
%token tknUSERS                         /* keyword */
%token tknESCAPE                        /* keyword */
%token tknROLE                          /* keyword */
%token tknPRIVILEGES                    /* keyword */



%token tknEQ
%token tknLT
%token tknGT
%token tknNE
%token tknLE
%token tknGE

%type <Pointer> sql_command
%type <Pointer> to_parse_data
%type <Pointer> to_define_data
%type <Pointer> to_modify_data
%type <Pointer> alter_command
%type <Pointer> create_command
%type <Pointer> drop_command
%type <Pointer> comment_command
%type <Pointer> alter_table
%type <Pointer> alter_table_spec
%type <Pointer> create_database
%type <Pointer> drop_database
%type <Pointer> drop_table
%type <Pointer> drop_view
%type <Pointer> option_if_exists
%type <Pointer> rename_user
%type <Pointer> drop_user
%type <Pointer> drop_index
%type <Pointer> drop_join_index
%type <Pointer> drop_trigger
%type <Pointer> create_table
%type <Pointer> create_view
%type <Pointer> create_user
%type <Pointer> alter_user
%type <Pointer> database_name
%type <Pointer> table_name
%type <Pointer> view_name
%type <Pointer> user_id
%type <Pointer> option_password
%type <Pointer> password_value
%type <Pointer> option_specify_column_list
%type <Pointer> file_name
%type <Pointer> reftable_name
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
%type <Pointer> defcol_references_clause
%type <Pointer> deftbl_references_clause
%type <Pointer> option_column_list
%type <Pointer> column_list
%type <Pointer> character_type
%type <Pointer> boolean_type
%type <Pointer> date_type
%type <Pointer> option_time_precision
%type <Pointer> time_fractional_seconds_precision
%type <Pointer> option_time_zone
%type <Pointer> with_or_without_time_zone
%type <Pointer> float_type
%type <Pointer> decimal_type
%type <Pointer> integer_type
%type <Pointer> create_index
%type <Pointer> index_name
%type <Pointer> indexed_column_list
%type <Pointer> indexed_column
%type <Pointer> option_sort
%type <Pointer> comment
%type <Pointer> select_sys_command
%type <Pointer> select_command
%type <Pointer> option_duplicate
%type <Pointer> option_columns
%type <Pointer> selected_tables
%type <Pointer> rptjoin_tables
%type <Pointer> rpttable_list
%type <Pointer> option_indexed_column_list
%type <Pointer> option_where_condition
%type <Pointer> rptdisplayed_column
%type <Pointer> displayed_column
%type <Pointer> alias
%type <Pointer> selected_table
%type <Pointer> condition
%type <Integer> comparaison_op
%type <Pointer> exp_set
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
%type <Pointer> set_column
%type <Pointer> rptset_column
%type <Pointer> rptcolumn_name
%type <Pointer> delete_command
%type <Pointer> option_column_name
%type <Pointer> option_delete_cascade
%type <string>  pattern
%type <Pointer> variable
%type <Pointer> constant
%type <Pointer> function
%type <Pointer> option_expression
%type <Pointer> group_function
%type <Pointer> option_group
%type <Pointer> number_function
%type <Pointer> char_function
%type <Pointer> conversion_function
%type <Pointer> create_trigger
%type <Pointer> trigger_name
%type <Pointer> trigger_sync
%type <Pointer> option_trigger_when
%type <Pointer> trigger_action
%type <Pointer> create_role
%type <Pointer> role_name
%type <Pointer> grant_privileges
%type <Pointer> grant_system_privileges
%type <Pointer> grant_object_privileges
%type <Pointer> grant_roles
%type <Pointer> option_object
%type <Pointer> revoke_privileges
%type <Pointer> revoke_system_privileges
%type <Pointer> revoke_object_privileges
%type <Pointer> option_system_privilege
%type <Pointer> option_system_privileges
%type <Pointer> option_object_privilege
%type <Pointer> option_object_privileges
%type <Pointer> option_grant_option
%type <Pointer> before_after
%type <Pointer> rpt_delete_insert_update
%type <Pointer> delete_insert_update
%type <Pointer> trigger_step
%type <Pointer> rpt_trigger_step
%type <Pointer> option_of_column_list
%type <Pointer> option_for_each
%type <Pointer> option_when_condition
%type <Pointer> create_join_index
%type <Pointer> option_order_by
%type <Pointer> json_string
%type <Pointer> json_format
%type <Pointer> json_object
%type <Pointer> json_members
%type <Pointer> json_pair
%type <Pointer> json_array
%type <Pointer> json_elements
%type <Pointer> json_value
%type <Pointer> sql_transcations_statement
%type <Pointer> sql_locks_statement
%type <Pointer> lock_table_list
%type <Pointer> lock_table
%type <Pointer> transaction_name
%type <Pointer> option_transaction_name
%type <Pointer> savepoint_name
%type <Pointer> cursor_declaration
%type <Pointer> open_cursor
%type <Pointer> fetch_cursor
%type <Pointer> close_cursor
%type <Pointer> cursor_name
%type <Pointer> cursor_deallocation

%left tknOR
%left tknAND
%left tknEQ tknNE
%left tknLT tknGT tknLE tknGE tknBETWEEN URELATIONAL
%left '-' '+'  tknUNION tknMINUS           /* operators */
%left '*' '/' '%' tknINTERSECT
%left '(' ')'
%right UMINUS '.'


/*
  Operators	        Precedence
  postfix	                expr++ expr--                            ()   []   ->   .   ::	Function call, scope, array/member access
  unary	                ++expr --expr +expr -expr ~ !            !   ~   -   +   *   &   sizeof   type cast   ++   --  	(most) unary operators, sizeof and type casts (right to left)
  multiplicative	        * / % MOD
  additive	        + -
  shift	                << >> >>>  Bitwise shift left and right
  relational      	< > <= >= instanceof
  equality	        == !=
  bitwise AND	        &
  bitwise exclusive       OR (XOR)	^
  bitwise inclusive       OR (normal) 	|
  logical AND	        &&
  logical OR	        ||
  ternary	? :             Conditional expression
  assignment	        = += -= *= /= %= &= ^= |= <<= >>= >>>=  Assignment operators (right to left)
  ,	                Comma operator
*/


%token ILLEGAL                          /* illegal token */

%%

input	: /* empty */
	| input '\n'
                { yyaccept; }
        | input json_format '\n'
                { ex($2); yyaccept; }
        | input sql_transcations_statement ';' '\n'
                { ex($2); yyaccept; }
        | input sql_locks_statement ';' '\n'
                { ex($2); yyaccept; }
        | input sql_command ';' '\n'
                { ex($2); yyaccept; }
        | input condition '\n'
                { ex($2); yyaccept; }
	| error '\n'
                { yyerrok; }
	;


json_format : json_object
                { $$ := $1; }
            | json_array
                { $$ := $1; }
            ;

json_object : '{' '}'
                 { $$ := opr(196,'EMPTY JSON OBJECT',[]); }
            | '{' json_members '}'
                 { $$ := opr(197,'MEMBERS OBJECT',[$2]); }
            ;

json_members : json_pair
                 { $$ := opr(200,'JSON MEMBER',[$1]); }
             | json_pair ',' json_members
                 { $$ := opr(200,'REPEAT JSON MEMBER',[$1,$3]); }
             ;


json_pair : json_string ':' json_value
               { $$ := opr(202,'JSON PAIR',[$1,$3]); }
          ;

json_array : '[' ']'
               { $$ := opr(198,'EMPTY JSON ARRAY',[]); }
           | '[' json_elements ']'
               { $$ := opr(199,'ELEMENTS ARRAY',[$2]); }
           ;

json_elements : json_value
                  { $$ := opr(201,'JSON ELEMENT',[$1]); }
              | json_value ',' json_elements
                  { $$ := opr(201,'REPEAT JSON ELEMENT',[$1,$3]); }
              ;

json_value : json_string
               { $$ := opr(203,'JSON STRING VALUE',[$1]); }
           | json_object
                { $$ := opr(204,'JSON OBJECT VALUE',[$1]); }
           | json_array
                { $$ := opr(205,'JSON ARRAY VALUE',[$1]); }
/*
           | 'true'
           ;
           | 'false'
           ;
           | 'null'
*/
           ;

json_string : number_quoted_string
                { $$ := opr(206,'JSON STRING',[$1]); }
            ;
/*
json_chars : json_char
           | json_char json_chars
           ;

json_char : any-Unicode-character-except-"-or-\-or-control-character
          | \"
          | \\
          | \/
          | \b
          | \f
          | \n
          | \r
          | \t
          | \u four-hex-digits
          ;

json_number : | json_int json_frac
              | json_int json_exp
              | json_int json_frac json_exp
              ;

json_int : json_digit
         | json_digit1-9 json_digits
         | '-' json_digit
         | json_digit1-9 json_digits
         ;

json_frac : '.' json_digits
          ;

json_exp : json_e json_digits
         ;

json_digits : json_digit
            | json_digit json_digits
            ;

json_e : 'e'
       | 'e+'
       | 'e-'
       | 'E'
       | 'E+'
       | 'E-'
       ;

*/

sql_transcations_statement : tknSTART tknTRANSACTION option_transaction_name
                               { $$ := opr(207,'START TRANSACTION',[$3]); }
                           | tknROLLBACK option_transaction_name
                               { $$ := opr(208,'ROLLBACK TRANSACTION',[$2]); }
                           | tknROLLBACK tknTO savepoint_name
                               { $$ := opr(209,'ROLLBACK TO',[$3]); }
                           | tknCOMMIT option_transaction_name
                               { $$ := opr(210,'COMMIT TRANSACTION',[$2]); }
                           | tknSAVEPOINT savepoint_name
                               { $$ := opr(212,'HOLD SAVEPOINT',[$2]); }
                           | tknRELEASE tknSAVEPOINT savepoint_name
                               { $$ := opr(214,'RELEASE SAVEPOINT',[$3]); }
                           ;

transaction_name : QUOTED_STRING
                    { $$ := opr(211,'TRANSACTION NAME',[stcon($1)]); }
                 ;

option_transaction_name : /* empty */
                            { $$ := nil; }
                        | transaction_name
                            { $$ := $1; }
                        ;

savepoint_name : ID
                   { $$ := opr(213,'SAVEPOINT NAME',[DBName($1)]); }
                ;


/*
LOCK TABLES tbl_name [AS alias] {READ | [READ LOCAL] | [LOW_PRIORITY] WRITE}
            [, tbl_name {READ | [LOW_PRIORITY] WRITE} ...]

UNLOCK TABLES
*/

sql_locks_statement : tknLOCK tknTABLES lock_table_list
                      { $$ := opr(251,'LOCK TABLES',[$3]); }
                    | tknUNLOCK tknTABLES
                      { $$ := opr(254,'UNLOCK TABLES',[]); }
                    ;

lock_table_list :  lock_table
                { $$ := $1; }
                |   lock_table_list ',' lock_table
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                ;

lock_table : table_name tknREAD
             { $$ := opr(252,'READ',[$1]); }
           | table_name tknAS alias tknREAD
             { $$ := opr(252,'READ',[$1,opr(255,'LOCK TABLE ALIAS NAME',[$3])]); }
           | table_name tknWRITE
             { $$ := opr(253,'WRITE',[$1]); }
           | table_name tknAS alias tknWRITE
             { $$ := opr(253,'WRITE',[$1,opr(255,'LOCK TABLE ALIAS NAME',[$3])]); }
           ;

sql_command : to_define_data
                { $$ := $1; }
            | to_modify_data
                { $$ := $1; }
            /*
            | to_parse_data
                { $$ := $1; }
            */
            ;

/* User Name --> Directory
   DataBase  --> Sub-Directory
   Use Database command
*/

/*
to_parse_data: tknPARSE comment
                { $$ := opr(189,'PARSE',[$2]); }
              ;
should be:
to_parse_data: tknPARSE comment tknAS datatype
                { $$ := opr(189,'PARSE',[$2]); }
              ;
*/

to_define_data : create_command
                { $$ := $1; }
               | alter_command
                { $$ := $1; }
               | drop_command
                { $$ := $1; }
               | comment_command
                { $$ := $1; }
               ;

alter_command : alter_table
               { $$ := $1; }
              | rename_user
               { $$ := $1; }
              | alter_user
               { $$ := $1; }
              ;

alter_table  : tknALTER tknTABLE table_name tknADD new_column option_new_constraint_list
              { $$ := opr(166,'ALTER TABLE',[$3,opr(167,'ADD',[$5,$6])]); }
             | tknALTER tknTABLE table_name tknADD '(' new_column_list option_new_constraint_list ')'
              { $$ := opr(166,'ALTER TABLE',[$3,opr(167,'ADD',[$6,$7])]); }
             | tknALTER tknTABLE table_name tknADD new_constraint_list
              { $$ := opr(166,'ALTER TABLE',[$3,opr(242,'ADD CONSTRAINT',[$5])]); }
             | tknALTER tknTABLE table_name tknDROP tknCOLUMN column_name
              { $$ := opr(166,'ALTER TABLE',[$3,opr(168,'DROP',[$6])]); }
             | tknALTER tknTABLE table_name tknDROP tknCONSTRAINT constraint_name
              { $$ := opr(166,'ALTER TABLE',[$3,opr(169,'DROP CONSTRAINT',[$6])]); }
             | tknALTER tknTABLE table_name tknDROP '(' column_list ')'
              { $$ := opr(166,'ALTER TABLE',[$3,opr(168,'DROP',[$6])]); }
             | tknALTER tknTABLE table_name tknMODIFY new_column option_new_constraint_list
              { $$ := opr(166,'ALTER TABLE',[$3,opr(170,'MODIFY',[$5,$6])]); }
             | tknALTER tknTABLE table_name tknMODIFY '(' new_column_list option_new_constraint_list ')'
              { $$ := opr(166,'ALTER TABLE',[$3,opr(170,'MODIFY',[$6,$7])]); }
             | tknALTER tknTABLE table_name tknRENAME tknCOLUMN column_name tknTO column_name
              { $$ := opr(166,'ALTER TABLE',[$3,opr(155,'RENAME COLUMN',[$6,$8])]); }
             | tknALTER tknTABLE table_name tknRENAME tknTO table_name
              { $$ := opr(166,'ALTER TABLE',[$3,opr(239,'RENAME TABLE',[$6])]); }
             ;

create_command : create_database
                { $$ := $1; }
               | create_table
                { $$ := $1; }
               | create_view
                { $$ := $1; }
               | create_user
                { $$ := $1; }
               | create_index
                { $$ := $1; }
               | create_join_index
                { $$ := $1; }
               | create_trigger
                { $$ := $1; }
               | grant_privileges
                { $$ := $1; }
               | create_role
                { $$ := $1; }
               ;

drop_command : drop_database
                { $$ := $1; }
               | drop_table
                { $$ := $1; }
               | drop_view
                { $$ := $1; }
               | drop_user
                { $$ := $1; }
               | drop_index
                { $$ := $1; }
               | drop_join_index
                { $$ := $1; }
               | drop_trigger
               | revoke_privileges
                { $$ := $1; }
             ;

create_database : tknCREATE tknDATABASE database_name
                { $$ := opr(1,'CREATE DATABASE',[$3]); }
                ;

database_name : ID
                { $$ := opr(2,'DATABASE NAME',[DBName($1)]); }
                ;

drop_database: tknDROP tknDATABASE database_name
               { $$ := opr(165,'DROP DATABASE',[$3]); }
               ;

create_table : tknCREATE tknTABLE table_name '(' new_column_list option_new_constraint_list ')'
               { $$ := opr(3,'CREATE TABLE',[$3,$5,$6]); }
             | tknCREATE tknTABLE table_name tknAS select_command
               { $$ := opr(3,'CREATE TABLE',[$3,$5]); }
               ;

table_name : ID
               { $$ := opr(4,'TABLE NAME',[DBName($1)]); }
           | ID '.' ID
               { $$ := opr(229,'NULL COMMAND',[opr(2,'DATABASE NAME',[DBName($1)]),opr(4,'TABLE NAME',[DBName($3)])]); }
           ;

create_view : tknCREATE tknVIEW view_name tknAS select_command
               { $$ := opr(226,'CREATE VIEW',[$3,$5]); }
            ;

view_name : ID
               { $$ := opr(227,'VIEW NAME',[DBName($1)]); }
          ;

create_user : tknCREATE tknUSER user_id tknIDENTIFIED tknBY option_password password_value
               { $$ := opr(228,'CREATE USER',[$3,opr(230,'PASSWORD',[$7])]); }
            ;

alter_user   :  tknALTER tknUSER user_id tknIDENTIFIED tknBY option_password password_value tknTO option_password password_value
               { $$ := opr(248,'ALTER USER',[$3,opr(230,'PASSWORD',[$7]),opr(249,'NEW PASSWORD',[$10])]); }
             ;

create_role : tknCREATE tknROLE role_name
               { $$ := opr(246,'CREATE ROLE',[$3]); }
            ;

role_name : ID
          { $$ := opr(247,'ROLE ID',[DBName($1)]); }
        ;

option_password :  /* empty */
                { $$ := nil; }
                      | tknPASSWORD
                { $$ := nil; }
                   ;

password_value : QUOTED_STRING
                { $$ := stcon($1); }
             ;

/*
  GRANT [system_privileges | roles]
  TO [user | role | PUBLIC] {WITH GRANT OPTION}
    to grant generic privileges

  GRANT [ALL {PRIVILEGES} | SELECT | INSERT | UPDATE | DELETE] ON object
  TO [user | role | PUBLIC] {WITH GRANT OPTION}
    to grant privileges on objects

  SQL GRANT ON
  The ON clause is only used to grant object privileges – not system privileges.
  This clause specifies which object privileges (as in which table privileges,
    view privileges, etc..) are being granted.


 Strored Procedure at compile time can see the GRANT if satisfy


 Privilege Statements
  Privilege        Description
  DELETE           Allows a user to delete rows from tables within the schema
  INSERT           Allows a user to insert rows of data into tables within the schema
  REFERENCES       Allows a user to set up references to primary keys within the schema
  SELECT           Allows a user to select rows from tables within the schema
  TRIGGER          Allows a user to create triggers on tables within the schema
  UPDATE           Allows a user to update rows in tables within the schema
  EXECUTE          Allows users to execute functions or stored procedures within the schema
  CREATE [ANY] [TABLE | VIEW | SEQUENCE | ...]
                   Allows a user to create a table in his own database or ANY table from any database,
                     in this case he is not allow to issue a select statement
                   You can also use the ALL keyword to indicate that you wish to grant
                     the ANSI-92 permissions (ie: SELECT, INSERT, UPDATE, DELETE, and REFERENCES) to a user.
*/

/*
  CREATE SEQUENCE [schema].[Name_of_Sequence]
    [ AS <data type> ]
    [ START WITH <value> ]
    [ INCREMENT BY <value> ]
    [ MINVALUE <value > | NO MINVALUE ]
    [ MAXVALUE <value> | NO MAXVALUE ]
    [ CYCLE | NO CYCLE ]
    [ CACHE value | NO CACHE ];
  Parameter                Description

  CREATE SEQUENCE          Used to create a sequence followed by a database schema and the name of the sequence
  AS                       Specifies the data type of the sequence.
                           Data types can be Decimal, Int, SmallInt, TinyInt, and BigInt.
                           The default value for the data type is BigInt
  START WITH               Sets the starting value for the sequence object
  INCREMENT BY             Sets the amount that you want your sequence object to increment by
  MIN VALUE                This is an optional parameter that specifies the minimum value for the sequence object
  MAX VALUE                This is an optional parameter that sets the maximum value for the sequence object
  CYCLE                    This specifies if the sequence object should be restarted once it has reached its maximum or minimum value.
                           It is an optional parameter for which the default value is NO CYCLE
  CACHE                    This is used to cache sequence object values.
                           It is also optional parameter with the default value of NO CACHE

  ALTER SEQUENCE [NewCounter]
  RESTART WITH 7
    The above script will modify the existing sequence object ‘NewCounter’ by updating its starting value to 7.
    Now if you execute the following statement:
      SELECT NEXT VALUE FOR [dbo].[NewCounter]
    You will see ‘7’ in the output.

  INSERT INTO Students VALUES (NEXT VALUE FOR [dbo].[IdCounter], 'Sally', 20)

  DROP SEQUENCE [NewCounter]
    Cannot be used anymore
*/

grant_privileges : grant_system_privileges
                   { $$ := $1; }
                 | grant_object_privileges
                   { $$ := $1; }
                 | grant_roles
                   { $$ := $1; }
                 ;

grant_system_privileges : tknGRANT option_system_privileges tknTO ID option_grant_option
                          { $$ := opr(231,'GRANT',[$2,opr(250,'USER_ID OR ROLE_NAME',DBName($4)),$5]); }
                        ;

option_system_privileges : option_system_privilege
                           { $$ := $1; }
                         |   option_system_privileges ',' option_system_privilege
                             { $$ := opr(0,'REPEAT',[$1,$3]); }
                         ;

option_system_privilege : tknCREATE tknDATABASE
                          { $$ := opr(233,'PRIVILEGE',[stOption('CREATE DATABASE')]); }
                        | tknCREATE tknTABLE
                          { $$ := opr(233,'PRIVILEGE',[stOption('CREATE TABLE')]); }
                        | tknCREATE tknANY tknTABLE /* Could be a table not belong to the user in another schema */
                            { $$ := opr(233,'PRIVILEGE',[stOption('CREATE ANY TABLE')]); }
                        | tknCREATE tknVIEW
                            { $$ := opr(233,'PRIVILEGE',[stOption('CREATE VIEW')]); }
                        | tknCREATE tknANY tknVIEW
                            { $$ := opr(233,'PRIVILEGE',[stOption('CREATE ANY VIEW')]); }
                        | tknCREATE tknINDEX
                          { $$ := opr(233,'PRIVILEGE',[stOption('CREATE INDEX')]); }
                        | tknCREATE tknANY tknINDEX
                          { $$ := opr(233,'PRIVILEGE',[stOption('CREATE ANY INDEX')]); }
                        | tknALTER
                            { $$ := opr(233,'PRIVILEGE',[stOption('ALTER')]); }
                        | tknDROP
                            { $$ := opr(233,'PRIVILEGE',[stOption('DROP')]); }
                        | tknCREATE tknUSERS
                            { $$ := opr(233,'PRIVILEGE',[stOption('CREATE USERS')]); }
                        | tknALL tknPRIVILEGES
                            { $$ := opr(233,'PRIVILEGE',[stOption('ALL PRIVILEGES')]); }
                        ;

grant_roles             : tknGRANT tknROLE role_name tknTO user_id option_grant_option
                          { $$ := opr(250,'GRANT ROLE',[$3,$5,$6]); }
                        ;

grant_object_privileges : tknGRANT option_object_privileges tknON option_object tknTO user_id option_grant_option
                          { $$ := opr(231,'GRANT',[$2,opr(240,'DATABASE OBJECT',[$4]),$6,$7]); }
                        ;

option_object : QUOTED_STRING
                { $$ := DBName($1); }
              | ID
                { $$ := DBName($1); }
              ;

option_object_privileges : option_object_privilege
                      { $$ := $1; }
                  |   option_object_privileges ',' option_object_privilege
                      { $$ := opr(0,'REPEAT',[$1,$3]); }
                  ;

option_object_privilege : tknSELECT
                          { $$ := opr(233,'PRIVILEGE',[stOption('SELECT')]); }
                        | tknINSERT
                            { $$ := opr(233,'PRIVILEGE',[stOption('INSERT')]); }
                        | tknUPDATE
                            { $$ := opr(233,'PRIVILEGE',[stOption('UPDATE')]); }
                        | tknDELETE
                            { $$ := opr(233,'PRIVILEGE',[stOption('DELETE')]); }
                        | tknALL
                            { $$ := opr(233,'PRIVILEGE',[stOption('ALL')]); }
                        ;

option_grant_option :  /* empty */
                        { $$ := nil; }
                    | tknWITH tknGRANT tknOPTION
                        { $$ := opr(233,'PRIVILEGE',[stOption('GRANT OPTION')]); }
                    ;

drop_table : tknDROP tknTABLE table_name
               { $$ := opr(190,'DROP TABlE',[$3]); }
           ;

drop_view : tknDROP tknVIEW option_if_exists view_name
               { $$ := opr(235,'DROP VIEW',[$4,$3]); }
           ;

option_if_exists : /* empty */
                   { $$ := nil; }
                 | tknIF tknEXISTS
                   { $$ := opr(68,'EXISTS',[]); }
                 ;

rename_user : tknRENAME tknUSER user_id tknTO user_id
               { $$ := opr(236,'RENAME USER',[$3,$5]); }
            ;

drop_user : tknDROP tknUSER user_id
               { $$ := opr(237,'DROP USER',[$3]); }
            ;

drop_index: tknDROP tknINDEX index_name
               { $$ := opr(191,'DROP INDEX',[$3]); }
          ;

drop_join_index: tknDROP tknJOIN tknINDEX index_name
               { $$ := opr(192,'DROP JOIN INDEX',[$4]); }
               ;

drop_trigger : tknDROP tknTRIGGER trigger_name
               { $$ := opr(238,'DROP TRIGGER',[$3]); }
             ;


revoke_privileges : revoke_system_privileges
                    { $$ := $1; }
                  |
                    revoke_object_privileges
                    { $$ := $1; }
                 ;

revoke_system_privileges : tknREVOKE option_system_privileges tknFROM user_id
                           { $$ := opr(232,'REVOKE',[$2,$4]); }
                         ;

revoke_object_privileges : tknREVOKE option_object_privileges tknON option_object tknFROM user_id
                           { $$ := opr(232,'REVOKE',[$2,opr(240,'DATABASE OBJECT',[$4]),$6]); }
                         ;

file_name : QUOTED_STRING
                { $$ := opr(188,'FILE NAME',[DBName($1)]); }
           ;

reftable_name : ID
                { $$ := opr(156,'REFERENCE TABLE NAME',[DBName($1)]); }
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
         | boolean_type
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
               | tknCHARACTER tknVARYING '(' NUM ')'
                { $$ := opr(11,'CHARACTER VARYING',[con($4)]); }
               | tknCLOB '(' NUM ')'
                { $$ := opr(12,'CLOB',[con($3)]); }
               ;

boolean_type : tknBOOLEAN
                { $$ := opr(136,'BOOLEAN'); }
             ;

date_type : tknDATE
                { $$ := opr(13,'DATE'); }
          | tknDATETIME option_time_precision option_time_zone
                { $$ := opr(223,'DATETIME',[$2,$3]); }
          | tknTIME option_time_precision option_time_zone
                { $$ := opr(132,'TIME',[$2,$3]); }
          | tknTIMESTAMP option_time_precision option_time_zone
                { $$ := opr(133,'TIMESTAMP',[$2,$3]); }
          ;

option_time_precision :  /* empty */
                { $$ := nil; }
                      | '(' time_fractional_seconds_precision ')'
                { $$ := $2; }
                   ;

time_fractional_seconds_precision : NUM
                { $$ := con($1); }
                                  ;

option_time_zone :  /* empty */
                { $$ := nil; }
                 | with_or_without_time_zone
                { $$ := $1; }
                   ;


with_or_without_time_zone : tknWITH tknTIME tknZONE
                { $$ := opr(134,'WITH TIME ZONE'); }
                          | tknWITHOUT tknTIME tknZONE
                { $$ := opr(135,'WITHOUT TIME ZONE'); }
                          ;

float_type : tknNUMBER '(' NUM ')'
                { $$ := opr(14,'NUMBER',[con($3)]); } /* not in SQL-92 */
           | tknFLOAT '(' NUM ')'
                { $$ := opr(15,'FLOAT',[con($3)]); }
           | tknREAL
                { $$ := opr(16,'REAL'); }
           | tknDOUBLE
                { $$ := opr(17,'DOUBLE PRECISION'); }
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
integer_type : tknNUMBER
                { $$ := opr(22,'NUMBER1'); } /* not in SQL-92 */
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
                          | tknAUTOINCREMENT
                { $$ := opr(176,'AUTOINCREMENT'); }
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
                      | defcol_references_clause
                { $$ := $1; }
                      | tknCHECK condition
                { $$ := opr(130,'CHECK',[$2]); }
                      ;

defcol_references_clause : tknFOREIGN tknKEY tknREFERENCES reftable_name option_column_name
                          { $$ := opr(137,'FOREIGN KEY',[$4,$5]); }
                         | tknREFERENCES reftable_name option_column_name option_delete_cascade
                          { $$ := opr(31,'REFERENCES',[$2,$3,$4]); }
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
                     | tknCHECK condition
                { $$ := opr(130,'CHECK',[$2]); }
                     | tknFOREIGN tknKEY '(' column_list ')' deftbl_references_clause
                { $$ := opr(137,'FOREIGN KEY',[$4,$6]); }
                     ;

deftbl_references_clause : tknREFERENCES reftable_name option_column_list option_delete_cascade
                { $$ := opr(31,'REFERENCES',[$2,$3,$4]); }
                         ;

option_column_list :  /* empty */
                { $$ := nil; }
                   | '(' column_list ')'
                { $$ := $2; }
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

create_join_index : tknCREATE tknJOIN tknINDEX index_name tknON rpttable_list tknWHERE condition option_order_by
                { $$ := opr(152,'CREATE JOIN INDEX',[$4,$6,opr(154,'JOIN TABLES CONDITION',[$8]),$9]); }
                  ;

rpttable_list : table_name option_indexed_column_list
            { $$ :=  opr(153,'BASE TABLE',[$1,$2]); }
              | rpttable_list ',' table_name option_indexed_column_list
            { $$ :=  opr(153,'BASE TABLE',[$1,$3,$4]); }
              ;

option_indexed_column_list :  /* empty */
                         { $$ := nil; }
                           | '(' indexed_column_list ')'
                         { $$ := $2; }
                           ;

option_order_by :  /* empty */
                { $$ := nil; }
                | tknORDER tknBY rptsorted_def
                { $$ := opr(70,'ORDER BY',[$3]); }
                ;

comment_command : tknCOMMENT tknON tknTABLE ID tknIS comment
                 { $$ := opr(125,'TABLE COMMENT',[opr(4,'TABLE NAME',[DBName($4)]),$6]); }
                |  tknCOMMENT tknON tknTABLE ID '.' ID tknIS comment
                 { $$ := opr(125,'TABLE COMMENT',[opr(2,'DATABASE NAME',[DBName($4)]),opr(4,'TABLE NAME',[DBName($6)]),$8]); }
                |  tknCOMMENT tknON tknCOLUMN ID '.' ID tknIS comment
                 { $$ := opr(126,'COLUMN COMMENT',[opr(4,'TABLE NAME',[DBName($4)]),opr(6,'COLUMN NAME',[DBName($6)]),$8]); }
                |  tknCOMMENT tknON tknCOLUMN ID '.' ID '.' ID tknIS comment
                 { $$ := opr(126,'COLUMN COMMENT',[opr(2,'DATABASE NAME',[DBName($4)]),opr(4,'TABLE NAME',[DBName($6)]),opr(6,'COLUMN NAME',[DBName($8)]),$10]); }
                ;

comment : QUOTED_STRING
                { $$ := opr(129,'VOID',[DBcomment($1)]); }
        ;

to_modify_data : cursor_declaration
                { $$ := $1; }
               | open_cursor
                { $$ := $1; }
               | fetch_cursor
                { $$ := $1; }
               | close_cursor
                { $$ := $1; }
               | cursor_deallocation
                { $$ := $1; }
               | select_command
                { $$ := $1; }
               | select_sys_command
                { $$ := $1; }
               | insert_command
                { $$ := $1; }
               | update_command
                { $$ := $1; }
               | delete_command
                { $$ := $1; }
               ;

cursor_declaration : tknDECLARE cursor_name tknCURSOR tknFOR select_command
                    { $$ := opr(216,'START CURSOR DECLARATION',[$2,$5]); }
                   ;

open_cursor : tknOPEN cursor_name
             { $$ := opr(217,'OPEN CURSOR',[$2]); }
            ;

fetch_cursor : tknFETCH cursor_name
              { $$ := opr(218,'FETCH CURSOR',[$2]); }
             ;

close_cursor : tknCLOSE cursor_name
              { $$ := opr(219,'CLOSE CURSOR',[$2]); }
             ;

cursor_deallocation : tknDEALLOCATE cursor_name
                     { $$ := opr(220,'END CURSOR DECLARATION',[$2]); }
                    ;
cursor_name : ID
             { $$ := opr(215,'CURSOR NAME',[DBName($1)]); }
            ;
/*
  mysql> SHOW COLUMNS FROM City;
  +------------+----------+------+-----+---------+----------------+
  | Field      | Type     | Null | Key | Default | Extra          |
  +------------+----------+------+-----+---------+----------------+
  | Id         | int(11)  | NO   | PRI | NULL    | auto_increment |
  | Name       | char(35) | NO   |     |         |                |
  | Country    | char(3)  | NO   | UNI |         |                |
  | District   | char(20) | YES  | MUL |         |                |
  | Population | int(11)  | NO   |     | 0       |                |
  +------------+----------+------+-----+---------+----------------+
*/

select_sys_command :tknSHOW tknDATABASES
                    { $$ := opr(157,'SHOW ALL DATABASES',[]); }
                    | tknSHOW tknDATABASES tknFOR user_id
                    { $$ := opr(157,'SHOW ALL DATABASES',[$4]); }
                    | tknUSE database_name
                    { $$ := opr(159,'SWITCH DATABASE',[$2]); }
                    | tknSHOW tknTABLES
                    { $$ := opr(160,'SHOW ALL TABLES',[]); }
                    | tknSHOW tknCOLUMNS tknFROM table_name
                    { $$ := opr(161,'SHOW ALL COLUMNS',[$4]); }
                    | tknSHOW tknJOIN tknINDEXES
                    { $$ := opr(162,'SHOW ALL JOIN INDEXES',[]); }
                    | tknSHOW tknINDEXES tknFROM table_name
                    { $$ := opr(163,'SHOW ALL INDEXES FROM TABLE',[$4]); }
                    | tknSHOW tknINDEXES
                    { $$ := opr(164,'SHOW ALL INDEXES',[]); }
                    | tknSHOW tknCONSTRAINTS tknFROM table_name
                    { $$ := opr(245,'SHOW ALL CONSTRAINTS',[$4]); }
                    | tknSYSTEM tknLOAD_CSV tknFROM file_name
                    { $$ := opr(186,' LOAD CSV',[$4]); }
                    | tknSYSTEM tknUPLOAD_CSV tknFROM table_name
                    { $$ := opr(195,' UPLOAD CSV',[$4]); }
                    | tknSYSTEM tknLOAD_SQL tknFROM file_name
                    { $$ := opr(187,' LOAD SQL',[$4]); }
                    | tknHEADER set_clause
                    { $$ := opr(184,'SHOW SELECT STATEMENT HEADER',[$2]); }
                    ;

user_id : ID
         { $$ := opr(158,'USER ID',[DBName($1)]); }
         ;

select_command : set_clause
                { $$ := $1; }
               ;

option_duplicate : /* empty */
                { $$ := nil; }
                 | tknALL
                { $$ := opr(35,'ALL'); }
                 | tknDISTINCT
                { $$ := opr(36,'DISTINCT'); }
                 ;

option_columns : rptdisplayed_column
                { $$ := $1; }
               ;

rptdisplayed_column : displayed_column
                { $$ := $1; }
                    | rptdisplayed_column ',' displayed_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                    ;

/* The trick to placing a subquery in the SELECT clause is that the subquery must
     return a single value. This is why an aggregate function such as the SUM,
     COUNT, MIN, MAX, or AVG function is commonly used in the subquery.
*/

displayed_column : '*'
                { $$ := opr(37,'ALL COLUMNS'); }
                 | table_name '.' '*'
                { $$ := opr(38,'ALL TABLE COLUMNS',[$1]); }
                 | expr
                { $$ := opr(39,'COLUMNS WITHIN EXPRESSION',[$1]); }
                 | expr alias
                { $$ := opr(39,'COLUMNS WITHIN EXPRESSION',[$1,opr(178,'COLUMN ALIAS NAME',[$2])]); }
                 | expr tknAS alias
                { $$ := opr(39,'COLUMNS WITHIN EXPRESSION',[$1,opr(178,'COLUMN ALIAS NAME',[$3])]); }
                 | subquery
                { $$ := opr(224,'COLUMN FROM SUBQUERY',[$1]); }
                 | subquery alias
                { $$ := opr(224,'COLUMN FROM SUBQUERY',[$1,opr(178,'COLUMN ALIAS NAME',[$2])]); }
                 | subquery tknAS alias
                { $$ := opr(224,'COLUMN FROM SUBQUERY',[$1,opr(178,'COLUMN ALIAS NAME',[$3])]); }
                 ;

alias : ID
       { $$ := DBName($1); }
      ;

selected_tables : selected_table
                { $$ := $1; }
                | selected_tables ',' selected_table
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                ;
/* The [AS] name clause is mandatory, because every table in a FROM clause must
     have a name.
   Any columns in the subquery select list must have unique names.
   This will be the name used to reference this subquery or any of its fields.
   The subquery can be seen as a view */
/*

FROM table1
[ { INNER JOIN
  | LEFT [OUTER] JOIN
  | RIGHT [OUTER] JOIN
  | FULL [OUTER] JOIN } table2
ON table1.column1 = table2.column1 ]

SELECT O.OrderNumber, CONVERT(date,O.OrderDate) AS Date,
       P.ProductName, I.Quantity, I.UnitPrice
  FROM [Order] O
  JOIN OrderItem I ON O.Id = I.OrderId
  JOIN Product P ON P.Id = I.ProductId
ORDER BY O.OrderNumber
*/


selected_table : table_name
                { $$ := $1; }
               | table_name rptjoin_tables
                { $$ := opr(129,'VOID',[$1,$2]); }
               | table_name alias
                { $$ := opr(129,'VOID',[$1,opr(40,'FROM ALIAS NAME',[$2])]); }
               | table_name tknAS alias
                { $$ := opr(129,'VOID',[$1,opr(40,'FROM ALIAS NAME',[$3])]); }
               | subquery alias
                { $$ := opr(225,'TABLE FROM SUBQUERY',[$1,opr(40,'FROM ALIAS NAME',[$2])]); }
               | subquery tknAS alias
                { $$ := opr(225,'TABLE FROM SUBQUERY',[$1,opr(40,'FROM ALIAS NAME',[$3])]); }
               ;

rptjoin_tables : tknJOIN table_name tknON condition
                { $$ := opr(129,'TABLE COLUMN COUPLES',[opr(153,'BASE TABLE',[$2]),opr(154,'JOIN TABLES CONDITION',[$4])])  ; }
            | rptjoin_tables tknJOIN table_name tknON condition
                { $$ := opr(129,'TABLE COLUMN COUPLES',[$1,opr(153,'BASE TABLE',[$3]),opr(154,'JOIN TABLES CONDITION',[$5])])  ; }
                       ;

option_where_condition : /* empty */
                { $$ := nil; }
                       | tknWHERE condition
                { $$ := opr(41,'WHERE',[$2]); }
                       ;

condition : condition tknEQ condition
                { $$ := opr(50,'EQ',[$1,$3]); }
          | condition tknNE condition
                { $$ := opr(53,'NE',[$1,$3]); }
          | condition tknLT condition
                { $$ := opr(51,'LT',[$1,$3]); }
          | condition tknGT condition
                { $$ := opr(52,'GT',[$1,$3]); }
          | condition tknLE condition
                { $$ := opr(54,'LE',[$1,$3]); }
          | condition tknGE condition
                { $$ := opr(55,'GE',[$1,$3]); }
        |  condition '+' condition
                { $$ := opr(83,'ADD',[$1,$3]); }
        |  condition tknAND condition
                { $$ := opr(44,'AND',[$1,$3]); }
	|  condition '-' condition
                { $$ := opr(84,'SUB',[$1,$3]); }
	|  condition '*' condition
                { $$ := opr(85,'MUL',[$1,$3]); }
        |  condition tknOR condition
                { $$ := opr(43,'OR',[$1,$3]); }
	|  condition '/' condition
                { $$ := opr(86,'DIV',[$1,$3]); }
        |  condition '%' condition
                { $$ := opr(95,'MOD',[$1,$3]); }
	|  '(' condition ')'
                { $$ := $2; }
	|  '+' condition %prec UMINUS
                { $$ := $2; }
	|  '-' condition %prec UMINUS
                { $$ := opr(79,'UMINUS',[$2]); }
	|  tknNOT condition %prec UMINUS
                { $$ := opr(42,'NOT',[$2]); }
        |  constant
                { $$ := $1; }
        |  variable
                { $$ := $1; }

        | condition tknLIKE pattern
                { $$ := opr(46,'LIKE',[$1,stcon($3)]); }

        | condition tknLIKE pattern tknESCAPE QUOTED_STRING
                { $$ := opr(46,'LIKE',[$1,stcon($3),opr(243,'ESCAPE',stcon($5))]); }

        | condition tknNOT tknLIKE pattern
                { $$ := opr(42,'NOT',[opr(46,'LIKE',[$1,stcon($4)])]); }

        | condition tknNOT tknLIKE pattern tknESCAPE QUOTED_STRING
                { $$ := opr(42,'NOT',[opr(46,'LIKE',[$1,stcon($4),opr(243,'ESCAPE',stcon($6))])]); }

        | condition tknBETWEEN expr tknAND expr
                /*
                { $$ := opr(47,'BETWEEN',[$1,$3,$5]); }

                { $$ := opr(129,'void',[opr(55,'GE',[$1,$3]),opr(54,'LE',[$1,$5]),opr(44,'AND',[])]); }
                */
                { $$ := opr(44,'AND',[opr(55,'GE',[$1,$3]),opr(54,'LE',[$1,$5])]); }

        | condition tknNOT tknBETWEEN expr tknAND expr %prec URELATIONAL
                /*
                { $$ := opr(42,'NOT',[opr(47,'BETWEEN',[$1,$4,$6])]); }
                { $$ := opr(129,'void',[opr(51,'LT',[$1,$4]),opr(52,'GT',[$1,$6]),opr(43,'OR',[])]); }
                */
                { $$ := opr(43,'OR',[opr(51,'LT',[$1,$4]),opr(52,'GT',[$1,$6])]); }

        | condition tknIS tknNULL
                { $$ := opr(48,'IS NULL',[$1]); }

        | condition tknIS tknNOT tknNULL
                { $$ := opr(49,'IS NOT NULL',[$1]); }

        | condition tknIN '(' exp_set ')'
                { $$ := opr(45,'IN',[$1,$4]); }

        | condition tknNOT tknIN '(' exp_set ')'
                { $$ := opr(42,'NOT',[opr(45,'IN',[$1,$5])]); }

        | condition tknIN subquery
                { $$ := opr(45,'IN',[$1,$3]); }

        | condition tknNOT tknIN subquery
                { $$ := opr(42,'NOT',[opr(45,'IN',[$1,$4])]); }

        | tknEXISTS subquery
                { $$ := opr(68,'EXISTS',[opr(244,'START EXISTS',[]),$2]); }

        | quantified_factor
                { $$ := $1; }

        | '(' condition ')'
                { $$ := $2; }



        | function
                { $$ := $1; }
        | function option_expression
                { $$ := opr(129,'VOID',[$1,$2]); }

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


pattern : QUOTED_STRING
                { $$ := $1; }
        ;

exp_set : rptnumber_quoted_string
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
                     | BOOL
                { $$ := boolcon($1); }
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
                  ;

rptgroup_order_clause : /* empty */
                { $$ := nil; }
                      | group_order_clause
                { $$ := $1; }
                   ;

group_order_clause : tknGROUP tknBY expr_list option_having
                { $$ := opr(69,'GROUP BY',[$4,$3]); }
                   |  group_order_clause tknGROUP tknBY expr_list option_having
                { $$ := opr(69,'GROUP BY',[$1,$5,$4]); }
                   |  tknORDER tknBY rptsorted_def
                { $$ := opr(70,'ORDER BY',[$3]); }
                   |  group_order_clause tknORDER tknBY rptsorted_def
                { $$ := opr(70,'ORDER BY',[$1,$4]); }
             ;

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

set_clause : tknSELECT option_duplicate option_columns
             tknFROM selected_tables
             option_where_condition
             rptgroup_order_clause
                { $$ := opr(34,'SELECT',[opr(87,'FROM',[$5]),$2, opr(150,'COLUMNS PROJECTION',[$3]),$6,$7]); }
           | set_clause tknUNION set_clause
                { $$ := opr(222,'UNION',[$1,$3]); }
           | set_clause tknUNION tknALL set_clause
                { $$ := opr(72,'UNION ALL',[$1,$4]); }
           | set_clause tknINTERSECT set_clause
                { $$ := opr(73,'INTERSECT',[$1,$3]); }
           | set_clause tknMINUS set_clause
                { $$ := opr(74,'MINUS',[$1,$3]); }
           ;

rptsorted_def : sorted_def
                { $$ := $1; }
              | rptsorted_def ',' sorted_def
                { $$ := opr(0,'REPEAT',[$1,$3]); }
              ;

sorted_def : column_name
                { $$ := opr(75,'ASC',[$1]); }
           | table_name '.' column_name
                { $$ := opr(75,'ASC',[$1,$3]); }
           | column_name tknASC
                { $$ := opr(75,'ASC',[$1]); }
           | table_name '.' column_name tknASC
                { $$ := opr(75,'ASC',[$1,$3]); }
           | column_name tknDESC
                { $$ := opr(76,'DESC',[$1]); }
           | table_name '.' column_name tknDESC
                { $$ := opr(76,'ASC',[$1,$3]); }
           ;

insert_command : tknINSERT tknINTO  table_name '(' column_list ')' value_list
                { $$ := opr(77,'INSERT INTO',[$3,$5,$7]); }
               | tknINSERT tknINTO  table_name value_list
                { $$ := opr(77,'INSERT INTO',[$3,$4]); }
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

value : expr
         { opr(129,'VOID',[$1]) }
      ;

query : select_command
                { $$ := $1; }
      ;


update_command : simple_update
                { $$ := $1; }
               | subquery_update
                { $$ := $1; }
               ;

simple_update : tknUPDATE table_name tknSET rptset_column option_where_condition /* tknWHERE condition */
                { $$ := opr(80,'UPDATE',[$2,$4,$5]); }
              | tknUPDATE table_name alias tknSET rptset_column option_where_condition /* tknWHERE condition */
                { $$ := opr(80,'UPDATE',[$2,$3,$5,$6]); }
              ;

rptset_column : set_column
                { $$ := $1; }
              | rptset_column ',' set_column
                { $$ := opr(0,'REPEAT',[$1,$3]); }
              ;

set_column : column tknEQ expr /*%prec UMINUS*/
                { $$ := opr(81,'SET',[$1,$3]); }
           | column tknEQ  subquery %prec UMINUS
                { $$ := opr(81,'SET',[$1,$3]); }
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

delete_command : tknDELETE tknFROM table_name option_where_condition
                { $$ := opr(82,'DELETE FROM',[$3,$4]); }
               | tknDELETE  '*'  tknFROM table_name
                { $$ := opr(82,'DELETE FROM',[$4]); }
               | tknTRUNCATE tknTABLE table_name
                { $$ := opr(82,'DELETE FROM',[$3]); }
               ;

/* exp → term {OR term};
   term → factor {AND factor};
   factor → id;
   factor → NOT factor;
   factor → LPAREN exp RPAREN;
*/


expr    :  expr '+' expr
                { $$ := opr(83,'ADD',[$1,$3]); }
	|  expr '-' expr
                { $$ := opr(84,'SUB',[$1,$3]); }
        |  expr tknAND expr
                { $$ := opr(44,'AND',[$1,$3]); }
	|  expr '*' expr
                { $$ := opr(85,'MUL',[$1,$3]); }
        |  expr tknOR expr
                { $$ := opr(43,'OR',[$1,$3]); }
	|  expr '/' expr
                { $$ := opr(86,'DIV',[$1,$3]); }
	|  expr '%' expr
                { $$ := opr(95,'MOD',[$1,$3]); }
	|  '(' expr ')'
                { $$ := $2; }
	|  '+' expr %prec UMINUS
                { $$ := $2; }
	|  '-' expr %prec UMINUS
                { $$ := opr(79,'UMINUS',[$2]); }
	|  tknNOT expr %prec UMINUS
                { $$ := opr(42,'NOT',[$2]); }
        |  constant
                { $$ := $1; }
        |  variable
                { $$ := $1; }
        | function
                { $$ := $1; }
        | function option_expression
                { $$ := opr(129,'VOID',[$1,$2]); }
        | group_function '(' option_group ')'
                { $$ := opr(129,'VOID',[$1,$3]); }
	;

option_expression : '(' expr_list ')'
                  ;

option_group : '*'
                { $$ := opr(180,'ALL COLUMNS AGGREGATE'); }
             |  expr
                { $$ := opr(181,'EXPRESSION AGGREGATE',[$1]); }
             | tknALL expr
                { $$ := opr(181,'EXPRESSION AGGREGATE',[$2]); }
             | tknDISTINCT expr
                { $$ := opr(182,'DISTINCT AGGREGATE',[$2]); }
             ;

variable : ID
                { $$ := opr(6,'COLUMN NAME',[DBName($1)]); }
         | ID '.' ID
                { $$ := opr(229,'DOT',[opr(4,'TABLE NAME',[DBName($1)]),opr(6,'COLUMN NAME',[DBName($3)])]); }
         | ID '.' ID '.' ID
                { $$ := opr(229,'DOT',[opr(2,'DATABASE NAME',[DBName($1)]),opr(4,'TABLE NAME',[DBName($3)]),opr(6,'COLUMN NAME',[DBName($5)])]); }
         ;

constant :  NUM
                { $$ := con($1); }
	 |  NUMBER
                { $$ := con($1); }
         | QUOTED_STRING
                { $$ := stcon($1); }
         | BOOL
                { $$ := boolcon($1); }
         | tknNULL
                { $$ := nullcon(); }
         ;


/* SQL Aggregate Functions

   SQL aggregate functions return a single value, calculated from values in a column.

   Useful aggregate functions:

     AVG() - Returns the average value
     COUNT() - Returns the number of rows
     FIRST() - Returns the first value
     LAST() - Returns the last value
     MAX() - Returns the largest value
     MIN() - Returns the smallest value
     SUM() - Returns the sum

   SQL Scalar functions

   SQL scalar functions return a single value, based on the input value.

   Useful scalar functions:

     UCASE() - Converts a field to upper case  *** char_function ***
     LCASE() - Converts a field to lower case  *** char_function ***
     MID() - Extract characters from a text field   *** char_function ***
     LEN() - Returns the length of a text field
     ROUND() - Rounds a numeric field to the number of decimals specified  *** number_function ***
     NOW() - Returns the current system date and time  *** date_function ***
     FORMAT() - Formats how a field is to be displayed
*/



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
              | tknLEN
                { $$ := opr(108,'LENGTH'); }
              | tknUCASE
                { $$ := opr(171,'UCASE'); }
              | tknLCASE
                { $$ := opr(172,'LCASE'); }
              | tknMID
                { $$ := opr(173,'MID'); }
              | tknNOW
                { $$ := opr(174,'NOW'); }
              | tknFORMAT
                { $$ := opr(175,'FORMAT'); }
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
               | tknSUM
                { $$ := opr(116,'SUM'); }
               /*
               | tknSTDDEV
                { $$ := opr(117,'STDDEV'); }
               | tknVARIANCE
                { $$ := opr(118,'VARIANCE'); }
               */
               ;



create_trigger : tknCREATE tknTRIGGER trigger_name trigger_sync option_trigger_when trigger_action
                { $$ := opr(138,'CREATE TRIGGER',[$3,$4,$5,$6]); }
               ;

trigger_name : ID
                { $$ := opr(139,'TRIGGER NAME',[DBName($1)]); }
             ;

trigger_sync : before_after rpt_delete_insert_update tknON table_name
                { $$ := opr(140,'TRIGGER SYNC',[$1,$2,$4]); }
             ;

before_after: tknBEFORE
                { $$ := opr(141,'BEFORE'); }
            | tknAFTER
                { $$ := opr(142,'AFTER'); }
            ;

rpt_delete_insert_update : delete_insert_update
                { $$ := $1; }
                    | rpt_delete_insert_update tknOR delete_insert_update
                { $$ := opr(0,'REPEAT',[$1,$3]); }
                    ;

delete_insert_update: tknDELETE
                { $$ := opr(143,'TRIGGER DELETE'); }
                    | tknINSERT
                { $$ := opr(144,'TRIGGER INSERT'); }
                    | tknUPDATE option_of_column_list
                { $$ := opr(145,'TRIGGER UPDATE',[$2]); }
                    ;

option_of_column_list: tknOF column_list
                { $$ := $2; }
                    ;

option_trigger_when : /* empty */
                { $$ := nil; }
                    | option_for_each option_when_condition
                { $$ := opr(129,'VOID',[$1,$2]); }
                    ;

option_for_each : /* empty */
                { $$ := nil; }
                | tknFOR tknEACH tknROW
                { $$ := opr(146,'FOR EACH ROW'); }
                ;

option_when_condition:/* empty */
                { $$ := nil; }
                     |  tknWHEN '(' condition ')'
                { $$ := opr(147,'WHEN CONDITION',[$3]); }
                     ;

trigger_action : trigger_step
                { $$ := opr(148,'TRIGGER STEP',[$1]); }
               | tknBEGIN rpt_trigger_step tknEND
                { $$ := opr(149,'BLOCK',[$2]); }
               ;

trigger_step : update_command
                { $$ := $1; }
             | insert_command
                { $$ := $1; }
             | delete_command
                { $$ := $1; }
             | select_command
                { $$ := $1; }
             ;

rpt_trigger_step: trigger_step ';'
                { $$ := $1; }
                | rpt_trigger_step trigger_step ';'
                { $$ := opr(0,'REPEAT',[$1,$2]); }
                ;


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
      end;
    5:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 221;
        sqlMemProg[High(sqlMemProg)].boolvalue := p^.boolNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
        if boolValue then
          printInstruction := Mnemonics[mnemonic] + ' TRUE'
         else
          printInstruction := Mnemonics[mnemonic] + ' FALSE'
      end;
    6:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 234;
        sqlMemProg[High(sqlMemProg)].stvalue := p^.stNode^.value;
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic] + ' ' + stvalue
      end;
    7:
      begin
        SetLength(sqlMemProg,length(sqlMemProg)+1);
        sqlMemProg[High(sqlMemProg)].mnemonic := 241;
        sqlMemProg[High(sqlMemProg)].stvalue := 'null';
        with sqlMemProg[High(sqlMemProg)] do
          printInstruction := Mnemonics[mnemonic]
      end;
  end
end;

begin

end.

