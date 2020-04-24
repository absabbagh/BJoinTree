unit dlib;

{$MODE Delphi}

interface


type
  TLexerParserBase = class
  public
    function parse(): integer; virtual; abstract;
  end;


implementation


end.
