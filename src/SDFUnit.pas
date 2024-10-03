unit SDFUnit;

interface

const
  MaxLexemes = 70;
type
  SDFClass = class
  private
    { Private declarations }
    function GetNextState(CurrentState: Integer; Input: char): Integer;
    function FilterLexeme(Lexeme: string): string;
  public
    { Public declarations }
    CountLexemes: Integer;
    Lexemes: array [0..MaxLexemes-1] of string;
    function AddString(Line: string; st: string; UseComma: Boolean): string;
    procedure GetLexemes(Line: string);
  end;

implementation

function SDFClass.AddString(Line: string; st: string; UseComma: Boolean): string;
var
  EmptyLine: Boolean;
  UseTmp: Boolean;
  Tmp: string;
  i: Integer;
begin
  EmptyLine := Line = '';
  UseTmp := False;
  Tmp := '';
  for i := 1 to Length(st) do
    begin
      if st[i] = '"' then Tmp := Tmp + '""' else Tmp := Tmp + st[i];
      if (st[i] = '"') or (st[i] = ',') or (st[i] = ' ' ) then UseTmp := True
    end;
  if Length(st) = 0 then UseTmp := True;
  if UseTmp then
    begin
      Tmp := '"' + Tmp + '"';
      if EmptyLine then Result :=  Tmp else
        if UseComma then Result := Line + ',' + Tmp else Result := Line + ' ' + Tmp
    end else
    if EmptyLine then Result :=  st else
      if UseComma then Result := Line + ',' + st else Result := Line + ' ' + st
end;

function SDFClass.GetNextState(CurrentState: Integer; Input: char): Integer;
begin
  Result := -1;
  case CurrentState of
    0: if (Input = ',') then Result := 3 else
         if Input = '"' then Result := 5 else Result := 4;
    1, 3: ;
    4: if (Input <> '"') and (Input <> ',') then
         Result := 6;
    5: if (Input = '"') then Result := 8 else Result := 7;
    6: if (Input <> '"') and (Input <> ',') then
         Result := 6;
    7: if Input = '"' then Result := 10 else Result := 9;
    8: if Input = '"' then Result := 11;
    9: if Input = '"' then Result := 10 else Result := 9;
   10: if Input = '"' then Result := 12;
   11: if Input = '"' then Result := 10 else Result := 9;
   12: if Input = '"' then Result := 10 else Result := 9
  end
end;

function SDFClass.FilterLexeme(Lexeme: string): string;
var
  FindDoubleComma: Boolean;
  i: Integer;
begin
  Result := Lexeme;
  if Length(Lexeme) > 0 then
    if Lexeme[1] <> '"' then
      Exit else
      begin
        Result := '';
        FindDoubleComma := False;
        for i := 2 to Length(Lexeme) - 1 do
          begin
            if Lexeme[i] <> '"' then Result := Result + Lexeme[i] else
              begin
                if FindDoubleComma then
                  begin
                    Result := Result + '"';
                    FindDoubleComma := False
                  end else FindDoubleComma := True
              end
          end
      end
end;

procedure SDFClass.GetLexemes(Line: string);
var
  CurrentState, NextState: Integer;
  i: Integer;
  CurrentLexeme: string;
begin
  CountLexemes := 0;
  CurrentState := 0;
  CurrentLexeme := '';
  for i := 1 to Length(Line) do
    begin
      NextState := GetNextState(CurrentState,Line[i]);
      while NextState = -1 do
        begin
          if CurrentState in [4,6,8,10] then
            begin
              Lexemes[CountLexemes] := CurrentLexeme;
              CurrentLexeme := '';
              if CountLexemes < MaxLexemes then Inc(CountLexemes)
            end;
          CurrentState := 0;
          NextState := GetNextState(CurrentState,Line[i])
        end;
      CurrentState := NextState;
      if CurrentState <> 3 then CurrentLexeme := CurrentLexeme + Line[i]
    end;
  if CurrentState in [3,4,6,8,10] then
    begin
      Lexemes[CountLexemes] := CurrentLexeme;
      if CountLexemes < MaxLexemes then Inc(CountLexemes)
    end;
  for i := 0 to CountLexemes - 1 do
    Lexemes[i] := FilterLexeme(Lexemes[i])
end;

end.
