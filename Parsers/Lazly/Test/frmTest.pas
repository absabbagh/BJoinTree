unit frmtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, expr;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    Parser: TExprParser;
    procedure WriteCB(Value: Real);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: char);
var
  StrStream: TStringStream;
begin
  if Key = #13 then
  begin
    Key := #0;
    Memo1.Lines.Add('> ' + Edit1.Text);

    {$IFDEF FPC}
    StrStream := TStringStream.Create('');
    {$ELSE}
    StrStream := TStringStream.Create;
    {$ENDIF}
    try
      StrStream.WriteString(Edit1.Text);
      StrStream.Position := 0;
      try
        Parser.parse(StrStream, @WriteCB);
      except
        on E: EExprParserException do
          Memo1.Lines.Add(E.Message);
      end;
      Edit1.Text := '';
    finally
      StrStream.Free;
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Parser.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Parser := TExprParser.Create;
end;

procedure TForm1.WriteCB(Value: Real);
begin
  Memo1.Lines.Add(Format('%2.2f',[Value]));
end;

end.

