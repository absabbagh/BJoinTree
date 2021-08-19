program sqlProject;

{$MODE Delphi}

uses
  Forms, Interfaces,
  sqlUnit in 'sqlUnit.pas' {Form1},
  execProgramUnit in 'execProgramUnit.pas',
  sql in 'sql.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
