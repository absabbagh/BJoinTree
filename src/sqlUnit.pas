unit sqlUnit;

// {$OPTIMIZATION LEVEL3}


interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Menus, ExtCtrls, Buttons,
  sql,
  regexpr;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    Button8: TButton;
    Edit1: TEdit;
    Edit7: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label10: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }

    counter: Integer;

    procedure ExecuteQuery(dbuserId: string; dbName: string; stQuery: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{$Define joinindex}
{$DEFINE Debug}
{$DEFINE NoIndexDebug}

uses
  msgslib,
  execProgramUnit,
  SDFUnit,
  {$IFDEF joinindex} BJoinTreeU; {$ELSE} BPlusTreeU; {$ENDIF}


(********************************************

type

  { TMyThread }

  TMyThread = class(TThread)
  private
    id: Integer;
    fStatusText: string;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  MyThread : TMyThread;
  secThread: TMyThread;
begin
  MyThread := TMyThread.Create(True); // With the True parameter it doesn't start automatically
  if Assigned(MyThread.FatalException) then
    raise MyThread.FatalException;
  MyThread.id := 1;
  // Here the code initialises anything required before the threads starts executing

  MyThread.Start;
  SecThread := TMyThread.Create(True); // With the True parameter it doesn't start automatically
  if Assigned(SecThread.FatalException) then
    raise SecThread.FatalException;
 SecThread.id := 2;
  // Here the code initialises anything required before the threads starts executing

  SecThread.Start;
end;

{ TMyThread }

procedure TMyThread.ShowStatus;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example Form1.Caption.
begin
  Form1.Caption := fStatusText;
end;

procedure TMyThread.Execute;
var
  newStatus : string;
begin
  fStatusText := 'TMyThread Starting ...';
  Synchronize(@Showstatus);
  fStatusText := 'TMyThread Running ...';
  while (not Terminated) and (true {any condition required}) do begin

    //here goes the code of the main thread loop
    newStatus:='TMyThread Time ' + intTostr(id) + ': ' +FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);

    if NewStatus <> fStatusText then begin
      fStatusText := newStatus;
      Synchronize(@Showstatus);
    end;
  end;
end;

constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

*************************************************)


var
  stElapsedTime: string;

procedure TForm1.ExecuteQuery(dbuserId: string; dbName: string; stQuery: string);
var
  InputSt: string;

  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;


  flagCreate: boolean;
  st: string;
  I, J: Integer;
  SDFInstance: SDFClass;
  ColCount: Integer = 0;
begin
  dbname := lowercase(dbName);
  InputSt := stQuery;
  StartTime := Now;
  sqlresults := nil;

  ParseSQLStatement(InputSt,sqlMemProg);

  sqlResults := nil;
  {$IFDEF Debug}
  Memo2.clear;
  for I := 0 to length(sqlMemProg) - 1 do
    with sqlMemProg[i] do
      Memo2.Lines.add(printInstruction);
  {$ENDIF}


  selectColsInstructions := nil;

  if yyerrmsgs = nil then
  begin
    flagCreate := Pos('CREATE DATABASE ', UpperCase(stQuery)) <> 0;
    flagCreate := flagCreate  or  ((Pos('SYSTEM',trim(uppercase(stQuery))) = 1) and (Pos('UPLOAD',trim(uppercase(stQuery))) = 0));
    // openTables; // Use to force to flush
    if trim(InputSt) <> '' then
      executeProgram(sqlMemProg, dbuserId, dbName);
    // closeTables;
  end;

  Diff := Now - StartTime;
  DecodeTime(Diff, Hour, Min, Sec, MSec);
  st := 'Elapsed Time = ';
  if Hour <> 0 then
    st := st + IntToStr(Hour) + ' Hours ';
  if Min <> 0 then
    st := st + IntToStr(Min) + ' Minutes ';
  if sec <> 0 then
    st := st + IntToStr(Sec) + ' Seconds ';
  st := st + IntToStr(Msec) + ' MilliSeconds';
  stElapsedTime := st;
  // Writeln('Elapsed Time: ' + st);



  {$IFDEF Debug}
  Memo3.Clear;
  if selectColsInstructions <> nil then
  begin
    for I := 0 to length(selectColsInstructions) - 1 do
      for J := 0 to length(selectColsInstructions[I]) - 1 do
        with selectColsInstructions[i, J] do
          if stvalue <> '' then
            Memo3.Lines.Add(printInstruction + ': ' + stvalue) else
            Memo3.Lines.Add(printInstruction)
  end;
  {$ENDIF}

  SDFInstance := SDFClass.Create;

  if length(sqlResults) <> 0 then
  begin
    ColCount := 0;
    for I := low(sqlResults) to high(sqlResults) do
      begin
        SDFInstance.GetLexemes(sqlResults[I]);
        if ColCount < SDFInstance.CountLexemes then
          ColCount := SDFInstance.CountLexemes;
      end;
    StringGrid1.ColCount :=  ColCount;
    StringGrid1.RowCount := length(sqlResults);
  end;


  for I := low(sqlResults) to high(sqlResults) do
  begin
    SDFInstance.GetLexemes(sqlResults[I]);
    for J := 0 to SDFInstance.CountLexemes - 1 do
      StringGrid1.Cells[J,I] := SDFInstance.Lexemes[J];
  end;

  SDFInstance.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I, j :integer;
  InputSt: string;
  userId: string;
  dbName: string;
  stQuery: string;
  st: string;
  tmp: string;
begin
  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;
  StringGrid1.Clear;
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount := 5;
  yyerrmsgs := nil;

  userId := edit4.Text;
  dbName := Label9.Caption;
  dbName := lowercase(dbName);

  sqlMemProg := nil;
  tmp := '';
  for I := 0 to memo1.Lines.Count - 1 do
    begin
      st := memo1.Lines[i];
      for j := 1 to length(st) do
        if (st[j] = #9) or (st[j] = #10) then
          st[j] := ' ';
      st := trim(st);
      tmp := tmp + ' ' + st;
    end;
  InputSt := tmp;

  executeQuery(userId, dbName, InputSt);

  Memo4.Clear;
  if yyerrmsgs <> nil then
    for I := 0 to length(yyerrmsgs) - 1 do
      Memo4.Lines.Add(yyerrmsgs[I]);

  if yymiscmsgs <> nil then
    for I := 0 to length(yymiscmsgs) - 1 do
      begin
        Memo4.Lines.Add(yymiscmsgs[I]);
        if pos('Switch',yymiscmsgs[I]) <> 0 then
          Label9.Caption := copy(yymiscmsgs[I],29,length(yymiscmsgs[I]));
      end;

  StQuery := '';
  if length(sqlResults) <> 0 then
    stQuery := stQuery + sqlResults[0];

  memo4.lines.Add(stElapsedTime);
  yyerrmsgs := nil;
  yymiscmsgs := nil;

end;

const Path: string = '';


type
    DataPointerType =  Integer; //string[24];

procedure TForm1.Button5Click(Sender: TObject);
var
  {$IFDEF joinindex}
  jdx: BJoinTreeClass;
  {$ELSE}
  idx: BtrPlusClass;
  Thekeys: array of string;
  TheInheritedKeys: array of string;
  InheritedKeys: array of variant;
  i, j: Integer;
  count: Integer;
  {$ENDIF}
  keys: array of variant;
  dataref: array of datapointertype;
  st: string;


  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;


begin
  StartTime := Now;

  memo4.Clear;

  {$IFDEF joinindex}


  EraseBJoinTree('TS1\jdx_test',6);

  jdx := BJoinTreeClass.Create('TS1\jdx_test',['EMPLOYEES', 'COUNTRIES', 'JOBS', 'JOB_HISTORY',
                                                'DEPARTMENTS', 'LOCATIONS']);

  jdx.AddTableToDictionary('EMPLOYEES');
  jdx.AddColumnToDictionary('EMPLOYEE_ID','INTEGER','EMPLOYEES');
  jdx.AddColumnToDictionary('NAME','STRING[35]','EMPLOYEES');
  jdx.AddColumnToDictionary('MANAGER_ID','INTEGER','EMPLOYEES');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','EMPLOYEES');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','EMPLOYEES');

  jdx.AddTableToDictionary('JOBS');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','JOBS');
  jdx.AddColumnToDictionary('JOB_TITTLE','STRING[35]','JOBS');

  jdx.AddTableToDictionary('JOB_HISTORY');
  jdx.AddColumnToDictionary('EMPLOYEE_ID','INTEGER','JOB_HISTORY');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','JOB_HISTORY');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','JOB_HISTORY');

  jdx.AddTableToDictionary('DEPARTMENTS');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','DEPARTMENTS');
  jdx.AddColumnToDictionary('DEPARTMENT_NAME','STRING[35]','DEPARTMENTS');
  jdx.AddColumnToDictionary('MANAGER_ID','INTEGER','DEPARTMENTS');
  jdx.AddColumnToDictionary('LOCATION_ID','INTEGER','DEPARTMENTS');

  jdx.AddTableToDictionary('LOCATIONS');
  jdx.AddColumnToDictionary('LOCATION_ID','INTEGER','LOCATIONS');
  jdx.AddColumnToDictionary('ADDRESS','STRING[40]','LOCATIONS');
  jdx.AddColumnToDictionary('COUNTRY_ID','STRING[2]','LOCATIONS');

  jdx.AddTableToDictionary('COUNTRIES');
  jdx.AddColumnToDictionary('COUNTRY_ID','STRING[2]','COUNTRIES');
  jdx.AddColumnToDictionary('COUNTRY_NAME','STRING[40]','COUNTRIES');

  jdx.AddJoin('EMPLOYEES','JOB_HISTORY','EMPLOYEE_ID');
  jdx.AddJoin('JOB_HISTORY','EMPLOYEES','EMPLOYEE_ID');
  jdx.AddJoin('JOBS','JOB_HISTORY','JOB_ID');
  jdx.AddJoin('JOB_HISTORY','JOBS','JOB_ID');
  jdx.AddJoin('EMPLOYEES','DEPARTMENTS','DEPARTMENT_ID');
  jdx.AddJoin('DEPARTMENTS','EMPLOYEES','DEPARTMENT_ID');
  jdx.AddJoin('DEPARTMENTS','LOCATIONS','LOCATION_ID');
  jdx.AddJoin('LOCATIONS','DEPARTMENTS','LOCATION_ID');
  jdx.AddJoin('LOCATIONS','COUNTRIES','COUNTRY_ID');
  jdx.AddJoin('COUNTRIES','LOCATIONS','COUNTRY_ID');

  jdx.createBTrees(['EMPLOYEES','JOBS','JOB_HISTORY','COUNTRIES','DEPARTMENTS','LOCATIONS'],
                   false,['EMPLOYEES.NAME']);
  jdx.Free;



  jdx := BJoinTreeClass.Create('TS1\jdx_test',['EMPLOYEES', 'COUNTRIES', 'JOBS', 'JOB_HISTORY',
                                                'DEPARTMENTS', 'LOCATIONS']);

  jdx.AddTableToDictionary('EMPLOYEES');
  jdx.AddColumnToDictionary('EMPLOYEE_ID','INTEGER','EMPLOYEES');
  jdx.AddColumnToDictionary('NAME','STRING[35]','EMPLOYEES');
  jdx.AddColumnToDictionary('MANAGER_ID','INTEGER','EMPLOYEES');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','EMPLOYEES');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','EMPLOYEES');

  jdx.AddTableToDictionary('JOBS');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','JOBS');
  jdx.AddColumnToDictionary('JOB_TITTLE','STRING[35]','JOBS');

  jdx.AddTableToDictionary('JOB_HISTORY');
  jdx.AddColumnToDictionary('EMPLOYEE_ID','INTEGER','JOB_HISTORY');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','JOB_HISTORY');
  jdx.AddColumnToDictionary('JOB_ID','STRING[10]','JOB_HISTORY');

  jdx.AddTableToDictionary('DEPARTMENTS');
  jdx.AddColumnToDictionary('DEPARTMENT_ID','STRING[10]','DEPARTMENTS');
  jdx.AddColumnToDictionary('DEPARTMENT_NAME','STRING[35]','DEPARTMENTS');
  jdx.AddColumnToDictionary('MANAGER_ID','INTEGER','DEPARTMENTS');
  jdx.AddColumnToDictionary('LOCATION_ID','INTEGER','DEPARTMENTS');

  jdx.AddTableToDictionary('LOCATIONS');
  jdx.AddColumnToDictionary('LOCATION_ID','INTEGER','LOCATIONS');
  jdx.AddColumnToDictionary('ADDRESS','STRING[40]','LOCATIONS');
  jdx.AddColumnToDictionary('COUNTRY_ID','STRING[2]','LOCATIONS');

  jdx.AddTableToDictionary('COUNTRIES');
  jdx.AddColumnToDictionary('COUNTRY_ID','STRING[2]','COUNTRIES');
  jdx.AddColumnToDictionary('COUNTRY_NAME','STRING[40]','COUNTRIES');

  jdx.AddJoin('EMPLOYEES','JOB_HISTORY','EMPLOYEE_ID');
  jdx.AddJoin('JOB_HISTORY','EMPLOYEES','EMPLOYEE_ID');
  jdx.AddJoin('JOBS','JOB_HISTORY','JOB_ID');
  jdx.AddJoin('JOB_HISTORY','JOBS','JOB_ID');
  jdx.AddJoin('EMPLOYEES','DEPARTMENTS','DEPARTMENT_ID');
  jdx.AddJoin('DEPARTMENTS','EMPLOYEES','DEPARTMENT_ID');
  jdx.AddJoin('DEPARTMENTS','LOCATIONS','LOCATION_ID');
  jdx.AddJoin('LOCATIONS','DEPARTMENTS','LOCATION_ID');
  jdx.AddJoin('LOCATIONS','COUNTRIES','COUNTRY_ID');
  jdx.AddJoin('COUNTRIES','LOCATIONS','COUNTRY_ID');

  jdx.createBTrees(['EMPLOYEES','JOBS','JOB_HISTORY','DEPARTMENTS','COUNTRIES','LOCATIONS'],
                   true,['EMPLOYEES.NAME']);

  jdx.AddKey('EMPLOYEES',[1016, 'ALAN', 1001, 'JANITOR', 'CLEANING'],1);
  jdx.AddKey('JOBS',['CLEANING','CLEANNER'],1);
  jdx.AddKey('JOB_HISTORY',[1016, 'JANITOR', 'CLEANING'],1);
  jdx.AddKey('DEPARTMENTS',['JANITOR', 'JANITOR CLEANING', 1001, 1266],1);
  jdx.AddKey('LOCATIONS',[1266, 'RICHMOND','CA'],1);
  jdx.AddKey('COUNTRIES',['CA', 'CANADA'],1);

  setlength(Keys,1);
  setlength(DataRef,6);
  jdx.ClearKey;
  repeat
    jdx.NextKey(Keys,DataRef);
    if dataref[0] <> -1 then
      begin
        st := 'NAME: ' + keys[0];
        memo4.Lines.Add(st);
        st :=       '  DR0: ' + intToStr(dataref[0]);
        st := st +  '  DR1: ' + intToStr(dataref[1]);
        st := st +  '  DR2: ' + intToStr(dataref[2]);
        st := st +  '  DR3: ' + intToStr(dataref[3]);
        st := st +  '  DR4: ' + intToStr(dataref[4]);
        st := st +  '  DR5: ' + intToStr(dataref[5]);
        memo4.Lines.Add(st);
        memo4.Lines.Add('--------------------------------');
      end

  until DataRef[0] = -1;

  jdx.Free;
  Exit;


  EraseBJoinTree('TS1\jdx_test',6);

  jdx := BJoinTreeClass.Create('TS1\jdx_test',['t','s','u','v']);
  jdx.AddTableToDictionary('t');
  jdx.AddColumnToDictionary('a1','INTEGER','t');
  jdx.AddColumnToDictionary('a2','INTEGER','t');
  jdx.AddTableToDictionary('s');
  jdx.AddColumnToDictionary('a1','INTEGER','s');
  jdx.AddColumnToDictionary('a3','INTEGER','s');
  jdx.AddTableToDictionary('u');
  jdx.AddColumnToDictionary('a1','INTEGER','u');
  jdx.AddColumnToDictionary('a4','INTEGER','u');
  jdx.AddTableToDictionary('v');
  jdx.AddColumnToDictionary('a3','INTEGER','v');
  jdx.AddColumnToDictionary('a4','INTEGER','v');

  jdx.AddJoin('s','u','a1');
  jdx.AddJoin('u','s','a1');
  jdx.AddJoin('t','s','a1');
  jdx.AddJoin('v','s','a3');
  jdx.AddJoin('s','v','a3');
  jdx.AddJoin('v','u','a4');
  jdx.AddJoin('u','v','a4');
  jdx.AddJoin('s','t','a1');


  jdx.createBTrees(['s','u','t','v'],false,['t.a2','s.a3']);
  jdx.Free;

  jdx := BJoinTreeClass.Create('TS1\jdx_test',['t','s','u','v']);
  jdx.AddTableToDictionary('t');
  jdx.AddColumnToDictionary('a1','INTEGER','t');
  jdx.AddColumnToDictionary('a2','INTEGER','t');
  jdx.AddTableToDictionary('s');
  jdx.AddColumnToDictionary('a1','INTEGER','s');
  jdx.AddColumnToDictionary('a3','INTEGER','s');
  jdx.AddTableToDictionary('u');
  jdx.AddColumnToDictionary('a1','INTEGER','u');
  jdx.AddColumnToDictionary('a4','INTEGER','u');
  jdx.AddTableToDictionary('v');
  jdx.AddColumnToDictionary('a3','INTEGER','v');
  jdx.AddColumnToDictionary('a4','INTEGER','v');

  jdx.AddJoin('s','u','a1');
  jdx.AddJoin('u','s','a1');
  jdx.AddJoin('t','s','a1');
  jdx.AddJoin('v','s','a3');
  jdx.AddJoin('s','v','a3');
  jdx.AddJoin('v','u','a4');
  jdx.AddJoin('u','v','a4');
  jdx.AddJoin('s','t','a1');


  jdx.createBTrees(['s','u','t','v'],true,['t.a2','s.a3']);

  jdx.AddKey('s',[12,22],2);
  jdx.AddKey('u',[10,16],1);
  jdx.AddKey('s',[10,14],1);
  jdx.AddKey('t',[12,21],2);
  jdx.AddKey('u',[12,19],2);
  jdx.AddKey('t',[10,27],1);
  jdx.AddKey('v',[14,16],1);
  jdx.AddKey('v',[22,19],2);

  setlength(Keys,2);
  setlength(DataRef,4);
  jdx.ClearKey;
  repeat
    jdx.NextKey(Keys,DataRef);
    if dataref[0] <> -1 then
      begin
        st :=      '  K0: ' + inttostr(keys[0]);
        st := st + '  K1: ' + inttostr(keys[1]);
        memo4.Lines.Add(st);
        st :=       '  DR0: ' + intToStr(dataref[0]);
        st := st +  '  DR1: ' + intToStr(dataref[1]);
        st := st +  '  DR2: ' + intToStr(dataref[2]);
        st := st +  '  DR3: ' + intToStr(dataref[3]);
        memo4.Lines.Add(st);
        memo4.Lines.Add('');
      end

  until DataRef[0] = -1;

  jdx.Free;

  {$ELSE}
  thekeys := nil;
  theInheritedkeys := nil;
  setlength(thekeys,3);
  TheKeys[0] :=  'k0: integer';
  TheKeys[1] :=  'k1: string[7]';
  TheKeys[2] :=  'k2: string[5]';
  setLength(TheInheritedKeys,3);
  TheinheritedKeys[0] :=  'IK0:string[7]';
  TheinheritedKeys[1] :=  'IK1: string[5]';
  TheinheritedKeys[2] :=  'IK2: integer';

  if fileExists('p.Idx') then
    begin
      DeleteFile('p.Idx');
    end;

  idx := BtrPlusClass.Create('p',False,Thekeys,TheinheritedKeys,3);
  idx.free;

  idx := BtrPlusClass.Create('p',True,Thekeys,TheInheritedKeys,3);

  keys := nil;
  InheritedKeys := nil;

  setlength(keys,3);
  setLength(InheritedKeys,3);
  setlength(dataref,3);




  memo4.Clear;
  randomize;

  for i := 1 to 1031 do
    begin
      edit6.Text := inttostr(i);
      edit6.Repaint;

      if not(odd(i)) then j :=  2*i else
      j := i;
      j := random(10000);
      keys[0] :=  j;
      keys[1] :=  intToStr(j+3);
      keys[2] :=  intToStr(j+4);

      if i = 480 then begin
      InheritedKeys[0] := intToStr(j + 40);
      InheritedKeys[1] := intToStr(j + 60);
      InheritedKeys[2] :=  j + 90;
      end else begin
        InheritedKeys[0] := intToStr(j + 40);
        InheritedKeys[1] := intToStr(j + 60);
        InheritedKeys[2] :=  j + 90;

      end;
      dataref[0] :=  j*2;
      dataref[1] :=  j;
      dataref[2] :=  j*3;

      idx.AddKey(Keys,inheritedkeys,dataref);

    end;

   count := 0;
   idx.ClearKey;
   j := 0;
   st := '';
   repeat
     st := '';
     idx.nextkey(keys,InheritedKeys,dataref);
     inc(count);
     if dataref[0] <> -1 then
       begin
         j := j +1;
         st := st + 'K0: '   + inttostr(keys[0]);
         st := st + '  K1: ' + keys[1];
         st := st + '  K2: ' + keys[2];
         memo4.Lines.Add(st);
         st :=       '  IK0: ' + Inheritedkeys[0];
         st := st +  '  IK1: ' + Inheritedkeys[1];
         st := st +  '  IK2: ' + intToStr(Inheritedkeys[2]);
         memo4.Lines.Add(st);
         st :=       '  DR0: ' + intToStr(dataref[0]);
         st := st +  '  DR1: ' + intToStr(dataref[1]);
         st := st +  '  DR2: ' + intToStr(dataref[2]);
         memo4.Lines.Add(st);
         memo4.Lines.Add('');
       end
   until dataref[0] = -1;
   idx.Free;
  {$ENDIF}

   Diff := Now - StartTime;
   DecodeTime(Diff, Hour, Min, Sec, MSec);
   st := 'Elapsed Time = ';
   if Hour <> 0 then
     st := st + IntToStr(Hour) + ' Hours ';
   if Min <> 0 then
     st := st + IntToStr(Min) + ' Minutes ';
   if sec <> 0 then
     st := st + IntToStr(Sec) + ' Seconds ';
   st := st + IntToStr(Msec) + ' MilliSeconds';
   stElapsedTime := st;
   memo4.Lines.add(stelapsedtime);

end;

procedure yyacceptmessage(msg: string);
begin
  msg := 'ACCEPT: '+ msg;
  setLength(yymiscmsgs,length(yymiscmsgs) + 1);
  yymiscmsgs[High(yymiscmsgs)] := msg;
end(*yyacceptmsg*);

procedure yyerror(msg: string);
begin
  // writeln(msg);
  msg := 'ERROR: '+ msg;
  setLength(yyerrmsgs,length(yyerrmsgs) + 1);
  yyerrmsgs[High(yyerrmsgs)] := msg;
  (********************) //Halt(1)
end (* yyerrmsg *);

procedure TForm1.Button6Click(Sender: TObject);
var
  userId: string;
  dbName: string;
  SQLFileName: string;
  ExtName: string;
  SQLInstructions: array of string;
  SQLTextFile: Text;
  Line: string;
  I: Integer;
  flagcomment: boolean;
begin
  userId := edit4.Text;
  dbName := Label9.Caption;
  SQLFileName := edit3.Text;
  yymiscmsgs := nil;
  yyerrmsgs := nil;
  SQLFileName := ExtractFileName(SQLFileName);
  ExtName := ExtractFileExt(SQLFileName);
  if not (upCase(ExtName) = '.SQL') then
   begin
    yyerror('Not a .SQL file');
    memo4.Clear;
    memo4.Lines.add(yyerrmsgs[0]);
    Exit;
  end;

  Path := edit5.Text + PathDelim;
  if not FileExists( Path + SQLFileName) then
  begin
    yyerror('File ' + SQLFileName + ' doesn''t Exist ');
    memo4.Clear;
    memo4.Lines.add(yyerrmsgs[0]);
    Exit;
  end;

  connectToDB(Edit8.Text,Edit9.Text);
  SQLInstructions := nil;
  setlength(SQLInstructions,1);
  AssignFile(SQLTextFile, Path + SQLFileName);
  Reset(SQLTextFile);
  flagcomment := false;
  while not EOF(SQLTextFile) do
    begin
      readLn(SQLTextFile, Line);


      if pos('//',Line) <> 0 then
       Line := copy(Line,1,pos('//',Line)-1);
      if pos('/*',Line) <> 0 then
        begin
          if pos('*/',Line) <> 0 then
            begin
              Line := copy(Line,1,pos('/*',Line)-1) + copy(Line,pos('*/',Line)+2,length(Line));
              flagComment := false;
            end else
            begin
              flagComment := true;
              Line := copy(Line,1,pos('/*',Line)-1);
           end;
        end;
      if flagcomment then
        if pos('*/',Line) <> 0 then
          begin
            Line := copy(Line,pos('*/',Line)+2,length(Line));
            flagComment := false;
          end else Line := '';
      if trim(line) = '' then continue;
      if pos(';', Line) = 0 then
        SQLInstructions[High(SQLInstructions)] :=  SQLInstructions[High(SQLInstructions)] + ' ' + Line
       else
        begin
          SQLInstructions[High(SQLInstructions)] := SQLInstructions[High(SQLInstructions)] + copy(Line,1,pos(';',Line));
          setlength(SQLInstructions,length(SQLInstructions)+1);
          SQLInstructions[High(SQLInstructions)] := SQLInstructions[High(SQLInstructions)] + copy(Line,pos(';',Line)+1,length(Line));
        end;
    end;

  setlength(SQLInstructions,length(SQLInstructions)-1);

  memo4.clear;

  for I := low(SQLInstructions) to high(SQLInstructions) do
  begin
    ParseSQLStatement(SQLInstructions[I],sqlMemProg);
    selectColsInstructions := nil;
    executeProgram(sqlMemProg, userId, dbName);
    if yyerrmsgs <> nil then
      begin
        memo4.Lines.add('Instruction #: ' + intToStr(I));
        break;
      end;
    if pos('ACCEPT: Switch to Database:',yymiscmsgs[high(yymiscmsgs)]) <> 0 then
      dbName := copy(yymiscmsgs[high(yymiscmsgs)],
                     pos('Database:',yymiscmsgs[high(yymiscmsgs)])+10,
                     length(yymiscmsgs[high(yymiscmsgs)]));
    Label9.Caption := dbName;
  end;

  if yyerrmsgs <> nil then
    for I := 0 to length(yyerrmsgs) -1 do
      memo4.Lines.add(yyerrmsgs[I]);

  if yymiscmsgs <> nil then
    for I := 0 to length(yymiscmsgs) -1 do
      memo4.Lines.add(yymiscmsgs[I]);

  disconnectFromDB;

end;


function authenticate(userId, password: string): boolean;
begin
  result := userIdExists(userId,password);
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  userId: string;
  stpassword: string;
begin
  if Button8.Caption = 'CONNECT' then
    begin
      userId := edit4.Text;
      stpassword := edit7.Text;
      connectToDB(Edit8.Text,Edit9.Text); // Every entity should be seen as a tablespace
      if authenticate(userId, stpassword) then
        begin
          Label9.Caption:= 'DATABASE NAME:';
          edit4.Enabled := false;
          edit7.Enabled := false;
          Memo1.Enabled := true;
          Button2.Enabled := true;
          //label6.Visible := false;
          edit7.Visible := false;
          button5.Visible := false;
          button1.Visible := false;
          button6.Visible := false;
          edit1.Visible := false;
          edit5.Visible := false;
          edit6.Visible := false;
          edit3.Visible := false;
          edit8.Enabled := false;
          edit9.Enabled := false;
          Memo1.SetFocus;
          Button8.Caption := 'DISCONNECT'
        end else disconnectFromDB;
    end else
    begin
      disconnectFromDB;
      Button8.Caption := 'CONNECT';
      edit4.Enabled := true;
      edit7.Enabled := true;
      Button2.Enabled := false;
      //label6.Visible := true;
      edit7.Visible := true;
      {$IFDEF IndexDebug}
      Button5.Visible := true;
       {$ELSE}
       Button5.Visible := False;
      {$ENDIF}
      button1.Visible := true;
      button6.Visible := true;
      edit1.Visible := true;
      edit5.Visible := true;
      edit6.Visible := true;
      edit3.Visible := true;
      edit8.Enabled := true;
      edit9.Enabled := true;
      Memo1.Enabled := false;
      Memo1.Clear;
      Memo2.Clear;
      Memo3.Clear;
      Memo4.Clear;
      StringGrid1.Clear;
      StringGrid1.ColCount := 4;
      StringGrid1.RowCount := 5;
      Button8.SetFocus;
    end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // if the database is dropped, the program should restarted
  {$IFDEF IndexDebug}
  Button5.Visible := true;
   {$ELSE}
   Button5.Visible := False;
  {$ENDIF}
  button8.SetFocus;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Button8.Caption = 'DISCONNECT' then
    Button8Click(self);
  closeAction := caFree;
end;


function getRandomString: string;
var
  i: Integer;
begin
  result := '';
  for i := 1 to 10 do
    result += chr(ord('a') + random(26));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  userId: string;
  dbName: string;
  CSVName: string;
  FileName: string;
  ExtName: string;
  CSVTextFile: Text;
  SDFInstance: SDFClass;
  Line: string;
  LineCounter: integer;
  found: boolean;
  sqlInsert: string;
  I, J: integer;
  stQuery: string;
  CSVFileName: string;
  tmp: string;
  RowsInserted: int64;
  tblindex: integer;
  TableName: string;
  columnsHeader: array of string;
  columnsType: array of inttype..stringtype;
  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;
  st: string;
begin

  Randomize;

  userId := edit4.Text;
  dbName := Label9.Caption;

  RowsInserted := 0;
  for i := 1 to 10000 do
    begin
      sqlInsert := 'INSERT INTO q (a) VALUES (';
      sqlInsert := sqlInsert + '"' + getRandomString + '"' + ');';

      ParseSQLStatement(sqlInsert,sqlMemProg);

      RowsInserted += 1;

      edit6.Text := inttostr(RowsInserted);
      edit6.Repaint;
      if yyerrmsgs <> nil then
        yyerror('at row' + inttostr(Rowsinserted));

      selectColsInstructions := nil;

      executeProgram(sqlMemProg, userId, dbName);
    end;
  exit;


  userId := edit4.Text;
  dbName := Label9.Caption;
  CSVFileName := edit1.Text;
//  CSVFileName := 'hello-pippo-' + CSVFileName;
  yymiscmsgs := nil;
  yyerrmsgs := nil;

  StartTime := Now;


  Path := edit5.Text + PathDelim;

  CSVName := PChar(CSVFileName);



  FileName := ExtractFileName(CSVName);
  ExtName := ExtractFileExt(CSVName);
  if upCase(ExtName) = '.CSV' then
    FileName := copy(FileName, 1, Pos('.', FileName) - 1)
  else
  begin
    yyerror('Not a .CSV file');
    Exit;
  end;
  FileName := copy (FileName,pos('-', FileName)+1,length(FileName));
  FileName := copy (FileName,pos('-', FileName)+1,length(FileName));
  Tablename := FileName;
  if not TableExists(TableName) then
  begin
    yyerror('Table ' + TableName + ' doesn''t Exist ');
    Exit;
  end;
  if not FileExists(Path+CSVName) then
  begin
    yyerror('File ' + CSVName + ' doesn''t Exist ');
    Exit;
  end;


  // Check if a missing column has default value, if not is nullable

  AssignFile(CSVTextFile, Path + CSVName);
  Reset(CSVTextFile);
  SDFInstance := SDFClass.Create();
  LineCounter := 0;
  RowsInserted := 0;
  tblindex := GetTableStructure(FileName);
  columnsHeader := nil;
  setlength(columnsHeader,workingSchema.tables[tblIndex].numCols);
  columnsType := nil;
  setlength(columnsType,workingSchema.tables[tblIndex].numCols);
  counter := 0;

  while not EOF(CSVTextFile) do
    begin
      readLn(CSVTextFile, Line);
      if Line = '' then
        continue;
      SDFInstance.GetLexemes(Line);
      Inc(LineCounter);


      if LineCounter = 1 then
      begin
        // Check for the columns Name
        with workingSchema.tables[tblIndex] do
          begin
            if SDFInstance.CountLexemes < Length(columns) then
            begin
              yyacceptmessage('Number of columns in CSV file are less than ' +
                'the number of columns in the table');
              Exit;
            end;
            found := True;
            for I := 0 to SDFInstance.CountLexemes - 1 do
            begin
              found := False;
              for J := 0 to Length(columns) - 1 do
                if uppercase(columns[J].colname) =
                  trim(upperCase(SDFInstance.Lexemes[I])) then
                begin
                  found := True;
                  columnsHeader[counter] := columns[J].colname;
                  columnsType[counter] := columns[J].coltype;
                  counter := counter + 1;
                  break;
                end;
              if not found then
                break;
            end;
            if not found then
            begin
              yyerror('Missing columns');
              Exit;
            end;
          end;
          continue;
      end;
      with workingSchema.tables[tblIndex] do
        begin
          sqlInsert := 'INSERT INTO ' + Filename + ' (';
          for J := 0 to Length(columns) - 1 do
            sqlInsert := sqlInsert + columnsHeader[J] + ', ';
          sqlInsert := copy(sqlInsert, 1, length(sqlInsert) - 2) + ') VALUES (';

          for I := 0 to SDFInstance.CountLexemes - 1 do
          begin
            tmp := '';

            tmp := columns[I].colname;



            tmp := '';
            if (columnsType[I] = TDateTimeType) or (columnstype[I] = stringtype) then
              begin
                tmp := ''' ';
                tmp := trim(tmp);
              end;
            for j := 1 to length(SDFInstance.Lexemes[I]) do
              tmp := tmp + SDFInstance.Lexemes[I][j];
            if (columnsType[I] = TDateTimeType) or (columnsType[I] = stringtype) then
                sqlInsert := sqlInsert + trim(tmp) +'''' + ',' else sqlInsert := sqlInsert + trim(tmp) + ',';
            TMP := TRIM(TMP);
          end;
          sqlInsert := copy(sqlInsert, 1, length(sqlInsert) - 1) + ');';

          ParseSQLStatement(sqlInsert,sqlMemProg);

          RowsInserted := RowsInserted + 1;
          if RowsInserted = 29 then
            RowsInserted := RowsInserted;

          edit6.Text := inttostr(RowsInserted);
          edit6.Repaint;
          if yyerrmsgs <> nil then
            yyerror('at row' + inttostr(Rowsinserted));
          selectColsInstructions := nil;

          executeProgram(sqlMemProg, userId, dbName);
        end;
    end;

  yymiscmsgs := nil;
  if yyerrmsgs = nil then
    stQuery := intToStr(Rowsinserted) + ': Rows inserted in ' + tableName;

  if yyerrmsgs <> nil then
    for I := 0 to length(yyerrmsgs) -1 do
      stQuery := stQuery + yyerrmsgs[I] + ' ';

  if yymiscmsgs <> nil then
    for I := 0 to length(yymiscmsgs) -1 do
      stQuery := stQuery + yymiscmsgs[I] + ' ';

  memo4.Lines.Add(stQuery);
  {
  PQuery := getmem(1000);
  strPcopy(PQuery,stQuery);
  result := PQuery;
  }

  Diff := Now - StartTime;
  DecodeTime(Diff, Hour, Min, Sec, MSec);
  st := 'Elapsed Time = ';
  if Hour <> 0 then
    st := st + IntToStr(Hour) + ' Hours ';
  if Min <> 0 then
    st := st + IntToStr(Min) + ' Minutes ';
  if sec <> 0 then
    st := st + IntToStr(Sec) + ' Seconds ';
  st := st + IntToStr(Msec) + ' MilliSeconds';
  stElapsedTime := st;
  Writeln('Elapsed Time: ' + st);
  memo4.Lines.Add(st);


end;


end.
