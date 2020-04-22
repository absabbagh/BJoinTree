unit sqlUnit;

// {$OPTIMIZATION LEVEL3}


interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Menus, ExtCtrls,
  sql, lexlib,  yacclib,
  {$IFDEF UNIX}
  BSONTypes,
  Mongo, MongoDB, MongoCollection,
  MongoDBCursorIntf,
  {$ENDIF}
  regexpr;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    {$IFDEF UNIX}
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    {$ENDIF}
    procedure Button8Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    {$IFDEF UNIX}
    procedure Timer1Timer(Sender: TObject);
    {$ENDIF}
  private
    { Private declarations }

    {$IFDEF UNIX}
    counterCollection: TMongoCollection;
    {$ENDIF}
    counter: Integer;
    startCounter: Integer;

    function ExecuteReturnHeaderFromQuery(dbuserId: string; dbName: string; stQuery: string): string;
    procedure ExecuteQuery(dbuserId: string; dbName: string; stQuery: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  execProgramUnit,
  SDFUnit,
  {$IFDEF UNIX}
  MongoJoinTreeU,
  {$ENDIF}
  BPlusTreeU; // momentarely


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
  colnameFlag: boolean;
  colcount, rowcount: Integer;
begin
  {$DEFINE Debug}
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
    if trim(yyInputText) <> '' then
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
          Memo3.Lines.Add(IntToStr(mnemonic) + '-' + floattostr(Value) + '-' +
            stvalue + '-' + printInstruction);
  end;
  {$ENDIF}

  colnameFlag := true;
  colcount := 1;
  rowcount := 1;
  SDFInstance := SDFClass.Create();
  if length(sqlResults) <> 0 then
  begin
    SDFInstance.GetLexemes(sqlResults[0]);
    StringGrid1.ColCount :=  SDFInstance.CountLexemes;
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

function TForm1.executereturnHeaderfromQuery(dbuserId: string; dbName: string; stQuery: string): string;
var
  InputSt: string;

  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;

  st: string;

begin

  StartTime := now;
  InputSt := stQuery;
  sqlResults := nil;
  lQueryId := 'Temporary';

  ParseSQLStatement(InputSt,sqlMemProg);

  selectColsInstructions := nil;

  if yyerrmsgs = nil then
    executeProgram(sqlMemProg, dbuserId,dbName);

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
  //stElapsedTime := st;
  // Writeln('Elapsed Time: ' + st);

  result := rescolname;

end;


procedure TForm1.Button2Click(Sender: TObject);
var
  I, J :integer;
  InputSt: string;
  userId: string;
  dbName: string;
  stQuery: string;
  st: string;
begin
  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;
  StringGrid1.Clear;
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount := 5;
  yyerrmsgs := nil;

  // timer1.enabled := false;
  userId := edit4.Text;
  dbName := edit2.text;
  dbName := lowercase(dbName);

  sqlMemProg := nil;
  yyInputText := '';
  for I := 0 to memo1.Lines.Count - 1 do
    begin
      st := memo1.Lines[i];
   {   for j := 1 to length(st) do
        if st[j] = #9 then
         st[j] := ' ';       }
      yyinputText := yyinputText + ' ' + st;
    end;
  InputSt := yyinputText;

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
          edit2.Text := copy(yymiscmsgs[I],29,length(yymiscmsgs[I]));
      end;

  StQuery := '';
  if length(sqlResults) <> 0 then
    stQuery := stQuery + sqlResults[0];

  memo4.lines.Add(stElapsedTime);
  yyerrmsgs := nil;
  yymiscmsgs := nil;
  // timer1.enabled := true;

end;

{$IFDEF UNIX}
 const Path: string = '/opt/';
{$ELSE}
const Path: string = '';
{$ENDIF}

type
    DataPointerType =  Integer;//string[24];

//{$IFDEF UNIX}
procedure TForm1.Button5Click(Sender: TObject);
var
  idx: BtrPlusClass;
  Thekeys: array of string;
  TheInheritedKeys: array of string;
  keys: array of variant;
  inheritedkeys: array of variant;
  dataref: array  of datapointertype;
  i, j: Integer;
  st: string;
  //jdx: MongoBJoinTreeClass;
  jdata,ref: array of Integer;
  rowt: array of variant;
  rows: array of variant;

  tablelist: string;
  Thebasetables: array of string;
  cptablelist: string;
  tmptablelist: string;
  dbtblName: string;
  index: integer;
  dbName: string;
  found: boolean;

  Targettablelist: string;
  TargetdbtblName: string;
  a: array [0..5] of integer = (3,5,7,9,11,13);
  len : integer;
  l,R,m: Integer;

  T: integer;

  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;
  count: Integer;
begin
  (*memo4.lines.add(inttostr(GetProcessID));
  exit;

  len := 6;

  T:= 15;

  L := 0;
  R := len - 1;
  while (L < R) do
    begin
      m := (L + R) div 2;
      if a[m] < T then  L := m + 1;
      if a[m] > T then R := m-1;
    end;
  m := (L + R) div 2;
  m := m;
  exit;
*)
  {
  TargetTableList := 'd.v,d.s';
  tablelist := 'd.n,d2.v,d.e,d.s,d.o,d.v,d.r';

  tmptablelist := tablelist;

  while Targettablelist <> '' do
    begin
      if pos(',',Targettablelist) = 0 then
        begin
          TargetdbtblName := Targettablelist;
          Targettablelist := '';
        end else
        begin
          TargetdbtblName := copy(Targettablelist,1,pos(',',Targettablelist)-1);
          Targettablelist := copy(Targettablelist,pos(',',Targettablelist)+1,length(Targettablelist));
        end;

      cptablelist := '';
      while tmptablelist <> '' do
        begin
          if pos(',',tmptablelist) = 0 then
            begin
              dbtblName := tmptablelist;
              tmptablelist := '';
            end else
            begin
              dbtblName := copy(tmptablelist,1,pos(',',tmptablelist)-1);
              tmptablelist := copy(tmptablelist,pos(',',tmptablelist)+1,length(tmptablelist));
            end;
          if dbtblName <> TargetdbtblName then
            cptablelist := cptablelist + dbtblName + ',';
        end;
      tmptablelist := cptablelist;
    end;
    tablelist := tmptablelist;

  exit;
  }

  StartTime := Now;
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

  setlength(keys,3);
  setLength(InheritedKeys,3);
  setlength(dataref,3);




  memo4.Clear;
  randomize;

  for i := 1 to 1031  do
    begin
      edit6.Text := inttostr(i);
      edit6.Repaint;

      j := i;
      keys[0] :=  random(100000);
      keys[1] :=  intToStr(j+3);
      keys[2] :=  intToStr(j+4);

      InheritedKeys[0] := intToStr(j + 40);
      InheritedKeys[1] := intToStr(j + 60);
      InheritedKeys[2] :=  j + 90;

      dataref[0] :=  j*2;//random(1000000);//j*2;
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
    if count = 1031 then
      begin
        st := st;
      end;
    { if (Keys[0] <> count) and (dataref[0] <> -1) then
       begin
         memo4.Lines.Add('Error');
         exit;
       end; }
     if dataref[0] <> -1 then
       begin
         j := j +1;
         st := st + 'K0: ' + inttostr(keys[0]);
         st := st + '  K1: ' + keys[1];
         st := st + '  K2: ' + keys[2];
         memo4.Lines.Add(st);
         st := 'IK0: ' + Inheritedkeys[0];
         st := st +  '  IK1: ' + Inheritedkeys[1];
         st := st +  '  IK2: ' + intToStr(Inheritedkeys[2]);
         memo4.Lines.Add(st);
         st := 'DR0: ' + intToStr(dataref[0]);
         st := st +  '  DR1: ' + intToStr(dataref[1]);
         st := st +  '  DR2: ' + intToStr(dataref[2]);
         memo4.Lines.Add(st);
         memo4.Lines.Add('');


       end
   until dataref[0] = -1;

(*
   setlength(keys,3);
   keys[0] := 4;
   idx.findkey(keys,InheritedKeys,dataref);

   st := 'IK0: ' + Inheritedkeys[0];
   memo4.Lines.Add(st);
   st := 'DR0: ' + intToStr(dataref[0]);
   st := st +  '  DR1: ' + intToStr(dataref[1]);
   st := st +  '  DR2: ' + intToStr(dataref[2]);
   memo4.Lines.Add(st);
*)
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

   idx.Free;

  exit;

  (*
  GMongo := TMongo.Create;
  GMongo.Connect();

  GDB := GMongo.getDB('bjoin');

  setlength(thekeys,3);
  TheKeys[0] :=  'a0: int';
  TheKeys[1] :=  'a1: string[12]';
  TheKeys[2] :=  'a2: int';
  setLength(TheInheritedKeys,3);
  TheinheritedKeys[0] :=  'a0: int';
  TheinheritedKeys[1] :=  'a1: string[12]';
  TheinheritedKeys[2] :=  'a2: int';


  jdx := BJoinTreeClass.Create('ix',['t','s']);
  jdx.AddTableToDictionary('t');
  jdx.AddColumnToDictionary('a1','INTEGER','t');
  jdx.AddColumnToDictionary('a2','INTEGER','t');
  jdx.AddTableToDictionary('s');
  jdx.AddColumnToDictionary('a2','INTEGER','s');
  jdx.AddColumnToDictionary('a3','INTEGER','s');
  jdx.AddJoin('t','s','a2');
  jdx.AddJoin('s','t','a2');
  jdx.createBTrees(['t','s'],false,['t.a1']);
  jdx.Free;
*)

(*
  jdx := MongoBJoinTreeClass.Create('ix',GDB,['t','s']);
  jdx.AddTableToDictionary('t');
  jdx.AddColumnToDictionary('t0','INTEGER','t');
  jdx.AddColumnToDictionary('t1','INTEGER','t');
  jdx.AddColumnToDictionary('t2','INTEGER','t');

  jdx.AddTableToDictionary('s');
  jdx.AddColumnToDictionary('s0','INTEGER','s');
  jdx.AddColumnToDictionary('s1','INTEGER','s');
  jdx.AddColumnToDictionary('s2','INTEGER','s');
  jdx.AddColumnToDictionary('s3','INTEGER','s');
{
  jdx.AddTableToDictionary('v');
  jdx.AddColumnToDictionary('v0','INTEGER','v');
  jdx.AddColumnToDictionary('v1','INTEGER','v');
  jdx.AddColumnToDictionary('v2','INTEGER','v');
  jdx.AddColumnToDictionary('v3','INTEGER','v');
}
  jdx.AddJoin('t','s','t1');
  jdx.AddJoin('t','s','t2');
 // jdx.AddJoin('s','v','s3');

  jdx.AddJoin('s','t','s1');
  jdx.AddJoin('s','t','s2');
 // jdx.AddJoin('v','s','v3');

  jdx.createBTrees(['t','s'],true,['t.t0']);
*)

(*
  setlength(rowt,3);
  setlength(rows,4);
  for i := 1 to 0 do
    begin
      rowt[0] :=  i;
      rowt[1] :=  i+1;
      rowt[2] :=  i+2;
      rows[0] := i+3;
      rows[1] := rowt[1];
      rows[1] := rowt[1];

      rows[2] :=  i+4;
      rows[3] :=  i+4;
      jdx.AddKey('s',rows,inttostr(i));
      jdx.AddKey('t',rowt,inttostr(i));
      //memo4.Lines.Add(inttostr(i));
    end;

  jdx.AddKey('t',[0,1,2],'50');
  jdx.AddKey('s',[0,1,2,3],'3');
  jdx.AddKey('v',[0,1,2,3],'13');
  exit;
  setlength(keys,1);
  setlength(jDataref,2);
{
  jdx.ClearKey;
  jdx.NextKey(keys,jDataRef) ;
  jdx.NextKey(keys,jDataRef) ;
  jdx.NextKey(keys,jDataRef) ;
  jdx.NextKey(keys,jDataRef) ;
}
  keys := keys;
  jdx.Free;
*)

  (*
  idx := BtrPlusClass.Create('p',False,Thekeys,[true,true,true],TheinheritedKeys,1);
  idx.free;
  exit;
  *)



end;

{$IFDEF UNIX}
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
  stQuery: string;
  flagCreate: boolean;
  IBSONInstance: IBSONObject;
  flagcomment: boolean;
  QueryCollection: TMongoCollection;
begin
  userId := edit4.Text;
  dbName := edit2.text;
  SQLFileName := edit3.Text;
  yymiscmsgs := nil;
  yyerrmsgs := nil;
  SQLFileName := ExtractFileName(SQLFileName);
  ExtName := ExtractFileExt(SQLFileName);
  if not (upCase(ExtName) = '.SQL') then
   begin
    yyerror('Not a .SQL file');
    Exit;
  end;

  Path := edit5.Text;
  if not FileExists( Path + SQLFileName) then
  begin
    yyerror('File ' + SQLFileName + ' doesn''t Exist ');
    Exit;
  end;

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


(*
  QueryCollection := GDB.GetCollection('query');
  for I := low(SQLInstructions) to high(SQLInstructions) do
  begin

    IBSONInstance :=  TBSONObject.Create;

    IBSONInstance.Put('queryname','sys_' + SQLFileName + '_q'+ IntToStr(I+1));

    IBSONInstance.Put('querytext',SQLInstructions[I]);

    IBSONInstance.Put('querydbname',dbName);

    IBSONInstance.Put('userid',userID);

    IBSONInstance.Put('username','undefined');

    IBSONInstance.Put('submitted',123456789);

    IBSONInstance.Put('_id', TBSONObjectId.NewFrom);

    QueryCollection.Insert(IBSONInstance);

    ParseSQLStatement(SQLInstructions[I],sqlMemProg);

    selectColsInstructions := nil;

    executeProgram(sqlMemProg, userId, dbName);
  end;
*)

  memo4.clear;

  if yyerrmsgs <> nil then
    for I := 0 to length(yyerrmsgs) -1 do
      memo4.Lines.add(yyerrmsgs[I]);

  if yymiscmsgs <> nil then
    for I := 0 to length(yymiscmsgs) -1 do
      memo4.Lines.add(yymiscmsgs[I]);

end;

procedure TForm1.Button7Click(Sender: TObject);
var
  RequestsCollection: TMongoCollection;
  RequestsIBSONInstance: IBSonObject;
  Inputst: string;
  userId: string;
  dbName: string;
  I: integer;
  counterCursor:  IMongoDBCursor;
  counterIBSONInstance: IBSonObject;
  flag: boolean;
begin
  userId := edit4.Text;
  dbName := edit2.text;

  yyInputText := '';
  for I := 0 to memo1.Lines.Count - 1 do
    yyinputText := yyinputText + ' ' + memo1.Lines[i];
  InputSt := yyinputText;

  flag := false;
  counterCursor := counterCollection.find();
  if counterCursor.HasNext then
    begin
      flag := true;
      counterIBSONInstance := countercursor.next;
      Counter := counterIBSONInstance.Items['counter'].AsInteger;
    end;
(*
  RequestsCollection := GDB.GetCollection('requests');
  RequestsIBSONInstance := TBSONObject.create;
  RequestsIBSONInstance.Put( 'sys_user', userId );
  RequestsIBSONInstance.Put( 'sys_dbname', dbName );
  RequestsIBSONInstance.Put( 'sys_query', Inputst );
  RequestsIBSONInstance.Put( 'sys_flag', true );
  RequestsIBSONInstance.Put( 'sys_counter', counter );
  RequestsIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
  RequestsCollection.Insert(RequestsIBSONInstance);
*)
  Counter := Counter + 1;
  counterIBSONInstance.Put('counter',Counter);
  counterCollection.Update(counterIBSONInstance,counterIBSONInstance);

end;
{$ENDIF}

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
      connectToDB('TS1','data'); // Database in Mongo seen as a tablespace
      if authenticate(userId, stpassword) then
        begin
          edit4.Enabled := false;
          edit7.Enabled := false;
          Memo1.Enabled := true;
          Button8.Caption := 'DISCONNECT'
        end else disconnectFromDB;
    end else
    begin
      disconnectFromDB;
      Button8.Caption := 'CONNECT';
      edit4.Enabled := true;
      edit7.Enabled := true;
      Memo1.Enabled := false;
      Memo1.Clear;
      Memo2.Clear;
      Memo3.Clear;
      Memo4.Clear;
      StringGrid1.Clear;
      StringGrid1.ColCount := 4;
      StringGrid1.RowCount := 5;
    end;
end;

procedure TForm1.FormActivate(Sender: TObject);
{var
  counterCursor:  IMongoDBCursor;
  counterIBSONInstance: IBSonObject;
}
begin
  // if the database is dropped, the program should restarted
  {
  counter := 0;
  counterCollection := GDB.GetCollection('counter');
  counterCursor := counterCollection.find();
  if counterCursor.HasNext then
    begin
      counterIBSONInstance := countercursor.next;
      startCounter := counterIBSONInstance.Items['counter'].AsInteger - 1
    end else
    begin
      counterIBSONInstance := TBSONObject.Create;
      counterIBSONInstance.Put('counter',Counter);
      counterIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
      counterCollection.Insert(counterIBSONInstance);
      startCounter := -1;
    end;


  Timer1.Enabled := true; }
end;


{$IFDEF UNIX}
procedure TForm1.Timer1Timer(Sender: TObject);
var
  I, J: Integer;
  ResponseCollection: TMongoCollection;
  ResponseIBSONInstance: IBSonObject;
  hInputst: string;
  RequestsCollection: TMongoCollection;
  RequestsCursor:  IMongoDBCursor;
  RequestsIBSONInstance: IBSonObject;
  sys_queryId: string;
  userId: string;
  dbName: string;
  Inputst: string;
  StartTime: TDate;
  Diff: TTime;
  Hour, Min, Sec, MSec: word;
  st: string;
begin
  (*
  timer1.Interval := 1500;
  RequestsCollection := GDB.GetCollection('requests');
  RequestsIBSONInstance := TBSONObject.create;

  if RequestsCollection.Count <> 0 then
    begin
      RequestsCursor := RequestsCollection.Find();
      while RequestsCursor.HasNext do
        begin
          RequestsIBSONInstance := RequestsCursor.Next;
          Counter := RequestsIBSONInstance.Items['sys_counter'].AsInteger;
          if Counter > startCounter then
            begin

              StartTime := Now;

              if RequestsIBSONInstance.HasOid then
                sys_queryId := RequestsIBSONInstance.GetOid.GetOID else
                sys_queryId := RequestsIBSONInstance.Items['_id'].AsString;
              lqueryId := sys_queryId;
              userId := RequestsIBSONInstance.Items['sys_user'].AsString;
              dbname := RequestsIBSONInstance.Items['sys_dbname'].AsString;
              InputSt := RequestsIBSONInstance.Items['sys_query'].AsString;

              yyerrmsgs := nil;
              yymiscmsgs := nil;
              ResponseCollection := GDB.GetCollection('response');
              ResponseIBSONInstance := TBSONObject.create;
              // Process the query
              rescolname := '';
              startCounter := Counter;

              memo1.Clear;
              memo1.Lines.Add(InputSt);
              if pos('PROCESS SYNTAX',trim(Uppercase(Inputst))) <> 0 then
                begin
                  Inputst := trim(Inputst);
                  Inputst := copy(Inputst,16,length(Inputst));
                  ParseSQLStatement(InputSt,sqlMemProg);
                  sqlresults := nil;

                  memo4.clear;
                  if yyerrmsgs <> nil then
                    for I := 0 to length(yyerrmsgs) - 1 do
                      begin
                        ResponseIBSONInstance.put('sys_queryId',sys_queryId);
                        ResponseIBSONInstance.put('sys_user',userId);
                        ResponseIBSONInstance.put('sys_dbname',dbname);
                        ResponseIBSONInstance.put('headertext',rescolname);
                        ResponseIBSONInstance.put('messagetext',yyerrmsgs[i]);
                        ResponseIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                        ResponseCollection.Insert(ResponseIBSONInstance);
                        memo4.Lines.Add(yyerrmsgs[i]);
                      end;
                  if yymiscmsgs <> nil then
                    for I := 0 to length(yymiscmsgs) - 1 do
                      begin
                        ResponseIBSONInstance.put('sys_queryId',sys_queryId);
                        ResponseIBSONInstance.put('sys_user',userId);
                        ResponseIBSONInstance.put('sys_dbname',dbname);
                        ResponseIBSONInstance.put('headertext',rescolname);
                        ResponseIBSONInstance.put('messagetext',yymiscmsgs[i]);
                        ResponseIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                        ResponseCollection.Insert(ResponseIBSONInstance);

                        if yymiscmsgs <> nil then
                          for J := 0 to length(yymiscmsgs) - 1 do
                            begin
                              Memo4.Lines.Add(yymiscmsgs[J]);
                              if pos('Switch',yymiscmsgs[J]) <> 0 then
                                edit2.Text := copy(yymiscmsgs[J],29,length(yymiscmsgs[J]));
                            end;
                      end;
                  yyerrmsgs := nil;
                  yymiscmsgs := nil;

                  timer1.Interval := 500;
                  exit;
                end;

              edit4.Text := userId;
              edit2.Text := dbname;

              rescolname := '';
              if pos('SELECT',trim(Uppercase(Inputst))) <> 0 then
               begin
                 hInputSt  := 'HEADER ' + Inputst;
                 executeQuery(userId, dbName, hInputSt);
                end;
              executeQuery(userId, dbName, InputSt);

              memo4.clear;
              if yyerrmsgs <> nil then
                for I := 0 to length(yyerrmsgs) - 1 do
                  begin
                    ResponseIBSONInstance.put('sys_queryId',sys_queryId);
                    ResponseIBSONInstance.put('sys_user',userId);
                    ResponseIBSONInstance.put('sys_dbname',dbname);
                    ResponseIBSONInstance.put('headertext',rescolname);
                    ResponseIBSONInstance.put('messagetext',yyerrmsgs[i]);
                    ResponseIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                    ResponseCollection.Insert(ResponseIBSONInstance);
                    memo4.Lines.Add(yyerrmsgs[i]);
                  end;
              if yymiscmsgs <> nil then
                begin
                  for I := 0 to length(yymiscmsgs) - 1 do
                    begin
                      ResponseIBSONInstance.put('sys_queryId',sys_queryId);
                      ResponseIBSONInstance.put('sys_user',userId);
                      ResponseIBSONInstance.put('sys_dbname',dbname);
                      ResponseIBSONInstance.put('headertext',rescolname);
                      ResponseIBSONInstance.put('messagetext',yymiscmsgs[i]);
                      ResponseIBSONInstance.Put('_id', TBSONObjectId.NewFrom);
                      ResponseCollection.Insert(ResponseIBSONInstance);
                   end;

                  if yymiscmsgs <> nil then
                    for J := 0 to length(yymiscmsgs) - 1 do
                      begin
                        Memo4.Lines.Add(yymiscmsgs[J]);
                        if pos('Switch',yymiscmsgs[J]) <> 0 then
                          edit2.Text := copy(yymiscmsgs[J],29,length(yymiscmsgs[J]));
                      end;
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
              memo4.Lines.Add(stElapsedTime);

              timer1.Interval := 500;
            end;
        end;

    end;
    *)
end;
{$ENDIF}

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
  dbName := edit2.text;

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
  dbName := edit2.text;
  CSVFileName := edit1.Text;
//  CSVFileName := 'hello-pippo-' + CSVFileName;
  yymiscmsgs := nil;
  yyerrmsgs := nil;

  StartTime := Now;


  Path := edit5.Text;

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
