unit lockmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  maxLocksEntries = 15;

type
  LockClass = class
    private
      LockTable: array of record
        dbname: string;
        tblname: string;
        ListLink: array of record
          TransactionId: Integer;
          request: (Shared, Exclusive, IntentShared, IntentExclusive, SharedWithIntentExclusive);
          Granted: Boolean
        end
      end;

      FSync: array of record
        cntLX: Integer;
        cntLS: Integer;
        databaseName: string;
        tableName: string;
        dataPointer: LongInt;
        Sync: IReadWriteSync;
      end;
      function SyncAvailable(dbName: string; tblName: string; DP: LongInt): Integer; overload;
      function SyncAvailable(dbName: string): Integer; overload;
      function SyncAvailable(dbName: string; tblName: string): Integer; overload;
      procedure LockShared(dbName: string); overload;
      procedure LockIntentShared(TransactionId: Integer; dbName: string; tblName: string);
      procedure LockIntentExclusive(TransactionId: Integer; dbName: string; tblName: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LockShared(TransactionId: Integer; dbName: string; tblName: string); overload;
      procedure LockShared(TransactionId: Integer; dbName: string; tblName: string; DP: LongInt); overload;
      procedure LockSharedWithIntentExclusive(TransactionId: Integer; dbName:string; tblName: string); // To use when select many for update some
      procedure LockExclusive(dbName:string); overload; // Only for Backup, Restore and Drop Database
      procedure LockExclusive(TransactionId: Integer; dbName: string; tblName: string); overload;
      procedure LockExclusive(TransactionId: Integer; dbName: string; tblName: string; DP: LongInt); overload;

      procedure UnLockShared(TransactionId: Integer; dbName: string; tblName: string); overload;
      procedure UnLockShared(TransactionId: Integer; dbName: string; tblName: string; DP: LongInt); overload;

      procedure UnlockShared(dbName: string; tblName: string; DP: LongInt);
      procedure UnlockExclusive(dbName: string; tblName: string; DP: LongInt);
  end;


implementation

{ #note :
At the row level, the following two lock modes can be applied:
                Exclusive (X)        Shared (S)
  Exclusive (X)     D                   D
  Shared (S)        D                   A

  At the table level, there are four different types of locks:
                                      (X)         (S)     (IX)    (IS)    (SIX)
  Exclusive (X)                        D           D        D       D       D
  Shared(S)                            D           A        D       A       D
  Intent Exclusive (IX)                D           D        A       A       D
  Intent shared (IS)                   D           A        A       A       A
  Shared with intent exclusive (SIX)   D           D        D       A       D
}

constructor LockClass.Create;
begin
  Inherited create;
  FSync := nil;
  LockTable := nil;
end;

destructor LockClass.Destroy;
begin
  FSync := nil; // Check if any lock exist and unlock it
  LockTable := nil;
  free;

end;

function LockClass.SyncAvailable(dbName: string; tblName: string; DP: LongInt): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := low(FSync) to High(FSync) do
    with FSync[i] do
      if (dbName = databaseName) and (tblName = tableName) and
         (DP = dataPointer) then
        begin
          result := i;
          break
        end;
end;

function LockClass.SyncAvailable(dbName: string): Integer;
begin
  result := SyncAvailable(dbName,'',-1)
end;

function LockClass.SyncAvailable(dbName: string; tblName: string): Integer;
begin
  result := SyncAvailable(dbName,tblName,-1)
end;

procedure LockClass.LockShared(dbName: string);
var
  Index: Integer;
begin
  Index := SyncAvailable(dbName);
  if Index = -1 then
    begin
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := '';
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end else
    begin
      with FSync[Index] do
        begin
          cntLS += 1;
          Sync.BeginRead;
        end;
    end;
end;

procedure LockClass.LockShared(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i, j: Integer;
  flagExclusive: Boolean = false;
begin
  LockShared(dbName);
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if not found then
    begin
      setLength(LockTable,length(LockTable)+1);
      LockTable[High(LockTable)].dbname := dbname;
      LockTable[High(LockTable)].tblname := tblname;
      setLength(LockTable[High(LockTable)].ListLink,1);
      LockTable[High(LockTable)].ListLink[0].TransactionId := TransactionId ;
      LockTable[High(LockTable)].ListLink[0].request := Shared;
      LockTable[High(LockTable)].ListLink[0].Granted := false;
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end
   else
    begin
      for j := 0 to High(LockTable[i].ListLink) do
        if (LockTable[i].ListLink[j].request = Exclusive) or
           (LockTable[i].ListLink[j].request = IntentExclusive) or
           (LockTable[i].ListLink[j].request = SharedWithIntentExclusive) then
          begin
            flagExclusive := true;
            break
          end;
      setLength(LockTable[i].ListLink,length(LockTable[i].ListLink)+1);
      LockTable[i].ListLink[High(LockTable[i].ListLink)].TransactionId := TransactionId ;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].Granted := false;
      if flagExclusive then
        with FSync[SyncAvailable(dbName,tblName)] do
          begin
            LockTable[i].ListLink[High(LockTable[i].ListLink)].request := Exclusive;
            cntLX += 1;
            Sync.BeginWrite;
          end
       else
       with FSync[SyncAvailable(dbName,tblName)] do
         begin
           LockTable[i].ListLink[High(LockTable[i].ListLink)].request := Shared;
           cntLS += 1;
           Sync.BeginRead;
         end
    end;
end;

procedure LockClass.UnlockShared(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i, j: integer;
begin
  UnlockShared(dbName);
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if found then
    for j := 0 to High(LockTable[i].ListLink) do
      if (LockTable[i].ListLink[j].TransactionId = TransactionId) then
        begin
          if LockTable[i].ListLink[j].request = Exclusive then
            begin
              if FSync[SyncAvailable(dbName,tblName)].cntLX > 0 then
                begin
                  FSync[SyncAvailable(dbName,tblName)].cntLX -= 1;
                  FSync[SyncAvailable(dbName,tblName)].Sync.EndWrite;
                end;
            end;
          if LockTable[i].ListLink[j].request = Shared then
            begin
              if FSync[SyncAvailable(dbName,tblName)].cntLS > 0 then
                begin
                  FSync[SyncAvailable(dbName,tblName)].cntLS -= 1;
                  FSync[SyncAvailable(dbName,tblName)].Sync.EndRead;
                end;
            end;
          break
        end;

end;

procedure LockClass.LockShared(TransactionId: Integer; dbName: string; tblName: string; DP: LongInt);
var
  Index: Integer;
begin
  LockShared(dbName);
  LockIntentShared(TransactionId,dbName,tblName);
  Index := SyncAvailable(dbName,tblName,DP);
  if Index = -1 then
    begin
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := DP;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end else
    begin
      with FSync[Index] do
        begin
          cntLS += 1;
          Sync.BeginRead;
        end;
    end;
end;

procedure LockClass.LockIntentShared(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i: Integer;
begin
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if not found then
    begin
      setLength(LockTable,length(LockTable)+1);
      LockTable[High(LockTable)].dbname := dbname;
      LockTable[High(LockTable)].tblname := tblname;
      setLength(LockTable[High(LockTable)].ListLink,1);
      LockTable[High(LockTable)].ListLink[0].TransactionId := TransactionId ;
      LockTable[High(LockTable)].ListLink[0].request := IntentShared;
      LockTable[High(LockTable)].ListLink[0].Granted := false;
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end
   else
    begin
      setLength(LockTable[i].ListLink,length(LockTable[i].ListLink)+1);
      LockTable[i].ListLink[High(LockTable[i].ListLink)].TransactionId := TransactionId ;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].request := IntentShared;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].Granted := false;
      with FSync[SyncAvailable(dbName,tblName)] do
        begin
          cntLS += 1;
          Sync.BeginRead;
        end
    end;
end;

procedure LockClass.LockIntentExclusive(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i, j: Integer;
  flagExclusive: Boolean = false;
begin
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if not found then
    begin
      setLength(LockTable,length(LockTable)+1);
      LockTable[High(LockTable)].dbname := dbname;
      LockTable[High(LockTable)].tblname := tblname;
      setLength(LockTable[High(LockTable)].ListLink,1);
      LockTable[High(LockTable)].ListLink[0].TransactionId := TransactionId ;
      LockTable[High(LockTable)].ListLink[0].request := Shared;
      LockTable[High(LockTable)].ListLink[0].Granted := false;
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end
   else
    begin
      for j := 0 to High(LockTable[i].ListLink) do
        if (LockTable[i].ListLink[j].request = Exclusive) or
           (LockTable[i].ListLink[j].request = Shared) or
           (LockTable[i].ListLink[j].request = SharedWithIntentExclusive) then
          begin
            flagExclusive := true;
            break
          end;
      setLength(LockTable[i].ListLink,length(LockTable[i].ListLink)+1);
      LockTable[i].ListLink[High(LockTable[i].ListLink)].TransactionId := TransactionId ;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].request := Shared;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].Granted := false;
      if flagExclusive then
        with FSync[SyncAvailable(dbName,tblName)] do
          begin
            cntLX += 1;
            Sync.BeginWrite;
          end
       else
       with FSync[SyncAvailable(dbName,tblName)] do
         begin
           cntLS += 1;
           Sync.BeginRead;
         end
    end;
end;

procedure LockClass.LockSharedWithIntentExclusive(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i, j: Integer;
  flagExclusive: Boolean = true;
begin
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if not found then
    begin
      setLength(LockTable,length(LockTable)+1);
      LockTable[High(LockTable)].dbname := dbname;
      LockTable[High(LockTable)].tblname := tblname;
      setLength(LockTable[High(LockTable)].ListLink,1);
      LockTable[High(LockTable)].ListLink[0].TransactionId := TransactionId ;
      LockTable[High(LockTable)].ListLink[0].request := Shared;
      LockTable[High(LockTable)].ListLink[0].Granted := false;
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 1;
          cntLX := 0;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginRead;
        end
    end
   else
    begin
      for j := 0 to High(LockTable[i].ListLink) do
        if (LockTable[i].ListLink[j].request = Shared) then
          begin
            flagExclusive := false;
            break
          end;
      setLength(LockTable[i].ListLink,length(LockTable[i].ListLink)+1);
      LockTable[i].ListLink[High(LockTable[i].ListLink)].TransactionId := TransactionId ;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].request := Shared;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].Granted := false;
      if flagExclusive then
        with FSync[SyncAvailable(dbName,tblName)] do
          begin
            cntLX += 1;
            Sync.BeginWrite;
          end
       else
       with FSync[SyncAvailable(dbName,tblName)] do
         begin
           cntLS += 1;
           Sync.BeginRead;
         end
    end;
end;

procedure LockClass.LockExclusive(dbName: string);
var
  Index: Integer;
begin
  Index := SyncAvailable(dbName);
  if Index = -1 then
    begin
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 0;
          cntLX := 1;
          databaseName := dbName;
          tableName := '';
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginWrite;
        end
    end else
    begin
      with FSync[Index] do
        begin
          cntLX += 1;
          Sync.BeginWrite;
        end;
    end;
end;

procedure LockClass.LockExclusive(TransactionId: Integer; dbName: string; tblName: string);
var
  found: boolean = false;
  i: Integer;
begin
  for i := 0 to high(LockTable) do
    begin
      if (LockTable[i].dbname = dbName) and (LockTable[i].tblname = tblName) then
        begin
          found := true;
          break
        end;
    end;
  if not found then
    begin
      setLength(LockTable,length(LockTable)+1);
      LockTable[High(LockTable)].dbname := dbname;
      LockTable[High(LockTable)].tblname := tblname;
      setLength(LockTable[High(LockTable)].ListLink,1);
      LockTable[High(LockTable)].ListLink[0].TransactionId := TransactionId ;
      LockTable[High(LockTable)].ListLink[0].request := Exclusive;
      LockTable[High(LockTable)].ListLink[0].Granted := false;
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 0;
          cntLX := 1;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := -1;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginWrite;
        end
    end
   else
    begin
      setLength(LockTable[i].ListLink,length(LockTable[i].ListLink)+1);
      LockTable[i].ListLink[High(LockTable[i].ListLink)].TransactionId := TransactionId ;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].request := Exclusive;
      LockTable[i].ListLink[High(LockTable[i].ListLink)].Granted := false;
      with FSync[SyncAvailable(dbName,tblName)] do
        begin
          cntLX += 1;
          Sync.BeginWrite;
        end;
    end;
end;

procedure LockClass.LockExclusive(TransactionId: Integer; dbName: string; tblName: string; DP: LongInt);
var
  Index: Integer;
begin
  LockShared(dbName);
  LockIntentExclusive(TransactionId,dbName,tblName);
  Index := SyncAvailable(dbName,tblName,DP);
  if Index = -1 then
    begin
      setlength(FSync,length(FSync)+1);
      with FSync[High(FSync)] do
        begin
          cntLS := 0;
          cntLX := 1;
          databaseName := dbName;
          tableName := tblName;
          dataPointer := DP;
          Sync := TSimpleRWSync.Create as IReadWriteSync;
          Sync.BeginWrite;
        end
    end else
    begin
      with FSync[Index] do
        begin
          cntLX += 1;
          Sync.BeginWrite;
        end;
    end;
end;

procedure LockClass.UnlockShared(dbName: string; tblName: string; DP: LongInt);
var
  Index: Integer;
begin
  Index := SyncAvailable(dbName,tblName,DP);
  if Index <> -1 then
    with FSync[Index] do
      if cntLS > 0 then
        begin
          cntLS -= 1;
          Sync.EndRead;
        end;
end;

procedure LockClass.UnlockExclusive(dbName: string; tblName: string; DP: LongInt);
var
  Index: Integer;
begin
  Index := SyncAvailable(dbName,tblName,DP);
  if Index <> -1 then
    with FSync[Index] do
      if cntLX > 0 then
        begin
          cntLX -= 1;
          Sync.EndWrite;
      end;
end;

end.

