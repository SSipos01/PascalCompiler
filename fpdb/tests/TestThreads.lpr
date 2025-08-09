program TestThreads;

{$mode objfpc}{$H+}

uses
  CThreads, Classes, SysUtils,
  uCoreTypes in '../src/uCoreTypes.pas',
  uDBEntities in '../src/uDBEntities.pas',
  uCrypto in '../src/uCrypto.pas';

const
  NUM_READERS = 5;
  TEST_DURATION_MS = 5000; // 5 seconds

var
  DB: TDatabase;
  TerminationFlag: Boolean = False;
  WriterCounter: Integer = 0;
  ReaderCounters: array[1..NUM_READERS] of Integer;

type
  TWriterThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TReaderThread = class(TThread)
  private
    FReaderID: Integer;
  public
    constructor Create(ReaderID: Integer);
    procedure Execute; override;
  end;

{ TWriterThread }
procedure TWriterThread.Execute;
var
  UsersTable: TDBTable;
  NewRecord: TDBRecord;
begin
  Writeln('Writer thread started.');
  UsersTable := DB.GetTable('Users');
  if UsersTable = nil then
  begin
    Writeln('Writer: Table "Users" not found!');
    Exit;
  end;

  while not Terminated do
  begin
    NewRecord := UsersTable.AddNew;
    NewRecord.Values['ID'] := UsersTable.NextAutoIncValue;
    NewRecord.Values['Name'] := 'Thread ' + IntToStr(ThreadID);
    NewRecord.Values['Age'] := Random(50) + 20;
    NewRecord.Values['Registered'] := Now;
    NewRecord.Values['IsActive'] := True;

    Inc(WriterCounter);
    Sleep(Random(50) + 10); // Simulate work
  end;
  Writeln('Writer thread finished.');
end;

{ TReaderThread }
constructor TReaderThread.Create(ReaderID: Integer);
begin
  FReaderID := ReaderID;
  inherited Create(False); // Create suspended
end;

procedure TReaderThread.Execute;
var
  UsersTable: TDBTable;
  RecCount: Integer;
begin
  Writeln(Format('Reader thread %d started.', [FReaderID]));
  UsersTable := DB.GetTable('Users');
  if UsersTable = nil then
  begin
    Writeln(Format('Reader %d: Table "Users" not found!', [FReaderID]));
    Exit;
  end;

  while not Terminated do
  begin
    RecCount := UsersTable.RecordCount;
    // We could do more work here, like iterating, but this is enough
    // to test the read lock.
    Inc(ReaderCounters[FReaderID]);
    Sleep(Random(20) + 5); // Readers are faster
  end;
  Writeln(Format('Reader thread %d finished.', [FReaderID]));
end;


var
  Writer: TWriterThread;
  Readers: array[1..NUM_READERS] of TReaderThread;
  i: Integer;

begin
  Writeln('Starting Thread Safety Test...');
  Randomize;
  for i := 1 to NUM_READERS do
    ReaderCounters[i] := 0;

  // --- Create Shared DB and Table ---
  DB := TDatabase.Create;
  with DB.CreateTable('Users') do
  begin
    AddField('ID', ftAutoInc);
    AddField('Name', ftString, 50);
    AddField('Age', ftInteger);
    AddField('Registered', ftDateTime);
    AddField('IsActive', ftBoolean);
  end;
  Writeln('Shared database and table created.');

  // --- Create and Start Threads ---
  Writeln('Starting threads...');
  Writer := TWriterThread.Create(False);
  for i := 1 to NUM_READERS do
    Readers[i] := TReaderThread.Create(i);

  // Start threads (they were created suspended)
  Writer.Start;
  for i := 1 to NUM_READERS do
    Readers[i].Start;

  // --- Run Test ---
  Writeln(Format('Running test for %d seconds...', [TEST_DURATION_MS div 1000]));
  Sleep(TEST_DURATION_MS);

  // --- Terminate Threads ---
  Writeln('Signaling threads to terminate...');
  Writer.Terminate;
  for i := 1 to NUM_READERS do
    Readers[i].Terminate;

  Writeln('Waiting for threads to finish...');
  Writer.WaitFor;
  for i := 1 to NUM_READERS do
    Readers[i].WaitFor;

  Writeln('All threads finished.');
  Writeln('');

  // --- Print Results ---
  Writeln('--- Test Results ---');
  Writeln(Format('Writer thread added %d records.', [WriterCounter]));
  for i := 1 to NUM_READERS do
    Writeln(Format('Reader thread %d performed %d reads.', [i, ReaderCounters[i]]));
  Writeln(Format('Final record count in table: %d', [TDBTable(DB.Tables[0]).RecordCount]));
  Writeln('--------------------');

  // --- Cleanup ---
  Writer.Free;
  for i := 1 to NUM_READERS do
    Readers[i].Free;
  DB.Free;

  Writeln('Test finished successfully.');
end.
