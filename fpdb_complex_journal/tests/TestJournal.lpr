program TestJournal;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Contnrs,
  uCoreTypes in '../src/uCoreTypes.pas',
  uDBEntities in '../src/uDBEntities.pas',
  uCrypto in '../src/uCrypto.pas',
  uJournal in '../src/uJournal.pas';

const
  DB_FILE = 'journal_test.db';

var
  DB: TDatabase;
  Table: TDBTable;
  Rec: TDBRecord;

begin
  Writeln('--- Starting Journaling and Recovery Test ---');

  // Clean up from previous runs
  if FileExists(DB_FILE) then DeleteFile(DB_FILE);
  if FileExists(DB_FILE + '-wal') then DeleteFile(DB_FILE + '-wal');

  // --- Test 1: Create data and "crash" ---
  Writeln('');
  Writeln('Step 1: Creating data and committing, then simulating a crash...');
  DB := TDatabase.Create;
  // We need to associate the DB with a file name for journaling to work
  DB.LoadFromFile(DB_FILE, []); // This sets the filename internally

  DB.BeginTransaction;
  Writeln('  Transaction started.');
  Table := DB.CreateTable('TestTable');
  Table.AddField('ID', ftInteger);
  Table.AddField('Data', ftString, 20);
  Rec := Table.AddNew;
  Rec.Values['ID'] := 1;
  Rec.Values['Data'] := 'Committed';
  DB.Commit;
  Writeln('  Transaction committed. WAL file should exist.');
  DB.Free; // "Crash" without checkpointing

  if FileExists(DB_FILE + '-wal') then
    Writeln('  SUCCESS: WAL file exists as expected.');
  else
    Writeln('  FAILURE: WAL file was not created.');

  // --- Test 2: Recovery ---
  Writeln('');
  Writeln('Step 2: Loading database to trigger recovery...');
  DB := TDatabase.Create;
  DB.LoadFromFile(DB_FILE, []); // This should find the WAL and recover

  Writeln('  Recovery process finished. Verifying data...');
  Table := DB.GetTable('TestTable');
  if Table = nil then
    Writeln('  FAILURE: Table "TestTable" not found after recovery.')
  else if Table.RecordCount <> 1 then
    Writeln(Format('  FAILURE: Expected 1 record, but found %d.', [Table.RecordCount]))
  else
  begin
    Rec := TDBRecord(Table.Records[0]);
    if VarToStr(Rec.Values['Data']) = 'Committed' then
      Writeln('  SUCCESS: Committed data was recovered correctly.')
    else
      Writeln('  FAILURE: Recovered data is incorrect.');
  end;

  if not FileExists(DB_FILE + '-wal') then
    Writeln('  SUCCESS: WAL file was deleted by checkpoint after recovery.')
  else
    Writeln('  FAILURE: WAL file was not deleted after recovery.');

  // --- Test 3: Rollback ---
  Writeln('');
  Writeln('Step 3: Testing transaction rollback...');
  DB.BeginTransaction;
  Table := DB.GetTable('TestTable');
  Rec := Table.AddNew;
  Rec.Values['ID'] := 2;
  Rec.Values['Data'] := 'Rolled Back';
  Writeln(Format('  Record count before rollback: %d', [Table.RecordCount]));
  DB.Rollback;
  Writeln('  Transaction rolled back.');

  Table := DB.GetTable('TestTable');
  Writeln(Format('  Record count after rollback: %d', [Table.RecordCount]));
  if Table.RecordCount = 1 then
    Writeln('  SUCCESS: Rollback correctly discarded the new record.')
  else
    Writeln('  FAILURE: Rollback did not work as expected.');

  DB.Free;

  Writeln('');
  Writeln('--- Test Finished ---');
end.
