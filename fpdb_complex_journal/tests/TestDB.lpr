program TestDB;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Contnrs,
  uCoreTypes in '../src/uCoreTypes.pas',
  uDBEntities in '../src/uDBEntities.pas',
  uCrypto in '../src/uCrypto.pas';

var
  DB: TDatabase;
  Authors, Books: TDBTable;
  NewRecord: TDBRecord;
  JoinResult: TDBJoinResult;
  i: Integer;

begin
  Writeln('Starting Join Engine Test...');
  DB := TDatabase.Create;
  try
    // All modifications must be in a transaction
    DB.BeginTransaction;

    // --- Create Tables ---
    Writeln('Creating tables: Authors and Books...');
    Authors := DB.CreateTable('Authors');
    Authors.AddField('ID', ftAutoInc);
    Authors.AddField('Name', ftString, 50);

    Books := DB.CreateTable('Books');
    Books.AddField('ID', ftAutoInc);
    Books.AddField('AuthorID', ftInteger);
    Books.AddField('Title', ftString, 100);

    // --- Populate Tables ---
    Writeln('Populating tables...');
    NewRecord := Authors.AddNew;
    NewRecord.Values['ID'] := Authors.NextAutoIncValue; // ID = 1
    NewRecord.Values['Name'] := 'Jules Verne';

    NewRecord := Authors.AddNew;
    NewRecord.Values['ID'] := Authors.NextAutoIncValue; // ID = 2
    NewRecord.Values['Name'] := 'Frank Herbert';

    NewRecord := Books.AddNew;
    NewRecord.Values['ID'] := Books.NextAutoIncValue;
    NewRecord.Values['AuthorID'] := 1; // Jules Verne
    NewRecord.Values['Title'] := '20,000 Leagues Under the Seas';

    NewRecord := Books.AddNew;
    NewRecord.Values['ID'] := Books.NextAutoIncValue;
    NewRecord.Values['AuthorID'] := 2; // Frank Herbert
    NewRecord.Values['Title'] := 'Dune';

    NewRecord := Books.AddNew;
    NewRecord.Values['ID'] := Books.NextAutoIncValue;
    NewRecord.Values['AuthorID'] := 1; // Jules Verne
    NewRecord.Values['Title'] := 'Journey to the Center of the Earth';

    // --- Create Index for Join ---
    Writeln('Creating index on Authors.ID...');
    Authors.CreateIndex('idx_ID', 'ID');

    Writeln('Committing transaction...');
    DB.Commit;

    // --- Perform Join (Read operation, no transaction needed) ---
    Writeln('Performing join on Authors.ID = Books.AuthorID...');
    JoinResult := DB.Join(Authors, 'ID', Books, 'AuthorID');

    // --- Display Results ---
    Writeln('');
    Writeln('--- Joined Results ---');
    if JoinResult.Count > 0 then
    begin
      for i := 0 to JoinResult.Count - 1 do
      begin
        Writeln(Format(
          'Author: %s, Book: %s',
          [
            VarToStr(JoinResult[i].Values['Name']),
            VarToStr(JoinResult[i].Values['Title'])
          ]
        ));
      end;
    end
    else
    begin
      Writeln('(No results from join)');
    end;
    Writeln('----------------------');

    JoinResult.Free;

  finally
    DB.Free;
  end;

  Writeln('Test finished.');
end.
