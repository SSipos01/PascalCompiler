program TestDB;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Contnrs,
  uCoreTypes in '../src/uCoreTypes.pas',
  uDBEntities in '../src/uDBEntities.pas';

var
  DB: TDatabase;
  UsersTable: TDBTable;
  NewRecord: TDBRecord;
  i: Integer;

begin
  Writeln('Starting Basic Test...');
  DB := TDatabase.Create;
  try
    UsersTable := DB.CreateTable('Users');
    UsersTable.AddField('ID', ftInteger);
    UsersTable.AddField('Name', ftString, 50);

    NewRecord := UsersTable.AddNew;
    NewRecord.Values[0] := 1;
    NewRecord.Values[1] := 'Jules';

    Writeln('Test Finished.');
  finally
    DB.Free;
  end;
end.
