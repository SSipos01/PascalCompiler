unit uJournal;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Variants, uCoreTypes;

type
  TLogOpType = (
    opUnknown,
    opBeginTx,
    opCommitTx,
    opRollbackTx,
    opCreateTable,
    opAddField,
    opAddRecord,
    opUpdateValue
  );

  { TLogEntryHeader defines the common structure for all log entries. }
  TLogEntryHeader = record
    TxID: Int64;
    OpType: TLogOpType;
    DataSize: Integer; // Size of the payload that follows
  end;

// For now, we will just define the structure.
// The implementation will require careful handling of serialization for
// different payload types.

type
  TJournalWriter = class
  private
    FStream: TFileStream;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure WriteEntry(AHeader: TLogEntryHeader; APayload: TBytes);
  end;

  TJournalReader = class
  private
    FStream: TFileStream;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Reset;
    function ReadNextEntry(var AHeader: TLogEntryHeader; var APayload: TBytes): Boolean;
  end;


implementation

{ TJournalWriter }

constructor TJournalWriter.Create(const AFileName: string);
begin
  inherited Create;
  FStream := TFileStream.Create(AFileName, fmOpenWrite or fmCreate);
  FStream.Position := FStream.Size; // Append to end of file
end;

destructor TJournalWriter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TJournalReader.Reset;
begin
  FStream.Position := 0;
end;

procedure TJournalWriter.WriteEntry(AHeader: TLogEntryHeader; APayload: TBytes);
begin
  AHeader.DataSize := Length(APayload);
  FStream.WriteBuffer(AHeader, SizeOf(TLogEntryHeader));
  if AHeader.DataSize > 0 then
    FStream.WriteBuffer(APayload[0], AHeader.DataSize);
end;

{ TJournalReader }

constructor TJournalReader.Create(const AFileName: string);
begin
  inherited Create;
  FStream := TFileStream.Create(AFileName, fmOpenRead);
end;

destructor TJournalReader.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TJournalReader.ReadNextEntry(var AHeader: TLogEntryHeader; var APayload: TBytes): Boolean;
begin
  Result := False;
  if FStream.Position < FStream.Size then
  begin
    FStream.ReadBuffer(AHeader, SizeOf(TLogEntryHeader));
    SetLength(APayload, AHeader.DataSize);
    if AHeader.DataSize > 0 then
      FStream.ReadBuffer(APayload[0], AHeader.DataSize);
    Result := True;
  end;
end;

end.
