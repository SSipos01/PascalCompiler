unit uDBEntities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, uCoreTypes, SyncObjs, uJournal;

type
  { Forward declarations }
  TDBFieldDef = class;
  TDBRecord = class;
  TDBTable = class;
  TDatabase = class;
  TDBIndex = class;
  TIndexNode = class;
  TDBJoinedRecord = class;
  TDBJoinResult = class;

  { TDBFieldDef: Defines a column (field) in a table. }
  TDBFieldDef = class(TObject)
  private
    FName: string;
    FDataType: TDBFieldType;
    FSize: Integer;
  public
    constructor Create(const AName: string; ADataType: TDBFieldType; ASize: Integer = 0);
    property Name: string read FName;
    property DataType: TDBFieldType read FDataType;
    property Size: Integer read FSize;
  end;

  { TDBRecord: Represents a single row of data in a table. }
  TDBRecord = class(TObject)
  private
    FTable: TDBTable;
    FValues: array of Variant; // Use Variant to hold data
    function GetValue(const AFieldName: string): Variant;
    procedure SetValue(const AFieldName: string; const AValue: Variant);
    function GetValueByIndex(AIndex: Integer): Variant;
    procedure SetValueByIndex(AIndex: Integer; const AValue: Variant);
  public
    constructor Create(ATable: TDBTable);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Values[const AFieldName: string]: Variant read GetValue write SetValue;
    property ValuesByIndex[AIndex: Integer]: Variant read GetValueByIndex write SetValueByIndex;
  end;

  TDBTable = class(TObject)
  private
    FDatabase: TDatabase;
    FTableName: string;
    FFieldDefs: TObjectList; // List of TDBFieldDef
    FRecords: TObjectList;   // List of TDBRecord
    FIndexes: TObjectList;   // List of TDBIndex
    FAutoIncCounter: Int64;
    function GetFieldDef(const AFieldName: string): TDBFieldDef;
    function GetFieldDefIndex(const AFieldName: string): Integer;
    function GetIndex(const AIndexName: string): TDBIndex;
    function GetIndexesForField(AFieldDef: TDBFieldDef): TList;
  public
    constructor Create(ADatabase: TDatabase; const ATableName: string);
    destructor Destroy; override;
    procedure AddField(const AName: string; ADataType: TDBFieldType; ASize: Integer = 0);
    function AddNew: TDBRecord;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function CreateIndex(const AIndexName: string; const AFieldName: string): TDBIndex;
    function Find(const AIndexName: string; const AValue: Variant): TObjectList;
    function RecordCount: Integer;
    function NextAutoIncValue: Int64;
    property TableName: string read FTableName;
    property FieldDefs: TObjectList read FFieldDefs;
    property Records: TObjectList read FRecords;
    property Database: TDatabase read FDatabase;
  end;

  TIndexNode = class(TObject)
  public
    Key: Variant;
    Records: TObjectList;
    constructor Create(AKey: Variant);
    destructor Destroy; override;
  end;

  TDBIndex = class(TObject)
  private
    FName: string;
    FTable: TDBTable;
    FFieldDef: TDBFieldDef;
    FIndexData: TObjectList; // Sorted list of TIndexNode
    function FindNode(AValue: Variant; var AIndex: Integer): TIndexNode;
  public
    constructor Create(ATable: TDBTable; const AIndexName, AFieldName: string);
    destructor Destroy; override;
    procedure AddRecord(ARecord: TDBRecord);
    procedure RemoveRecord(ARecord: TDBRecord);
    function Find(AValue: Variant): TObjectList;
    property Name: string read FName;
  end;

  TDBJoinedRecord = class(TObject)
  private
    FRecordA: TDBRecord;
    FRecordB: TDBRecord;
  public
    constructor Create(ARecordA, ARecordB: TDBRecord);
    function GetValue(const AFieldName: string): Variant;
    property Values[const AFieldName: string]: Variant read GetValue;
  end;

  TDBJoinResult = class(TObject)
  private
    FRecords: TObjectList; // List of TDBJoinedRecord
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ARecord: TDBJoinedRecord);
    function Count: Integer;
    function GetRecord(AIndex: Integer): TDBJoinedRecord;
    property Records[AIndex: Integer]: TDBJoinedRecord read GetRecord; default;
  end;

  TDatabase = class(TObject)
  private
    FDBFileName: string;
    FJournalWriter: TJournalWriter;
    FInTransaction: Boolean;
    FCurrentTxID: Int64;
    FTables: TObjectList; // List of TDBTable
    FLock: TMultiReadExclusiveWriteSynchronizer;
    procedure InternalLoadFromFile(const AFileName: string; const AKey: TBytes);
    procedure RecoverFromJournal;
  public
    function GetTable(const ATableName: string): TDBTable;
    constructor Create;
    destructor Destroy; override;
    function CreateTable(const ATableName: string): TDBTable;
    procedure DropTable(const ATableName: string);
    procedure SaveToFile(const AFileName: string; const AKey: TBytes);
    procedure LoadFromFile(const AFileName: string; const AKey: TBytes);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function Join(
      TableA: TDBTable; FieldA: string;
      TableB: TDBTable; FieldB: string
    ): TDBJoinResult;
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    procedure Checkpoint;
    property Tables: TObjectList read FTables;
    property Lock: TMultiReadExclusiveWriteSynchronizer read FLock;
    property InTransaction: Boolean read FInTransaction;
  end;


implementation

uses
  uStreamUtils, uCrypto;

{ TDBFieldDef }

constructor TDBFieldDef.Create(const AName: string; ADataType: TDBFieldType; ASize: Integer);
begin
  inherited Create;
  FName := AName;
  FDataType := ADataType;
  FSize := ASize;
end;

{ TDBRecord }

constructor TDBRecord.Create(ATable: TDBTable);
begin
  inherited Create;
  FTable := ATable;
  SetLength(FValues, FTable.FieldDefs.Count);
end;

destructor TDBRecord.Destroy;
begin
  Finalize(FValues);
  inherited Destroy;
end;

function TDBRecord.GetValue(const AFieldName: string): Variant;
var
  Index: Integer;
begin
  Index := FTable.GetFieldDefIndex(AFieldName);
  Result := GetValueByIndex(Index);
end;

procedure TDBRecord.SetValue(const AFieldName: string; const AValue: Variant);
var
  Index: Integer;
begin
  Index := FTable.GetFieldDefIndex(AFieldName);
  SetValueByIndex(Index, AValue);
end;

function TDBRecord.GetValueByIndex(AIndex: Integer): Variant;
begin
  FTable.Database.Lock.BeginRead;
  try
    if (AIndex >= 0) and (AIndex < Length(FValues)) then
      Result := FValues[AIndex]
    else
      raise Exception.CreateFmt('Field index [%d] out of bounds.', [AIndex]);
  finally
    FTable.Database.Lock.EndRead;
  end;
end;

procedure TDBRecord.SetValueByIndex(AIndex: Integer; const AValue: Variant);
var
  OldValue: Variant;
  IndexesToUpdate: TList;
  FieldDef: TDBFieldDef;
  i: Integer;
begin
  if not FTable.Database.InTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FTable.Database.Lock.BeginWrite;
  try
    if (AIndex >= 0) and (AIndex < Length(FValues)) then
    begin
      FieldDef := TDBFieldDef(FTable.FieldDefs[AIndex]);
      IndexesToUpdate := FTable.GetIndexesForField(FieldDef);
      try
        if IndexesToUpdate.Count > 0 then
        begin
          OldValue := FValues[AIndex];
          for i := 0 to IndexesToUpdate.Count - 1 do
            TDBIndex(IndexesToUpdate[i]).RemoveRecord(Self);
        end;
        FValues[AIndex] := AValue;
        if IndexesToUpdate.Count > 0 then
        begin
          for i := 0 to IndexesToUpdate.Count - 1 do
            TDBIndex(IndexesToUpdate[i]).AddRecord(Self);
        end;
      finally
        IndexesToUpdate.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Field index [%d] out of bounds.', [AIndex]);
  finally
    FTable.Database.Lock.EndWrite;
  end;
end;

procedure TDBRecord.SaveToStream(Stream: TStream);
var
  i: Integer;
begin
  for i := 0 to High(FValues) do
    WriteVariant(Stream, FValues[i]);
end;

procedure TDBRecord.LoadFromStream(Stream: TStream);
var
  i: Integer;
begin
  for i := 0 to High(FValues) do
    FValues[i] := ReadVariant(Stream);
end;

{ TDBTable }

constructor TDBTable.Create(ADatabase: TDatabase; const ATableName: string);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTableName := ATableName;
  FFieldDefs := TObjectList.Create(True);
  FRecords := TObjectList.Create(True);
  FIndexes := TObjectList.Create(True);
  FAutoIncCounter := 0;
end;

destructor TDBTable.Destroy;
begin
  FIndexes.Free;
  FRecords.Free;
  FFieldDefs.Free;
  inherited Destroy;
end;

procedure TDBTable.AddField(const AName: string; ADataType: TDBFieldType; ASize: Integer);
begin
  if not FDatabase.InTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FDatabase.FLock.BeginWrite;
  try
    if GetFieldDefIndex(AName) > -1 then
      raise Exception.CreateFmt('Field "%s" already exists in table "%s".', [AName, FTableName]);
    FFieldDefs.Add(TDBFieldDef.Create(AName, ADataType, ASize));
    // TODO: Write to journal
  finally
    FDatabase.FLock.EndWrite;
  end;
end;

function TDBTable.AddNew: TDBRecord;
var
  i: Integer;
  Header: TLogEntryHeader;
  MS: TMemoryStream;
  Payload: TBytes;
begin
  if not FDatabase.InTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FDatabase.FLock.BeginWrite;
  try
    Result := TDBRecord.Create(Self);
    FRecords.Add(Result);
    for i := 0 to FIndexes.Count - 1 do
      TDBIndex(FIndexes[i]).AddRecord(Result);

    // Write to journal
    with FDatabase do
    begin
      MS := TMemoryStream.Create;
      try
        WriteString(MS, FTableName);
        Result.SaveToStream(MS);
        SetLength(Payload, MS.Size);
        MS.Position := 0;
        MS.ReadBuffer(Payload[0], MS.Size);
      finally
        MS.Free;
      end;
      Header.TxID := FCurrentTxID;
      Header.OpType := opAddRecord;
      FJournalWriter.WriteEntry(Header, Payload);
    end;
  finally
    FDatabase.FLock.EndWrite;
  end;
end;

procedure TDBTable.SaveToStream(Stream: TStream);
var
  i: Integer;
  Field: TDBFieldDef;
  Rec: TDBRecord;
  Index: TDBIndex;
begin
  WriteString(Stream, FTableName);
  WriteInt64(Stream, FAutoIncCounter);
  WriteInteger(Stream, FFieldDefs.Count);
  for i := 0 to FFieldDefs.Count - 1 do
  begin
    Field := TDBFieldDef(FFieldDefs[i]);
    WriteString(Stream, Field.Name);
    WriteByte(Stream, Byte(Field.DataType));
    WriteInteger(Stream, Field.Size);
  end;
  WriteInteger(Stream, FIndexes.Count);
  for i := 0 to FIndexes.Count - 1 do
  begin
    Index := TDBIndex(FIndexes[i]);
    WriteString(Stream, Index.Name);
    WriteString(Stream, Index.FFieldDef.Name);
  end;
  WriteInteger(Stream, FRecords.Count);
  for i := 0 to FRecords.Count - 1 do
  begin
    Rec := TDBRecord(FRecords[i]);
    Rec.SaveToStream(Stream);
  end;
end;

procedure TDBTable.LoadFromStream(Stream: TStream);
var
  i, FieldCount, RecCount, IndexCount: Integer;
  FieldName, IndexName: string;
  FieldType: TDBFieldType;
  FieldSize: Integer;
  NewRec: TDBRecord;
  TempIndexDefs: TStringList;
begin
  FTableName := ReadString(Stream);
  FAutoIncCounter := ReadInt64(Stream);
  FieldCount := ReadInteger(Stream);
  FFieldDefs.Clear;
  for i := 0 to FieldCount - 1 do
  begin
    FieldName := ReadString(Stream);
    FieldType := TDBFieldType(ReadByte(Stream));
    FieldSize := ReadInteger(Stream);
    Self.AddField(FieldName, FieldType, FieldSize);
  end;
  IndexCount := ReadInteger(Stream);
  TempIndexDefs := TStringList.Create;
  try
    for i := 0 to IndexCount - 1 do
    begin
      TempIndexDefs.Add(ReadString(Stream));
      TempIndexDefs.Add(ReadString(Stream));
    end;
    RecCount := ReadInteger(Stream);
    FRecords.Clear;
    for i := 0 to RecCount - 1 do
    begin
      NewRec := TDBRecord.Create(Self);
      NewRec.LoadFromStream(Stream);
      FRecords.Add(NewRec);
    end;
    for i := 0 to (TempIndexDefs.Count div 2) - 1 do
    begin
      Self.CreateIndex(TempIndexDefs[i*2], TempIndexDefs[i*2+1]);
    end;
  finally
    TempIndexDefs.Free;
  end;
end;

function TDBTable.CreateIndex(const AIndexName: string; const AFieldName: string): TDBIndex;
var
  i: Integer;
begin
  if not FDatabase.InTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FDatabase.FLock.BeginWrite;
  try
    if GetIndex(AIndexName) <> nil then
      raise Exception.CreateFmt('Index "%s" already exists.', [AIndexName]);
    Result := TDBIndex.Create(Self, AIndexName, AFieldName);
    FIndexes.Add(Result);
    for i := 0 to FRecords.Count - 1 do
      Result.AddRecord(TDBRecord(FRecords[i]));
    // TODO: Write to journal
  finally
    FDatabase.FLock.EndWrite;
  end;
end;

function TDBTable.Find(const AIndexName: string; const AValue: Variant): TObjectList;
var
  Index: TDBIndex;
begin
  FDatabase.FLock.BeginRead;
  try
    Index := GetIndex(AIndexName);
    if Index <> nil then
      Result := Index.Find(AValue)
    else
      Result := nil;
  finally
    FDatabase.FLock.EndRead;
  end;
end;

function TDBTable.GetFieldDefIndex(const AFieldName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FFieldDefs.Count - 1 do
  begin
    if AnsiCompareText(TDBFieldDef(FFieldDefs[i]).Name, AFieldName) = 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TDBTable.GetFieldDef(const AFieldName: string): TDBFieldDef;
var
  Index: Integer;
begin
  Index := GetFieldDefIndex(AFieldName);
  if Index > -1 then
    Result := TDBFieldDef(FFieldDefs[Index])
  else
    Result := nil;
end;

function TDBTable.GetIndex(const AIndexName: string): TDBIndex;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FIndexes.Count - 1 do
  begin
    if AnsiCompareText(TDBIndex(FIndexes[i]).Name, AIndexName) = 0 then
    begin
      Result := TDBIndex(FIndexes[i]);
      Break;
    end;
  end;
end;

function TDBTable.GetIndexesForField(AFieldDef: TDBFieldDef): TList;
var
  i: Integer;
  Index: TDBIndex;
begin
  Result := TList.Create;
  for i := 0 to FIndexes.Count - 1 do
  begin
    Index := TDBIndex(FIndexes[i]);
    if Index.FFieldDef = AFieldDef then
      Result.Add(Index);
  end;
end;

function TDBTable.RecordCount: Integer;
begin
  FDatabase.FLock.BeginRead;
  try
    Result := FRecords.Count;
  finally
    FDatabase.FLock.EndRead;
  end;
end;

function TDBTable.NextAutoIncValue: Int64;
begin
  FDatabase.FLock.BeginWrite;
  try
    Inc(FAutoIncCounter);
    Result := FAutoIncCounter;
  finally
    FDatabase.FLock.EndWrite;
  end;
end;

{ TIndexNode }

constructor TIndexNode.Create(AKey: Variant);
begin
  inherited Create;
  Key := AKey;
  Records := TObjectList.Create(False);
end;

destructor TIndexNode.Destroy;
begin
  Records.Free;
  inherited Destroy;
end;

{ TDBIndex }

constructor TDBIndex.Create(ATable: TDBTable; const AIndexName, AFieldName: string);
begin
  inherited Create;
  FTable := ATable;
  FName := AIndexName;
  FFieldDef := FTable.GetFieldDef(AFieldName);
  if FFieldDef = nil then
    raise Exception.CreateFmt('Field "%s" not found for index "%s".', [AFieldName, AIndexName]);
  FIndexData := TObjectList.Create(True);
end;

destructor TDBIndex.Destroy;
begin
  FIndexData.Free;
  inherited Destroy;
end;

function TDBIndex.FindNode(AValue: Variant; var AIndex: Integer): TIndexNode;
var
  L, H, C: Integer;
  Node: TIndexNode;
begin
  Result := nil;
  L := 0;
  H := FIndexData.Count - 1;
  AIndex := 0;
  while L <= H do
  begin
    C := (L + H) div 2;
    Node := TIndexNode(FIndexData[C]);
    if Node.Key = AValue then
    begin
      Result := Node;
      AIndex := C;
      Exit;
    end;
    if Node.Key < AValue then
      L := C + 1
    else
      H := C - 1;
  end;
  AIndex := L;
end;

procedure TDBIndex.AddRecord(ARecord: TDBRecord);
var
  Value: Variant;
  Node: TIndexNode;
  Index: Integer;
begin
  Value := ARecord.Values[FFieldDef.Name];
  Node := FindNode(Value, Index);
  if Node = nil then
  begin
    Node := TIndexNode.Create(Value);
    FIndexData.Insert(Index, Node);
  end;
  Node.Records.Add(ARecord);
end;

procedure TDBIndex.RemoveRecord(ARecord: TDBRecord);
var
  Value: Variant;
  Node: TIndexNode;
  Index: Integer;
begin
  Value := ARecord.Values[FFieldDef.Name];
  Node := FindNode(Value, Index);
  if Node <> nil then
  begin
    Node.Records.Remove(ARecord);
    if Node.Records.Count = 0 then
      FIndexData.Delete(Index);
  end;
end;

function TDBIndex.Find(AValue: Variant): TObjectList;
var
  Node: TIndexNode;
  Index: Integer;
begin
  Node := FindNode(AValue, Index);
  if Node <> nil then
    Result := Node.Records
  else
    Result := nil;
end;

{ TDBJoinedRecord }

constructor TDBJoinedRecord.Create(ARecordA, ARecordB: TDBRecord);
begin
  inherited Create;
  FRecordA := ARecordA;
  FRecordB := ARecordB;
end;

function TDBJoinedRecord.GetValue(const AFieldName: string): Variant;
begin
  if FRecordA.FTable.GetFieldDefIndex(AFieldName) > -1 then
  begin
    Result := FRecordA.Values[AFieldName];
    Exit;
  end;
  if FRecordB.FTable.GetFieldDefIndex(AFieldName) > -1 then
  begin
    Result := FRecordB.Values[AFieldName];
    Exit;
  end;
  Result := Null;
end;

{ TDBJoinResult }

constructor TDBJoinResult.Create;
begin
  inherited Create;
  FRecords := TObjectList.Create(True);
end;

destructor TDBJoinResult.Destroy;
begin
  FRecords.Free;
  inherited Destroy;
end;

procedure TDBJoinResult.Add(ARecord: TDBJoinedRecord);
begin
  FRecords.Add(ARecord);
end;

function TDBJoinResult.Count: Integer;
begin
  Result := FRecords.Count;
end;

function TDBJoinResult.GetRecord(AIndex: Integer): TDBJoinedRecord;
begin
  Result := TDBJoinedRecord(FRecords[AIndex]);
end;

{ TDatabase }

constructor TDatabase.Create;
begin
  inherited Create;
  FTables := TObjectList.Create(True);
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FInTransaction := False;
  FJournalWriter := nil;
  FCurrentTxID := 0;
  FDBFileName := '';
end;

destructor TDatabase.Destroy;
begin
  FLock.Free;
  FTables.Free;
  inherited Destroy;
end;

function TDatabase.CreateTable(const ATableName: string): TDBTable;
var
  Header: TLogEntryHeader;
  MS: TMemoryStream;
  Payload: TBytes;
begin
  if not FInTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FLock.BeginWrite;
  try
    if GetTable(ATableName) <> nil then
      raise Exception.CreateFmt('Table "%s" already exists.', [ATableName]);
    Result := TDBTable.Create(Self, ATableName);
    FTables.Add(Result);

    // Write to journal
    MS := TMemoryStream.Create;
    try
      WriteString(MS, ATableName);
      SetLength(Payload, MS.Size);
      MS.Position := 0;
      MS.ReadBuffer(Payload[0], MS.Size);
    finally
      MS.Free;
    end;

    Header.TxID := FCurrentTxID;
    Header.OpType := opCreateTable;
    FJournalWriter.WriteEntry(Header, Payload);

  finally
    FLock.EndWrite;
  end;
end;

procedure TDatabase.DropTable(const ATableName: string);
var
  i: Integer;
  Table: TDBTable;
begin
  if not FInTransaction then
    raise Exception.Create('Operation must be within a transaction.');

  FLock.BeginWrite;
  try
    for i := 0 to FTables.Count - 1 do
    begin
      Table := TDBTable(FTables[i]);
      if AnsiCompareText(Table.TableName, ATableName) = 0 then
      begin
        FTables.Delete(i);
        Break;
      end;
    end;
    // TODO: Write to journal
  finally
    FLock.EndWrite;
  end;
end;

function TDatabase.GetTable(const ATableName: string): TDBTable;
var
  i: Integer;
begin
  FLock.BeginRead;
  try
    Result := nil;
    for i := 0 to FTables.Count - 1 do
    begin
      if AnsiCompareText(TDBTable(FTables[i]).TableName, ATableName) = 0 then
      begin
        Result := TDBTable(FTables[i]);
        Break;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TDatabase.InternalSaveToFile(const AFileName: string; const AKey: TBytes);
var
  MS: TMemoryStream;
  FS: TFileStream;
  Buffer, EncryptedBuffer: TBytes;
begin
  FLock.BeginRead;
  try
    MS := TMemoryStream.Create;
    try
      Self.SaveToStream(MS);
      MS.Position := 0;
      SetLength(Buffer, MS.Size);
      MS.ReadBuffer(Buffer[0], MS.Size);
      EncryptedBuffer := Encrypt(Buffer);
      FS := TFileStream.Create(AFileName, fmCreate);
      try
        FS.WriteBuffer(EncryptedBuffer[0], Length(EncryptedBuffer));
      finally
        FS.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TDatabase.SaveToFile(const AFileName: string; const AKey: TBytes);
begin
  FLock.BeginWrite;
  try
    FDBFileName := AFileName;
    Self.Checkpoint;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDatabase.LoadFromFile(const AFileName: string; const AKey: TBytes);
begin
  FLock.BeginWrite;
  try
    FDBFileName := AFileName;
    InternalLoadFromFile(AFileName, AKey);

    if FileExists(FDBFileName + '-wal') then
    begin
      RecoverFromJournal;
      Checkpoint;
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDatabase.RecoverFromJournal;
var
  Reader: TJournalReader;
  Header: TLogEntryHeader;
  Payload: TBytes;
  CommittedTxIDs: TStringList;
  MS: TMemoryStream;
  TableName: string;
  Table: TDBTable;
  NewRec: TDBRecord;
begin
  Writeln('Recovery from journal...');
  Reader := TJournalReader.Create(FDBFileName + '-wal');
  CommittedTxIDs := TStringList.Create;
  try
    // First pass: find all committed transaction IDs
    while Reader.ReadNextEntry(Header, Payload) do
    begin
      if Header.OpType = opCommitTx then
        CommittedTxIDs.Add(IntToStr(Header.TxID));
    end;

    // Second pass: replay committed transactions
    Reader.Reset;
    while Reader.ReadNextEntry(Header, Payload) do
    begin
      if CommittedTxIDs.IndexOf(IntToStr(Header.TxID)) > -1 then
      begin
        case Header.OpType of
          opCreateTable:
            begin
              MS := TMemoryStream.Create;
              try
                MS.WriteBuffer(Payload[0], Length(Payload));
                MS.Position := 0;
                TableName := ReadString(MS);
                Self.CreateTable(TableName);
              finally
                MS.Free;
              end;
            end;
          opAddRecord:
            begin
              MS := TMemoryStream.Create;
              try
                MS.WriteBuffer(Payload[0], Length(Payload));
                MS.Position := 0;
                TableName := ReadString(MS);
                Table := Self.GetTable(TableName);
                if Table <> nil then
                begin
                  NewRec := Table.AddNew;
                  NewRec.LoadFromStream(MS);
                end;
              finally
                MS.Free;
              end;
            end;
        end;
      end;
    end;
  finally
    CommittedTxIDs.Free;
    Reader.Free;
  end;
end;

procedure TDatabase.Checkpoint;
begin
  FLock.BeginWrite;
  try
    InternalSaveToFile(FDBFileName, []); // Key is empty for now
    if FileExists(FDBFileName + '-wal') then
      DeleteFile(FDBFileName + '-wal');
  finally
    FLock.EndWrite;
  end;
end;

const
  FILE_HEADER = 'FPDB';
  FILE_VERSION = 1;

procedure TDatabase.SaveToStream(Stream: TStream);
var
  i: Integer;
  Table: TDBTable;
begin
  FLock.BeginRead;
  try
    WriteString(Stream, FILE_HEADER);
    WriteInteger(Stream, FILE_VERSION);
    WriteInteger(Stream, FTables.Count);
    for i := 0 to FTables.Count - 1 do
    begin
      Table := TDBTable(FTables[i]);
      Table.SaveToStream(Stream);
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TDatabase.LoadFromStream(Stream: TStream);
var
  i, TableCount, Version: Integer;
  Header: string;
  NewTable: TDBTable;
begin
  FLock.BeginWrite;
  try
    Header := ReadString(Stream);
    if Header <> FILE_HEADER then
      raise Exception.Create('Invalid database file format.');
    Version := ReadInteger(Stream);
    FTables.Clear;
    TableCount := ReadInteger(Stream);
    for i := 0 to TableCount - 1 do
    begin
      NewTable := TDBTable.Create(Self, '');
      NewTable.LoadFromStream(Stream);
      FTables.Add(NewTable);
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TDatabase.Join(
  TableA: TDBTable; FieldA: string;
  TableB: TDBTable; FieldB: string
): TDBJoinResult;
var
  i, j: Integer;
  RecA, RecB: TDBRecord;
  IndexA: TDBIndex;
  Value: Variant;
  MatchList: TObjectList;
begin
  Result := TDBJoinResult.Create;
  FLock.BeginRead;
  try
    IndexA := TableA.GetIndex('idx_' + FieldA);
    if IndexA <> nil then
    begin
      for i := 0 to TableB.RecordCount - 1 do
      begin
        RecB := TDBRecord(TableB.Records[i]);
        Value := RecB.Values[FieldB];
        MatchList := IndexA.Find(Value);
        if MatchList <> nil then
        begin
          for j := 0 to MatchList.Count - 1 do
          begin
            RecA := TDBRecord(MatchList[j]);
            Result.Add(TDBJoinedRecord.Create(RecA, RecB));
          end;
        end;
      end;
    end
    else
    begin
      for i := 0 to TableA.RecordCount - 1 do
      begin
        RecA := TDBRecord(TableA.Records[i]);
        for j := 0 to TableB.RecordCount - 1 do
        begin
          RecB := TDBRecord(TableB.Records[j]);
          if RecA.Values[FieldA] = RecB.Values[FieldB] then
          begin
            Result.Add(TDBJoinedRecord.Create(RecA, RecB));
          end;
        end;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TDatabase.BeginTransaction;
var
  Header: TLogEntryHeader;
begin
  FLock.BeginWrite;
  try
    if FInTransaction then
      raise Exception.Create('Transaction already active.');
    Inc(FCurrentTxID);
    FJournalWriter := TJournalWriter.Create(FDBFileName + '-wal');
    Header.TxID := FCurrentTxID;
    Header.OpType := opBeginTx;
    FJournalWriter.WriteEntry(Header, []);
    FInTransaction := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDatabase.Commit;
var
  Header: TLogEntryHeader;
begin
  FLock.BeginWrite;
  try
    if not FInTransaction then
      raise Exception.Create('No active transaction to commit.');
    Header.TxID := FCurrentTxID;
    Header.OpType := opCommitTx;
    FJournalWriter.WriteEntry(Header, []);
    FJournalWriter.Free;
    FJournalWriter := nil;
    FInTransaction := False;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDatabase.Rollback;
begin
  FLock.BeginWrite;
  try
    if not FInTransaction then
      raise Exception.Create('No active transaction to roll back.');

    FJournalWriter.Free;
    FJournalWriter := nil;
    FInTransaction := False;

    FTables.Clear;
    InternalLoadFromFile(FDBFileName, []);
  finally
    FLock.EndWrite;
  end;
end;

end.
