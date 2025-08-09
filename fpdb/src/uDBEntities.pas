unit uDBEntities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, uCoreTypes, SyncObjs;

type
  { Forward declarations }
  TDBFieldDef = class;
  TDBRecord = class;
  TDBTable = class;
  TDatabase = class;

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

  TDBRecord = class(TObject)
  private
    FTable: TDBTable;
    FValues: array of Variant;
  public
    constructor Create(ATable: TDBTable);
    property Values: array of Variant read FValues;
  end;

  TDBTable = class(TObject)
  private
    FDatabase: TDatabase;
    FTableName: string;
    FFieldDefs: TObjectList;
    FRecords: TObjectList;
  public
    constructor Create(ADatabase: TDatabase; const ATableName: string);
    destructor Destroy; override;
    procedure AddField(const AName: string; ADataType: TDBFieldType; ASize: Integer = 0);
    function AddNew: TDBRecord;
    function RecordCount: Integer;
    property TableName: string read FTableName;
    property FieldDefs: TObjectList read FFieldDefs;
    property Records: TObjectList read FRecords;
  end;

  TDatabase = class(TObject)
  private
    FTables: TObjectList;
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateTable(const ATableName: string): TDBTable;
    function GetTable(const ATableName: string): TDBTable;
    property Tables: TObjectList read FTables;
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

{ TDBTable }

constructor TDBTable.Create(ADatabase: TDatabase; const ATableName: string);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTableName := ATableName;
  FFieldDefs := TObjectList.Create(True);
  FRecords := TObjectList.Create(True);
end;

destructor TDBTable.Destroy;
begin
  FRecords.Free;
  FFieldDefs.Free;
  inherited Destroy;
end;

procedure TDBTable.AddField(const AName: string; ADataType: TDBFieldType; ASize: Integer);
begin
  FFieldDefs.Add(TDBFieldDef.Create(AName, ADataType, ASize));
end;

function TDBTable.AddNew: TDBRecord;
begin
  Result := TDBRecord.Create(Self);
  FRecords.Add(Result);
end;

function TDBTable.RecordCount: Integer;
begin
  Result := FRecords.Count;
end;

{ TDatabase }

constructor TDatabase.Create;
begin
  inherited Create;
  FTables := TObjectList.Create(True);
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TDatabase.Destroy;
begin
  FLock.Free;
  FTables.Free;
  inherited Destroy;
end;

function TDatabase.CreateTable(const ATableName: string): TDBTable;
begin
  Result := TDBTable.Create(Self, ATableName);
  FTables.Add(Result);
end;

function TDatabase.GetTable(const ATableName: string): TDBTable;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTables.Count - 1 do
  begin
    if AnsiCompareText(TDBTable(FTables[i]).TableName, ATableName) = 0 then
    begin
      Result := TDBTable(FTables[i]);
      Break;
    end;
  end;
end;

end.
