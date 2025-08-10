unit symboltable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast;

type
  TSymbolType = (stVariable, stType, stProcedure, stFunction, stConstant, stProgram, stUnit, stClass, stField);

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: string;
    SymbolType: TSymbolType;
    DeclNode: TDeclarationNode; // Pointer to the declaration in the AST
    ScopeLevel: Integer;
  end;

  TScope = class
  private
    FSymbols: TStringList; // Stores names for quick lookup
    FRecords: TList;     // Stores TSymbol records
    FOuterScope: TScope;
    FScopeName: string;
    FScopeLevel: Integer;
  public
    constructor Create(AScopeName: string; AScopeLevel: Integer; AOuterScope: TScope);
    destructor Destroy; override;
    function Define(ASymbol: TSymbol): Boolean;
    function Lookup(AName: string; ACurrentScopeOnly: Boolean = False): PSymbol;
    property OuterScope: TScope read FOuterScope;
    property ScopeName: string read FScopeName;
    property ScopeLevel: Integer read FScopeLevel;
  end;

  TSymbolTable = class
  private
    FCurrentScope: TScope;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnterScope(AName: string);
    procedure LeaveScope;
    function Define(ASymbol: TSymbol): Boolean;
    function Lookup(AName: string): PSymbol;
    function GetCurrentScope: TScope;
  end;

implementation

{ TScope }

constructor TScope.Create(AScopeName: string; AScopeLevel: Integer; AOuterScope: TScope);
begin
  inherited Create;
  FScopeName := AScopeName;
  FScopeLevel := AScopeLevel;
  FOuterScope := AOuterScope;
  FSymbols := TStringList.Create;
  FSymbols.CaseSensitive := False;
  FRecords := TList.Create;
end;

destructor TScope.Destroy;
var
  i: Integer;
begin
  FSymbols.Free;
  for i := 0 to FRecords.Count - 1 do
    Dispose(PSymbol(FRecords[i]));
  FRecords.Free;
  inherited Destroy;
end;

function TScope.Define(ASymbol: TSymbol): Boolean;
var
  newSymbol: PSymbol;
begin
  if FSymbols.IndexOf(ASymbol.Name) <> -1 then
  begin
    Result := False; // Symbol already defined in this scope
    Exit;
  end;

  newSymbol := New(PSymbol);
  newSymbol^ := ASymbol;
  FSymbols.Add(ASymbol.Name);
  FRecords.Add(newSymbol);
  Result := True;
end;

function TScope.Lookup(AName: string; ACurrentScopeOnly: Boolean): PSymbol;
var
  index: Integer;
  currentScope: TScope;
begin
  Result := nil;
  currentScope := Self;
  while Assigned(currentScope) do
  begin
    index := currentScope.FSymbols.IndexOf(AName);
    if index <> -1 then
    begin
      Result := PSymbol(currentScope.FRecords[index]);
      Exit;
    end;
    if ACurrentScopeOnly then
      Break;
    currentScope := currentScope.FOuterScope;
  end;
end;


{ TSymbolTable }

constructor TSymbolTable.Create;
begin
  inherited Create;
  FCurrentScope := nil;
  EnterScope('global'); // Global scope
end;

destructor TSymbolTable.Destroy;
begin
  while Assigned(FCurrentScope) do
  begin
    LeaveScope;
  end;
  inherited Destroy;
end;

procedure TSymbolTable.EnterScope(AName: string);
var
  newLevel: Integer;
begin
  if FCurrentScope = nil then
    newLevel := 0
  else
    newLevel := FCurrentScope.ScopeLevel + 1;
  FCurrentScope := TScope.Create(AName, newLevel, FCurrentScope);
end;

procedure TSymbolTable.LeaveScope;
var
  oldScope: TScope;
begin
  oldScope := FCurrentScope;
  FCurrentScope := FCurrentScope.OuterScope;
  oldScope.Free;
end;

function TSymbolTable.Define(ASymbol: TSymbol): Boolean;
begin
  ASymbol.ScopeLevel := FCurrentScope.ScopeLevel;
  Result := FCurrentScope.Define(ASymbol);
end;

function TSymbolTable.Lookup(AName: string): PSymbol;
begin
  Result := FCurrentScope.Lookup(AName, False);
end;

function TSymbolTable.GetCurrentScope: TScope;
begin
    Result := FCurrentScope;
end;

end.
