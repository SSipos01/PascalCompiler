unit symboltable;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ast;

type
  TSymbol = class;
  TVarSymbol = class;
  TClassSymbol = class;
  TMethodSymbol = class;

  // --- Symbol Definitions ---

  TSymbol = class
  public
    Name: string;
    SymbolType: TTypeNode;
    constructor Create(const AName: string; ASymbolType: TTypeNode);
    destructor Destroy; override;
  end;

  TVarSymbol = class(TSymbol)
  public
    // Can be extended with info like memory address
  end;

  TClassSymbol = class(TSymbol)
  public
    ParentClassSymbol: TClassSymbol;
    MemberSymbols: TStringList; // Keyed list of TSymbol
    constructor Create(const AName: string; AParent: TClassSymbol);
    destructor Destroy; override;
    procedure AddMember(AMemberSymbol: TSymbol);
    function LookupMember(const AName: string): TSymbol;
  end;

  TMethodSymbol = class(TSymbol)
  public
    Parameters: TList; // of TVarSymbol
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  // --- Symbol Table Definition ---

  TSymbolTable = class
  private
    FScopeStack: TList; // Stack of TStringList (scopes)
    procedure InitBuiltins;
    function GetCurrentScope: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnterScope;
    procedure LeaveScope;
    procedure Insert(ASymbol: TSymbol);
    function Lookup(const AName: string; ACurrentScopeOnly: boolean = False): TSymbol;
  end;

implementation

{ TSymbol }
constructor TSymbol.Create(const AName: string; ASymbolType: TTypeNode);
begin
  inherited Create;
  Name := AName;
  SymbolType := ASymbolType;
end;

destructor TSymbol.Destroy;
begin
  if Assigned(SymbolType) then
    SymbolType.Free;
  inherited Destroy;
end;

{ TClassSymbol }
constructor TClassSymbol.Create(const AName: string; AParent: TClassSymbol);
begin
  inherited Create(AName, TTypeNode.Create(AName)); // A class is its own type
  ParentClassSymbol := AParent;
  MemberSymbols := TStringList.Create;
  MemberSymbols.OwnsObjects := True; // The list will free the symbols
end;

destructor TClassSymbol.Destroy;
begin
  MemberSymbols.Free;
  inherited Destroy;
end;

procedure TClassSymbol.AddMember(AMemberSymbol: TSymbol);
begin
  MemberSymbols.AddObject(AMemberSymbol.Name, AMemberSymbol);
end;

function TClassSymbol.LookupMember(const AName: string): TSymbol;
var
  index: Integer;
begin
  index := MemberSymbols.IndexOf(AName);
  if index > -1 then
    Result := TSymbol(MemberSymbols.Objects[index])
  else if Assigned(ParentClassSymbol) then
    Result := ParentClassSymbol.LookupMember(AName)
  else
    Result := nil;
end;

{ TMethodSymbol }
constructor TMethodSymbol.Create(const AName: string);
begin
  inherited Create(AName, nil); // Type decided by return type
  Parameters := TList.Create;
end;

destructor TMethodSymbol.Destroy;
var
  i: Integer;
begin
  for i := 0 to Parameters.Count - 1 do
    TSymbol(Parameters[i]).Free;
  Parameters.Free;
  inherited Destroy;
end;

{ TSymbolTable }
constructor TSymbolTable.Create;
begin
  inherited Create;
  FScopeStack := TList.Create;
  InitBuiltins;
end;

destructor TSymbolTable.Destroy;
var
  i: integer;
begin
  while FScopeStack.Count > 0 do
    LeaveScope;
  FScopeStack.Free;
  inherited Destroy;
end;

procedure TSymbolTable.InitBuiltins;
begin
  EnterScope; // Global scope
  Insert(TSymbol.Create('integer', TTypeNode.Create('integer')));
  Insert(TSymbol.Create('string', TTypeNode.Create('string')));
  Insert(TSymbol.Create('boolean', TTypeNode.Create('boolean')));
  Insert(TSymbol.Create('char', TTypeNode.Create('char')));
  Insert(TSymbol.Create('real', TTypeNode.Create('real')));
  Insert(TSymbol.Create('byte', TTypeNode.Create('byte')));
  Insert(TSymbol.Create('longint', TTypeNode.Create('longint')));
  Insert(TSymbol.Create('currency', TTypeNode.Create('currency')));
  // Also add TObject as a built-in class
  Insert(TClassSymbol.Create('TObject', nil));
end;

function TSymbolTable.GetCurrentScope: TStringList;
begin
  if FScopeStack.Count = 0 then
    Result := nil
  else
    Result := TStringList(FScopeStack.Last);
end;

procedure TSymbolTable.EnterScope;
var
  scope: TStringList;
begin
  scope := TStringList.Create;
  scope.OwnsObjects := True; // Scope frees its symbols
  FScopeStack.Add(scope);
end;

procedure TSymbolTable.LeaveScope;
var
  scope: TStringList;
begin
  if FScopeStack.Count > 0 then
  begin
    scope := GetCurrentScope;
    FScopeStack.Delete(FScopeStack.Count - 1);
    scope.Free;
  end;
end;

procedure TSymbolTable.Insert(ASymbol: TSymbol);
var
  scope: TStringList;
begin
  scope := GetCurrentScope;
  if Assigned(scope) then
    scope.AddObject(ASymbol.Name, ASymbol)
  else
    raise Exception.Create('Cannot insert symbol into a non-existent scope.');
end;

function TSymbolTable.Lookup(const AName: string; ACurrentScopeOnly: boolean): TSymbol;
var
  i, index: Integer;
  scope: TStringList;
begin
  Result := nil;
  if ACurrentScopeOnly then
  begin
    scope := GetCurrentScope;
    if Assigned(scope) then
    begin
      index := scope.IndexOf(AName);
      if index > -1 then
        Result := TSymbol(scope.Objects[index]);
    end;
  end
  else
  begin
    // Search from current scope outwards
    for i := FScopeStack.Count - 1 downto 0 do
    begin
      scope := TStringList(FScopeStack[i]);
      index := scope.IndexOf(AName);
      if index > -1 then
      begin
        Result := TSymbol(scope.Objects[index]);
        Exit;
      end;
    end;
  end;
end;

end.
