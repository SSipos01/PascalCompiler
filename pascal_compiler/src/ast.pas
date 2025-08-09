unit ast;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TExpressionNode = class;
  TBlockNode = class;

  TVisibility = (vPrivate, vProtected, vPublic, vPublished);
  TMethodDirective = (mdVirtual, mdOverride, mdOverload, mdStatic);
  TMethodDirectives = set of TMethodDirective;

  TASTNode = class
  public
    destructor Destroy; override;
  end;

  TTypeNode = class(TASTNode)
  public
    TypeName: string;
    constructor Create(const ATypeName: string);
  end;

  TSubrangeNode = class(TTypeNode)
  public
    LowerBound: TExpressionNode;
    UpperBound: TExpressionNode;
    constructor Create(ALower, AUpper: TExpressionNode);
    destructor Destroy; override;
  end;

  TPointerNode = class(TTypeNode)
  public
    BaseType: TTypeNode;
    constructor Create(ABaseType: TTypeNode);
    destructor Destroy; override;
  end;

  TArrayNode = class(TTypeNode)
  public
    IndexTypes: TList; // of TTypeNode
    ElementType: TTypeNode;
    constructor Create(AElementType: TTypeNode);
    destructor Destroy; override;
  end;

  TSetNode = class(TTypeNode)
  public
    BaseType: TTypeNode;
    constructor Create(ABaseType: TTypeNode);
    destructor Destroy; override;
  end;

  TRecordNode = class(TTypeNode)
  public
    Fields: TList; // of TVarDeclNode
    constructor Create;
    destructor Destroy; override;
  end;

  TEnumNode = class(TTypeNode)
  public
    Elements: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TVarDeclNode = class(TASTNode)
  public
    VarName: string;
    VarType: TTypeNode;
    Visibility: TVisibility;
    constructor Create(const AVarName: string; AVarType: TTypeNode; AVisibility: TVisibility = vPublic);
    destructor Destroy; override;
  end;

  TMethodNode = class(TASTNode)
  public
    FMethodName: string;
    Parameters: TList; // of TVarDeclNode
    ReturnType: TTypeNode; // nil for procedure
    Directives: TMethodDirectives;
    Visibility: TVisibility;
    IsConstructor: boolean;
    IsDestructor: boolean;
    constructor Create(const AName: string; AVisibility: TVisibility);
    destructor Destroy; override;
  end;

  TClassNode = class(TASTNode)
  public
    FClassName: string;
    ParentClassName: string;
    Members: TList; // of TVarDeclNode and TMethodNode
    constructor Create(const AName, AParentName: string);
    destructor Destroy; override;
  end;

  TStatementNode = class(TASTNode)
  end;

  TExpressionNode = class(TASTNode)
  end;

  TVarAccessNode = class(TExpressionNode)
  public
    VarName: string;
    constructor Create(const AVarName: string);
  end;

  TIntegerLiteralNode = class(TExpressionNode)
  public
    Value: Integer;
    constructor Create(AValue: Integer);
  end;

  TStringLiteralNode = class(TExpressionNode)
  public
    Value: string;
    constructor Create(const AValue: string);
  end;

  TOperatorType = (opPlus, opMinus, opStar, opSlash,
                   opEqual, opNotEqual, opLess, opLessEqual, opGreater, opGreaterEqual);

  TBinaryOpNode = class(TExpressionNode)
  public
    Left: TExpressionNode;
    Op: TOperatorType;
    Right: TExpressionNode;
    constructor Create(ALeft: TExpressionNode; AOp: TOperatorType; ARight: TExpressionNode);
    destructor Destroy; override;
  end;

  TMemberAccessNode = class(TExpressionNode)
  public
    BaseObject: TExpressionNode;
    MemberName: string;
    constructor Create(ABaseObject: TExpressionNode; const AMemberName: string);
    destructor Destroy; override;
  end;

  TMethodCallNode = class(TExpressionNode)
  public
    MethodExpr: TExpressionNode; // Could be TVarAccessNode or TMemberAccessNode
    Arguments: TList; // of TExpressionNode
    constructor Create(AMethodExpr: TExpressionNode);
    destructor Destroy; override;
  end;

  TAssignmentNode = class(TStatementNode)
  public
    Left: TExpressionNode;
    Right: TExpressionNode;
    constructor Create(ALeft: TExpressionNode; ARight: TExpressionNode);
    destructor Destroy; override;
  end;

  TExpressionStatementNode = class(TStatementNode)
  public
    Expression: TExpressionNode;
    constructor Create(AExpression: TExpressionNode);
    destructor Destroy; override;
  end;

  TIfNode = class(TStatementNode)
  public
    Condition: TExpressionNode;
    ThenStatement: TStatementNode;
    ElseStatement: TStatementNode; // Can be nil
    constructor Create(ACondition: TExpressionNode; AThenStatement, AElseStatement: TStatementNode);
    destructor Destroy; override;
  end;

  TWhileNode = class(TStatementNode)
  public
    Condition: TExpressionNode;
    Body: TStatementNode;
    constructor Create(ACondition: TExpressionNode; ABody: TStatementNode);
    destructor Destroy; override;
  end;

  TRepeatNode = class(TStatementNode)
  public
    Statements: TList; // List of TStatementNode
    UntilCondition: TExpressionNode;
    constructor Create(AUntilCondition: TExpressionNode);
    destructor Destroy; override;
  end;

  TSubroutineDeclNode = class(TASTNode)
  public
    SubroutineName: string;
    Parameters: TList; // of TVarDeclNode
    ReturnType: TTypeNode; // nil for procedure
    Body: TBlockNode;
    constructor Create(const AName: string; ABody: TBlockNode);
    destructor Destroy; override;
  end;

  TCompoundStatementNode = class(TStatementNode)
  private
    FStatements: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddStatement(AStatement: TStatementNode);
    function GetStatement(Index: Integer): TStatementNode;
    function StatementCount: Integer;
    property Statements: TList read FStatements;
  end;

  TBlockNode = class(TASTNode)
  private
    FDeclarations: TList; // List of TVarDeclNode and TClassNode
    FCompoundStatement: TCompoundStatementNode;
  public
    constructor Create(ACompoundStatement: TCompoundStatementNode);
    destructor Destroy; override;
    procedure AddDeclaration(ADeclaration: TASTNode);
    function GetDeclaration(Index: Integer): TASTNode;
    function DeclarationCount: Integer;
    property Declarations: TList read FDeclarations;
    property CompoundStatement: TCompoundStatementNode read FCompoundStatement;
  end;

  TProgramNode = class(TASTNode)
  public
    ProgramName: string;
    Block: TBlockNode;
    constructor Create(const AProgramName: string; ABlock: TBlockNode);
    destructor Destroy; override;
  end;

implementation

destructor TASTNode.Destroy;
begin
  inherited Destroy;
end;

{ TTypeNode }
constructor TTypeNode.Create(const ATypeName: string);
begin
  inherited Create;
  TypeName := ATypeName;
end;

{ TSubrangeNode }
constructor TSubrangeNode.Create(ALower, AUpper: TExpressionNode);
begin
    inherited Create('subrange'); // Use a generic name for now
    LowerBound := ALower;
    UpperBound := AUpper;
end;

destructor TSubrangeNode.Destroy;
begin
    LowerBound.Free;
    UpperBound.Free;
    inherited Destroy;
end;

{ TPointerNode }
constructor TPointerNode.Create(ABaseType: TTypeNode);
begin
    inherited Create('^' + ABaseType.TypeName);
    BaseType := ABaseType;
end;

destructor TPointerNode.Destroy;
begin
    BaseType.Free;
    inherited Destroy;
end;

{ TArrayNode }
constructor TArrayNode.Create(AElementType: TTypeNode);
begin
    inherited Create('array'); // Use a generic name for now
    IndexTypes := TList.Create;
    ElementType := AElementType;
end;

destructor TArrayNode.Destroy;
var
    i: integer;
begin
    for i := 0 to IndexTypes.Count - 1 do
        TTypeNode(IndexTypes[i]).Free;
    IndexTypes.Free;
    ElementType.Free;
    inherited Destroy;
end;

{ TSetNode }
constructor TSetNode.Create(ABaseType: TTypeNode);
begin
    inherited Create('set'); // Use a generic name for now
    BaseType := ABaseType;
end;

destructor TSetNode.Destroy;
begin
    BaseType.Free;
    inherited Destroy;
end;

{ TRecordNode }
constructor TRecordNode.Create;
var
    i: integer;
begin
    inherited Create('record'); // Use a generic name for now
    Fields := TList.Create;
end;

destructor TRecordNode.Destroy;
var
    i: integer;
begin
    for i := 0 to Fields.Count - 1 do
        TVarDeclNode(Fields[i]).Free;
    Fields.Free;
    inherited Destroy;
end;

{ TEnumNode }
constructor TEnumNode.Create;
begin
    inherited Create('enum');
    Elements := TStringList.Create;
end;

destructor TEnumNode.Destroy;
begin
    Elements.Free;
    inherited Destroy;
end;

{ TVarDeclNode }
constructor TVarDeclNode.Create(const AVarName: string; AVarType: TTypeNode; AVisibility: TVisibility);
begin
  inherited Create;
  VarName := AVarName;
  VarType := AVarType;
  Visibility := AVisibility;
end;

destructor TVarDeclNode.Destroy;
begin
  VarType.Free;
  inherited Destroy;
end;

{ TMethodNode }
constructor TMethodNode.Create(const AName: string; AVisibility: TVisibility);
begin
    inherited Create;
    FMethodName := AName;
    Visibility := AVisibility;
    Parameters := TList.Create;
    ReturnType := nil;
    Directives := [];
    IsConstructor := False;
    IsDestructor := False;
end;

destructor TMethodNode.Destroy;
var
    i: integer;
begin
    for i := 0 to Parameters.Count - 1 do
        TVarDeclNode(Parameters[i]).Free;
    Parameters.Free;
    if Assigned(ReturnType) then
        ReturnType.Free;
    inherited Destroy;
end;

{ TClassNode }
constructor TClassNode.Create(const AName, AParentName: string);
begin
    inherited Create;
    FClassName := AName;
    ParentClassName := AParentName;
    Members := TList.Create;
end;

destructor TClassNode.Destroy;
var
    i: integer;
begin
    for i := 0 to Members.Count - 1 do
        TASTNode(Members[i]).Free;
    Members.Free;
    inherited Destroy;
end;

{ TVarAccessNode }
constructor TVarAccessNode.Create(const AVarName: string);
begin
    inherited Create;
    VarName := AVarName;
end;

{ TIntegerLiteralNode }
constructor TIntegerLiteralNode.Create(AValue: Integer);
begin
    inherited Create;
    Value := AValue;
end;

{ TBinaryOpNode }
constructor TBinaryOpNode.Create(ALeft: TExpressionNode; AOp: TOperatorType; ARight: TExpressionNode);
begin
    inherited Create;
    Left := ALeft;
    Op := AOp;
    Right := ARight;
end;

destructor TBinaryOpNode.Destroy;
begin
    Left.Free;
    Right.Free;
    inherited Destroy;
end;

{ TStringLiteralNode }
constructor TStringLiteralNode.Create(const AValue: string);
begin
    inherited Create;
    Value := AValue;
end;

{ TMemberAccessNode }
constructor TMemberAccessNode.Create(ABaseObject: TExpressionNode; const AMemberName: string);
begin
    inherited Create;
    BaseObject := ABaseObject;
    MemberName := AMemberName;
end;

destructor TMemberAccessNode.Destroy;
begin
    BaseObject.Free;
    inherited Destroy;
end;

{ TMethodCallNode }
constructor TMethodCallNode.Create(AMethodExpr: TExpressionNode);
var
    i: integer;
begin
    inherited Create;
    MethodExpr := AMethodExpr;
    Arguments := TList.Create;
end;

destructor TMethodCallNode.Destroy;
var
    i: integer;
begin
    MethodExpr.Free;
    for i := 0 to Arguments.Count - 1 do
        TExpressionNode(Arguments[i]).Free;
    Arguments.Free;
    inherited Destroy;
end;

{ TExpressionStatementNode }
constructor TExpressionStatementNode.Create(AExpression: TExpressionNode);
begin
    inherited Create;
    Expression := AExpression;
end;

destructor TExpressionStatementNode.Destroy;
begin
    Expression.Free;
    inherited Destroy;
end;

{ TIfNode }
constructor TIfNode.Create(ACondition: TExpressionNode; AThenStatement, AElseStatement: TStatementNode);
begin
    inherited Create;
    Condition := ACondition;
    ThenStatement := AThenStatement;
    ElseStatement := AElseStatement;
end;

destructor TIfNode.Destroy;
begin
    Condition.Free;
    ThenStatement.Free;
    if Assigned(ElseStatement) then
        ElseStatement.Free;
    inherited Destroy;
end;

{ TWhileNode }
constructor TWhileNode.Create(ACondition: TExpressionNode; ABody: TStatementNode);
begin
    inherited Create;
    Condition := ACondition;
    Body := ABody;
end;

destructor TWhileNode.Destroy;
begin
    Condition.Free;
    Body.Free;
    inherited Destroy;
end;

{ TRepeatNode }
constructor TRepeatNode.Create(AUntilCondition: TExpressionNode);
begin
    inherited Create;
    Statements := TList.Create;
    UntilCondition := AUntilCondition;
end;

destructor TRepeatNode.Destroy;
var
    i: integer;
begin
    for i := 0 to Statements.Count - 1 do
        TStatementNode(Statements[i]).Free;
    Statements.Free;
    UntilCondition.Free;
    inherited Destroy;
end;

{ TSubroutineDeclNode }
constructor TSubroutineDeclNode.Create(const AName: string; ABody: TBlockNode);
begin
    inherited Create;
    SubroutineName := AName;
    Parameters := TList.Create;
    ReturnType := nil;
    Body := ABody;
end;

destructor TSubroutineDeclNode.Destroy;
var
    i: integer;
begin
    for i := 0 to Parameters.Count - 1 do
        TVarDeclNode(Parameters[i]).Free;
    Parameters.Free;
    if Assigned(ReturnType) then
        ReturnType.Free;
    Body.Free;
    inherited Destroy;
end;

{ TAssignmentNode }
constructor TAssignmentNode.Create(ALeft: TExpressionNode; ARight: TExpressionNode);
begin
    inherited Create;
    Left := ALeft;
    Right := ARight;
end;

destructor TAssignmentNode.Destroy;
begin
    Left.Free;
    Right.Free;
    inherited Destroy;
end;

{ TCompoundStatementNode }
constructor TCompoundStatementNode.Create;
begin
  inherited Create;
  FStatements := TList.Create;
end;

destructor TCompoundStatementNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStatements.Count - 1 do
    TStatementNode(FStatements[i]).Free;
  FStatements.Free;
  inherited Destroy;
end;

procedure TCompoundStatementNode.AddStatement(AStatement: TStatementNode);
begin
  FStatements.Add(AStatement);
end;

function TCompoundStatementNode.GetStatement(Index: Integer): TStatementNode;
begin
  Result := TStatementNode(FStatements[Index]);
end;

function TCompoundStatementNode.StatementCount: Integer;
begin
  Result := FStatements.Count;
end;

{ TBlockNode }
constructor TBlockNode.Create(ACompoundStatement: TCompoundStatementNode);
begin
  inherited Create;
  FDeclarations := TList.Create;
  FCompoundStatement := ACompoundStatement;
end;

destructor TBlockNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDeclarations.Count - 1 do
    TASTNode(FDeclarations[i]).Free;
  FDeclarations.Free;
  FCompoundStatement.Free;
  inherited Destroy;
end;

procedure TBlockNode.AddDeclaration(ADeclaration: TASTNode);
begin
  FDeclarations.Add(ADeclaration);
end;

function TBlockNode.GetDeclaration(Index: Integer): TASTNode;
begin
  Result := TASTNode(FDeclarations[Index]);
end;

function TBlockNode.DeclarationCount: Integer;
begin
  Result := FDeclarations.Count;
end;

{ TProgramNode }
constructor TProgramNode.Create(const AProgramName: string; ABlock: TBlockNode);
begin
  inherited Create;
  ProgramName := AProgramName;
  Block := ABlock;
end;

destructor TProgramNode.Destroy;
begin
  Block.Free;
  inherited Destroy;
end;

end.
