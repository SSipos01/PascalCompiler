unit ast;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TNodeType = (
    ntProgram, ntUnit, ntUses, ntBlock,
    ntVarDecl, ntConstDecl, ntTypeDecl,
    ntProcedure, ntFunction, ntConstructor, ntDestructor,
    ntAssign, ntProcCall,
    ntIf, ntFor, ntWhile, ntRepeat, ntCase,
    ntBinaryOp, ntUnaryOp,
    ntVar, ntIntegerLiteral, ntRealLiteral, ntStringLiteral, ntNil,
    ntClass, ntMethod, ntField, ntInherited, ntSelf,
    ntTypeSpec, ntArray, ntRecord, ntSet, ntFile, ntPointer, ntEnum, ntSubrange,
    ntAsm
  );

  TAstNode = class
  private
    FNodeType: TNodeType;
  public
    constructor Create(ANodeType: TNodeType);
    destructor Destroy; override;
    property NodeType: TNodeType read FNodeType;
  end;

  TExpressionNode = class(TAstNode)
  public
    constructor Create(ANodeType: TNodeType);
  end;

  TStatementNode = class(TAstNode)
   public
    constructor Create(ANodeType: TNodeType);
  end;

  TDeclarationNode = class(TAstNode)
  public
    constructor Create(ANodeType: TNodeType);
  end;

  TIdentifier = class(TExpressionNode)
  public
    Name: string;
    constructor Create(AName: string);
  end;

  TTypeSpecNode = class(TAstNode)
  private
    FTypeName: string;
    FTypeKind: TNodeType;
  public
    constructor Create(AKind: TNodeType; ATypeName: string);
    property TypeName: string read FTypeName;
    property TypeKind: TNodeType read FTypeKind;
  end;

  TSubrangeNode = class(TAstNode)
  public
      Low, High: TExpressionNode;
      constructor Create(ALow, AHigh: TExpressionNode);
      destructor Destroy; override;
  end;

  TArrayTypeNode = class(TTypeSpecNode)
  public
    IndexTypes: TList; // List of TSubrangeNode or TIdentifier
    ElementType: TTypeSpecNode;
    constructor Create(AIndexTypes: TList; AElementType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TRecordFieldNode = class(TAstNode)
  public
    FieldNames: TList; // List of TIdentifier
    FieldType: TTypeSpecNode;
    constructor Create(AFieldNames: TList; AFieldType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TRecordTypeNode = class(TTypeSpecNode)
  public
    Fields: TList; // List of TRecordFieldNode
    constructor Create(AFields: TList);
    destructor Destroy; override;
  end;

  TSetTypeNode = class(TTypeSpecNode)
  public
    BaseType: TTypeSpecNode;
    constructor Create(ABaseType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TFileTypeNode = class(TTypeSpecNode)
  public
    ComponentType: TTypeSpecNode;
    constructor Create(AComponentType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TPointerTypeNode = class(TTypeSpecNode)
  public
    BaseType: TIdentifier;
    constructor Create(ABaseType: TIdentifier);
    destructor Destroy; override;
  end;

  TEnumTypeNode = class(TTypeSpecNode)
  public
    EnumValues: TList; // List of TIdentifier
    constructor Create(AEnumValues: TList);
    destructor Destroy; override;
  end;

  TSubrangeTypeNode = class(TTypeSpecNode)
  public
    Range: TSubrangeNode;
    constructor Create(ARange: TSubrangeNode);
    destructor Destroy; override;
  end;

  TProgramNode = class(TDeclarationNode)
  public
    ProgramName: TIdentifier;
    UsesClause: TList; // List of TIdentifier
    Block: TDeclarationNode; // Actually a TBlockNode
    constructor Create(AName: TIdentifier; AUses: TList; ABlock: TDeclarationNode);
    destructor Destroy; override;
  end;

  TUnitNode = class(TDeclarationNode)
  public
    FUnitName: TIdentifier;
    InterfaceSection: TList; // List of TDeclarationNode
    ImplementationSection: TList; // List of TDeclarationNode
    InitializationBlock, FinalizationBlock: TStatementNode; // TBlockNode
    constructor Create(AName: TIdentifier; AInterface, AImplementation: TList; AInit, AFinal: TStatementNode);
    destructor Destroy; override;
  end;

  TBlockNode = class(TDeclarationNode)
  public
    Declarations: TList; // List of TDeclarationNode
    Statements: TList; // List of TStatementNode
    constructor Create(ADeclarations: TList; AStatements: TList);
    destructor Destroy; override;
  end;

  TVarDeclNode = class(TDeclarationNode)
  public
    VarNames: TList; // List of TIdentifier
    VarType: TTypeSpecNode;
    constructor Create(AVarNames: TList; AVarType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TConstDeclNode = class(TDeclarationNode)
  public
    ConstName: TIdentifier;
    ConstValue: TExpressionNode;
    constructor Create(AConstName: TIdentifier; AConstValue: TExpressionNode);
    destructor Destroy; override;
  end;

  TTypeDeclNode = class(TDeclarationNode)
  public
    TypeName: TIdentifier;
    TypeSpec: TTypeSpecNode;
    constructor Create(ATypeName: TIdentifier; ATypeSpec: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TSubroutineNode = class(TDeclarationNode)
  public
    Name: TIdentifier;
    Parameters: TList; // List of TVarDeclNode
    Block: TBlockNode;
    constructor Create(ANodeType: TNodeType; AName: TIdentifier; AParams: TList; ABlock: TBlockNode);
    destructor Destroy; override;
  end;

  TProcedureNode = class(TSubroutineNode)
  public
    constructor Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode);
  end;

  TFunctionNode = class(TSubroutineNode)
  public
    ReturnType: TTypeSpecNode;
    constructor Create(AName: TIdentifier; AParams: TList; AReturnType: TTypeSpecNode; ABlock: TBlockNode);
    destructor Destroy; override;
  end;

  TMethodNode = class(TSubroutineNode)
  public
    IsOverride: Boolean;
    constructor Create(ANodeType: TNodeType; AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
  end;

  TConstructorNode = class(TMethodNode)
  public
     constructor Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
  end;

  TDestructorNode = class(TMethodNode)
  public
     constructor Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
  end;

  TClassNode = class(TTypeSpecNode)
  public
    ParentClass: TIdentifier;
    Members: TList; // of TDeclarationNode (TFieldNode, TMethodNode)
    constructor Create(AClassName: TIdentifier; AParent: TIdentifier; AMembers: TList);
    destructor Destroy; override;
  end;

  TFieldNode = class(TDeclarationNode)
  public
    FieldNames: TList; // List of TIdentifier
    FieldType: TTypeSpecNode;
    constructor Create(AFieldNames: TList; AFieldType: TTypeSpecNode);
    destructor Destroy; override;
  end;

  TAssignNode = class(TStatementNode)
  public
    Left: TExpressionNode;
    Right: TExpressionNode;
    constructor Create(ALeft, ARight: TExpressionNode);
    destructor Destroy; override;
  end;

  TCompoundStatementNode = class(TStatementNode)
  public
    Statements: TList;
    constructor Create(AStatements: TList);
    destructor Destroy; override;
  end;

  TExpressionStatementNode = class(TStatementNode)
  public
    Expression: TExpressionNode;
    constructor Create(AExpression: TExpressionNode);
    destructor Destroy; override;
  end;

  TProcCallNode = class(TExpressionNode)
  public
    ProcName: TExpressionNode; // Can be TIdentifier or TMemberAccessNode
    Arguments: TList; // List of TExpressionNode
    constructor Create(AProcName: TExpressionNode; AArguments: TList);
    destructor Destroy; override;
  end;

  TIfNode = class(TStatementNode)
  public
    Condition: TExpressionNode;
    ThenBranch: TStatementNode;
    ElseBranch: TStatementNode; // Can be nil
    constructor Create(ACondition: TExpressionNode; AThenBranch, AElseBranch: TStatementNode);
    destructor Destroy; override;
  end;

  TForNode = class(TStatementNode)
  public
    LoopVar: TIdentifier;
    StartValue, EndValue: TExpressionNode;
    IsDownTo: Boolean;
    Body: TStatementNode;
    constructor Create(ALoopVar: TIdentifier; AStart, AEnd: TExpressionNode; AIsDownTo: Boolean; ABody: TStatementNode);
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
    Condition: TExpressionNode;
    Statements: TList; // List of TStatementNode
    constructor Create(ACondition: TExpressionNode; AStatements: TList);
    destructor Destroy; override;
  end;

  TCaseStatementNode = class(TStatementNode)
  public
    Expression: TExpressionNode;
    Cases: TList; // List of TCaseBranchNode
    ElseBranch: TList; // List of TStatementNode
    constructor Create(AExpression: TExpressionNode; ACases: TList; AElse: TList);
    destructor Destroy; override;
  end;

  TCaseBranchNode = class(TAstNode)
  public
    Values: TList; // List of TExpressionNode or TSubrangeNode
    Statement: TStatementNode;
    constructor Create(AValues: TList; AStatement: TStatementNode);
    destructor Destroy; override;
  end;

  TAsmNode = class(TStatementNode)
  public
    AsmLines: TStringList;
    constructor Create(AAsmLines: TStringList);
    destructor Destroy; override;
  end;

  TOperatorType = (opPlus, opMinus, opStar, opSlash, opEqual, opNotEqual, opLess, opLessEqual, opGreater, opGreaterEqual, opAnd, opOr, opNot, opXor, opShl, opShr, opAs, opIs, opIn, opDot, opDeref, opAddressOf);

  TBinaryOpNode = class(TExpressionNode)
  public
    Left, Right: TExpressionNode;
    Op: TOperatorType;
    constructor Create(ALeft: TExpressionNode; AOp: TOperatorType; ARight: TExpressionNode);
    destructor Destroy; override;
  end;

  TUnaryOpNode = class(TExpressionNode)
  public
    Expr: TExpressionNode;
    Op: TOperatorType;
    constructor Create(AOp: TOperatorType; AExpr: TExpressionNode);
    destructor Destroy; override;
  end;

  TIntegerLiteralNode = class(TExpressionNode)
  public
    Value: Integer;
    constructor Create(AValue: Integer);
  end;

  TRealLiteralNode = class(TExpressionNode)
  public
    Value: Double;
    constructor Create(AValue: Double);
  end;

  TStringLiteralNode = class(TExpressionNode)
  public
    Value: string;
    constructor Create(AValue: string);
  end;

  TNilNode = class(TExpressionNode)
  public
    constructor Create;
  end;

  TInheritedNode = class(TExpressionNode)
  public
    constructor Create;
  end;

  TSelfNode = class(TExpressionNode)
  public
    constructor Create;
  end;

implementation

{ TAstNode }
constructor TAstNode.Create(ANodeType: TNodeType);
begin
  inherited Create;
  FNodeType := ANodeType;
end;

destructor TAstNode.Destroy;
begin
  inherited Destroy;
end;

{ TExpressionNode }
constructor TExpressionNode.Create(ANodeType: TNodeType);
begin
  inherited Create(ANodeType);
end;

{ TStatementNode }
constructor TStatementNode.Create(ANodeType: TNodeType);
begin
  inherited Create(ANodeType);
end;

{ TDeclarationNode }
constructor TDeclarationNode.Create(ANodeType: TNodeType);
begin
  inherited Create(ANodeType);
end;

{ TIdentifier }
constructor TIdentifier.Create(AName: string);
begin
  inherited Create(ntVar);
  Name := AName;
end;

{ TTypeSpecNode }
constructor TTypeSpecNode.Create(AKind: TNodeType; ATypeName: string);
begin
  inherited Create(AKind);
  FTypeName := ATypeName;
  FTypeKind := AKind;
end;

{ TArrayTypeNode }
constructor TArrayTypeNode.Create(AIndexTypes: TList; AElementType: TTypeSpecNode);
begin
  inherited Create(ntArray, 'array');
  IndexTypes := AIndexTypes;
  ElementType := AElementType;
end;

destructor TArrayTypeNode.Destroy;
var i: integer;
begin
  for i := 0 to IndexTypes.Count - 1 do
    TObject(IndexTypes[i]).Free;
  IndexTypes.Free;
  ElementType.Free;
  inherited Destroy;
end;

{ TRecordFieldNode }
constructor TRecordFieldNode.Create(AFieldNames: TList; AFieldType: TTypeSpecNode);
begin
  inherited Create(ntField);
  FieldNames := AFieldNames;
  FieldType := AFieldType;
end;

destructor TRecordFieldNode.Destroy;
var i: integer;
begin
  for i := 0 to FieldNames.Count - 1 do
    TObject(FieldNames[i]).Free;
  FieldNames.Free;
  FieldType.Free;
  inherited Destroy;
end;

{ TRecordTypeNode }
constructor TRecordTypeNode.Create(AFields: TList);
begin
  inherited Create(ntRecord, 'record');
  Fields := AFields;
end;

destructor TRecordTypeNode.Destroy;
var i: integer;
begin
  for i := 0 to Fields.Count - 1 do
    TObject(Fields[i]).Free;
  Fields.Free;
  inherited Destroy;
end;

{ TSetTypeNode }
constructor TSetTypeNode.Create(ABaseType: TTypeSpecNode);
begin
  inherited Create(ntSet, 'set');
  BaseType := ABaseType;
end;

destructor TSetTypeNode.Destroy;
begin
  BaseType.Free;
  inherited Destroy;
end;

{ TFileTypeNode }
constructor TFileTypeNode.Create(AComponentType: TTypeSpecNode);
begin
  inherited Create(ntFile, 'file');
  ComponentType := AComponentType;
end;

destructor TFileTypeNode.Destroy;
begin
  if Assigned(ComponentType) then
    ComponentType.Free;
  inherited Destroy;
end;

{ TPointerTypeNode }
constructor TPointerTypeNode.Create(ABaseType: TIdentifier);
begin
  inherited Create(ntPointer, 'pointer');
  BaseType := ABaseType;
end;

destructor TPointerTypeNode.Destroy;
begin
  BaseType.Free;
  inherited Destroy;
end;

{ TEnumTypeNode }
constructor TEnumTypeNode.Create(AEnumValues: TList);
begin
  inherited Create(ntEnum, 'enum');
  EnumValues := AEnumValues;
end;

destructor TEnumTypeNode.Destroy;
var i: integer;
begin
  for i := 0 to EnumValues.Count - 1 do
    TObject(EnumValues[i]).Free;
  EnumValues.Free;
  inherited Destroy;
end;

{ TSubrangeNode }
constructor TSubrangeNode.Create(ALow, AHigh: TExpressionNode);
begin
    inherited Create(ntSubrange);
    Low := ALow;
    High := AHigh;
end;

destructor TSubrangeNode.Destroy;
begin
    Low.Free;
    High.Free;
    inherited Destroy;
end;

{ TSubrangeTypeNode }
constructor TSubrangeTypeNode.Create(ARange: TSubrangeNode);
begin
  inherited Create(ntSubrange, 'subrange');
  Range := ARange;
end;

destructor TSubrangeTypeNode.Destroy;
begin
  Range.Free;
  inherited Destroy;
end;

{ TProgramNode }
constructor TProgramNode.Create(AName: TIdentifier; AUses: TList; ABlock: TDeclarationNode);
begin
  inherited Create(ntProgram);
  ProgramName := AName;
  UsesClause := AUses;
  Block := ABlock;
end;

destructor TProgramNode.Destroy;
var i: integer;
begin
  ProgramName.Free;
  for i := 0 to UsesClause.Count - 1 do
    TObject(UsesClause[i]).Free;
  UsesClause.Free;
  Block.Free;
  inherited Destroy;
end;

{ TUnitNode }
constructor TUnitNode.Create(AName: TIdentifier; AInterface, AImplementation: TList; AInit, AFinal: TStatementNode);
begin
    inherited Create(ntUnit);
    FUnitName := AName;
    InterfaceSection := AInterface;
    ImplementationSection := AImplementation;
    InitializationBlock := AInit;
    FinalizationBlock := AFinal;
end;

destructor TUnitNode.Destroy;
var i: integer;
begin
    FUnitName.Free;
    for i := 0 to InterfaceSection.Count-1 do TObject(InterfaceSection[i]).Free;
    InterfaceSection.Free;
    for i := 0 to ImplementationSection.Count-1 do TObject(ImplementationSection[i]).Free;
    ImplementationSection.Free;
    if Assigned(InitializationBlock) then InitializationBlock.Free;
    if Assigned(FinalizationBlock) then FinalizationBlock.Free;
    inherited Destroy;
end;


{ TBlockNode }
constructor TBlockNode.Create(ADeclarations: TList; AStatements: TList);
begin
  inherited Create(ntBlock);
  Declarations := ADeclarations;
  Statements := AStatements;
end;

destructor TBlockNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to Declarations.Count - 1 do
    TObject(Declarations[i]).Free;
  Declarations.Free;
  for i := 0 to Statements.Count - 1 do
    TObject(Statements[i]).Free;
  Statements.Free;
  inherited Destroy;
end;

{ TVarDeclNode }
constructor TVarDeclNode.Create(AVarNames: TList; AVarType: TTypeSpecNode);
begin
  inherited Create(ntVarDecl);
  VarNames := AVarNames;
  VarType := AVarType;
end;

destructor TVarDeclNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to VarNames.Count - 1 do
    TObject(VarNames[i]).Free;
  VarNames.Free;
  VarType.Free;
  inherited Destroy;
end;

{ TConstDeclNode }
constructor TConstDeclNode.Create(AConstName: TIdentifier; AConstValue: TExpressionNode);
begin
  inherited Create(ntConstDecl);
  ConstName := AConstName;
  ConstValue := AConstValue;
end;

destructor TConstDeclNode.Destroy;
begin
  ConstName.Free;
  ConstValue.Free;
  inherited Destroy;
end;

{ TTypeDeclNode }
constructor TTypeDeclNode.Create(ATypeName: TIdentifier; ATypeSpec: TTypeSpecNode);
begin
  inherited Create(ntTypeDecl);
  TypeName := ATypeName;
  TypeSpec := ATypeSpec;
end;

destructor TTypeDeclNode.Destroy;
begin
  TypeName.Free;
  TypeSpec.Free;
  inherited Destroy;
end;

{ TSubroutineNode }
constructor TSubroutineNode.Create(ANodeType: TNodeType; AName: TIdentifier; AParams: TList; ABlock: TBlockNode);
begin
    inherited Create(ANodeType);
    Name := AName;
    Parameters := AParams;
    Block := ABlock;
end;

destructor TSubroutineNode.Destroy;
var i: integer;
begin
    Name.Free;
    for i := 0 to Parameters.Count - 1 do
        TObject(Parameters[i]).Free;
    Parameters.Free;
    Block.Free;
    inherited Destroy;
end;

{ TProcedureNode }
constructor TProcedureNode.Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode);
begin
  inherited Create(ntProcedure, AName, AParams, ABlock);
end;

{ TFunctionNode }
constructor TFunctionNode.Create(AName: TIdentifier; AParams: TList; AReturnType: TTypeSpecNode; ABlock: TBlockNode);
begin
  inherited Create(ntFunction, AName, AParams, ABlock);
  ReturnType := AReturnType;
end;

destructor TFunctionNode.Destroy;
begin
    ReturnType.Free;
    inherited Destroy;
end;

{ TMethodNode }
constructor TMethodNode.Create(ANodeType: TNodeType; AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
begin
    inherited Create(ANodeType, AName, AParams, ABlock);
    IsOverride := AIsOverride;
end;

{ TConstructorNode }
constructor TConstructorNode.Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
begin
    inherited Create(ntConstructor, AName, AParams, ABlock, AIsOverride);
end;

{ TDestructorNode }
constructor TDestructorNode.Create(AName: TIdentifier; AParams: TList; ABlock: TBlockNode; AIsOverride: Boolean);
begin
    inherited Create(ntDestructor, AName, AParams, ABlock, AIsOverride);
end;

{ TClassNode }
constructor TClassNode.Create(AClassName: TIdentifier; AParent: TIdentifier; AMembers: TList);
begin
  inherited Create(ntClass, AClassName.Name);
  ParentClass := AParent;
  Members := AMembers;
end;

destructor TClassNode.Destroy;
var i: integer;
begin
  if Assigned(ParentClass) then ParentClass.Free;
  for i := 0 to Members.Count-1 do TObject(Members[i]).Free;
  Members.Free;
  inherited Destroy;
end;

{ TFieldNode }
constructor TFieldNode.Create(AFieldNames: TList; AFieldType: TTypeSpecNode);
begin
    inherited Create(ntField);
    FieldNames := AFieldNames;
    FieldType := AFieldType;
end;

destructor TFieldNode.Destroy;
var i: integer;
begin
    for i := 0 to FieldNames.Count-1 do TObject(FieldNames[i]).Free;
    FieldNames.Free;
    FieldType.Free;
    inherited Destroy;
end;

{ TAssignNode }
constructor TAssignNode.Create(ALeft, ARight: TExpressionNode);
begin
  inherited Create(ntAssign);
  Left := ALeft;
  Right := ARight;
end;

destructor TAssignNode.Destroy;
begin
  Left.Free;
  Right.Free;
  inherited Destroy;
end;

{ TCompoundStatementNode }
constructor TCompoundStatementNode.Create(AStatements: TList);
begin
  inherited Create(ntBlock); // Using ntBlock for now, maybe add ntCompound
  Statements := AStatements;
end;

destructor TCompoundStatementNode.Destroy;
var i: integer;
begin
  for i := 0 to Statements.Count - 1 do
    TObject(Statements[i]).Free;
  Statements.Free;
  inherited Destroy;
end;

{ TExpressionStatementNode }
constructor TExpressionStatementNode.Create(AExpression: TExpressionNode);
begin
    inherited Create(ntProcCall); // Reuse this type for now
    Expression := AExpression;
end;

destructor TExpressionStatementNode.Destroy;
begin
    Expression.Free;
    inherited Destroy;
end;

{ TProcCallNode }
constructor TProcCallNode.Create(AProcName: TExpressionNode; AArguments: TList);
begin
  inherited Create(ntProcCall);
  ProcName := AProcName;
  Arguments := AArguments;
end;

destructor TProcCallNode.Destroy;
var
  i: Integer;
begin
  ProcName.Free;
  for i := 0 to Arguments.Count - 1 do
    TObject(Arguments[i]).Free;
  Arguments.Free;
  inherited Destroy;
end;

{ TIfNode }
constructor TIfNode.Create(ACondition: TExpressionNode; AThenBranch, AElseBranch: TStatementNode);
begin
  inherited Create(ntIf);
  Condition := ACondition;
  ThenBranch := AThenBranch;
  ElseBranch := AElseBranch;
end;

destructor TIfNode.Destroy;
begin
  Condition.Free;
  ThenBranch.Free;
  if Assigned(ElseBranch) then
    ElseBranch.Free;
  inherited Destroy;
end;

{ TForNode }
constructor TForNode.Create(ALoopVar: TIdentifier; AStart, AEnd: TExpressionNode; AIsDownTo: Boolean; ABody: TStatementNode);
begin
  inherited Create(ntFor);
  LoopVar := ALoopVar;
  StartValue := AStart;
  EndValue := AEnd;
  IsDownTo := AIsDownTo;
  Body := ABody;
end;

destructor TForNode.Destroy;
begin
  LoopVar.Free;
  StartValue.Free;
  EndValue.Free;
  Body.Free;
  inherited Destroy;
end;

{ TWhileNode }
constructor TWhileNode.Create(ACondition: TExpressionNode; ABody: TStatementNode);
begin
  inherited Create(ntWhile);
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
constructor TRepeatNode.Create(ACondition: TExpressionNode; AStatements: TList);
begin
  inherited Create(ntRepeat);
  Condition := ACondition;
  Statements := AStatements;
end;

destructor TRepeatNode.Destroy;
var i: integer;
begin
  Condition.Free;
  for i := 0 to Statements.Count - 1 do
    TObject(Statements[i]).Free;
  Statements.Free;
  inherited Destroy;
end;

{ TCaseStatementNode }
constructor TCaseStatementNode.Create(AExpression: TExpressionNode; ACases: TList; AElse: TList);
begin
    inherited Create(ntCase);
    Expression := AExpression;
    Cases := ACases;
    ElseBranch := AElse;
end;

destructor TCaseStatementNode.Destroy;
var i: integer;
begin
    Expression.Free;
    for i := 0 to Cases.Count - 1 do TObject(Cases[i]).Free;
    Cases.Free;
    if Assigned(ElseBranch) then
    begin
        for i := 0 to ElseBranch.Count - 1 do TObject(ElseBranch[i]).Free;
        ElseBranch.Free;
    end;
    inherited Destroy;
end;

{ TCaseBranchNode }
constructor TCaseBranchNode.Create(AValues: TList; AStatement: TStatementNode);
begin
    inherited Create(ntCase); // Not a great node type, but works for now
    Values := AValues;
    Statement := AStatement;
end;

destructor TCaseBranchNode.Destroy;
var i: integer;
begin
    for i := 0 to Values.Count - 1 do TObject(Values[i]).Free;
    Values.Free;
    Statement.Free;
    inherited Destroy;
end;

{ TAsmNode }
constructor TAsmNode.Create(AAsmLines: TStringList);
begin
  inherited Create(ntAsm);
  AsmLines := AAsmLines;
end;

destructor TAsmNode.Destroy;
begin
  AsmLines.Free;
  inherited Destroy;
end;

{ TBinaryOpNode }
constructor TBinaryOpNode.Create(ALeft: TExpressionNode; AOp: TOperatorType; ARight: TExpressionNode);
begin
  inherited Create(ntBinaryOp);
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

{ TUnaryOpNode }
constructor TUnaryOpNode.Create(AOp: TOperatorType; AExpr: TExpressionNode);
begin
  inherited Create(ntUnaryOp);
  Op := AOp;
  Expr := AExpr;
end;

destructor TUnaryOpNode.Destroy;
begin
  Expr.Free;
  inherited Destroy;
end;

{ TIntegerLiteralNode }
constructor TIntegerLiteralNode.Create(AValue: Integer);
begin
  inherited Create(ntIntegerLiteral);
  Value := AValue;
end;

{ TRealLiteralNode }
constructor TRealLiteralNode.Create(AValue: Double);
begin
  inherited Create(ntRealLiteral);
  Value := AValue;
end;

{ TStringLiteralNode }
constructor TStringLiteralNode.Create(AValue: string);
begin
  inherited Create(ntStringLiteral);
  Value := AValue;
end;

{ TNilNode }
constructor TNilNode.Create;
begin
  inherited Create(ntNil);
end;

{ TInheritedNode }
constructor TInheritedNode.Create;
begin
    inherited Create(ntInherited);
end;

{ TSelfNode }
constructor TSelfNode.Create;
begin
    inherited Create(ntSelf);
end;

end.
