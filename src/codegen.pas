unit codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast;

type
  TCodeGenerator = class
  private
    FOutputFile: TStreamWriter;
    procedure Visit(ANode: TAstNode);
    procedure VisitProgram(ANode: TProgramNode);
    procedure VisitUnit(ANode: TUnitNode);
    procedure VisitBlock(ANode: TBlockNode);
    procedure VisitVarDecl(ANode: TVarDeclNode);
    procedure VisitConstDecl(ANode: TConstDeclNode);
    procedure VisitTypeDecl(ANode: TTypeDeclNode);
    procedure VisitProcedure(ANode: TProcedureNode);
    procedure VisitFunction(ANode: TFunctionNode);
    procedure VisitConstructor(ANode: TConstructorNode);
    procedure VisitDestructor(ANode: TDestructorNode);
    procedure VisitAssign(ANode: TAssignNode);
    procedure VisitProcCall(ANode: TProcCallNode);
    procedure VisitIf(ANode: TIfNode);
    procedure VisitFor(ANode: TForNode);
    procedure VisitWhile(ANode: TWhileNode);
    procedure VisitRepeat(ANode: TRepeatNode);
    procedure VisitCase(ANode: TCaseStatementNode);
    procedure VisitAsm(ANode: TAsmNode);
    procedure VisitBinaryOp(ANode: TBinaryOpNode);
    procedure VisitUnaryOp(ANode: TUnaryOpNode);
    procedure VisitVar(ANode: TIdentifier);
    procedure VisitIntegerLiteral(ANode: TIntegerLiteralNode);
    procedure VisitRealLiteral(ANode: TRealLiteralNode);
    procedure VisitStringLiteral(ANode: TStringLiteralNode);
    procedure VisitNil(ANode: TNilNode);
    procedure VisitClass(ANode: TClassNode);
    procedure VisitMethod(ANode: TMethodNode);
    procedure VisitField(ANode: TFieldNode);
    procedure VisitInherited(ANode: TInheritedNode);
    procedure VisitSelf(ANode: TSelfNode);
    procedure VisitTypeSpec(ANode: TTypeSpecNode);
    procedure VisitArrayType(ANode: TArrayTypeNode);
    procedure VisitRecordType(ANode: TRecordTypeNode);
    procedure VisitSetType(ANode: TSetTypeNode);
    procedure VisitFileType(ANode: TFileTypeNode);
    procedure VisitPointerType(ANode: TPointerTypeNode);
    procedure VisitEnumType(ANode: TEnumTypeNode);
    procedure VisitSubrangeType(ANode: TSubrangeTypeNode);

  public
    constructor Create(AOutputFileName: string);
    destructor Destroy; override;
    procedure GenerateCode(AProgramNode: TProgramNode);
  end;

implementation

{ TCodeGenerator }

constructor TCodeGenerator.Create(AOutputFileName: string);
begin
  inherited Create;
  FOutputFile := TStreamWriter.Create(AOutputFileName);
end;

destructor TCodeGenerator.Destroy;
begin
  FOutputFile.Free;
  inherited Destroy;
end;

procedure TCodeGenerator.GenerateCode(AProgramNode: TProgramNode);
begin
  Visit(AProgramNode);
end;

procedure TCodeGenerator.Visit(ANode: TAstNode);
begin
  if not Assigned(ANode) then Exit;

  case ANode.NodeType of
    ntProgram: VisitProgram(TProgramNode(ANode));
    ntUnit: VisitUnit(TUnitNode(ANode));
    ntBlock: VisitBlock(TBlockNode(ANode));
    ntVarDecl: VisitVarDecl(TVarDeclNode(ANode));
    ntConstDecl: VisitConstDecl(TConstDeclNode(ANode));
    ntTypeDecl: VisitTypeDecl(TTypeDeclNode(ANode));
    ntProcedure: VisitProcedure(TProcedureNode(ANode));
    ntFunction: VisitFunction(TFunctionNode(ANode));
    ntConstructor: VisitConstructor(TConstructorNode(ANode));
    ntDestructor: VisitDestructor(TDestructorNode(ANode));
    ntAssign: VisitAssign(TAssignNode(ANode));
    ntProcCall: VisitProcCall(TProcCallNode(ANode));
    ntIf: VisitIf(TIfNode(ANode));
    ntFor: VisitFor(TForNode(ANode));
    ntWhile: VisitWhile(TWhileNode(ANode));
    ntRepeat: VisitRepeat(TRepeatNode(ANode));
    ntCase: VisitCase(TCaseStatementNode(ANode));
    ntAsm: VisitAsm(TAsmNode(ANode));
    ntBinaryOp: VisitBinaryOp(TBinaryOpNode(ANode));
    ntUnaryOp: VisitUnaryOp(TUnaryOpNode(ANode));
    ntVar: VisitVar(TIdentifier(ANode));
    ntIntegerLiteral: VisitIntegerLiteral(TIntegerLiteralNode(ANode));
    ntRealLiteral: VisitRealLiteral(TRealLiteralNode(ANode));
    ntStringLiteral: VisitStringLiteral(TStringLiteralNode(ANode));
    ntNil: VisitNil(TNilNode(ANode));
    ntClass: VisitClass(TClassNode(ANode));
    ntMethod: VisitMethod(TMethodNode(ANode));
    ntField: VisitField(TFieldNode(ANode));
    ntInherited: VisitInherited(TInheritedNode(ANode));
    ntSelf: VisitSelf(TSelfNode(ANode));
    ntTypeSpec: VisitTypeSpec(TTypeSpecNode(ANode));
    ntArray: VisitArrayType(TArrayTypeNode(ANode));
    ntRecord: VisitRecordType(TRecordTypeNode(ANode));
    ntSet: VisitSetType(TSetTypeNode(ANode));
    ntFile: VisitFileType(TFileTypeNode(ANode));
    ntPointer: VisitPointerType(TPointerTypeNode(ANode));
    ntEnum: VisitEnumType(TEnumTypeNode(ANode));
    ntSubrange: VisitSubrangeType(TSubrangeTypeNode(ANode));
  else
    FOutputFile.WriteLine('; Unknown AST Node: ' + GetEnumName(TypeInfo(TNodeType), Ord(ANode.NodeType)));
  end;
end;

procedure TCodeGenerator.VisitProgram(ANode: TProgramNode);
var i: integer;
begin
  FOutputFile.WriteLine('; Program: ' + ANode.ProgramName.Name);
  FOutputFile.WriteLine('JMP @main');
  Visit(ANode.Block);
  FOutputFile.WriteLine('@main:');
end;

procedure TCodeGenerator.VisitUnit(ANode: TUnitNode);
var i: integer;
begin
    FOutputFile.WriteLine('; Unit: ' + ANode.UnitName.Name);
    FOutputFile.WriteLine('; Interface');
    for i := 0 to ANode.InterfaceSection.Count - 1 do
        Visit(TAstNode(ANode.InterfaceSection[i]));
    FOutputFile.WriteLine('; Implementation');
    for i := 0 to ANode.ImplementationSection.Count - 1 do
        Visit(TAstNode(ANode.ImplementationSection[i]));

    if Assigned(ANode.InitializationBlock) then
    begin
        FOutputFile.WriteLine('; Initialization');
        Visit(ANode.InitializationBlock);
    end;
    // Finalization is not called automatically, it's up to the runtime
end;

procedure TCodeGenerator.VisitBlock(ANode: TBlockNode);
var
  i: Integer;
begin
  for i := 0 to ANode.Declarations.Count - 1 do
    Visit(TAstNode(ANode.Declarations[i]));
  for i := 0 to ANode.Statements.Count - 1 do
    Visit(TAstNode(ANode.Statements[i]));
end;

procedure TCodeGenerator.VisitVarDecl(ANode: TVarDeclNode);
var i: integer;
begin
  for i := 0 to ANode.VarNames.Count - 1 do
    FOutputFile.WriteLine('; VAR ' + TIdentifier(ANode.VarNames[i]).Name + ': ' + ANode.VarType.TypeName);
end;

procedure TCodeGenerator.VisitConstDecl(ANode: TConstDeclNode);
begin
    FOutputFile.WriteLine('; CONST ' + ANode.ConstName.Name);
end;

procedure TCodeGenerator.VisitTypeDecl(ANode: TTypeDeclNode);
begin
    FOutputFile.WriteLine('; TYPE ' + ANode.TypeName.Name);
    Visit(ANode.TypeSpec);
end;

procedure TCodeGenerator.VisitProcedure(ANode: TProcedureNode);
begin
  FOutputFile.WriteLine(ANode.Name.Name + ':');
  Visit(ANode.Block);
  FOutputFile.WriteLine('RET');
end;

procedure TCodeGenerator.VisitFunction(ANode: TFunctionNode);
begin
  FOutputFile.WriteLine(ANode.Name.Name + ':');
  Visit(ANode.Block);
  FOutputFile.WriteLine('; function return value should be in accumulator');
  FOutputFile.WriteLine('RET');
end;

procedure TCodeGenerator.VisitConstructor(ANode: TConstructorNode);
begin
    FOutputFile.WriteLine('; CONSTRUCTOR ' + ANode.Name.Name);
    Visit(ANode.Block);
    FOutputFile.WriteLine('RET');
end;

procedure TCodeGenerator.VisitDestructor(ANode: TDestructorNode);
begin
    FOutputFile.WriteLine('; DESTRUCTOR ' + ANode.Name.Name);
    Visit(ANode.Block);
    FOutputFile.WriteLine('RET');
end;

procedure TCodeGenerator.VisitAssign(ANode: TAssignNode);
begin
  Visit(ANode.Right);
  FOutputFile.WriteLine('POP EAX');
  Visit(ANode.Left);
  FOutputFile.WriteLine('; In a real compiler, the address of Left would be on the stack');
  FOutputFile.WriteLine('POP EBX');
  FOutputFile.WriteLine('MOV [EBX], EAX');
end;

procedure TCodeGenerator.VisitProcCall(ANode: TProcCallNode);
var i: integer;
begin
  for i := ANode.Arguments.Count - 1 downto 0 do
  begin
    Visit(TExpressionNode(ANode.Arguments[i]));
  end;
  Visit(ANode.ProcName);
  FOutputFile.WriteLine('; ProcName resolved to an address');
  FOutputFile.WriteLine('POP EAX');
  FOutputFile.WriteLine('CALL EAX');
end;

procedure TCodeGenerator.VisitIf(ANode: TIfNode);
begin
  FOutputFile.WriteLine('; IF statement');
end;

procedure TCodeGenerator.VisitFor(ANode: TForNode);
begin
  FOutputFile.WriteLine('; FOR loop');
end;

procedure TCodeGenerator.VisitWhile(ANode: TWhileNode);
begin
  FOutputFile.WriteLine('; WHILE loop');
end;

procedure TCodeGenerator.VisitRepeat(ANode: TRepeatNode);
begin
  FOutputFile.WriteLine('; REPEAT loop');
end;

procedure TCodeGenerator.VisitCase(ANode: TCaseStatementNode);
begin
    FOutputFile.WriteLine('; CASE statement');
end;

procedure TCodeGenerator.VisitAsm(ANode: TAsmNode);
begin
    FOutputFile.WriteLine('; ASM block');
end;

procedure TCodeGenerator.VisitBinaryOp(ANode: TBinaryOpNode);
begin
  Visit(ANode.Left);
  Visit(ANode.Right);
  FOutputFile.WriteLine('; BINARY OP ' + IntToStr(Ord(ANode.Op)));
end;

procedure TCodeGenerator.VisitUnaryOp(ANode: TUnaryOpNode);
begin
  Visit(ANode.Expr);
  FOutputFile.WriteLine('; UNARY OP ' + IntToStr(Ord(ANode.Op)));
end;

procedure TCodeGenerator.VisitVar(ANode: TIdentifier);
begin
  FOutputFile.WriteLine('PUSH @' + ANode.Name);
end;

procedure TCodeGenerator.VisitIntegerLiteral(ANode: TIntegerLiteralNode);
begin
  FOutputFile.WriteLine('PUSH ' + IntToStr(ANode.Value));
end;

procedure TCodeGendestructor TCodeGenerator.Destroy;
begin
  FOutputFile.Free;
  inherited Destroy;
end;

procedure TCodeGenerator.VisitRealLiteral(ANode: TRealLiteralNode);
begin
  FOutputFile.WriteLine('PUSH_REAL ' + FloatToStr(ANode.Value));
end;

procedure TCodeGenerator.VisitStringLiteral(ANode: TStringLiteralNode);
begin
  FOutputFile.WriteLine('PUSH_STRING "' + ANode.Value + '"');
end;

procedure TCodeGenerator.VisitNil(ANode: TNilNode);
begin
    FOutputFile.WriteLine('PUSH 0');
end;

procedure TCodeGenerator.VisitClass(ANode: TClassNode);
begin
    FOutputFile.WriteLine('; CLASS ' + ANode.TypeName);
end;

procedure TCodeGenerator.VisitMethod(ANode: TMethodNode);
begin
    FOutputFile.WriteLine('; METHOD placeholder');
end;

procedure TCodeGenerator.VisitField(ANode: TFieldNode);
begin
    FOutputFile.WriteLine('; FIELD placeholder');
end;

procedure TCodeGenerator.VisitInherited(ANode: TInheritedNode);
begin
    FOutputFile.WriteLine('; INHERITED call');
end;

procedure TCodeGenerator.VisitSelf(ANode: TSelfNode);
begin
    FOutputFile.WriteLine('; SELF reference');
end;

procedure TCodeGenerator.VisitTypeSpec(ANode: TTypeSpecNode);
begin
    FOutputFile.WriteLine('; TYPE SPEC ' + ANode.TypeName);
end;

procedure TCodeGenerator.VisitArrayType(ANode: TArrayTypeNode);
begin
    FOutputFile.WriteLine('; ARRAY TYPE');
end;

procedure TCodeGenerator.VisitRecordType(ANode: TRecordTypeNode);
begin
    FOutputFile.WriteLine('; RECORD TYPE');
end;

procedure TCodeGenerator.VisitSetType(ANode: TSetTypeNode);
begin
    FOutputFile.WriteLine('; SET TYPE');
end;

procedure TCodeGenerator.VisitFileType(ANode: TFileTypeNode);
begin
    FOutputFile.WriteLine('; FILE TYPE');
end;

procedure TCodeGenerator.VisitPointerType(ANode: TPointerTypeNode);
begin
    FOutputFile.WriteLine('; POINTER TYPE');
end;

procedure TCodeGenerator.VisitEnumType(ANode: TEnumTypeNode);
begin
    FOutputFile.WriteLine('; ENUM TYPE');
end;

procedure TCodeGenerator.VisitSubrangeType(ANode: TSubrangeTypeNode);
begin
    FOutputFile.WriteLine('; SUBRANGE TYPE');
end;


end.
