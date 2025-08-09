unit codegen;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ast, strutils;

type
  TCodeGenerator = class
  private
    FOutput: TStringList;
    procedure Visit(ANode: TASTNode);
    procedure VisitProgram(ANode: TProgramNode);
    procedure VisitBlock(ANode: TBlockNode);
    procedure VisitVarDecl(ANode: TVarDeclNode);
    procedure VisitClass(ANode: TClassNode);
    procedure VisitCompoundStatement(ANode: TCompoundStatementNode);
    procedure VisitAssignment(ANode: TAssignmentNode);
    procedure VisitExpressionStatement(ANode: TExpressionStatementNode);
    procedure VisitExpression(ANode: TExpressionNode);
    procedure VisitVarAccess(ANode: TVarAccessNode);
    procedure VisitMethodCall(ANode: TMethodCallNode);
    procedure VisitMemberAccess(ANode: TMemberAccessNode);
    procedure VisitBinaryOp(ANode: TBinaryOpNode);
    procedure VisitIntegerLiteral(ANode: TIntegerLiteralNode);
    procedure VisitStringLiteral(ANode: TStringLiteralNode);
    procedure VisitIf(ANode: TIfNode);
    procedure VisitWhile(ANode: TWhileNode);
    procedure VisitRepeat(ANode: TRepeatNode);
    procedure VisitSubroutineDecl(ANode: TSubroutineDeclNode);
  public
    constructor Create;
    destructor Destroy; override;
    function Generate(AProgramNode: TProgramNode): string;
  end;

implementation

constructor TCodeGenerator.Create;
begin
  FOutput := TStringList.Create;
end;

destructor TCodeGenerator.Destroy;
begin
  FOutput.Free;
  inherited;
end;

procedure TCodeGenerator.Visit(ANode: TASTNode);
begin
  if not Assigned(ANode) then Exit;

  if ANode is TProgramNode then VisitProgram(TProgramNode(ANode))
  else if ANode is TBlockNode then VisitBlock(TBlockNode(ANode))
  else if ANode is TVarDeclNode then VisitVarDecl(TVarDeclNode(ANode))
  else if ANode is TClassNode then VisitClass(TClassNode(ANode))
  else if ANode is TSubroutineDeclNode then VisitSubroutineDecl(TSubroutineDeclNode(ANode))
  else if ANode is TCompoundStatementNode then VisitCompoundStatement(TCompoundStatementNode(ANode))
  else if ANode is TAssignmentNode then VisitAssignment(TAssignmentNode(ANode))
  else if ANode is TExpressionStatementNode then VisitExpressionStatement(TExpressionStatementNode(ANode))
  else if ANode is TIfNode then VisitIf(TIfNode(ANode))
  else if ANode is TWhileNode then VisitWhile(TWhileNode(ANode))
  else if ANode is TRepeatNode then VisitRepeat(TRepeatNode(ANode))
  else if ANode is TBinaryOpNode then VisitBinaryOp(TBinaryOpNode(ANode))
  else if ANode is TMethodCallNode then VisitMethodCall(TMethodCallNode(ANode))
  else if ANode is TMemberAccessNode then VisitMemberAccess(TMemberAccessNode(ANode))
  else if ANode is TVarAccessNode then VisitVarAccess(TVarAccessNode(ANode))
  else if ANode is TIntegerLiteralNode then VisitIntegerLiteral(TIntegerLiteralNode(ANode))
  else if ANode is TStringLiteralNode then VisitStringLiteral(TStringLiteralNode(ANode))
  else if ANode is TExpressionNode then VisitExpression(TExpressionNode(ANode))
  else if ANode is TStatementNode then { Do nothing for generic/empty statements }
  else
    raise Exception.CreateFmt('Unknown AST node type: %s', [ANode.ClassName]);
end;

procedure TCodeGenerator.VisitProgram(ANode: TProgramNode);
begin
  FOutput.Add('; Program: ' + ANode.ProgramName);
  FOutput.Add('.CODE');
  FOutput.Add('  JMP main');
  FOutput.Add('');
  Visit(ANode.Block);
end;

procedure TCodeGenerator.VisitBlock(ANode: TBlockNode);
var
  i: Integer;
begin
  FOutput.Add('.DATA');
  for i := 0 to ANode.DeclarationCount - 1 do
    Visit(ANode.GetDeclaration(i));
  FOutput.Add('');
  FOutput.Add('.TEXT');
  FOutput.Add('main:');
  Visit(ANode.CompoundStatement);
  FOutput.Add('  HALT');
end;

procedure TCodeGenerator.VisitVarDecl(ANode: TVarDeclNode);
var
    size: Integer;
begin
    if not Assigned(ANode.VarType) then Exit; // Should not happen in a valid AST
    size := 4; // Default to integer size
    if SameText(ANode.VarType.TypeName, 'string') then
        size := 256; // Default string size
    FOutput.Add(Format('  VAR %s, %s, %d', [ANode.VarName, ANode.VarType.TypeName, size]));
end;

procedure TCodeGenerator.VisitClass(ANode: TClassNode);
begin
    FOutput.Add(Format('  ; Class definition for %s', [ANode.FClassName]));
end;

procedure TCodeGenerator.VisitCompoundStatement(ANode: TCompoundStatementNode);
var
  i: Integer;
begin
  FOutput.Add('  ; Compound statement begins');
  for i := 0 to ANode.StatementCount - 1 do
    Visit(ANode.GetStatement(i));
  FOutput.Add('  ; Compound statement ends');
end;

procedure TCodeGenerator.VisitAssignment(ANode: TAssignmentNode);
var
  varName: string;
begin
  // For now, only handle simple variable assignment
  if not (ANode.Left is TVarAccessNode) then
  begin
    FOutput.Add('  ; Code generation for complex assignments not yet supported.');
    Exit;
  end;

  varName := TVarAccessNode(ANode.Left).VarName;

  // Generate code for the right-hand side expression
  Visit(ANode.Right);

  // The result of the RHS is in R0. Store it.
  FOutput.Add(Format('  MOV %s, R0', [varName]));
end;

procedure TCodeGenerator.VisitExpressionStatement(ANode: TExpressionStatementNode);
begin
    FOutput.Add('  ; Expression statement');
    Visit(ANode.Expression);
end;

procedure TCodeGenerator.VisitExpression(ANode: TExpressionNode);
begin
    // Placeholder for expression code generation
end;

procedure TCodeGenerator.VisitVarAccess(ANode: TVarAccessNode);
begin
    // Load the variable's value into the accumulator register R0
    FOutput.Add(Format('  MOV R0, %s', [ANode.VarName]));
end;

procedure TCodeGenerator.VisitMethodCall(ANode: TMethodCallNode);
begin
    FOutput.Add('    ; Method call');
    Visit(ANode.MethodExpr);
    // Visit arguments...
end;

procedure TCodeGenerator.VisitMemberAccess(ANode: TMemberAccessNode);
begin
    FOutput.Add('    ; Member access');
    Visit(ANode.BaseObject);
end;

procedure TCodeGenerator.VisitBinaryOp(ANode: TBinaryOpNode);
var
  opStr: string;
begin
    // Generate code for the right operand first
    Visit(ANode.Right);
    // Push the result (in R0) onto the stack
    FOutput.Add('  PUSH R0');
    // Generate code for the left operand
    Visit(ANode.Left);
    // Pop the right operand's value into R1
    FOutput.Add('  POP R1');

    case ANode.Op of
        opPlus: opStr := 'ADD';
        opMinus: opStr := 'SUB';
        opStar: opStr := 'MUL';
        opSlash: opStr := 'DIV';
    else
        opStr := '???';
    end;

    // The result of the left operand is in R0.
    // Perform the operation: R0 = R0 op R1
    FOutput.Add(Format('  %s R0, R0, R1', [opStr]));
end;

procedure TCodeGenerator.VisitIntegerLiteral(ANode: TIntegerLiteralNode);
begin
    // Load the integer literal into the accumulator register R0
    FOutput.Add(Format('  MOV R0, #%d', [ANode.Value]));
end;

procedure TCodeGenerator.VisitStringLiteral(ANode: TStringLiteralNode);
begin
    FOutput.Add(Format('    ; String literal: %s', [ANode.Value]));
end;

function TCodeGenerator.Generate(AProgramNode: TProgramNode): string;
begin
  FOutput.Clear;
  Visit(AProgramNode);
  Result := FOutput.Text;
end;

procedure TCodeGenerator.VisitIf(ANode: TIfNode);
begin
    FOutput.Add('  ; If statement');
    Visit(ANode.Condition);
    Visit(ANode.ThenStatement);
    if Assigned(ANode.ElseStatement) then
        Visit(ANode.ElseStatement);
end;

procedure TCodeGenerator.VisitWhile(ANode: TWhileNode);
begin
    FOutput.Add('  ; While loop');
    Visit(ANode.Condition);
    Visit(ANode.Body);
end;

procedure TCodeGenerator.VisitRepeat(ANode: TRepeatNode);
begin
    FOutput.Add('  ; Repeat loop');
    // Visit statements...
    Visit(ANode.UntilCondition);
end;

procedure TCodeGenerator.VisitSubroutineDecl(ANode: TSubroutineDeclNode);
begin
    FOutput.Add(Format('  ; Subroutine declaration: %s', [ANode.SubroutineName]));
    Visit(ANode.Body);
end;

end.
