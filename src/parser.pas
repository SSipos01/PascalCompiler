unit parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lexer, ast, symboltable;

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FSymbolTable: TSymbolTable;
    FHasErrors: Boolean;
    FErrorMessages: TStringList;
    procedure Eat(ATokenType: TTokenType);
    procedure Error(AMessage: string);

    // Parsing methods
    function ParseProgram: TProgramNode;
    function ParseUnit: TUnitNode;
    function ParseBlock: TBlockNode;
    function ParseDeclarations: TList;
    procedure ParseConstDeclarations(ADeclarations: TList);
    procedure ParseTypeDeclarations(ADeclarations: TList);
    procedure ParseVarDeclarations(ADeclarations: TList);
    procedure ParseSubroutineDeclarations(ADeclarations: TList);
    function ParseFormalParameters: TList;
    function ParseStatement: TStatementNode;
    function ParseStatementList(ATerminator: TTokenType): TList;
    function ParseAssignmentOrProcCallStatement: TStatementNode;
    function ParseIfStatement: TIfNode;
    function ParseForStatement: TForNode;
    function ParseWhileStatement: TWhileNode;
    function ParseRepeatStatement: TRepeatNode;
    function ParseCaseStatement: TCaseStatementNode;
    function ParseAsmStatement: TAsmNode;

    function ParseExpression: TExpressionNode;
    function ParseSimpleExpression: TExpressionNode;
    function ParseTerm: TExpressionNode;
    function ParseFactor: TExpressionNode;
    function ParsePostfixExpression(AInitialNode: TExpressionNode): TExpressionNode;
    function ParseVariableOrFunctionCall: TExpressionNode;


    function ParseTypeSpec: TTypeSpecNode;
    function ParseArrayType: TArrayTypeNode;
    function ParseRecordType: TRecordTypeNode;
    function ParseSetType: TSetTypeNode;
    function ParseFileType: TFileTypeNode;
    function ParsePointerType: TPointerTypeNode;
    function ParseEnumType: TEnumTypeNode;
    function ParseSubrangeOrIdent: TTypeSpecNode;
    function ParseIdentifierList: TList;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    function Parse: TProgramNode;
    property HasErrors: Boolean read FHasErrors;
    property ErrorMessages: TStringList read FErrorMessages;
  end;

implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FCurrentToken := FLexer.GetNextToken;
  FSymbolTable := TSymbolTable.Create;
  FHasErrors := False;
  FErrorMessages := TStringList.Create;
end;

destructor TParser.Destroy;
begin
  FSymbolTable.Free;
  FErrorMessages.Free;
  inherited Destroy;
end;

procedure TParser.Error(AMessage: string);
var
  ErrorMsg: string;
begin
  FHasErrors := True;
  ErrorMsg := Format('Parser Error at Line %d, Col %d: %s (Token: %s, Value: ''%s'')',
    [FCurrentToken.Line, FCurrentToken.Column, AMessage, FCurrentToken.Value, FCurrentToken.Value]);
  FErrorMessages.Add(ErrorMsg);
  WriteLn(ErrorMsg);
end;

procedure TParser.Eat(ATokenType: TTokenType);
begin
  if FCurrentToken.TokenType = ATokenType then
    FCurrentToken := FLexer.GetNextToken
  else
  begin
    Error(Format('Expected token %s but got %s', [FCurrentToken.Value, FCurrentToken.Value]));
    // Basic error recovery: try to advance
    FCurrentToken := FLexer.GetNextToken;
  end;
end;

function TParser.ParseIdentifierList: TList;
begin
  Result := TList.Create;
  Result.Add(TIdentifier.Create(FCurrentToken.Value));
  Eat(tkIdentifier);
  while FCurrentToken.TokenType = tkComma do
  begin
    Eat(tkComma);
    Result.Add(TIdentifier.Create(FCurrentToken.Value));
    Eat(tkIdentifier);
  end;
end;

function TParser.ParseTypeSpec: TTypeSpecNode;
begin
  Result := nil;
  case FCurrentToken.TokenType of
    tkArray: Result := ParseArrayType;
    tkRecord: Result := ParseRecordType;
    tkSet: Result := ParseSetType;
    tkFile: Result := ParseFileType;
    tkCaret: Result := ParsePointerType;
    tkLParen: Result := ParseEnumType;
    tkIdentifier, tkIntegerLiteral, tkStringLiteral:
      begin
        // Could be a type identifier or a subrange
        Result := ParseSubrangeOrIdent;
      end;
    else
        // It could be a simple type identifier
        if FCurrentToken.TokenType = tkIdentifier then
        begin
            Result := TTypeSpecNode.Create(ntTypeSpec, FCurrentToken.Value);
            Eat(tkIdentifier);
        end
        else
            Error('Expected a type specification.');
  end;
end;

function TParser.ParseSubrangeOrIdent: TTypeSpecNode;
var
    ident: TIdentifier;
    low, high: TExpressionNode;
begin
    // This is tricky. It can be `MyType` or `1..10` or `'a'..'z'`.
    // We parse the first part as an expression.
    low := ParseFactor; // Use ParseFactor to handle literals and identifiers

    if FCurrentToken.TokenType = tkDotDot then
    begin
        Eat(tkDotDot);
        high := ParseFactor;
        Result := TSubrangeTypeNode.Create(TSubrangeNode.Create(low, high));
    end
    else
    begin
        // If there's no '..', it must have been a type identifier.
        // We need to check if `low` is actually a TIdentifier node.
        if low is TIdentifier then
        begin
            Result := TTypeSpecNode.Create(ntTypeSpec, TIdentifier(low).Name);
            low.Free; // Free the temporary node
        end
        else
        begin
            Error('Expected a type identifier but found an expression.');
            low.Free;
            Result := TTypeSpecNode.Create(ntTypeSpec, 'ErrorType');
        end;
    end;
end;

function TParser.ParseArrayType: TArrayTypeNode;
var
  indexTypes: TList;
  elementType: TTypeSpecNode;
begin
  Eat(tkArray);
  indexTypes := TList.Create;
  if FCurrentToken.TokenType = tkLBracket then
  begin
    Eat(tkLBracket);
    indexTypes.Add(ParseTypeSpec);
    while FCurrentToken.TokenType = tkComma do
    begin
      Eat(tkComma);
      indexTypes.Add(ParseTypeSpec);
    end;
    Eat(tkRBracket);
  end else
  begin
    // Support for old style: array [1..10] of ...
    indexTypes.Add(ParseTypeSpec);
  end;

  Eat(tkOf);
  elementType := ParseTypeSpec;
  Result := TArrayTypeNode.Create(indexTypes, elementType);
end;

function TParser.ParseRecordType: TRecordTypeNode;
var
  fields: TList;
  fieldNames: TList;
  fieldType: TTypeSpecNode;
begin
  Eat(tkRecord);
  fields := TList.Create;
  while FCurrentToken.TokenType <> tkEnd do
  begin
    // For now, not handling variant records (`case ... of`)
    fieldNames := ParseIdentifierList;
    Eat(tkColon);
    fieldType := ParseTypeSpec;
    fields.Add(TRecordFieldNode.Create(fieldNames, fieldType));
    Eat(tkSemi);
  end;
  Eat(tkEnd);
  Result := TRecordTypeNode.Create(fields);
end;

function TParser.ParseSetType: TSetTypeNode;
var
  baseType: TTypeSpecNode;
begin
  Eat(tkSet);
  Eat(tkOf);
  baseType := ParseTypeSpec;
  Result := TSetTypeNode.Create(baseType);
end;

function TParser.ParseFileType: TFileTypeNode;
var
  compType: TTypeSpecNode;
begin
  Eat(tkFile);
  compType := nil;
  if FCurrentToken.TokenType = tkOf then
  begin
    Eat(tkOf);
    compType := ParseTypeSpec;
  end;
  Result := TFileTypeNode.Create(compType);
end;

function TParser.ParsePointerType: TPointerTypeNode;
var
  baseType: TIdentifier;
begin
  Eat(tkCaret);
  baseType := TIdentifier.Create(FCurrentToken.Value);
  Eat(tkIdentifier);
  Result := TPointerTypeNode.Create(baseType);
end;

function TParser.ParseEnumType: TEnumTypeNode;
var
  values: TList;
begin
  Eat(tkLParen);
  values := ParseIdentifierList;
  Eat(tkRParen);
  Result := TEnumTypeNode.Create(values);
end;

function TParser.ParseVariableOrFunctionCall: TExpressionNode;
var
    identName: string;
    node: TExpressionNode;
    args: TList;
begin
    identName := FCurrentToken.Value;
    Eat(tkIdentifier);
    node := TIdentifier.Create(identName);

    // This is the entry point for postfix expressions
    Result := ParsePostfixExpression(node);
end;

function TParser.ParsePostfixExpression(AInitialNode: TExpressionNode): TExpressionNode;
var
    node: TExpressionNode;
    args: TList;
    indices: TList;
    member: TIdentifier;
begin
    node := AInitialNode;
    while True do
    begin
        case FCurrentToken.TokenType of
            tkLParen: // Function or method call
            begin
                Eat(tkLParen);
                args := TList.Create;
                if FCurrentToken.TokenType <> tkRParen then
                begin
                    args.Add(ParseExpression);
                    while FCurrentToken.TokenType = tkComma do
                    begin
                        Eat(tkComma);
                        args.Add(ParseExpression);
                    end;
                end;
                Eat(tkRParen);
                node := TProcCallNode.Create(node, args);
            end;
            tkLBracket: // Array indexing
            begin
                Eat(tkLBracket);
                indices := TList.Create;
                indices.Add(ParseExpression);
                while FCurrentToken.TokenType = tkComma do
                begin
                    Eat(tkComma);
                    indices.Add(ParseExpression);
                end;
                Eat(tkRBracket);
                // We need a proper ArrayAccessNode in AST
                // For now, fake it with a binary op
                node := TBinaryOpNode.Create(node, opDot, TIdentifier.Create('ArrayAccess'));
            end;
            tkDot: // Member access
            begin
                Eat(tkDot);
                member := TIdentifier.Create(FCurrentToken.Value);
                Eat(tkIdentifier);
                node := TBinaryOpNode.Create(node, opDot, member);
            end;
            tkCaret: // Pointer dereference
            begin
                Eat(tkCaret);
                node := TUnaryOpNode.Create(opDeref, node);
            end;
        else
            Break; // No more postfix operators
        end;
    end;
    Result := node;
end;


function TParser.ParseFactor: TExpressionNode;
var
    node: TExpressionNode;
    op: TOperatorType;
begin
    case FCurrentToken.TokenType of
      tkIdentifier:
        Result := ParseVariableOrFunctionCall;
      tkIntegerLiteral:
        begin
          Result := TIntegerLiteralNode.Create(StrToInt(FCurrentToken.Value));
          Eat(tkIntegerLiteral);
        end;
      tkRealLiteral:
        begin
          Result := TRealLiteralNode.Create(StrToFloat(FCurrentToken.Value));
          Eat(tkRealLiteral);
        end;
      tkStringLiteral:
        begin
          Result := TStringLiteralNode.Create(FCurrentToken.Value);
          Eat(tkStringLiteral);
        end;
      tkLParen:
        begin
          Eat(tkLParen);
          Result := ParseExpression;
          Eat(tkRParen);
        end;
      tkNil:
        begin
          Result := TNilNode.Create;
          Eat(tkNil);
        end;
      tkInherited:
        begin
            Result := TInheritedNode.Create;
            Eat(tkInherited);
        end;
      tkSelf:
        begin
            Result := TSelfNode.Create;
            Eat(tkSelf);
        end;
      tkPlus:
        begin
            Eat(tkPlus);
            Result := TUnaryOpNode.Create(opPlus, ParseFactor);
        end;
      tkMinus:
        begin
            Eat(tkMinus);
            Result := TUnaryOpNode.Create(opMinus, ParseFactor);
        end;
      // Add other unary operators like NOT if necessary
    else
      Error('Unexpected token in factor');
      Result := nil;
    end;
end;

function TParser.ParseTerm: TExpressionNode;
var
    node: TExpressionNode;
    opToken: TTokenType;
    op: TOperatorType;
begin
    node := ParseFactor;
    while FCurrentToken.TokenType in [tkStar, tkSlash] do
    begin
        opToken := FCurrentToken.TokenType;
        Eat(opToken);
        case opToken of
            tkStar: op := opStar;
            tkSlash: op := opSlash;
            // Add other multiplicative ops like DIV, MOD, AND
        else
            op := opStar; // Should not happen
        end;
        node := TBinaryOpNode.Create(node, op, ParseFactor);
    end;
    Result := node;
end;

function TParser.ParseSimpleExpression: TExpressionNode;
var
    node: TExpressionNode;
    opToken: TTokenType;
    op: TOperatorType;
begin
    node := ParseTerm;
    while FCurrentToken.TokenType in [tkPlus, tkMinus] do
    begin
        opToken := FCurrentToken.TokenType;
        Eat(opToken);
        case opToken of
            tkPlus: op := opPlus;
            tkMinus: op := opMinus;
            // Add other additive ops like OR, XOR
        else
            op := opPlus; // Should not happen
        end;
        node := TBinaryOpNode.Create(node, op, ParseTerm);
    end;
    Result := node;
end;

function TParser.ParseExpression: TExpressionNode;
var
    node: TExpressionNode;
    opToken: TTokenType;
    op: TOperatorType;
begin
    node := ParseSimpleExpression;
    while FCurrentToken.TokenType in [tkEqual, tkNotEqual, tkLess, tkLessEqual, tkGreater, tkGreaterEqual] do
    begin
        opToken := FCurrentToken.TokenType;
        Eat(opToken);
        case opToken of
            tkEqual: op := opEqual;
            tkNotEqual: op := opNotEqual;
            tkLess: op := opLess;
            tkLessEqual: op := opLessEqual;
            tkGreater: op := opGreater;
            tkGreaterEqual: op := opGreaterEqual;
            // Add IN here
        else
            op := opEqual; // Should not happen
        end;
        node := TBinaryOpNode.Create(node, op, ParseSimpleExpression);
    end;
    Result := node;
end;

function TParser.ParseAssignmentOrProcCallStatement: TStatementNode;
var
  left, right: TExpressionNode;
begin
    left := ParseExpression;

    if FCurrentToken.TokenType = tkAssign then
    begin
        Eat(tkAssign);
        right := ParseExpression;
        Result := TAssignNode.Create(left, right);
    end
    else
    begin
        // If it's not an assignment, it's a standalone expression used as a statement (e.g., a procedure call)
        if left is TProcCallNode then
            Result := TExpressionStatementNode.Create(left)
        else
        begin
            Error('This expression cannot be used as a statement.');
            left.Free;
            Result := nil;
        end;
    end;
end;

function TParser.ParseIfStatement: TIfNode;
var
  condition: TExpressionNode;
  thenBranch, elseBranch: TStatementNode;
begin
  Eat(tkIf);
  condition := ParseExpression;
  Eat(tkThen);
  thenBranch := ParseStatement;
  elseBranch := nil;
  if FCurrentToken.TokenType = tkElse then
  begin
    Eat(tkElse);
    elseBranch := ParseStatement;
  end;
  Result := TIfNode.Create(condition, thenBranch, elseBranch);
end;

function TParser.ParseForStatement: TForNode;
var
  loopVar: TIdentifier;
  start, finish: TExpressionNode;
  isDownTo: Boolean;
  body: TStatementNode;
begin
    Eat(tkFor);
    loopVar := TIdentifier.Create(FCurrentToken.Value);
    Eat(tkIdentifier);
    Eat(tkAssign);
    start := ParseExpression;
    isDownTo := False;
    if FCurrentToken.TokenType = tkTo then
    begin
        Eat(tkTo);
    end
    else if FCurrentToken.TokenType = tkDownTo then
    begin
        isDownTo := True;
        Eat(tkDownTo);
    end
    else
        Error('Expected TO or DOWNTO in for loop');
    finish := ParseExpression;
    Eat(tkDo);
    body := ParseStatement;
    Result := TForNode.Create(loopVar, start, finish, isDownTo, body);
end;

function TParser.ParseWhileStatement: TWhileNode;
var
    cond: TExpressionNode;
    body: TStatementNode;
begin
    Eat(tkWhile);
    cond := ParseExpression;
    Eat(tkDo);
    body := ParseStatement;
    Result := TWhileNode.Create(cond, body);
end;

function TParser.ParseRepeatStatement: TRepeatNode;
var
    cond: TExpressionNode;
    stmts: TList;
begin
    Eat(tkRepeat);
    stmts := ParseStatementList(tkUntil);
    Eat(tkUntil);
    cond := ParseExpression;
    Result := TRepeatNode.Create(cond, stmts);
end;

function TParser.ParseCaseStatement: TCaseStatementNode;
var
    expr: TExpressionNode;
    cases: TList;
    elseStmts: TList;
    caseValues: TList;
    stmt: TStatementNode;
begin
    Eat(tkCase);
    expr := ParseExpression;
    Eat(tkOf);
    cases := TList.Create;
    elseStmts := nil;
    while FCurrentToken.TokenType <> tkEnd do
    begin
        if FCurrentToken.TokenType = tkElse then
            break;

        caseValues := TList.Create;
        // Parse case labels
        while FCurrentToken.TokenType <> tkColon do
        begin
            caseValues.Add(ParseExpression); // Simple values for now
            if FCurrentToken.TokenType = tkComma then
                Eat(tkComma)
            else
                break;
        end;

        Eat(tkColon);
        stmt := ParseStatement;
        cases.Add(TCaseBranchNode.Create(caseValues, stmt));
        if FCurrentToken.TokenType = tkSemi then
            Eat(tkSemi);
    end;

    if FCurrentToken.TokenType = tkElse then
    begin
        Eat(tkElse);
        elseStmts := ParseStatementList(tkEnd);
    end;

    Eat(tkEnd);
    Result := TCaseStatementNode.Create(expr, cases, elseStmts);
end;

function TParser.ParseAsmStatement: TAsmNode;
var
  lines: TStringList;
begin
  Eat(tkAsm);
  lines := TStringList.Create;
  // This is a simplification. Real asm parsing is complex.
  // We'll just consume tokens until we find 'end'.
  while FCurrentToken.TokenType <> tkEnd do
  begin
    lines.Add(FCurrentToken.Value);
    FCurrentToken := FLexer.GetNextToken; // Advance without eating
  end;
  Eat(tkEnd);
  Result := TAsmNode.Create(lines);
end;

function TParser.ParseStatement: TStatementNode;
begin
  case FCurrentToken.TokenType of
    tkIdentifier: Result := ParseAssignmentOrProcCallStatement;
    tkBegin:
      begin
        Eat(tkBegin);
        Result := TCompoundStatementNode.Create(ParseStatementList(tkEnd));
        Eat(tkEnd);
      end;
    tkIf: Result := ParseIfStatement;
    tkFor: Result := ParseForStatement;
    tkWhile: Result := ParseWhileStatement;
    tkRepeat: Result := ParseRepeatStatement;
    tkCase: Result := ParseCaseStatement;
    tkAsm: Result := ParseAsmStatement;
  else
    // Empty statement
    Result := TStatementNode.Create(ntBlock); // Placeholder for empty
  end;
end;

function TParser.ParseStatementList(ATerminator: TTokenType): TList;
begin
  Result := TList.Create;

  // Don't parse anything if the list is immediately terminated (e.g. empty else block)
  if FCurrentToken.TokenType = ATerminator then
    Exit;

  Result.Add(ParseStatement); // Parse the first statement

  // Loop as long as there are semicolons separating statements
  while FCurrentToken.TokenType = tkSemi do
  begin
    Eat(tkSemi);
    // If we are at the terminator, it was a trailing semicolon. Stop.
    if FCurrentToken.TokenType = ATerminator then
      break;
    Result.Add(ParseStatement);
  end;
end;

function TParser.ParseFormalParameters: TList;
var
  paramNames: TList;
  paramType: TTypeSpecNode;
begin
  Result := TList.Create;
  if FCurrentToken.TokenType <> tkLParen then
    Exit;

  Eat(tkLParen);
  if FCurrentToken.TokenType = tkRParen then
  begin
      Eat(tkRParen);
      Exit;
  end;

  repeat
    // not handling var, const, out parameters yet
    paramNames := ParseIdentifierList;
    Eat(tkColon);
    paramType := ParseTypeSpec;
    Result.Add(TVarDeclNode.Create(paramNames, paramType));
    if FCurrentToken.TokenType = tkSemi then
        Eat(tkSemi)
    else
        break;
  until FCurrentToken.TokenType = tkRParen;
  Eat(tkRParen);
end;

procedure TParser.ParseSubroutineDeclarations(ADeclarations: TList);
var
  subroutineNode: TSubroutineNode;
  name: TIdentifier;
  params: TList;
  returnType: TTypeSpecNode;
  block: TBlockNode;
  isOverride, isConstructor, isDestructor: Boolean;
begin
  while FCurrentToken.TokenType in [tkProcedure, tkFunction, tkConstructor, tkDestructor] do
  begin
    isConstructor := FCurrentToken.TokenType = tkConstructor;
    isDestructor := FCurrentToken.TokenType = tkDestructor;

    Eat(FCurrentToken.TokenType);

    name := TIdentifier.Create(FCurrentToken.Value);
    Eat(tkIdentifier);

    FSymbolTable.EnterScope(name.Name);

    params := ParseFormalParameters;
    returnType := nil;

    if FCurrentToken.TokenType = tkColon then
    begin
        Eat(tkColon);
        returnType := ParseTypeSpec;
    end;

    Eat(tkSemi);

    isOverride := false;
    if FCurrentToken.TokenType = tkOverride then
    begin
        isOverride := true;
        Eat(tkOverride);
        Eat(tkSemi);
    end;

    block := ParseBlock; // Could be forward declaration
    Eat(tkSemi);

    if isConstructor then
        subroutineNode := TConstructorNode.Create(name, params, block, isOverride)
    else if isDestructor then
        subroutineNode := TDestructorNode.Create(name, params, block, isOverride)
    else if Assigned(returnType) then
        subroutineNode := TFunctionNode.Create(name, params, returnType, block)
    else
        subroutineNode := TProcedureNode.Create(name, params, block);

    ADeclarations.Add(subroutineNode);
    FSymbolTable.LeaveScope;
  end;
end;

procedure TParser.ParseVarDeclarations(ADeclarations: TList);
var
  varNames: TList;
  varType: TTypeSpecNode;
begin
  Eat(tkVar);
  while FCurrentToken.TokenType = tkIdentifier do
  begin
    varNames := ParseIdentifierList;
    Eat(tkColon);
    varType := ParseTypeSpec;
    ADeclarations.Add(TVarDeclNode.Create(varNames, varType));
    Eat(tkSemi);
  end;
end;

procedure TParser.ParseConstDeclarations(ADeclarations: TList);
var
  constName: TIdentifier;
  constValue: TExpressionNode;
begin
    Eat(tkConst);
    while FCurrentToken.TokenType = tkIdentifier do
    begin
        constName := TIdentifier.Create(FCurrentToken.Value);
        Eat(tkIdentifier);
        Eat(tkEqual);
        constValue := ParseExpression;
        ADeclarations.Add(TConstDeclNode.Create(constName, constValue));
        Eat(tkSemi);
    end;
end;

procedure TParser.ParseTypeDeclarations(ADeclarations: TList);
var
  typeName: TIdentifier;
  typeSpec: TTypeSpecNode;
  parent: TIdentifier;
  members: TList;
  fieldNames: TList;
  fieldType: TTypeSpecNode;
  subroutineNode: TSubroutineNode;
begin
  Eat(tkType);
  while FCurrentToken.TokenType = tkIdentifier do
  begin
    typeName := TIdentifier.Create(FCurrentToken.Value);
    Eat(tkIdentifier);
    Eat(tkEqual);

    case FCurrentToken.TokenType of
        tkClass, tkObject:
        begin
            Eat(FCurrentToken.TokenType);
            parent := nil;
            if FCurrentToken.TokenType = tkLParen then
            begin
                Eat(tkLParen);
                parent := TIdentifier.Create(FCurrentToken.Value);
                Eat(tkIdentifier);
                Eat(tkRParen);
            end;

            FSymbolTable.EnterScope(typeName.Name);
            members := TList.Create;
            while FCurrentToken.TokenType <> tkEnd do
            begin
                // Simplified: assuming public visibility
                // Could be fields or methods
                if FCurrentToken.TokenType in [tkProcedure, tkFunction, tkConstructor, tkDestructor] then
                begin
                    ParseSubroutineDeclarations(members);
                end
                else if FCurrentToken.TokenType = tkIdentifier then
                begin
                    // Field declaration
                    fieldNames := ParseIdentifierList;
                    Eat(tkColon);
                    fieldType := ParseTypeSpec;
                    Eat(tkSemi);
                    members.Add(TFieldNode.Create(fieldNames, fieldType));
                end
                else
                begin
                    // Could be visibility specifiers like private, protected
                    Eat(FCurrentToken.TokenType);
                end;
            end;
            Eat(tkEnd);
            Eat(tkSemi);
            FSymbolTable.LeaveScope;
            typeSpec := TClassNode.Create(typeName, parent, members);
        end;
    else
      // Any other type
      typeSpec := ParseTypeSpec;
      Eat(tkSemi);
    end;

    ADeclarations.Add(TTypeDeclNode.Create(typeName, typeSpec));
  end;
end;

function TParser.ParseDeclarations: TList;
begin
  Result := TList.Create;
  while True do
  begin
    case FCurrentToken.TokenType of
      tkConst: ParseConstDeclarations(Result);
      tkType: ParseTypeDeclarations(Result);
      tkVar: ParseVarDeclarations(Result);
      tkProcedure, tkFunction, tkConstructor, tkDestructor: ParseSubroutineDeclarations(Result);
    else
      Break;
    end;
  end;
end;

function TParser.ParseBlock: TBlockNode;
var
  declarations: TList;
  statements: TList;
begin
  declarations := ParseDeclarations;

  if FCurrentToken.TokenType = tkBegin then
  begin
    Eat(tkBegin);
    statements := ParseStatementList(tkEnd);
    Eat(tkEnd);
  end else
  begin
    // Handle forward declarations etc. - empty statement list
    statements := TList.Create;
  end;

  Result := TBlockNode.Create(declarations, statements);
end;

function TParser.ParseUnit: TUnitNode;
var
    lUnitName: TIdentifier;
    interfaceDecls, implDecls: TList;
    initBlock, finalBlock: TStatementNode;
begin
    Eat(tkUnit);
    lUnitName := TIdentifier.Create(FCurrentToken.Value);
    Eat(tkIdentifier);
    Eat(tkSemi);

    FSymbolTable.EnterScope(lUnitName.Name);

    Eat(tkInterface);
    // TODO: Parse USES clause
    interfaceDecls := ParseDeclarations;

    Eat(tkImplementation);
    // TODO: Parse USES clause
    implDecls := ParseDeclarations;

    initBlock := nil;
    finalBlock := nil;

    case FCurrentToken.TokenType of
      tkInitialization:
        begin
          Eat(tkInitialization);
          initBlock := TCompoundStatementNode.Create(ParseStatementList(tkFinalization)); // or end
          if FCurrentToken.TokenType = tkFinalization then
          begin
            Eat(tkFinalization);
            finalBlock := TCompoundStatementNode.Create(ParseStatementList(tkEnd));
          end;
        end;
      tkBegin:
        begin
          // This is the old-style initialization block
          initBlock := ParseStatement;
        end;
    end;

    Eat(tkEnd);
    Eat(tkDot);

    FSymbolTable.LeaveScope;
    Result := TUnitNode.Create(lUnitName, interfaceDecls, implDecls, initBlock, finalBlock);
end;

function TParser.ParseProgram: TProgramNode;
var
  programName: TIdentifier;
  usesClause: TList;
  block: TBlockNode;
begin
  if FCurrentToken.TokenType = tkUnit then
  begin
    // This is a special case. We're parsing a unit as if it's the main program.
    // We'll wrap it in a TProgramNode for consistency.
    Result := TProgramNode.Create(TIdentifier.Create('unit_wrapper'), TList.Create, ParseUnit);
    Exit;
  end;


  Eat(tkProgram);
  programName := TIdentifier.Create(FCurrentToken.Value);
  Eat(tkIdentifier);
  Eat(tkSemi);

  FSymbolTable.EnterScope(programName.Name);

  usesClause := TList.Create;
  if FCurrentToken.TokenType = tkUses then
  begin
    Eat(tkUses);
    while FCurrentToken.TokenType = tkIdentifier do
    begin
      usesClause.Add(TIdentifier.Create(FCurrentToken.Value));
      Eat(tkIdentifier);
      if FCurrentToken.TokenType = tkComma then
        Eat(tkComma)
      else
        break;
    end;
    Eat(tkSemi);
  end;

  block := ParseBlock;
  Eat(tkDot);

  FSymbolTable.LeaveScope;
  Result := TProgramNode.Create(programName, usesClause, block);
end;

function TParser.Parse: TProgramNode;
begin
  Result := ParseProgram;
  if FCurrentToken.TokenType <> tkEOF then
    Error('Unexpected tokens at end of file.');
end;

end.
