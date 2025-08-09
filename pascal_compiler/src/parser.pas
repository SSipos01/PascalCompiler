unit parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, lexer, ast, symboltable;

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FSymbolTable: TSymbolTable;
    procedure Eat(ATokenType: TTokenType);
    procedure NextToken;

    procedure ParseUsesClause;
    procedure ParseTypeSection(ADeclarations: TList);
    function ParseTypeDeclaration: TASTNode;
    function ParseClassDeclaration(const AClassName: string): TClassNode;
    procedure ParseClassBody(AClassNode: TClassNode);
    procedure ParseClassMember(AClassNode: TClassNode; var AVisibility: TVisibility);
    function ParseFieldDeclaration(AVisibility: TVisibility): TList;
    function ParseMethodDeclaration(AVisibility: TVisibility): TMethodNode;

    function ParseProgram: TProgramNode;
    function ParseBlock: TBlockNode;
    procedure ParseDeclarationPart(ADeclarations: TList);
    function ParseSubroutineDeclaration: TASTNode;
    procedure ParseVariableDeclarations(ADeclarations: TList);
    function ParseTypeSpec: TTypeNode;
    function ParsePointerType: TTypeNode;
    function ParseArrayType: TTypeNode;
    function ParseSetType: TTypeNode;
    function ParseRecordType: TTypeNode;
    function ParseSubrangeType: TTypeNode;
    function ParseProceduralType: TTypeNode;
    function ParseEnumType: TTypeNode;
    function ParseCompoundStatement: TCompoundStatementNode;
    function ParseStatement: TStatementNode;
    function ParseIdentifierStatement: TStatementNode;
    function ParseIfStatement: TStatementNode;
    function ParseWhileStatement: TStatementNode;
    function ParseRepeatStatement: TStatementNode;
    function ParseCaseStatement: TStatementNode;
    function ParseExpression: TExpressionNode;
    function ParseSimpleExpression: TExpressionNode;
    function ParseAdditiveExpression: TExpressionNode;
    function ParseTerm: TExpressionNode;
    function ParseFactor: TExpressionNode;
  public
    constructor Create(ALexer: TLexer);
    function Parse: TASTNode;
  end;

implementation

constructor TParser.Create(ALexer: TLexer);
begin
  FLexer := ALexer;
  FSymbolTable := TSymbolTable.Create;
  NextToken;
end;

procedure TParser.NextToken;
begin
  FCurrentToken := FLexer.GetNextToken;
  writeln(Format('NextToken: %s (%s)', [GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)), FCurrentToken.Value]));
end;

procedure TParser.Eat(ATokenType: TTokenType);
begin
  writeln(Format('Eating %s, have %s', [GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)), GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType))]));
  if FCurrentToken.TokenType = ATokenType then
    NextToken
  else
    raise Exception.CreateFmt('Expected token %d (%s) but got %d (%s)', [
      Ord(ATokenType), GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)),
      Ord(FCurrentToken.TokenType), GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType))
    ]);
end;

procedure TParser.ParseUsesClause;
begin
    Eat(tkUses);
    // For now, just parse and ignore the unit names
    Eat(tkIdentifier);
    while FCurrentToken.TokenType = tkComma do
    begin
        Eat(tkComma);
        Eat(tkIdentifier);
    end;
    Eat(tkSemicolon);
end;

function TParser.ParseFieldDeclaration(AVisibility: TVisibility): TList;
var
  varName: string;
  varTypeNode, finalTypeNode: TTypeNode;
  decls: TStringList;
  typeSymbol: TSymbol;
begin
  Result := TList.Create;
  decls := TStringList.Create;
  try
    decls.Add(FCurrentToken.Value);
    Eat(tkIdentifier);

    while FCurrentToken.TokenType = tkComma do
    begin
      Eat(tkComma);
      decls.Add(FCurrentToken.Value);
      Eat(tkIdentifier);
    end;

    Eat(tkColon);
    varTypeNode := ParseTypeSpec;

    finalTypeNode := varTypeNode;
    if varTypeNode.ClassType = TTypeNode then
    begin
        typeSymbol := FSymbolTable.Lookup(varTypeNode.TypeName);
        if not Assigned(typeSymbol) then
            raise Exception.CreateFmt('Unknown type: %s', [varTypeNode.TypeName]);
        finalTypeNode := typeSymbol.SymbolType;
        // The original varTypeNode is just a name, free it
        varTypeNode.Free;
    end;

    for varName in decls do
    begin
      if Assigned(FSymbolTable.Lookup(varName, True)) then
        raise Exception.CreateFmt('Duplicate identifier in scope: %s', [varName]);
      // TODO: This shares the finalTypeNode pointer, which will cause a double-free later.
      // A proper implementation needs to clone the node.
      FSymbolTable.Insert(TVarSymbol.Create(varName, finalTypeNode));
      Result.Add(TVarDeclNode.Create(varName, finalTypeNode, AVisibility));
    end;

  finally
    decls.Free;
  end;
  Eat(tkSemicolon);
end;

function TParser.ParseMethodDeclaration(AVisibility: TVisibility): TMethodNode;
var
  node: TMethodNode;
  tokenType: TTokenType;
  mName: string;
begin
  tokenType := FCurrentToken.TokenType;
  if not (tokenType in [tkProcedure, tkFunction, tkConstructor, tkDestructor]) then
    raise Exception.CreateFmt('Expected method keyword but got %s', [FCurrentToken.Value]);

  Eat(tokenType);

  mName := FCurrentToken.Value;
  Eat(tkIdentifier);

  node := TMethodNode.Create(mName, AVisibility);
  node.IsConstructor := tokenType = tkConstructor;
  node.IsDestructor := tokenType = tkDestructor;

  // Consume parameter list if present
  if FCurrentToken.TokenType = tkLParen then
  begin
    Eat(tkLParen);
    while FCurrentToken.TokenType <> tkRParen do
    begin
      NextToken(); // Just consume for now, don't build parameter AST yet
    end;
    Eat(tkRParen);
  end;

  // Consume return type if present (for functions)
  if FCurrentToken.TokenType = tkColon then
  begin
    Eat(tkColon);
    node.ReturnType := ParseTypeSpec;
  end;

  // Consume directives
  while FCurrentToken.TokenType in [tkVirtual, tkOverride, tkOverload] do
  begin
      case FCurrentToken.TokenType of
          tkVirtual: node.Directives := node.Directives + [mdVirtual];
          tkOverride: node.Directives := node.Directives + [mdOverride];
          tkOverload: node.Directives := node.Directives + [mdOverload];
      else
        // This case should not be reached due to the loop condition
      end;
      Eat(FCurrentToken.TokenType);
  end;

  Eat(tkSemicolon);
  Result := node;
end;

procedure TParser.ParseClassMember(AClassNode: TClassNode; var AVisibility: TVisibility);
var
    members: TList;
    i: integer;
begin
    case FCurrentToken.TokenType of
        tkIdentifier: // Field declaration
            begin
                members := ParseFieldDeclaration(AVisibility);
                for i := 0 to members.Count - 1 do
                    AClassNode.Members.Add(members[i]);
                members.Clear; // Clear list, but don't free nodes
                members.Free;
            end;
        tkProcedure, tkFunction, tkConstructor, tkDestructor:
            AClassNode.Members.Add(ParseMethodDeclaration(AVisibility));
    else
        raise Exception.CreateFmt('Unexpected token in class member list: %s', [FCurrentToken.Value]);
    end;
end;

procedure TParser.ParseClassBody(AClassNode: TClassNode);
var
    visibility: TVisibility;
begin
    visibility := vPrivate; // Default for classes
    while FCurrentToken.TokenType <> tkEnd do
    begin
        if FCurrentToken.TokenType = tkPublic then
        begin
            Eat(tkPublic);
            visibility := vPublic;
        end
        else if FCurrentToken.TokenType = tkPrivate then
        begin
            Eat(tkPrivate);
            visibility := vPrivate;
        end
        else if FCurrentToken.TokenType = tkProtected then
        begin
            Eat(tkProtected);
            visibility := vProtected;
        end
        else
            ParseClassMember(AClassNode, visibility);
    end;
end;

function TParser.ParseClassDeclaration(const AClassName: string): TClassNode;
var
    parentClassName: string;
    classSymbol: TClassSymbol;
    parentSymbol: TClassSymbol;
begin
    Eat(tkClass);
    parentClassName := '';
    parentSymbol := nil;
    if FCurrentToken.TokenType = tkLParen then
    begin
        Eat(tkLParen);
        parentClassName := FCurrentToken.Value;
        Eat(tkIdentifier);
        Eat(tkRParen);

        parentSymbol := FSymbolTable.Lookup(parentClassName) as TClassSymbol;
        if not Assigned(parentSymbol) then
            raise Exception.CreateFmt('Unknown parent class: %s', [parentClassName]);
    end;

    classSymbol := TClassSymbol.Create(AClassName, parentSymbol);
    FSymbolTable.Insert(classSymbol);

    if FCurrentToken.TokenType = tkSemicolon then
    begin
        // Forward declaration
        Result := TClassNode.Create(AClassName, '');
        Exit;
    end;

    Result := TClassNode.Create(AClassName, parentClassName);

    FSymbolTable.EnterScope;
    ParseClassBody(Result);
    FSymbolTable.LeaveScope;
    Eat(tkEnd);
end;

function TParser.ParseTypeDeclaration: TASTNode;
var
    typeName: string;
    typeNode: TTypeNode;
begin
    typeName := FCurrentToken.Value;
    Eat(tkIdentifier);
    Eat(tkEqual);

    if FCurrentToken.TokenType = tkClass then
    begin
        Result := ParseClassDeclaration(typeName);
    end
    else
    begin
        typeNode := ParseTypeSpec;
        FSymbolTable.Insert(TSymbol.Create(typeName, typeNode));
        Result := typeNode;
    end;

    Eat(tkSemicolon);
end;

procedure TParser.ParseTypeSection(ADeclarations: TList);
begin
    Eat(tkType);
    while FCurrentToken.TokenType = tkIdentifier do
    begin
        ADeclarations.Add(ParseTypeDeclaration);
    end;
end;

function TParser.ParseFactor: TExpressionNode;
var
    node: TExpressionNode;
    memberName: string;
    callNode: TMethodCallNode;
    arg: TExpressionNode;
begin
    case FCurrentToken.TokenType of
        tkIntegerLiteral:
            begin
                node := TIntegerLiteralNode.Create(StrToInt(FCurrentToken.Value));
                Eat(tkIntegerLiteral);
            end;
        tkFloatLiteral:
            begin
                // For now, we don't have a TFloatLiteralNode, so we'll just consume it
                // and create a placeholder. I should add TFloatLiteralNode to the AST.
                Eat(tkFloatLiteral);
                node := TExpressionNode.Create;
            end;
        tkStringLiteral:
            begin
                node := TStringLiteralNode.Create(FCurrentToken.Value);
                Eat(tkStringLiteral);
            end;
        tkIdentifier:
            begin
                node := TVarAccessNode.Create(FCurrentToken.Value);
                Eat(tkIdentifier);
            end;
        tkLParen:
            begin
                Eat(tkLParen);
                node := ParseExpression;
                Eat(tkRParen);
            end;
    else
        node := nil; // Should raise an error
    end;

    // Handle postfix operations like member access and method calls
    while FCurrentToken.TokenType in [tkDot, tkLParen] do
    begin
        if FCurrentToken.TokenType = tkDot then
        begin
            Eat(tkDot);
            memberName := FCurrentToken.Value;
            Eat(tkIdentifier);
            node := TMemberAccessNode.Create(node, memberName);
        end
        else if FCurrentToken.TokenType = tkLParen then
        begin
            Eat(tkLParen);
            callNode := TMethodCallNode.Create(node);
            if FCurrentToken.TokenType <> tkRParen then
            begin
                // Parse arguments
                arg := ParseExpression;
                callNode.Arguments.Add(arg);
                while FCurrentToken.TokenType = tkComma do
                begin
                    Eat(tkComma);
                    arg := ParseExpression;
                    callNode.Arguments.Add(arg);
                end;
            end;
            Eat(tkRParen);
            node := callNode;
        end;
    end;

    Result := node;
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
        end;
        node := TBinaryOpNode.Create(node, op, ParseFactor);
    end;
    Result := node;
end;

function TParser.ParseAdditiveExpression: TExpressionNode;
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
        end;
        node := TBinaryOpNode.Create(node, op, ParseTerm);
    end;
    Result := node;
end;

function TParser.ParseSimpleExpression: TExpressionNode;
var
    node: TExpressionNode;
    opToken: TTokenType;
    op: TOperatorType;
begin
    node := ParseAdditiveExpression;
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
        end;
        node := TBinaryOpNode.Create(node, op, ParseAdditiveExpression);
    end;
    Result := node;
end;

function TParser.ParseExpression: TExpressionNode;
begin
    Result := ParseSimpleExpression;
end;

function TParser.ParseIdentifierStatement: TStatementNode;
var
    expr: TExpressionNode;
begin
    expr := ParseExpression;
    if FCurrentToken.TokenType = tkAssign then
    begin
        Eat(tkAssign);
        Result := TAssignmentNode.Create(expr, ParseExpression);
    end
    else
    begin
        Result := TExpressionStatementNode.Create(expr);
    end;
end;

function TParser.ParseIfStatement: TStatementNode;
var
  cond: TExpressionNode;
  thenStmt, elseStmt: TStatementNode;
begin
  Eat(tkIf);
  cond := ParseExpression;
  Eat(tkThen);
  thenStmt := ParseStatement;
  if FCurrentToken.TokenType = tkElse then
  begin
    Eat(tkElse);
    elseStmt := ParseStatement;
  end
  else
  begin
    elseStmt := nil;
  end;
  Result := TIfNode.Create(cond, thenStmt, elseStmt);
end;

function TParser.ParseWhileStatement: TStatementNode;
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

function TParser.ParseRepeatStatement: TStatementNode;
var
  stmts: TList;
  cond: TExpressionNode;
  node: TRepeatNode;
  i: integer;
begin
  Eat(tkRepeat);
  stmts := TList.Create;
  try
    repeat
      stmts.Add(ParseStatement);
      if FCurrentToken.TokenType = tkSemicolon then
        Eat(tkSemicolon);
    until FCurrentToken.TokenType = tkUntil;

    Eat(tkUntil);
    cond := ParseExpression;

    node := TRepeatNode.Create(cond);
    for i := 0 to stmts.Count - 1 do
        node.Statements.Add(stmts[i]);
    stmts.Clear; // We've moved the nodes
    Result := node;
  finally
    stmts.Free;
  end;
end;

function TParser.ParseCaseStatement: TStatementNode;
begin
  Eat(tkCase);
  ParseExpression;
  Eat(tkOf);

  // Consume case labels and statements
  while (FCurrentToken.TokenType <> tkEnd) and (FCurrentToken.TokenType <> tkElse) do
  begin
      // crude consumption of case branches
      while FCurrentToken.TokenType <> tkSemicolon do
          NextToken();
      Eat(tkSemicolon);
  end;

  if FCurrentToken.TokenType = tkElse then
  begin
    Eat(tkElse);
    // consume statements in else part
    while FCurrentToken.TokenType <> tkEnd do
        NextToken();
  end;

  Eat(tkEnd);
  Result := TStatementNode.Create; // Placeholder
end;

function TParser.ParseStatement: TStatementNode;
begin
  case FCurrentToken.TokenType of
    tkBegin:
      Result := ParseCompoundStatement;
    tkIdentifier:
      Result := ParseIdentifierStatement;
    tkIf:
      Result := ParseIfStatement;
    tkWhile:
      Result := ParseWhileStatement;
    tkRepeat:
      Result := ParseRepeatStatement;
    tkCase:
      Result := ParseCaseStatement;
  else
    // Empty statement for now, or could raise error
    Result := TStatementNode.Create;
  end;
end;

function TParser.ParseCompoundStatement: TCompoundStatementNode;
var
  node: TCompoundStatementNode;
begin
  Eat(tkBegin);
  node := TCompoundStatementNode.Create;
  while FCurrentToken.TokenType <> tkEnd do
  begin
    node.AddStatement(ParseStatement);
    if FCurrentToken.TokenType = tkSemicolon then
      Eat(tkSemicolon);
  end;
  Eat(tkEnd);
  Result := node;
end;

function TParser.ParsePointerType: TTypeNode;
begin
    Eat(tkCaret);
    Result := TPointerNode.Create(ParseTypeSpec);
end;

function TParser.ParseArrayType: TTypeNode;
var
    node: TArrayNode;
    elementType: TTypeNode;
begin
    Eat(tkArray);

    // The element type is parsed at the end, so we can't create the node yet.
    // This is tricky. Let's parse the index types first.

    if FCurrentToken.TokenType = tkLBracket then
    begin
        Eat(tkLBracket);
        // We will create a temporary list of index types
        // and attach it to the node later.
        // This is getting too complex for a placeholder.
        // I will just consume the tokens for now.
        while FCurrentToken.TokenType <> tkRBracket do
            NextToken();
        Eat(tkRBracket);
    end;

    Eat(tkOf);
    elementType := ParseTypeSpec;

    node := TArrayNode.Create(elementType);
    // In a real implementation, I would add the parsed index types to node.IndexTypes

    Result := node;
end;

function TParser.ParseSetType: TTypeNode;
begin
    Eat(tkSet);
    Eat(tkOf);
    ParseTypeSpec;
    Result := TTypeNode.Create('set'); // Placeholder
end;

function TParser.ParseRecordType: TTypeNode;
var
    node: TRecordNode;
    fields: TList;
    i: integer;
    nestingLevel: integer;
begin
    Eat(tkRecord);
    node := TRecordNode.Create;

    // Parse fixed part
    while (FCurrentToken.TokenType <> tkEnd) and (FCurrentToken.TokenType <> tkCase) do
    begin
        fields := ParseFieldDeclaration(vPublic);
        for i := 0 to fields.Count - 1 do
            node.Fields.Add(fields[i]);
        fields.Clear;
        fields.Free;
    end;

    // Parse variant part if it exists
    if FCurrentToken.TokenType = tkCase then
    begin
        Eat(tkCase);
        // Consume variant part as placeholder
        nestingLevel := 1;
        while nestingLevel > 0 do
        begin
            if FCurrentToken.TokenType = tkRecord then // nested record
                Inc(nestingLevel)
            else if FCurrentToken.TokenType = tkEnd then
                Dec(nestingLevel);

            if nestingLevel = 0 then break;
            NextToken();
        end;
    end;

    Eat(tkEnd);
    Result := node;
end;

function TParser.ParseSubrangeType: TTypeNode;
begin
    // consume lower bound
    NextToken();
    Eat(tkDotDot);
    // consume upper bound
    NextToken();
    Result := TTypeNode.Create('subrange'); // Placeholder
end;

function TParser.ParseProceduralType: TTypeNode;
begin
    Eat(FCurrentToken.TokenType); // function or procedure

    // Consume parameters
    if FCurrentToken.TokenType = tkLParen then
    begin
        Eat(tkLParen);
        while FCurrentToken.TokenType <> tkRParen do NextToken();
        Eat(tkRParen);
    end;

    // Consume return type
    if FCurrentToken.TokenType = tkColon then
    begin
        Eat(tkColon);
        ParseTypeSpec;
    end;

    Result := TTypeNode.Create('procedural'); // Placeholder
end;

function TParser.ParseEnumType: TTypeNode;
var
    node: TEnumNode;
begin
    Eat(tkLParen);
    node := TEnumNode.Create;

    while FCurrentToken.TokenType <> tkRParen do
    begin
        node.Elements.Add(FCurrentToken.Value);
        Eat(tkIdentifier);
        if FCurrentToken.TokenType = tkComma then
            Eat(tkComma);
    end;

    Eat(tkRParen);
    Result := node;
end;

function TParser.ParseTypeSpec: TTypeNode;
begin
  case FCurrentToken.TokenType of
    tkIdentifier:
      begin
        Result := TTypeNode.Create(FCurrentToken.Value);
        Eat(tkIdentifier);
        if FCurrentToken.TokenType = tkLBracket then
        begin
            Eat(tkLBracket);
            while FCurrentToken.TokenType <> tkRBracket do
                NextToken();
            Eat(tkRBracket);
        end;
      end;
    tkCaret:
      Result := ParsePointerType;
    tkArray:
      Result := ParseArrayType;
    tkSet:
      Result := ParseSetType;
    tkRecord:
      Result := ParseRecordType;
    tkFunction, tkProcedure:
      Result := ParseProceduralType;
    tkLParen:
      Result := ParseEnumType;
    tkIntegerLiteral, tkStringLiteral: // For subranges
      Result := ParseSubrangeType;
  else
    raise Exception.CreateFmt('Unexpected token in type specification: %s', [FCurrentToken.Value]);
  end;
end;

procedure TParser.ParseVariableDeclarations(ADeclarations: TList);
var
  varName: string;
  varTypeNode, finalTypeNode: TTypeNode;
  decls: TStringList;
  typeSymbol: TSymbol;
begin
  Eat(tkVar);
  while FCurrentToken.TokenType = tkIdentifier do
  begin
    decls := TStringList.Create;
    try
      decls.Add(FCurrentToken.Value);
      Eat(tkIdentifier);

      while FCurrentToken.TokenType = tkComma do
      begin
        Eat(tkComma);
        decls.Add(FCurrentToken.Value);
        Eat(tkIdentifier);
      end;

      Eat(tkColon);
      varTypeNode := ParseTypeSpec;

      if FCurrentToken.TokenType = tkEqual then
      begin
          Eat(tkEqual);
          ParseExpression;
      end;

      finalTypeNode := varTypeNode;
      if varTypeNode.ClassType = TTypeNode then
      begin
          typeSymbol := FSymbolTable.Lookup(varTypeNode.TypeName);
          if not Assigned(typeSymbol) then
              raise Exception.CreateFmt('Unknown type: %s', [varTypeNode.TypeName]);
          finalTypeNode := typeSymbol.SymbolType;
          varTypeNode.Free;
      end;

      for varName in decls do
      begin
        if Assigned(FSymbolTable.Lookup(varName, True)) then
          raise Exception.CreateFmt('Duplicate identifier in scope: %s', [varName]);
        // TODO: This shares the finalTypeNode pointer, which will cause a double-free later.
        FSymbolTable.Insert(TVarSymbol.Create(varName, finalTypeNode));
        ADeclarations.Add(TVarDeclNode.Create(varName, finalTypeNode));
      end;

    finally
      decls.Free;
    end;
    Eat(tkSemicolon);
  end;
end;

function TParser.ParseSubroutineDeclaration: TASTNode;
var
    subroutineName: string;
    body: TBlockNode;
begin
    // This function parses both standalone subroutines and method implementations
    // For now, it treats method implementations as standalone for parsing purposes
    // A later semantic analysis step would be needed to link them to classes.

    Eat(FCurrentToken.TokenType); // procedure or function

    subroutineName := FCurrentToken.Value;
    Eat(tkIdentifier);

    if FCurrentToken.TokenType = tkDot then
    begin
        // It's a method implementation, consume and ignore for now
        Eat(tkDot);
        Eat(tkIdentifier);
    end;

    // Consume parameters
    if FCurrentToken.TokenType = tkLParen then
    begin
        Eat(tkLParen);
        while FCurrentToken.TokenType <> tkRParen do NextToken();
        Eat(tkRParen);
    end;

    // Consume return type
    if FCurrentToken.TokenType = tkColon then
    begin
        Eat(tkColon);
        ParseTypeSpec;
    end;

    Eat(tkSemicolon);

    // For now, we assume all subroutines have a body that follows
    FSymbolTable.EnterScope;
    body := ParseBlock; // ParseBlock handles local declarations and a compound statement
    FSymbolTable.LeaveScope;
    Eat(tkSemicolon);

    Result := TSubroutineDeclNode.Create(subroutineName, body);
end;

procedure TParser.ParseDeclarationPart(ADeclarations: TList);
begin
    while FCurrentToken.TokenType in [tkUses, tkType, tkVar, tkProcedure, tkFunction, tkConstructor, tkDestructor] do
    begin
        case FCurrentToken.TokenType of
            tkUses: ParseUsesClause;
            tkType: ParseTypeSection(ADeclarations);
            tkVar: ParseVariableDeclarations(ADeclarations);
            tkProcedure, tkFunction, tkConstructor, tkDestructor:
                ADeclarations.Add(ParseSubroutineDeclaration);
        end;
    end;
end;

function TParser.ParseBlock: TBlockNode;
var
  declarations: TList;
  compoundStatement: TCompoundStatementNode;
  i: integer;
begin
  declarations := TList.Create;
  try
    ParseDeclarationPart(declarations);
    compoundStatement := ParseCompoundStatement;
    Result := TBlockNode.Create(compoundStatement);
    for i := 0 to declarations.Count - 1 do
        Result.AddDeclaration(TASTNode(declarations[i]));
    declarations.Clear; // We've transferred ownership
  finally
    declarations.Free;
  end;
end;

function TParser.ParseProgram: TProgramNode;
var
  programName: string;
  blockNode: TBlockNode;
begin
  Eat(tkProgram);
  programName := FCurrentToken.Value;
  Eat(tkIdentifier);
  if FCurrentToken.TokenType = tkSemicolon then
    Eat(tkSemicolon);

  blockNode := ParseBlock;
  Result := TProgramNode.Create(programName, blockNode);
  Eat(tkDot);
end;

function TParser.Parse: TASTNode;
begin
  Result := ParseProgram;
  if FCurrentToken.TokenType <> tkEOF then
    raise Exception.Create('Extra characters at end of file.');
end;

end.
