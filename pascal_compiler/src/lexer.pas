unit lexer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TTokenType = (
    // Single-character tokens
    tkPlus, tkMinus, tkStar, tkSlash, tkLParen, tkRParen, tkLBracket, tkRBracket,
    tkComma, tkSemicolon, tkColon, tkDot, tkEqual, tkGreater, tkLess, tkCaret, tkAt,

    // Two-character tokens
    tkAssign, tkLessEqual, tkGreaterEqual, tkNotEqual, tkDotDot,

    // Literals
    tkIdentifier, tkIntegerLiteral, tkFloatLiteral, tkStringLiteral,

    // Keywords
    tkProgram, tkUnit, tkUses,
    tkVar, tkConst,
    tkBegin, tkEnd,
    tkIf, tkThen, tkElse,
    tkFor, tkTo, tkDo,
    tkWhile, tkRepeat, tkUntil,
    tkProcedure, tkFunction,
    tkClass, tkObject, tkInherits,
    tkAsm,
    tkType,
    tkPrivate, tkPublic, tkProtected,
    tkConstructor, tkDestructor,
    tkVirtual, tkOverride, tkOverload,
    tkCase, tkOf,
    tkArray, tkSet, tkRecord, tkFile,
    tkWith, tkGoto, tkLabel,

    // End of file
    tkEOF
  );

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line: Integer;
    Column: Integer;
  end;

  TLexer = class
  private
    FSource: string;
    FPosition: Integer;
    FCurrentChar: Char;
    FLine: Integer;
    FColumn: Integer;
    FKeywords: TStringList;

    procedure Advance;
    function Peek: Char;
    procedure SkipWhitespace;
    procedure SkipComment;
    function GetIdentifier: TToken;
    function GetNumber: TToken;
    function GetStringLiteral: TToken;
    function CreateToken(ATokenType: TTokenType; AValue: string): TToken;
  public
    constructor Create(const ASource: string);
    destructor Destroy; override;
    function GetNextToken: TToken;
  end;

implementation

constructor TLexer.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
  FPosition := 1;
  FLine := 1;
  FColumn := 1;
  if Length(FSource) > 0 then
    FCurrentChar := FSource[FPosition]
  else
    FCurrentChar := #0;

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := False;
  FKeywords.Add('PROGRAM');
  FKeywords.Add('UNIT');
  FKeywords.Add('USES');
  FKeywords.Add('VAR');
  FKeywords.Add('CONST');
  FKeywords.Add('BEGIN');
  FKeywords.Add('END');
  FKeywords.Add('IF');
  FKeywords.Add('THEN');
  FKeywords.Add('ELSE');
  FKeywords.Add('FOR');
  FKeywords.Add('TO');
  FKeywords.Add('DO');
  FKeywords.Add('WHILE');
  FKeywords.Add('REPEAT');
  FKeywords.Add('UNTIL');
  FKeywords.Add('PROCEDURE');
  FKeywords.Add('FUNCTION');
  FKeywords.Add('CLASS');
  FKeywords.Add('OBJECT');
  FKeywords.Add('INHERITS');
  FKeywords.Add('ASM');
  FKeywords.Add('TYPE');
  FKeywords.Add('PRIVATE');
  FKeywords.Add('PUBLIC');
  FKeywords.Add('PROTECTED');
  FKeywords.Add('CONSTRUCTOR');
  FKeywords.Add('DESTRUCTOR');
  FKeywords.Add('VIRTUAL');
  FKeywords.Add('OVERRIDE');
  FKeywords.Add('OVERLOAD');
  FKeywords.Add('CASE');
  FKeywords.Add('OF');
  FKeywords.Add('ARRAY');
  FKeywords.Add('SET');
  FKeywords.Add('RECORD');
  FKeywords.Add('FILE');
  FKeywords.Add('WITH');
  FKeywords.Add('GOTO');
  FKeywords.Add('LABEL');
end;

destructor TLexer.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TLexer.Advance;
begin
  if FPosition > Length(FSource) then
  begin
    FCurrentChar := #0; // EOF
    Exit;
  end;

  if FCurrentChar = #10 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else
  begin
    Inc(FColumn);
  end;

  Inc(FPosition);
  if FPosition > Length(FSource) then
    FCurrentChar := #0 // EOF
  else
    FCurrentChar := FSource[FPosition];
end;

function TLexer.Peek: Char;
var
  peekPos: Integer;
begin
  peekPos := FPosition + 1;
  if peekPos > Length(FSource) then
    Result := #0
  else
    Result := FSource[peekPos];
end;

procedure TLexer.SkipWhitespace;
begin
  while (FCurrentChar <> #0) and (FCurrentChar <= ' ') do
    Advance;
end;

procedure TLexer.SkipComment;
begin
  if FCurrentChar = '{' then
  begin
    Advance;
    while (FCurrentChar <> #0) and (FCurrentChar <> '}') do
      Advance;
    Advance; // Skip the closing '}'
    Exit;
  end;

  if (FCurrentChar = '/') and (Peek = '/') then
  begin
    while (FCurrentChar <> #0) and (FCurrentChar <> #10) do
      Advance;
    Exit;
  end;

  if (FCurrentChar = '(') and (Peek = '*') then
  begin
    Advance; // (
    Advance; // *
    while (FCurrentChar <> #0) and not ((FCurrentChar = '*') and (Peek = ')')) do
      Advance;
    Advance; // *
    Advance; // )
    Exit;
  end;
end;

function TLexer.GetIdentifier: TToken;
var
  ident: string;
  keywordIndex: Integer;
begin
  ident := '';
  while (FCurrentChar <> #0) and (FCurrentChar in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
  begin
    ident := ident + FCurrentChar;
    Advance;
  end;

  Result := CreateToken(tkIdentifier, ident);

  keywordIndex := FKeywords.IndexOf(ident);
  if keywordIndex > -1 then
  begin
    Result.TokenType := TTokenType(keywordIndex + Ord(tkProgram));
  end;
end;

function TLexer.GetNumber: TToken;
var
  numStr: string;
  isFloat: boolean;
begin
  numStr := '';
  isFloat := False;
  while (FCurrentChar <> #0) and (FCurrentChar in ['0'..'9']) do
  begin
    numStr := numStr + FCurrentChar;
    Advance;
  end;

  if (FCurrentChar = '.') and (Peek in ['0'..'9']) then
  begin
    isFloat := True;
    numStr := numStr + '.';
    Advance;
    while (FCurrentChar <> #0) and (FCurrentChar in ['0'..'9']) do
    begin
        numStr := numStr + FCurrentChar;
        Advance;
    end;
  end;

  if isFloat then
    Result := CreateToken(tkFloatLiteral, numStr)
  else
    Result := CreateToken(tkIntegerLiteral, numStr);
end;

function TLexer.GetStringLiteral: TToken;
var
  strValue: string;
begin
  strValue := '';
  Advance; // Skip the opening '
  while (FCurrentChar <> #0) and (FCurrentChar <> '''') do
  begin
    strValue := strValue + FCurrentChar;
    Advance;
  end;
  Advance; // Skip the closing '
  Result := CreateToken(tkStringLiteral, strValue);
end;

function TLexer.CreateToken(ATokenType: TTokenType; AValue: string): TToken;
begin
  Result.TokenType := ATokenType;
  Result.Value := AValue;
  Result.Line := FLine;
  Result.Column := FColumn;
end;

function TLexer.GetNextToken: TToken;
begin
  while FCurrentChar <> #0 do
  begin
    if FCurrentChar <= ' ' then
    begin
      SkipWhitespace;
      Continue;
    end;

    if (FCurrentChar = '{') or ((FCurrentChar = '/') and (Peek = '/')) or ((FCurrentChar = '(') and (Peek = '*')) then
    begin
        SkipComment;
        Continue;
    end;

    if FCurrentChar in ['a'..'z', 'A'..'Z', '_'] then
    begin
      Result := GetIdentifier;
      Exit;
    end;

    if FCurrentChar in ['0'..'9'] then
    begin
      Result := GetNumber;
      Exit;
    end;

    if FCurrentChar = '''' then
    begin
      Result := GetStringLiteral;
      Exit;
    end;

    if (FCurrentChar = ':') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := CreateToken(tkAssign, ':=');
      Exit;
    end;

    if (FCurrentChar = '<') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := CreateToken(tkLessEqual, '<=');
      Exit;
    end;

    if (FCurrentChar = '>') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := CreateToken(tkGreaterEqual, '>=');
      Exit;
    end;

    if (FCurrentChar = '<') and (Peek = '>') then
    begin
      Advance;
      Advance;
      Result := CreateToken(tkNotEqual, '<>');
      Exit;
    end;

    if (FCurrentChar = '.') and (Peek = '.') then
    begin
      Advance;
      Advance;
      Result := CreateToken(tkDotDot, '..');
      Exit;
    end;

    case FCurrentChar of
      '@': begin Result := CreateToken(tkAt, '@'); Advance; Exit; end;
      '^': begin Result := CreateToken(tkCaret, '^'); Advance; Exit; end;
      '+': begin Result := CreateToken(tkPlus, '+'); Advance; Exit; end;
      '-': begin Result := CreateToken(tkMinus, '-'); Advance; Exit; end;
      '*': begin Result := CreateToken(tkStar, '*'); Advance; Exit; end;
      '/': begin Result := CreateToken(tkSlash, '/'); Advance; Exit; end;
      '(': begin Result := CreateToken(tkLParen, '('); Advance; Exit; end;
      ')': begin Result := CreateToken(tkRParen, ')'); Advance; Exit; end;
      '[': begin Result := CreateToken(tkLBracket, '['); Advance; Exit; end;
      ']': begin Result := CreateToken(tkRBracket, ']'); Advance; Exit; end;
      ',': begin Result := CreateToken(tkComma, ','); Advance; Exit; end;
      ';': begin Result := CreateToken(tkSemicolon, ';'); Advance; Exit; end;
      ':': begin Result := CreateToken(tkColon, ':'); Advance; Exit; end;
      '.': begin Result := CreateToken(tkDot, '.'); Advance; Exit; end;
      '=': begin Result := CreateToken(tkEqual, '='); Advance; Exit; end;
      '>': begin Result := CreateToken(tkGreater, '>'); Advance; Exit; end;
      '<': begin Result := CreateToken(tkLess, '<'); Advance; Exit; end;
    else
      // Should handle error for unknown token
      Result := CreateToken(tkEOF, '');
      Exit;
    end;
  end;

  Result := CreateToken(tkEOF, '');
end;

end.
