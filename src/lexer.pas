unit lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTokenType = (
    // Keywords
    tkProgram, tkUnit, tkUses, tkType, tkObject, tkClass,
    tkVar, tkConst, tkBegin, tkEnd, tkIf, tkThen, tkElse,
    tkFor, tkTo, tkDownTo, tkDo, tkWhile, tkRepeat, tkUntil,
    tkCase, tkOf, tkFunction, tkProcedure, tkConstructor, tkDestructor,
    tkInherited, tkSelf, tkAs, tkIs, tkArray, tkRecord, tkSet, tkFile,
    tkPointer, tkString, tkInteger, tkReal, tkChar, tkBoolean,
    tkNil, tkAsm, tkImplementation, tkInterface, tkOverride,
    tkInitialization, tkFinalization,

    // Operators
    tkPlus, tkMinus, tkStar, tkSlash, tkAssign,
    tkEqual, tkNotEqual, tkLess, tkLessEqual, tkGreater, tkGreaterEqual,
    tkLParen, tkRParen, tkLBracket, tkRBracket, tkComma, tkSemi,
    tkColon, tkDot, tkDotDot, tkCaret,

    // Literals
    tkIdentifier, tkIntegerLiteral, tkRealLiteral, tkStringLiteral,

    // Misc
    tkEOF, tkUnknown
  );

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line, Column: Integer;
  end;

  TLexer = class
  private
    FSource: string;
    FPosition: Integer;
    FCurrentChar: Char;
    FLine, FColumn: Integer;
    FKeywords: TStringList;
    procedure Advance;
    function Peek: Char;
    procedure SkipWhitespace;
    procedure SkipComment;
    function GetIdentifier: TToken;
    function GetNumber: TToken;
    function GetStringLiteral: TToken;
    procedure InitializeKeywords;
  public
    constructor Create(ASource: string);
    destructor Destroy; override;
    function GetNextToken: TToken;
  end;

implementation

{ TLexer }

constructor TLexer.Create(ASource: string);
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
  InitializeKeywords;
end;

destructor TLexer.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TLexer.InitializeKeywords;
begin
  FKeywords.AddObject('program', TObject(PtrInt(tkProgram)));
  FKeywords.AddObject('unit', TObject(PtrInt(tkUnit)));
  FKeywords.AddObject('uses', TObject(PtrInt(tkUses)));
  FKeywords.AddObject('type', TObject(PtrInt(tkType)));
  FKeywords.AddObject('object', TObject(PtrInt(tkObject)));
  FKeywords.AddObject('class', TObject(PtrInt(tkClass)));
  FKeywords.AddObject('var', TObject(PtrInt(tkVar)));
  FKeywords.AddObject('const', TObject(PtrInt(tkConst)));
  FKeywords.AddObject('begin', TObject(PtrInt(tkBegin)));
  FKeywords.AddObject('end', TObject(PtrInt(tkEnd)));
  FKeywords.AddObject('if', TObject(PtrInt(tkIf)));
  FKeywords.AddObject('then', TObject(PtrInt(tkThen)));
  FKeywords.AddObject('else', TObject(PtrInt(tkElse)));
  FKeywords.AddObject('for', TObject(PtrInt(tkFor)));
  FKeywords.AddObject('to', TObject(PtrInt(tkTo)));
  FKeywords.AddObject('downto', TObject(PtrInt(tkDownTo)));
  FKeywords.AddObject('do', TObject(PtrInt(tkDo)));
  FKeywords.AddObject('while', TObject(PtrInt(tkWhile)));
  FKeywords.AddObject('repeat', TObject(PtrInt(tkRepeat)));
  FKeywords.AddObject('until', TObject(PtrInt(tkUntil)));
  FKeywords.AddObject('case', TObject(PtrInt(tkCase)));
  FKeywords.AddObject('of', TObject(PtrInt(tkOf)));
  FKeywords.AddObject('function', TObject(PtrInt(tkFunction)));
  FKeywords.AddObject('procedure', TObject(PtrInt(tkProcedure)));
  FKeywords.AddObject('constructor', TObject(PtrInt(tkConstructor)));
  FKeywords.AddObject('destructor', TObject(PtrInt(tkDestructor)));
  FKeywords.AddObject('inherited', TObject(PtrInt(tkInherited)));
  FKeywords.AddObject('self', TObject(PtrInt(tkSelf)));
  FKeywords.AddObject('as', TObject(PtrInt(tkAs)));
  FKeywords.AddObject('is', TObject(PtrInt(tkIs)));
  FKeywords.AddObject('array', TObject(PtrInt(tkArray)));
  FKeywords.AddObject('record', TObject(PtrInt(tkRecord)));
  FKeywords.AddObject('set', TObject(PtrInt(tkSet)));
  FKeywords.AddObject('file', TObject(PtrInt(tkFile)));
  FKeywords.AddObject('pointer', TObject(PtrInt(tkPointer)));
  FKeywords.AddObject('string', TObject(PtrInt(tkString)));
  FKeywords.AddObject('integer', TObject(PtrInt(tkInteger)));
  FKeywords.AddObject('real', TObject(PtrInt(tkReal)));
  FKeywords.AddObject('char', TObject(PtrInt(tkChar)));
  FKeywords.AddObject('boolean', TObject(PtrInt(tkBoolean)));
  FKeywords.AddObject('nil', TObject(PtrInt(tkNil)));
  FKeywords.AddObject('asm', TObject(PtrInt(tkAsm)));
  FKeywords.AddObject('implementation', TObject(PtrInt(tkImplementation)));
  FKeywords.AddObject('interface', TObject(PtrInt(tkInterface)));
  FKeywords.AddObject('override', TObject(PtrInt(tkOverride)));
  FKeywords.AddObject('initialization', TObject(PtrInt(tkInitialization)));
  FKeywords.AddObject('finalization', TObject(PtrInt(tkFinalization)));
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
  FCurrentChar := FSource[FPosition];
  Inc(FPosition);
end;


function TLexer.Peek: Char;
begin
  if FPosition > Length(FSource) then
    Result := #0
  else
    Result := FSource[FPosition];
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
    Advance; // Skip the '}'
  end
  else if (FCurrentChar = '/') and (Peek = '/') then
  begin
    while (FCurrentChar <> #0) and (FCurrentChar <> #10) do
      Advance;
  end
  else if (FCurrentChar = '(') and (Peek = '*') then
  begin
    Advance; // Skip '('
    Advance; // Skip '*'
    while (FCurrentChar <> #0) and not ((FCurrentChar = '*') and (Peek = ')')) do
        Advance;
    Advance; // Skip '*'
    Advance; // Skip ')'
  end;
end;

function TLexer.GetIdentifier: TToken;
var
  value: string;
  kwIndex: Integer;
begin
  Result.Line := FLine;
  Result.Column := FColumn;
  value := '';
  while (FCurrentChar <> #0) and (FCurrentChar in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
  begin
    value := value + FCurrentChar;
    Advance;
  end;
  Result.Value := value;
  kwIndex := FKeywords.IndexOf(value);
  if kwIndex <> -1 then
    Result.TokenType := TTokenType(PtrInt(FKeywords.Objects[kwIndex]))
  else
    Result.TokenType := tkIdentifier;
end;

function TLexer.GetNumber: TToken;
var
  value: string;
  isReal: Boolean;
begin
  Result.Line := FLine;
  Result.Column := FColumn;
  value := '';
  isReal := False;
  while (FCurrentChar <> #0) and (FCurrentChar in ['0'..'9']) do
  begin
    value := value + FCurrentChar;
    Advance;
  end;

  if FCurrentChar = '.' then
  begin
    if Peek in ['0'..'9'] then
    begin
        isReal := True;
        value := value + '.';
        Advance;
        while (FCurrentChar <> #0) and (FCurrentChar in ['0'..'9']) do
        begin
            value := value + FCurrentChar;
            Advance;
        end;
    end
    else if Peek = '.' then // Range
    begin
        Result.Value := value;
        Result.TokenType := tkIntegerLiteral;
        Exit;
    end;
  end;

  if FCurrentChar in ['e', 'E'] then
  begin
    isReal := True;
    value := value + FCurrentChar;
    Advance;
    if FCurrentChar in ['+', '-'] then
    begin
      value := value + FCurrentChar;
      Advance;
    end;
    while (FCurrentChar <> #0) and (FCurrentChar in ['0'..'9']) do
    begin
      value := value + FCurrentChar;
      Advance;
    end;
  end;

  Result.Value := value;
  if isReal then
    Result.TokenType := tkRealLiteral
  else
    Result.TokenType := tkIntegerLiteral;
end;

function TLexer.GetStringLiteral: TToken;
var
  value: string;
begin
  Result.Line := FLine;
  Result.Column := FColumn;
  value := '';
  Advance; // Skip the opening '
  while (FCurrentChar <> #0) and (FCurrentChar <> '''') do
  begin
    value := value + FCurrentChar;
    Advance;
  end;
  Advance; // Skip the closing '
  Result.Value := value;
  Result.TokenType := tkStringLiteral;
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

    case FCurrentChar of
      '+': begin Result.TokenType := tkPlus; Result.Value := '+'; Advance; Exit; end;
      '-': begin Result.TokenType := tkMinus; Result.Value := '-'; Advance; Exit; end;
      '*': begin Result.TokenType := tkStar; Result.Value := '*'; Advance; Exit; end;
      '/': begin Result.TokenType := tkSlash; Result.Value := '/'; Advance; Exit; end;
      '=': begin Result.TokenType := tkEqual; Result.Value := '='; Advance; Exit; end;
      '<':
        begin
          Advance;
          if FCurrentChar = '>' then
          begin
            Result.TokenType := tkNotEqual; Result.Value := '<>'; Advance;
          end
          else if FCurrentChar = '=' then
          begin
            Result.TokenType := tkLessEqual; Result.Value := '<='; Advance;
          end
          else
          begin
            Result.TokenType := tkLess; Result.Value := '<';
          end;
          Exit;
        end;
      '>':
        begin
          Advance;
          if FCurrentChar = '=' then
          begin
            Result.TokenType := tkGreaterEqual; Result.Value := '>='; Advance;
          end
          else
          begin
            Result.TokenType := tkGreater; Result.Value := '>';
          end;
          Exit;
        end;
      '(': begin Result.TokenType := tkLParen; Result.Value := '('; Advance; Exit; end;
      ')': begin Result.TokenType := tkRParen; Result.Value := ')'; Advance; Exit; end;
      '[': begin Result.TokenType := tkLBracket; Result.Value := '['; Advance; Exit; end;
      ']': begin Result.TokenType := tkRBracket; Result.Value := ']'; Advance; Exit; end;
      ',': begin Result.TokenType := tkComma; Result.Value := ','; Advance; Exit; end;
      ';': begin Result.TokenType := tkSemi; Result.Value := ';'; Advance; Exit; end;
      '^': begin Result.TokenType := tkCaret; Result.Value := '^'; Advance; Exit; end;
      ':':
        begin
          Advance;
          if FCurrentChar = '=' then
          begin
            Result.TokenType := tkAssign; Result.Value := ':='; Advance;
          end
          else
          begin
            Result.TokenType := tkColon; Result.Value := ':';
          end;
          Exit;
        end;
      '.':
        begin
          Advance;
          if FCurrentChar = '.' then
          begin
            Result.TokenType := tkDotDot; Result.Value := '..'; Advance;
          end
          else
          begin
            Result.TokenType := tkDot; Result.Value := '.';
          end;
          Exit;
        end;
    else
      begin
        Result.TokenType := tkUnknown;
        Result.Value := FCurrentChar;
        Advance;
        Exit;
      end;
    end;
  end;

  Result.TokenType := tkEOF;
  Result.Value := '';
end;

end.
