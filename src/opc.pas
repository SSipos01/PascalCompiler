program ObjectPascalCompiler;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, lexer, parser, codegen;

var
  Lexer: TLexer;
  Parser: TParser;
  CodeGenerator: TCodeGenerator;
  SyntaxTree: TProgramNode;
  InputFileName, OutputFileName: string;
  SourceCode: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: opc <input_file> [-o <output_file>]');
    Halt(1);
  end;

  InputFileName := ParamStr(1);
  OutputFileName := '';

  if (ParamCount > 2) and (ParamStr(2) = '-o') then
  begin
    if ParamCount > 3 then
      OutputFileName := ParamStr(3)
    else
    begin
      WriteLn('Error: Output file name not specified after -o');
      Halt(1);
    end;
  end;

  if OutputFileName = '' then
    OutputFileName := ChangeFileExt(InputFileName, '.pseudo');

  if not FileExists(InputFileName) then
  begin
    WriteLn('Error: Input file not found: ' + InputFileName);
    Halt(1);
  end;

  SourceCode := TIO.ReadFile(InputFileName);

  Lexer := TLexer.Create(SourceCode);
  Parser := TParser.Create(Lexer);
  try
    SyntaxTree := Parser.Parse;
    if Parser.HasErrors then
    begin
        WriteLn('Parsing failed with errors.');
        Halt(1);
    end;

    CodeGenerator := TCodeGenerator.Create(OutputFileName);
    CodeGenerator.GenerateCode(SyntaxTree);
    CodeGenerator.Free;

    WriteLn('Compilation successful. Output written to ' + OutputFileName);

  finally
    Parser.Free;
    Lexer.Free;
    if Assigned(SyntaxTree) then
      SyntaxTree.Free;
  end;
end.
