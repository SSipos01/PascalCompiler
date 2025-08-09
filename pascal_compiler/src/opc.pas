program opc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, lexer, parser, ast, codegen;

var
  InputFileName, OutputFileName: string;
  SourceCode: TStringList;
  Lex: TLexer;
  Par: TParser;
  Generator: TCodeGenerator;
  ProgramAST: TASTNode;
  ResultCode: string;
  OutputFile: TextFile;

begin
  if ParamCount <> 2 then
  begin
    Writeln('Usage: opc <input_file> <output_file>');
    Halt(1);
  end;

  InputFileName := ParamStr(1);
  OutputFileName := ParamStr(2);

  if not FileExists(InputFileName) then
  begin
    Writeln('Error: Input file not found: ', InputFileName);
    Halt(1);
  end;

  SourceCode := TStringList.Create;
  Lex := nil;
  Par := nil;
  Generator := nil;
  ProgramAST := nil;

  try
    SourceCode.LoadFromFile(InputFileName);

    Lex := TLexer.Create(SourceCode.Text);
    Par := TParser.Create(Lex);
    ProgramAST := Par.Parse;
    Generator := TCodeGenerator.Create;

    ResultCode := Generator.Generate(TProgramNode(ProgramAST));

    AssignFile(OutputFile, OutputFileName);
    Rewrite(OutputFile);
    Write(OutputFile, ResultCode);
    CloseFile(OutputFile);

    Writeln('Compilation successful. Output written to ', OutputFileName);

  except
    on E: Exception do
    begin
      Writeln('Compilation failed:');
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;

  ProgramAST.Free;
  Generator.Free;
  Par.Free;
  Lex.Free;
  SourceCode.Free;
end.
