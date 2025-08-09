program CompilerTestSuite;

{$mode objfpc}{$H+} // Use Object FPC mode and long strings

uses
  SysUtils, // For exception handling, file I/O helpers, string conversions
  Math;     // For advanced math functions
  // ShapeUtils; // <-- USING THE NEW UNIT

// --- SECTION 1: Type Declarations ---
// This section demonstrates various type definitions available in Pascal.
// The shape-related types have been moved to the ShapeUtils unit.

type
  TColor = (clRed, clGreen, clBlue, clYellow, clBlack, clWhite);
  // Subrange Type
  TSubRange = 1..100;
  TLetterRange = 'A'..'Z';

  // Array Types
  TStaticArray = array[1..10] of Integer;
  TDynamicArray = array of string;
  TMultiDimArray = array[0..2, 0..3] of Real;

  // Set Type
  TCharSet = set of Char;

  // Record Type (with variant part)
  TPerson = record
    FirstName: string[50];
    LastName: string[50];
    Age: Byte;
    case IsStudent: Boolean of
      True: (StudentID: Integer; GPA: Real);
      False: (EmployeeID: Integer; Salary: Currency);
  end;

  // Pointer Type
  PInteger = ^Integer;

  // Procedural Type (for standalone functions)
  TIntegerFunction = function(A, B: Integer): Integer;

  TShape = class;


// --- SECTION 2: Global Variables ---
// Declaration of global variables to be used throughout the program.

var
  GlobalInteger: Integer;
  GlobalString: string;
  GlobalPerson: TPerson;
  // GlobalShapeList: array of TShape; // Dynamic array of objects (TShape from unit)

// --- SECTION 3: Procedures and Functions ---
// Method implementations for TShape, TCircle, TRectangle are now in ShapeUtils.

// --- Standalone Procedures and Functions ---

// Function with value parameters
function Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Procedure with VAR (by reference) and CONST (read-only) parameters
procedure ModifyValues(var V: Integer; const C: string);
begin
  // WriteLn('Inside ModifyValues. Initial V: ', V, ', C: ', C);
  V := V * 2;
  // C := 'New'; // This would cause a compiler error
  // WriteLn('Inside ModifyValues. Final V: ', V);
end;

// Overloaded function
function PrintValue(I: Integer): string;
begin
  Result := 'Integer: ' + IntToStr(I);
end;

function PrintValue(R: Real): string;
begin
  Result := 'Real: ' + FloatToStr(R);
end;

// Recursive function: Factorial
function Factorial(N: LongInt): LongInt;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorial(N - 1);
end;

// Nested procedure
procedure OuterProcedure;
var
  OuterVar: Integer = 50;
  procedure InnerProcedure;
  begin
    // WriteLn('InnerProcedure can access OuterVar: ', OuterVar);
  end;
begin
  // WriteLn('--- Testing Nested Procedure ---');
  InnerProcedure;
end;


// --- SECTION 4: Main Program Logic ---

procedure TestBasicTypesAndOperators;
var
  I, J: Integer;
  R: Real;
  B: Boolean;
  C: Char;
  S: string;
  EnumColor: TColor; // TColor is now from ShapeUtils
  SubRangeNum: TSubRange;
begin
  // WriteLn('--- Testing Basic Types and Operators ---');
  I := 10;
  J := 3;
  R := 10.5;
  B := True;
  C := 'P';
  S := 'Pascal';

  // Arithmetic Operators
  // WriteLn('I + J = ', I + J);
  // WriteLn('I - J = ', I - J);
  // WriteLn('I * J = ', I * J);
  // WriteLn('I / J = ', I / J:0:2); // Real division
  // WriteLn('I div J = ', I div J); // Integer division
  // WriteLn('I mod J = ', I mod J); // Modulo

  // Boolean Operators
  // WriteLn('B AND (1 > 2) = ', B AND (1 > 2));
  // WriteLn('B OR (1 > 2) = ', B OR (1 > 2));
  // WriteLn('NOT B = ', NOT B);

  // String Concatenation
  S := S + ' is powerful.';
  // WriteLn(S);

  // Ordinal functions
  EnumColor := clGreen;
  // WriteLn('Color: ', Ord(EnumColor));
  Inc(EnumColor); // Move to next value in enum (clBlue)
  // WriteLn('Next Color: ', Ord(EnumColor));
  Dec(EnumColor); // Move back
  // WriteLn('Previous Color: ', Ord(EnumColor));
  // WriteLn('Successor of ''A'': ', Succ('A'));
  // WriteLn('Predecessor of ''C'': ', Pred('C'));

  // Type conversion
  S := IntToStr(123);
  I := StrToIntDef('-45', 0);
  // WriteLn('String from Int: ', S, ', Integer from string: ', I);

  // Math functions
  // WriteLn('Abs(-5): ', Abs(-5));
  // WriteLn('Sqr(4): ', Sqr(4));
  // WriteLn('Sqrt(16): ', Sqrt(16):0:1);
  // WriteLn('Sin(Pi/2): ', Sin(Pi/2):0:1);
  // WriteLn('Round(10.7): ', Round(10.7));
  // WriteLn('Trunc(10.7): ', Trunc(10.7));
  Randomize; // Initialize random number generator
  // WriteLn('Random number (0-99): ', Random(100));
end;

procedure TestControlStructures;
var
  i: Integer;
  color: TColor; // TColor is now from ShapeUtils
  input: Char;
begin
  // WriteLn('--- Testing Control Structures ---');

  // IF-THEN-ELSE
  i := 15;
  if i > 10 then
    // WriteLn('i is greater than 10')
  else if i < 10 then
    // WriteLn('i is less than 10')
  else
    // WriteLn('i is exactly 10');

  // CASE
  color := clBlue;
  case color of
    clRed, clGreen, clBlue:; // WriteLn('Primary light color');
    clYellow:; // WriteLn('A primary pigment color');
  else
    // WriteLn('Another color');
  end;

  // FOR loop (to and downto)
  // Write('FOR loop (to): ');
  for i := 1 to 5 do
    // Write(i, ' ');
  // WriteLn;

  // Write('FOR loop (downto): ');
  for i := 5 downto 1 do
    // Write(i, ' ');
  // WriteLn;

  // WHILE loop
  i := 1;
  // Write('WHILE loop: ');
  while i <= 5 do
  begin
    // Write(i, ' ');
    Inc(i);
  end;
  // WriteLn;

  // REPEAT-UNTIL loop
  i := 1;
  // Write('REPEAT-UNTIL loop: ');
  repeat
    // Write(i, ' ');
    Inc(i);
  until i > 5;
  // WriteLn;

  // GOTO statement (for demonstration purposes only)
label
  EndOfLoop;
  // WriteLn('Testing GOTO: Enter Q to quit loop, any other char to continue.');
  for i := 1 to 10 do
  begin
    // WriteLn('Iteration ', i);
    // In a real app, use ReadKey from Crt unit. For simplicity, we simulate.
    if i = 3 then
    begin
        // WriteLn('Jumping with GOTO...');
        goto EndOfLoop;
    end;
  end;
EndOfLoop:
  // WriteLn('Loop exited.');
end;

procedure TestStructuredTypes;
var
  StaticArr: TStaticArray;
  DynamicArr: TDynamicArray;
  MultiArr: TMultiDimArray;
  CharSet: TCharSet;
  Person: TPerson;
  i, j: Integer;
begin
  // WriteLn('--- Testing Structured Types ---');

  // Static Array
  for i := Low(StaticArr) to High(StaticArr) do
    StaticArr[i] := i * 10;
  // WriteLn('Static Array element 5: ', StaticArr[5]);

  // Dynamic Array
  SetLength(DynamicArr, 3);
  DynamicArr[0] := 'One';
  DynamicArr[1] := 'Two';
  DynamicArr[2] := 'Three';
  // WriteLn('Dynamic Array: ', Concat(DynamicArr[0], ', ', DynamicArr[1], ', ', DynamicArr[2]));
  SetLength(DynamicArr, 0); // Free memory

  // Record with WITH statement
  with Person do
  begin
    FirstName := 'John';
    LastName := 'Doe';
    Age := 30;
    IsStudent := False;
    EmployeeID := 101;
    Salary := 50000.00;
  end;
  // WriteLn('Person: ', Person.FirstName, ' (Employee ID: ', Person.EmployeeID, ')');

  // Set
  CharSet := ['A'..'E', 'Z'];
  CharSet := CharSet + ['M']; // Union
  CharSet := CharSet - ['C']; // Difference
  if 'B' in CharSet then
    // WriteLn('''B'' is in the character set.');
  if not ('X' in CharSet) then
    // WriteLn('''X'' is not in the character set.');
end;

procedure TestPointersAndMemory;
var
  p: PInteger;
  untyped_p: Pointer;
begin
  // WriteLn('--- Testing Pointers and Memory Management ---');
  // Using New/Dispose for typed pointers
  New(p);
  try
    p^ := 999;
    // WriteLn('Value pointed to by p: ', p^);
  finally
    Dispose(p);
    // WriteLn('Memory disposed.');
  end;

  // Using GetMem/FreeMem for untyped pointers
  GetMem(untyped_p, SizeOf(Integer));
  try
    PInteger(untyped_p)^ := 12345;
    // WriteLn('Value in untyped pointer block: ', PInteger(untyped_p)^);
  finally
    FreeMem(untyped_p, SizeOf(Integer));
    // WriteLn('Memory freed.');
  end;
end;

procedure TestProceduralType;
var
  MyFunc: TIntegerFunction;
  ResultValue: Integer;
begin
  // WriteLn('--- Testing Procedural Type ---');
  // Assign a compatible standalone function to the procedural type variable
  MyFunc := @Add; // Use @ operator to get the address of the function

  // Call the function through the variable
  ResultValue := MyFunc(7, 8);
  // WriteLn('Result of calling function via procedural type (7+8): ', ResultValue);
end;

procedure TestUnitUsage;
var
  Shape: TShape; // Can hold any descendant
  Circle: TCircle;
  Rect: TRectangle;
begin
  // WriteLn('--- Testing Unit Usage (OOP from ShapeUtils) ---');

  // Create objects using types from the unit
  // Circle := TCircle.Create('Unit Circle', clRed, 10.0);
  // Rect := TRectangle.Create('Unit Rectangle', clGreen, 5.0, 8.0);

  // Polymorphism
  // SetLength(GlobalShapeList, 2);
  // GlobalShapeList[0] := Circle;
  // GlobalShapeList[1] := Rect;

  // for Shape in GlobalShapeList do
  // begin
  //   Shape.DisplayInfo;
  //   // WriteLn('Calculated Area: ', Shape.CalculateArea:0:2);
  // end;

  // Freeing objects
  // Circle.Free; // Calls destructor
  // Rect.Free;
  // SetLength(GlobalShapeList, 0); // Clear the dynamic array
end;

procedure TestExceptionHandling;
var
  i: Integer;
  s: string;
begin
  // WriteLn('--- Testing Exception Handling ---');
  s := 'abc';
  try
    i := StrToInt(s);
    // WriteLn('Conversion successful (this should not happen)');
  except
    on E: EConvertError do
      // WriteLn('Caught expected exception: ', E.ClassName, ' - ', E.Message);
  end;

  try
    i := 10 div 0;
  except
    on E: EDivByZero do
      // WriteLn('Caught expected exception: ', E.ClassName, ' - ', E.Message);
  finally
    // WriteLn('This finally block is always executed.');
  end;
end;

procedure TestFileIO;
var
  TxtFile: TextFile;
  BinFile: file of TPerson;
  PersonRec, PersonRead: TPerson;
  TestFileName: string = 'testfile.txt';
  TestBinFileName: string = 'testfile.dat';
begin
  // WriteLn('--- Testing File I/O ---');

  // Text file
  AssignFile(TxtFile, TestFileName);
  try
    Rewrite(TxtFile);
    // WriteLn(TxtFile, 'Line 1: Hello from Pascal!');
    // WriteLn(TxtFile, 'Line 2: The value is ', 42);
    CloseFile(TxtFile);

    Reset(TxtFile);
    while not Eof(TxtFile) do
    begin
      ReadLn(TxtFile, GlobalString);
      // WriteLn('Read from text file: ', GlobalString);
    end;
  finally
    CloseFile(TxtFile);
    if FileExists(TestFileName) then DeleteFile(TestFileName);
  end;

  // Binary file
  AssignFile(BinFile, TestBinFileName);
  with PersonRec do
  begin
    FirstName := 'Jane';
    LastName := 'Smith';
    Age := 25;
    IsStudent := true;
    StudentID := 9876;
    GPA := 3.8;
  end;
  try
    Rewrite(BinFile);
    Write(BinFile, PersonRec);
    // WriteLn('Record written to binary file.');
    // WriteLn('File size: ', FileSize(BinFile), ' records. Position: ', FilePos(BinFile));
    Seek(BinFile, 0); // Go back to the beginning
    // WriteLn('Position after Seek(0): ', FilePos(BinFile));
    Read(BinFile, PersonRead);
    // WriteLn('Read from binary file: ', PersonRead.FirstName, ' (Student ID: ', PersonRead.StudentID, ')');
  finally
    CloseFile(BinFile);
    if FileExists(TestBinFileName) then DeleteFile(TestBinFileName);
  end;
end;

// Function using an inline assembler block
function AsmAdd(A, B: Integer): Integer;
asm
  // This uses Intel syntax for 32-bit architecture (default for FPC)
  // The parameters A and B are available on the stack.
  // The result is returned in the EAX register.
  MOV EAX, A     // Move the value of parameter A into the EAX register
  ADD EAX, B     // Add the value of parameter B to the EAX register
  // The result is now in EAX, which is the standard return for functions.
end;

procedure TestAsmBlock;
var
  Num1, Num2, AsmResult: Integer;
begin
  // WriteLn('--- Testing Inline Assembler (asm) Block ---');
  Num1 := 25;
  Num2 := 17;
  AsmResult := AsmAdd(Num1, Num2);
  // WriteLn('Result of AsmAdd(', Num1, ', ', Num2, '): ', AsmResult);
  // WriteLn('For comparison, Pascal add: ', Num1 + Num2);
end;

// --- Main Program Block ---
begin
  // WriteLn('======================================');
  // WriteLn('  Free Pascal Compiler Test Suite');
  // WriteLn('======================================');
  // WriteLn;

  TestBasicTypesAndOperators;
  // WriteLn;

  TestControlStructures;
  // WriteLn;

  TestStructuredTypes;
  // WriteLn;

  TestPointersAndMemory;
  // WriteLn;

  TestProceduralType;
  // WriteLn;

  OuterProcedure;
  // WriteLn;

  TestUnitUsage; // <-- Replaced TestOOP with this
  // WriteLn;

  TestExceptionHandling;
  // WriteLn;

  TestFileIO;
  // WriteLn;

  TestAsmBlock;
  // WriteLn;

  // Final test using overloaded function
  // WriteLn('--- Testing Overloaded Functions ---');
  // WriteLn(PrintValue(100));
  // WriteLn(PrintValue(123.456));
  // WriteLn;

  // WriteLn('======================================');
  // WriteLn('     Test Suite Completed');
  // WriteLn('======================================');

  // Halt(0); // Optional: Stop execution with exit code 0
end.
