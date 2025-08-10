unit CompilerTestSuite;

interface

uses
  SysUtils, Classes;

type
  TMyEnum = (meOne, meTwo, meThree);
  TSubRange = 1..100;
  PMyRecord = ^TMyRecord;

  TMyRecord = record
    field1: Integer;
    field2: string;
    field3: Boolean;
    case Integer of
      1: (variantField1: Real);
      2: (variantField2: TMyEnum);
  end;

  TMyObject = object
    constructor Create;
    destructor Destroy;
  end;

  TMyClass = class(TObject)
  private
    FField1: Integer;
    FField2: PMyRecord;
    procedure SetField1(AValue: Integer);
  public
    property Field1: Integer read FField1 write SetField1;
    constructor Create;
    destructor Destroy; override;
    procedure MyMethod(AParam: TMyEnum);
    function MyFunction(AParam: Integer): string;
  end;

  TAnotherClass = class(TMyClass)
  public
    procedure AnotherMethod; override;
  end;

  TComplexArray = array[TMyEnum] of array[1..10] of TMyClass;
  TSetType = set of TMyEnum;
  TFileType = file of TMyRecord;


procedure GlobalProcedure(AParam: Integer);
function GlobalFunction(AParam: string): Boolean;

implementation

var
  GMyClass: TMyClass;
  GArray: TComplexArray;

procedure GlobalProcedure(AParam: Integer);
var
  x: Integer;
begin
  x := AParam + 1;
  if x > 10 then
    WriteLn('Greater')
  else
    WriteLn('Smaller or equal');
end;

function GlobalFunction(AParam: string): Boolean;
begin
  if AParam = 'test' then
    Result := True
  else
    Result := False;
end;

{ TMyObject }
constructor TMyObject.Create;
begin
  // do nothing
end;

destructor TMyObject.Destroy;
begin
  // do nothing
end;

{ TMyClass }
constructor TMyClass.Create;
begin
  inherited Create;
  FField1 := 0;
  New(FField2);
end;

destructor TMyClass.Destroy;
begin
  Dispose(FField2);
  inherited Destroy;
end;

procedure TMyClass.MyMethod(AParam: TMyEnum);
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    // test loop
  end;

  while i < 20 do
  begin
    i := i + 1;
  end;

  repeat
    i := i - 1;
  until i = 10;
end;

function TMyClass.MyFunction(AParam: Integer): string;
begin
  case AParam of
    1: Result := 'One';
    2: Result := 'Two';
  else
    Result := 'Other';
  end;
end;

procedure TMyClass.SetField1(AValue: Integer);
begin
    FField1 := AValue;
end;

{ TAnotherClass }
procedure TAnotherClass.AnotherMethod;
begin
  inherited MyMethod(meOne);
  Self.MyMethod(meTwo);
end;

begin
  GMyClass := TMyClass.Create;
  GMyClass.MyMethod(meThree);
  GMyClass.Field1 := 123;
  GMyClass.Free;

  asm
    MOV EAX, EBX
    @JMP1:
    XOR ECX, ECX
  end;

end.
