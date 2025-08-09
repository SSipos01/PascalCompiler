program FullFeatureTest;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TAnimal = class
  private
    FName: string;
    FAge: integer;
  public
    constructor Create(const AName: string; AAge: integer);
    procedure Greet virtual;
    function GetAgeDescription: string;
    destructor Destroy override;
  end;

  TDog = class(TAnimal)
  public
    procedure Greet override;
    procedure WagTail;
  end;

procedure SayHello(count: integer);
var
  i: integer;
begin
  i := 0;
  while i < count do
  begin
    // writeln('Hello from standalone procedure!');
    i := i + 1;
  end;
end;

function GetDayOfWeek(day: integer): string;
begin
  case day of
    1: Result := 'Sunday';
    2: Result := 'Monday';
    3: Result := 'Tuesday';
    4: Result := 'Wednesday';
    5: Result := 'Thursday';
    6: Result := 'Friday';
    7: Result := 'Saturday';
  else
    Result := 'Unknown';
  end;
end;


{ TAnimal Implementation }
constructor TAnimal.Create(const AName: string; AAge: integer);
begin
  FName := AName;
  FAge := AAge;
  // writeln(FName, ' the animal is born.');
end;

procedure TAnimal.Greet;
begin
  // writeln(FName, ' says: ...');
end;

function TAnimal.GetAgeDescription: string;
begin
  if FAge < 2 then
    Result := 'a baby'
  else if FAge < 10 then
    Result := 'an adult'
  else
    Result := 'a senior';
end;

destructor TAnimal.Destroy;
begin
  // writeln(FName, ' is gone.');
  inherited Destroy;
end;

{ TDog Implementation }
procedure TDog.Greet;
begin
  // writeln(FName, ' says: Woof!');
end;

procedure TDog.WagTail;
var
  i: integer;
begin
  i := 0;
  repeat
    // writeln('*wags tail*');
    i := i + 1;
  until i > 1;
end;

var
  Animal: TAnimal;
  Dog: TDog;
  dayStr: string;

begin
  SayHello(2);
  dayStr := GetDayOfWeek(3);
  // writeln('Day 3 is ', dayStr);

  Dog := TDog.Create('Buddy', 5);
  Dog.Greet;
  Dog.WagTail;
  dayStr := Dog.GetAgeDescription;
  // writeln('Buddy is ', dayStr);
  Dog.Free;
end.
