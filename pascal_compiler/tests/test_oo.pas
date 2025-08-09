program ObjectOrientedExample;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TAnimal = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    procedure Eat overload;
    procedure Eat(const Food: string) overload;
    procedure MakeSound virtual;
    destructor Destroy override;
  end;

  TDog = class(TAnimal)
  public
    procedure MakeSound override;
  end;

var
  MyAnimal: TAnimal;
  MyDog: TDog;

{ TAnimal Implementation }
constructor TAnimal.Create(const AName: string);
begin
  FName := AName;
  writeln(FName, ' is born.');
end;

procedure TAnimal.Eat;
begin
  writeln(FName, ' eats.');
end;

procedure TAnimal.Eat(const Food: string);
begin
  writeln(FName, ' eats ', Food, '.');
end;

procedure TAnimal.MakeSound;
begin
  writeln(FName, ' makes a generic sound.');
end;

destructor TAnimal.Destroy;
begin
  writeln(FName, ' is gone.');
  inherited Destroy;
end;

{ TDog Implementation }
procedure TDog.MakeSound;
begin
  writeln(FName, ' barks: Woof! Woof!');
end;

begin
  MyAnimal := TAnimal.Create('Generic Animal');
  MyAnimal.MakeSound;
  MyAnimal.Eat;
  MyAnimal.Eat('food');
  MyAnimal.Free;

  writeln('');

  MyDog := TDog.Create('Buddy');
  MyDog.MakeSound;
  MyDog.Eat('kibble');
  MyDog.Free;
end.
