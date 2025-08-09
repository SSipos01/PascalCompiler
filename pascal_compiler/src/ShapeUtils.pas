unit ShapeUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math;

// --- Type Definitions exposed by the unit ---

type
  // Enumerated Type
  TColor = (clRed, clGreen, clBlue, clYellow, clBlack, clWhite);

  // Base Class Definition
  TShape = class
  private
    FColor: TColor;
    FName: string;
  protected
    function GetName: string;
  public
    constructor Create(const AName: string; AColor: TColor);
    destructor Destroy; override;
    procedure DisplayInfo;
    function CalculateArea: Real; virtual; abstract; // Abstract virtual method
    property Name: string read GetName;
    property Color: TColor read FColor write FColor;
  end;

  // Descendant Classes
  TCircle = class(TShape)
  private
    FRadius: Real;
  public
    constructor Create(const AName: string; AColor: TColor; ARadius: Real);
    function CalculateArea: Real; override; // Override the abstract method
    property Radius: Real read FRadius write FRadius;
  end;

  TRectangle = class(TShape)
  private
    FWidth: Real;
    FHeight: Real;
  public
    constructor Create(const AName: string; AColor: TColor; AWidth, AHeight: Real);
    function CalculateArea: Real; override;
  end;

implementation

// --- TShape Methods ---
constructor TShape.Create(const AName: string; AColor: TColor);
begin
  inherited Create; // Call ancestor constructor
  FName := AName;
  FColor := AColor;
  // WriteLn('TShape.Create (from unit) called for: ', FName);
end;

destructor TShape.Destroy;
begin
  // WriteLn('TShape.Destroy (from unit) called for: ', FName);
  inherited Destroy;
end;

function TShape.GetName: string;
begin
  Result := FName;
end;

procedure TShape.DisplayInfo;
begin
  // WriteLn('Shape Name: ', Name, ', Color index: ', Ord(Color));
end;

// --- TCircle Methods ---
constructor TCircle.Create(const AName: string; AColor: TColor; ARadius: Real);
begin
  inherited Create(AName, AColor); // Call parent constructor
  FRadius := ARadius;
  // WriteLn('TCircle.Create (from unit) called for: ', FName);
end;

function TCircle.CalculateArea: Real;
begin
  Result := Pi * Sqr(FRadius); // Using Pi from Math unit, Sqr function
end;

// --- TRectangle Methods ---
constructor TRectangle.Create(const AName: string; AColor: TColor; AWidth, AHeight: Real);
begin
  inherited Create(AName, AColor);
  FWidth := AWidth;
  FHeight := AHeight;
  // WriteLn('TRectangle.Create (from unit) called for: ', FName);
end;

function TRectangle.CalculateArea: Real;
begin
  Result := FWidth * FHeight;
end;

end.
