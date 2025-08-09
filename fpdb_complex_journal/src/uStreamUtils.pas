unit uStreamUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants;

procedure WriteString(Stream: TStream; const S: string);
function ReadString(Stream: TStream): string;
procedure WriteInteger(Stream: TStream; I: Integer);
function ReadInteger(Stream: TStream): Integer;
procedure WriteInt64(Stream: TStream; I: Int64);
function ReadInt64(Stream: TStream): Int64;
procedure WriteDateTime(Stream: TStream; DT: TDateTime);
function ReadDateTime(Stream: TStream): TDateTime;
procedure WriteBoolean(Stream: TStream; B: Boolean);
function ReadBoolean(Stream: TStream): Boolean;
procedure WriteByte(Stream: TStream; B: Byte);
function ReadByte(Stream: TStream): Byte;
procedure WriteBytes(Stream: TStream; const B: TBytes);
function ReadBytes(Stream: TStream): TBytes;
procedure WriteVariant(Stream: TStream; const V: Variant);
function ReadVariant(Stream: TStream): Variant;

implementation

procedure WriteString(Stream: TStream; const S: string);
var
  Len: Integer;
begin
  Len := Length(S);
  Stream.WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    Stream.WriteBuffer(S[1], Len);
end;

function ReadString(Stream: TStream): string;
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, SizeOf(Integer));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    Stream.ReadBuffer(Result[1], Len);
  end
  else
    Result := '';
end;

procedure WriteInteger(Stream: TStream; I: Integer);
begin
  Stream.WriteBuffer(I, SizeOf(Integer));
end;

function ReadInteger(Stream: TStream): Integer;
begin
  Stream.ReadBuffer(Result, SizeOf(Integer));
end;

procedure WriteInt64(Stream: TStream; I: Int64);
begin
  Stream.WriteBuffer(I, SizeOf(Int64));
end;

function ReadInt64(Stream: TStream): Int64;
begin
  Stream.ReadBuffer(Result, SizeOf(Int64));
end;

procedure WriteDateTime(Stream: TStream; DT: TDateTime);
begin
  Stream.WriteBuffer(DT, SizeOf(TDateTime));
end;

function ReadDateTime(Stream: TStream): TDateTime;
begin
  Stream.ReadBuffer(Result, SizeOf(TDateTime));
end;

procedure WriteBoolean(Stream: TStream; B: Boolean);
begin
  Stream.WriteBuffer(B, SizeOf(Boolean));
end;

function ReadBoolean(Stream: TStream): Boolean;
begin
  Stream.ReadBuffer(Result, SizeOf(Boolean));
end;

procedure WriteByte(Stream: TStream; B: Byte);
begin
  Stream.WriteBuffer(B, SizeOf(Byte));
end;

function ReadByte(Stream: TStream): Byte;
begin
  Stream.ReadBuffer(Result, SizeOf(Byte));
end;

procedure WriteBytes(Stream: TStream; const B: TBytes);
var
  Len: Integer;
begin
  Len := Length(B);
  Stream.WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    Stream.WriteBuffer(B[0], Len);
end;

function ReadBytes(Stream: TStream): TBytes;
var
  Len: Integer;
begin
  SetLength(Result, 0); // Initialize to be safe
  Stream.ReadBuffer(Len, SizeOf(Integer));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    Stream.ReadBuffer(Result[0], Len);
  end;
end;

procedure WriteVariant(Stream: TStream; const V: Variant);
var
  vt: Integer;
begin
  vt := VarType(V);
  WriteInteger(Stream, vt);
  case vt of
    varEmpty, varNull: ; // Do nothing
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
      WriteInteger(Stream, V);
    varInt64:
      WriteInt64(Stream, V);
    varSingle, varDouble, varCurrency:
      WriteDateTime(Stream, V); // TDateTime is a double
    varDate:
      WriteDateTime(Stream, V);
    varOleStr, varString:
      WriteString(Stream, V);
    varBoolean:
      WriteBoolean(Stream, V);
    varVariant:
      WriteVariant(Stream, V); // Should not happen in our case
    // For binary memo, we'll use a convention: an array of bytes
    varArray or varByte:
      WriteBytes(Stream, V);
  else
    raise Exception.CreateFmt('Unsupported variant type: %d', [vt]);
  end;
end;

function ReadVariant(Stream: TStream): Variant;
var
  vt: Integer;
begin
  vt := ReadInteger(Stream);
  case vt of
    varEmpty: Result := Null;
    varNull: Result := Null;
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
      Result := ReadInteger(Stream);
    varInt64:
      Result := ReadInt64(Stream);
    varSingle, varDouble, varCurrency:
      Result := ReadDateTime(Stream);
    varDate:
      Result := ReadDateTime(Stream);
    varOleStr, varString:
      Result := ReadString(Stream);
    varBoolean:
      Result := ReadBoolean(Stream);
    varArray or varByte:
      Result := ReadBytes(Stream);
  else
    raise Exception.CreateFmt('Unsupported variant type: %d', [vt]);
  end;
end;

end.
