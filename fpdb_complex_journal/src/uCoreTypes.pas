unit uCoreTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDBFieldType defines the supported data types in the database. }
  TDBFieldType = (
    ftUnknown,
    ftInteger,    // Standard 32-bit signed integer
    ftString,     // Variable-length string
    ftQWord,      // 64-bit signed integer (Int64)
    ftAutoInc,    // Auto-incrementing 64-bit integer
    ftDateTime,   // Date and time value
    ftMemo,       // Long text field
    ftBinaryMemo, // Binary Large Object (BLOB)
    ftByte,       // 8-bit unsigned integer
    ftBoolean     // Boolean type (True/False)
  );

implementation

end.
