unit uCoreTypes;
{
  This unit defines the core, fundamental data types and enumerations used
  throughout the database engine.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  /// <summary>
  ///   Defines the set of data types supported for table fields.
  /// </summary>
  TDBFieldType = (
    ftUnknown,
    ftInteger,    // Standard 32-bit signed integer
    ftString,     // Variable-length string
    ftQWord,      // 64-bit signed integer (Int64)
    ftAutoInc,    // Auto-incrementing 64-bit integer
    ftDateTime,   // Date and time value
    ftMemo,       // Long text field (treated as ftString for storage)
    ftBinaryMemo, // Binary Large Object (BLOB)
    ftByte,       // 8-bit unsigned integer
    ftBoolean     // Boolean type (True/False)
  );

implementation

end.
