program TestFGL;

{$mode objfpc}{$H+}

uses
  fgl;

var
  MyList: TFPObjectList;

begin
  MyList := TFPObjectList.Create(True);
  Writeln('List created successfully.');
  MyList.Free;
end.
