unit myMath;

interface

uses myDiverse;

  function median( values: array of double ): double;

implementation

function median( values: array of double ): double;
var temp: array of double;
    i: Integer;
    iMiddle: integer;
    len: integer;
begin
  len:= length( values );
  // create a copy
  setLength( temp, len );
  for i := 0 to len - 1 do begin
    temp[i]:= values[i];
  end;

  // sort the array
  QuickSort( temp, low(temp), high(temp) );

  //
  if odd( len ) then begin
    iMiddle:= (len-1) div 2;
  end else begin
    iMiddle:= len div 2;
  end;

  result:= temp[iMiddle];
end;

end.
