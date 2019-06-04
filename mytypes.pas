unit mytypes;

interface
uses Classes, SysUtils;

type
  TStringListPtr = ^TStringList;


  function IntToBool(const AnInt: Integer): Boolean;
  function BoolToStr( b:boolean ):string;

implementation

function IntToBool(const AnInt: Integer): Boolean;
begin
   if AnInt = 0 then Result := False
                else Result := True;
end;

function BoolToStr( b:boolean ):string;
begin
  if b then begin
    result:= intToStr( 1 );
  end else begin
    result:= intToStr( 0 );
  end;

end;

end.
