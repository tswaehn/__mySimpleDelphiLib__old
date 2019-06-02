unit mytypes;

interface
uses Classes;

type
  TStringListPtr = ^TStringList;


  function IntToBool(const AnInt: Integer): Boolean;

implementation

function IntToBool(const AnInt: Integer): Boolean;
begin
   if AnInt = 0 then Result := False
                else Result := True;
end;

end.
