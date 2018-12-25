unit regEx;

interface
uses sysUtils;

  function dirNameToRegex( strIn: string ):string;

implementation


function dirNameToRegex( strIn: string ):string;
var
  strOut: string;
begin
  strOut:= stringreplace(strIn, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  result:= strOut;
end;


end.
