unit myDiverse;

interface
uses sysutils, strUtils;

  // removes defined chars from a given string and replaces each occurence by a replaceStr
  function StripChars(const aSrc, aCharsToStrip, aReplaceStr: string): string;

  // same as pos() but starts from the end of the string
  function revPos( sub:string; str:string ):integer;

  // simple sorting algo that works on a list of doubles
  procedure QuickSort(var List: array of Double; iLo, iHi: Integer) ;

implementation

function StripChars(const aSrc, aCharsToStrip, aReplaceStr: string): string;
var
  c: Char;
begin
  Result := aSrc;
  for c in aCharsToStrip do
    Result := StringReplace(Result, c, aReplaceStr, [rfReplaceAll, rfIgnoreCase]);
end;

function revPos( sub:string; str:string ):integer;
var i,k:integer;
begin

  k:=0;
  repeat
    i:=k;
    k:=posEx( sub, str,i+1)
  until (k=0);
  result:= i;
end;


procedure QuickSort(var List: array of Double; iLo, iHi: Integer) ;
var
  Lo       : integer;
  Hi       : integer;
  T        : Double;
  Mid      : Double;
begin
  Lo := iLo;
  Hi := iHi;
  Mid:= List[(Lo + Hi) div 2];
  repeat

    while List[Lo] < Mid do Inc(Lo) ;
    while List[Hi] > Mid do Dec(Hi) ;

    if Lo <= Hi then
    begin
      T := List[Lo];
      List[Lo] := List[Hi];
      List[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;

  until Lo > Hi;

  if Hi > iLo then QuickSort(List, iLo, Hi);
  if Lo < iHi then QuickSort(List, Lo, iHi);

end;


end.
