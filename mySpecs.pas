unit helper;

interface
uses SysUtils, Windows, Forms, Messages, SHFolder, idGlobal, ShellApi;


  function RGB2TColor(const R, G, B: Byte): Integer;
  //function getHomePath():string;
  function quote( filename:string ):string;
  procedure openArticleInBrowser( article: string );
  procedure openFileExternal( filename: string );
  function replaceFixedDriveLetters( filename:string ):string;

implementation

function RGB2TColor(const R, G, B: Byte): Integer;
begin
  // convert hexa-decimal values to RGB
  Result := R + (G shl 8) + (B shl 16);
end;


procedure openArticleInBrowser( article: string );
var link:string;

begin
    link:='http://tank/abas/?action=search&search='+article;

    ShellExecute(0,
               'open',
               PCHAR(link),
               nil,
               nil,
               SW_SHOW);
end;


function replaceFixedDriveLetters( filename:string ):string;
var temp:string;
    index:integer;
begin
  temp:= filename;
  if (pos('W:', upperCase( filename)) > 0) then begin
    // replace "W:"
    temp:= copy(filename, 3, length(filename)-2);
    temp:= '\\hseb-sv2\Daten'+temp;
  end;

  result:= temp;
end;

function quote( filename:string ):string;
begin
  result:= '"'+filename+'"';
end;

end.
