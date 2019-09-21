unit versioninfo;

interface
uses
  Windows,SysUtils ;

type
  // 1: Major, 2: Minor, 3: Release, 4: Build
  TFileVersion = array[1..4] of Smallint;

function GetVersion(FileName: string): TFileVersion;
function GetVersionStr():string;


implementation

function GetVersionStr():string;
var    exeFilename:string;
      ver: TFileVersion;
begin
  exeFilename:=  ParamStr(0);
  ver:= GetVersion(exeFilename);

  result:= 'v'+inttostr(ver[1])+'.'+inttostr(ver[2])+'.'+inttostr(ver[3])+'.'+inttostr(ver[4] )  ;
//
end;

function GetVersion(FileName: string): TFileVersion;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  // Länge der Dateiinformationen bekommen.
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  // Speicherplatz reservieren.
  GetMem(VerInfo, VerInfoSize);
  // Dateiinformationen selektieren.
  GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
  // Prüfen ob Dateiinformationen vorhanden
  if VerInfo <> nil then
  begin
    // Dateiinformationen auslesen.
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    // Version auslesen und zurückgeben.
    with VerValue^ do
    begin
      // Major
      Result[1] := smallInt(dwFileVersionMS shr 16);
      // Minor
      Result[2] := smallInt(dwFileVersionMS and $0000FFFF);
      // Release
      Result[3] := smallInt(dwFileVersionLS shr 16);
      // Build
      Result[4] := smallInt(dwFileVersionLS and $0000FFFF);
    end;
  end;
  // Speicher freigeben (Dateiinformationen).
  FreeMem(VerInfo, VerInfoSize);
end;

end.
