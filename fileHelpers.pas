unit fileHelpers;

interface
uses
    WinApi.Windows, SysUtils;

  function FileSizeEx(const aFilename: String): Int64;
  procedure checkFolderIfNotExistsCreateIt( filename: string );

implementation


  function FileSizeEx(const aFilename: String): Int64;
  var
    info: TWin32FileAttributeData;
  begin
    result := -1;

    if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
      EXIT;

    result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
  end;

procedure checkFolderIfNotExistsCreateIt( filename: string );
var path:string;
begin
  path:= ExtractFilePath( filename );
  if (directoryexists(path) = false) then begin
    forcedirectories(path);
  end;
end;


end.
