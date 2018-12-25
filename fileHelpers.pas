unit fileHelpers;

interface
uses
    WinApi.Windows;

  function FileSizeEx(const aFilename: String): Int64;

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

end.
