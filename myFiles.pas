unit myFiles;

interface

uses SysUtils, Windows, Forms, Messages, SHFolder, idGlobal, ShellApi;


  procedure createPathIfNotExists( path: string );

  function ShellExecute_AndWait(Operation, FileName, Parameter, Directory: string; Show: Word; bWait: Boolean): Longint;

  function getHomePath():string;
  function getAppDataPath():string;
  function GetAppVersionStr: string;

  function isFile( filename:string ):boolean;
  function isSvgFile( filename:string ):boolean;


implementation


(*  typical values for folderType
      0: specialFolder := CSIDL_PERSONAL;
     //All Users\Application Data
     1: specialFolder := CSIDL_COMMON_APPDATA;
     //[User Specific]\Application Data
     2: specialFolder := CSIDL_LOCAL_APPDATA;
     //Program Files
     3: specialFolder := CSIDL_PROGRAM_FILES;
     //All Users\Documents
     4: specialFolder := CSIDL_COMMON_DOCUMENTS;
*)
function GetSpecialFolderPath(folderType : integer) : string;
 const
   SHGFP_TYPE_CURRENT = 0;
 var
   path: array [0..MAX_PATH] of char;
 begin
   if SUCCEEDED(SHGetFolderPath(0,folderType,0,SHGFP_TYPE_CURRENT,@path[0])) then begin
     Result := path
   end else begin
     Result := '';
   end;
 end;

procedure createPathIfNotExists( path: string );
begin
  if (DirectoryExists(path) = false) then begin
     ForceDirectories( path );
  end;
end;

function getHomePath():string;
begin
  result:= GetSpecialFolderPath( CSIDL_PERSONAL );
end;

function getAppDataPath():string;
var path:string;
begin
  path:= GetSpecialFolderPath( CSIDL_LOCAL_APPDATA );
  path:= path +'\HSEB Dresden GmbH\KabelEditor\';
  if (DirectoryExists(path) = false) then begin
     ForceDirectories( path );
  end;
  result:= path;
end;

function GetAppVersionStr: string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi,  //release
     LongRec(FixedPtr.dwFileVersionLS).Lo]) //build
end;

(*
    This function opens a file and lets windows decide
    which program to use.
*)
procedure openFileExternal( filename: string );
var link:string;

begin
    link:=filename;

    ShellExecute(0, nil, PChar(link), nil,  nil, SW_SHOWNORMAL);
end;

{
  ****** Parameters ******
  Operation:

  edit  Launches an editor and opens the document for editing.
  explore Explores the folder specified by lpFile.
  find Initiates a search starting from the specified directory.
  open Opens the file, folder specified by the lpFile parameter.
  print Prints the document file specified by lpFile.
  properties Displays the file or folder's properties.

  FileName:

  Specifies the name of the file or object on which
  ShellExecuteEx will perform the action specified by the lpVerb parameter.

  Parameter:

  String that contains the application parameters.
  The parameters must be separated by spaces.

  Directory:

  specifies the name of the working directory.
  If this member is not specified, the current directory is used as the working directory.

  Show:

  Flags that specify how an application is to be shown when it is opened.
  It can be one of the SW_ values

  bWait:

  If true, the function waits for the process to terminate
}
function ShellExecute_AndWait(Operation, FileName, Parameter, Directory: string;
  Show: Word; bWait: Boolean): Longint;
var
  bOK: Boolean;
  Info: TShellExecuteInfo;
begin
  FillChar(Info, SizeOf(Info), Chr(0));
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_NOCLOSEPROCESS;
  Info.lpVerb := PChar(Operation);
  Info.lpFile := PChar(FileName);
  Info.lpParameters := PChar(Parameter);
  Info.lpDirectory := PChar(Directory);
  Info.nShow := Show;
  bOK := Boolean(ShellExecuteEx(@Info));
  if bOK then
  begin
    if bWait then
    begin
      while
        WaitForSingleObject(Info.hProcess, 100) = WAIT_TIMEOUT
        do Application.ProcessMessages;
      bOK := GetExitCodeProcess(Info.hProcess, DWORD(Result));
    end
    else
      Result := 0;
  end;
  if not bOK then Result := -1;
end;

function isFile( filename:string ):boolean;
var
  DirEx   : Cardinal;
begin
  result:= false;
  if fileexists(filename)=false then begin
    result:=false;
    exit;
  end;
  DirEx:=GetFileAttributesA(PAnsiChar(filename));
  if DirEx<>DWord(-1) then
  begin
    if FILE_ATTRIBUTE_DIRECTORY and DirEx=FILE_ATTRIBUTE_DIRECTORY then result:=false else result:=true;
  end;
end;

function isSvgFile( filename:string ):boolean;
begin
  if isFile(filename)=false then begin
    result:=false;
    exit;
  end;
  if (pos('.SVG', uppercase(filename)) > 0) then begin
    result:=true;
  end else begin
    result:=false;
  end;
end;

end.
