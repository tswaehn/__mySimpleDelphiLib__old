unit fileCrawler;

interface
uses Classes, SysUtils, forms;

type
  TProgressUpdateProc = procedure(text: string) of object;

type
  TFileCrawler = class(TObject)
    constructor Create();
    destructor Destroy(); override;

    procedure start( rootDir_: string; onProgressUpdate_: TProgressUpdateProc);
    function getFileList():TStringList;
    function getTotalFolderScanned():integer;

    private
      procedure FileSearch(const dirName:string);
      procedure progressUpdate(text:string);

    private
      rootDir:string;
      totalFolderScanned:integer;
      onProgressUpdate: TProgressUpdateProc;

      fileList:TStringList;
  end;

implementation

constructor TFileCrawler.Create;
begin
  inherited Create();
  fileList:= TStringList.Create();
  onProgressUpdate:= nil;
end;

destructor TFileCrawler.Destroy();
begin
  fileList.destroy;
  onProgressUpdate:= nil;

  inherited Destroy();
end;

procedure TFileCrawler.start( rootDir_: string; onProgressUpdate_: TProgressUpdateProc);
begin
  if Assigned(onProgressUpdate_) then begin
    self.onProgressUpdate:= onProgressUpdate_;
  end;
  self.rootDir:= rootDir_;

  totalFolderScanned:= 0;
  fileSearch( rootDir );
end;

function TFileCrawler.getFileList():TStringList;
begin
  result:= fileList;
end;

function TFileCrawler.getTotalFolderScanned():integer;
begin
  result:= self.totalFolderScanned;
end;


procedure TFileCrawler.FileSearch(const dirName:string);
var
  searchResult: TSearchRec;
  filename:string;
  ext:string;
begin
  ext:= '.ini';
  ext:= '';

  progressUpdate(dirName);
  totalFolderScanned:= totalFolderScanned +1;
  if FindFirst(dirName+'\*', faAnyFile, searchResult)=0 then begin
    try
      repeat
        if (searchResult.Attr and faDirectory)=0 then begin
          if (ext <> '') then begin
            if SameText(ExtractFileExt(searchResult.Name), '.ini') then begin
              filename:= IncludeTrailingBackSlash(dirName)+searchResult.Name;
              fileList.Add( filename );
            end;
          end else begin
            filename:= IncludeTrailingBackSlash(dirName)+searchResult.Name;
            fileList.Add( filename );
          end;
        end else if (searchResult.Name<>'.') and (searchResult.Name<>'..') then begin
          FileSearch(IncludeTrailingBackSlash(dirName)+searchResult.Name);
        end;
      until FindNext(searchResult)<>0
    finally
      FindClose(searchResult);
    end;
  end;
end;

procedure TFileCrawler.progressUpdate(text:string);
begin
  // call only if already set
  if Assigned(onProgressUpdate)then begin
    onProgressUpdate( text );
  end;
  Application.ProcessMessages;

end;


end.
