unit logger;

interface
uses classes, windows, SysUtils, Dialogs,
      dateutils, baseObject;

type
  TLog = class(TBaseObject)
    constructor Create();
    destructor Destroy(); override;

  public
    procedure progress( line: string );
    procedure error( line: string );
    procedure warning( line: string );
    procedure notice( line: string );

  private
    procedure write( line: string );

  private
    fileStream:TFileStream;

    const LE: string = #13+#10;

    const LOG_PROGRESS: string = 'PROGRESS>';
    const LOG_ERROR: string = 'ERROR>';
    const LOG_WARNING: string = 'WARNING>';
    const LOG_NOTICE: string = 'NOTICE>';

  end;
var
    debugLog:TLog;

implementation

constructor TLog.Create();
var filename: string;
begin
  inherited Create();

  filename:= '.\debug.log';

  // always re-create
  try
    fileStream:= TFileStream.Create(filename, fmCreate, fmShareExclusive );
  except
    raise Exception.Create('Cannot create file '+filename);
  end;
  fileStream.Destroy;

  // open exclusive for read/write
  try
    fileStream:= TFileStream.Create(filename, fmOpenReadWrite, fmShareDenyWrite );
  except
    raise Exception.Create('Cannot open file '+filename);
  end;

  self.progress('start logging');
end;

destructor TLog.destroy;
begin
  if fileStream <> nil then begin
    fileStream.Destroy;
  end;
  inherited destroy;
end;

procedure TLog.write( line: string );
var tempStr:ansistring;
    count: integer;
begin
  tempStr:= line + LE;
  count:= length(tempStr);
  fileStream.WriteBuffer( Pointer(tempStr)^, count  );
end;

procedure TLog.progress( line: string );
begin
  write( LOG_PROGRESS + line );
end;

procedure TLog.error( line: string );
begin
  write( LOG_ERROR + line );
end;

procedure TLog.warning( line: string );
begin
  write( LOG_WARNING + line );
end;

procedure TLog.notice( line: string );
begin
  write( LOG_NOTICE + line );
end;


end.
