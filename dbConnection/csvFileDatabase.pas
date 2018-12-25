unit csvFileDatabase;

interface
uses SysUtils, Classes, dialogs, mytypes,
      csvHandler, baseObject;

type
  TCsvFileDataBase = class (TBaseObject)
    constructor Create( filename_:string );
    destructor Destroy(); override;

    // read by line
    procedure rewind();
    function readNextLine(): TStringList;

    //
    procedure appendLine( row: TStringList );

    // read all into mem and work on mem
    function loadAllRowsFromDBintoMem():TList; // list of TStringList objects
    procedure clearAllRowsFromDBfromMem();
    function searchRowInMem( searchStr:string; col:integer ):TStringList;

    private
      procedure writeHeader();
      procedure checkHeader();
      procedure goToEOF();

    private
      csvFile:TCsvHandler;
      filename:string;
      version:string;
      lastModify: string;
      totalLineCount:integer;

      tableRows: TList;
  end;

const VERSION_STR: string = 'Version';
CONST COMPATIBLE_VERSION: string = '1.0';
CONST FILE_TYPE: string = 'csvFileDatabase';

implementation

constructor TCsvFileDataBase.Create(filename_: string);
begin
  inherited Create();

  filename:= filename_;
  csvFile:= TCsvHandler.Create(filename);

  if csvFile.isEmpty then begin
    writeHeader();
  end;

  checkHeader();

end;

destructor TCsvFileDatabase.destroy;
begin
  csvFile.destroy;

  self.clearAllRowsFromDBfromMem;

  inherited destroy;
end;

procedure TCsvFileDatabase.checkHeader();
var headerLine:TStringList;
begin
  csvFile.rewind();

  // check header
  headerLine:= csvFile.readLine();
  // version string
  if (AnsiCompareStr(VERSION_STR, headerLine.Strings[0]) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' not a valid csvFileDatabase');
  end;

  // version info
  version:= headerLine.Strings[1];
  if (AnsiCompareStr(COMPATIBLE_VERSION, version) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' incompatible version of csvFileDatabase');
  end;

  // type name
  if (AnsiCompareStr(FILE_TYPE,headerLine.Strings[2]) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' not a valid csvFileDatabase');
  end;

  // last modify date
  lastModify:= headerLine.Strings[3];

  // line count
  totalLineCount:= strToInt( headerLine.Strings[4] );

  headerLine.free;
end;

procedure TCsvFileDatabase.writeHeader;
var headerLine:TStringList;
begin
  headerLine:= TStringList.Create();
  totalLineCount:= 0;

  headerLine.Add(VERSION_STR);
  headerLine.Add(COMPATIBLE_VERSION);
  headerLine.Add(FILE_TYPE);
  headerLine.Add( DateToStr(date()) );
  headerLine.Add( intToStr( totalLineCount ) );

  csvFile.writeLine( @headerLine );

  headerLine.Free;
end;

procedure TCsvFileDatabase.rewind();
begin
  // rewind + check header
  checkHeader();
end;

procedure TCsvFileDatabase.goToEOF();
var row:TStringList;
begin
  repeat
    row:= self.readNextLine;
  until row=nil;
end;

function TCsvFileDatabase.readNextLine():TStringList;
begin
  result:= csvFile.readLine();
end;

procedure TCsvFileDatabase.appendLine( row: TStringList );
begin
  goToEOF();
  csvFile.writeLine(@row);
end;

function TCsvFileDatabase.loadAllRowsFromDBintoMem():TList;
var
      row:TStringList;
begin
  clearAllRowsFromDBfromMem();

  tableRows:= TList.Create();

  rewind();

  repeat
    row:= readNextLine();
    if (row <> nil) then begin
      tableRows.Add( row );
    end;
  until row = nil;

  result:= tableRows;
end;

procedure TCsvFileDatabase.clearAllRowsFromDBfromMem();
var i:integer;
    row:TStringList;
begin
  if tableRows <> nil then begin
    for I := 0 to tableRows.Count-1 do begin
      row:= TStringList( tableRows.Items[i] );
      row.Free;
    end;
    tableRows.Free;
  end;
end;

function TCsvFileDatabase.searchRowInMem( searchStr:string; col:integer ):TStringList;
var
  i: Integer;
  row: TStringList;
  temp: string;
begin
  if tableRows = nil then begin
    raise Exception.Create('DB: search in non existing mem');
  end;

  for i := 0 to tableRows.Count-1 do begin
    row:= TStringList( tableRows.Items[i] );
    temp:= row.Strings[col];
    if AnsiCompareStr(searchStr, temp) = 0 then begin
      result:= row;
      exit;
    end;
  end;

  result:= nil;
end;

end.
