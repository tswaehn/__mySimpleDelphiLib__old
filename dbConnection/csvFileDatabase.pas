unit csvFileDatabase;

interface
uses SysUtils, Classes, dialogs, mytypes,
      csvHandler, baseObject, dbLoadingPanel_unit;

type
  TCsvFileDataBase = class (TBaseObject)
    constructor Create( filename_:string );
    destructor Destroy(); override;

    // mem functions
    function searchRowInMem( searchStr:string; searchCol:integer ):TStringListPtr;
    function getRowFromMem( index: integer ):TStringListPtr;
    function getRowMemCount():integer;
    procedure clearAllRows();

    // add line to memory
    procedure addRowToMem( row: TStringListPtr );
    procedure updateRowInMem( searchStr:string; searchCol:integer; newRow:TStringListPtr );

    // store memory into file
    procedure storeToFile();

    private
      // read all into mem and work on mem
      function loadAllRowsFromDBintoMem():boolean; // list of TStringList objects
      function saveAllRowsFromMemIntoDB():boolean;
      procedure clearAllRowsFromDBfromMem();

    private

      procedure rewriteCsvFile();
      procedure checkHeader( header:TStringList );

    private

      filename:string;
      version:string;
      lastModify: string;
      totalRowCount:integer;

      tableRows: TList;
  end;

const VERSION_STR: string = 'Version';
CONST COMPATIBLE_VERSION: string = '1.0';
CONST FILE_TYPE: string = 'csvFileDatabase';

CONST INDEX: string = '<index>';
CONST EMPTY: string = '<empty>';

implementation

constructor TCsvFileDataBase.Create(filename_: string);
begin
  inherited Create();

  filename:= filename_;
  totalRowCount:= 0;
  tableRows:= nil;

  try
    loadAllRowsFromDBintoMem();
  except
    // failed to load settings
    raise Exception.Create('TCsvFileDataBase.Create() cannot load from file '+filename_);
  end;


end;

destructor TCsvFileDatabase.destroy;
begin

  clearAllRowsFromDBfromMem;
  tableRows.Destroy();

  inherited destroy;
end;

procedure TCsvFileDatabase.storeToFile();
begin
  saveAllRowsFromMemIntoDB();
end;

procedure TCsvFileDatabase.checkHeader(header:TStringList);
begin

  // version string
  if (AnsiCompareStr(VERSION_STR, header.Strings[0]) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' not a valid csvFileDatabase');
  end;

  // version info
  version:= header.Strings[1];
  if (AnsiCompareStr(COMPATIBLE_VERSION, version) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' incompatible version of csvFileDatabase');
  end;

  // type name
  if (AnsiCompareStr(FILE_TYPE,header.Strings[2]) <> 0) then begin
    raise Exception.Create('CSV file '+filename+' not a valid csvFileDatabase');
  end;

  // last modify date
  lastModify:= header.Strings[3];

  // line count
  totalrowCount:= strToInt( header.Strings[4] );

end;

procedure TCsvFileDatabase.rewriteCsvFile();
var headerLine:TStringList;
    csvHandler:TCsvHandler;
begin
  headerLine:= TStringList.Create();
  //totalLineCount:= 0;

  headerLine.Add(VERSION_STR);
  headerLine.Add(COMPATIBLE_VERSION);
  headerLine.Add(FILE_TYPE);
  headerLine.Add( DateToStr(date()) );
  headerLine.Add( intToStr( totalRowCount ) );

  // open file
  csvHandler:= TCsvHandler.Create(filename, true);
  csvHandler.writeLine( @headerLine );
  csvHandler.Destroy;

  // free line
  headerLine.Free;
end;

function TCsvFileDatabase.loadAllRowsFromDBintoMem():boolean;
var
  row:TStringList;
  csvHandler:TCsvHandler;
  header:TStringList;
  i:integer;
  dbLoadingPanel:TDBLoadingPanel;
begin
  result:= false;
  clearAllRowsFromDBfromMem();

  // create empty list
  tableRows:= TList.Create();

  // open file
  csvHandler:= TCsvHandler.Create(filename);

  header:= csvHandler.readLine();
  // if header is empty
  if (header=nil) then begin
    // close file
    csvHandler.Destroy;
    exit;
  end;

  // check header
  try
    checkHeader(header);
  except
    // header is incorrect
    totalRowCount:=0;
    rewriteCsvFile();
    // set empty mem
    clearAllRowsFromDBfromMem();
    // bye
    csvHandler.Destroy;
    exit;
  end;

  dbLoadingPanel:=TDBLoadingPanel.Create('loading from table ['+filename+']... ', totalRowCount);

  // finally load all rows
  i:=0;
  repeat
    row:= csvHandler.readLine();
    if (row <> nil) then begin
      tableRows.Add( row );
      INC(i);
      dbLoadingPanel.updatePosition(i);
    end;
  until row = nil;

  // close file
  csvHandler.Destroy;

  dbLoadingPanel.Hide;

  // check the correct number of rows
  if (totalRowCount <> tableRows.Count) then begin
    // fix the database \TODO: create a copy before rewrite
    totalRowCount:=0;
    rewriteCsvFile();
    // set empty mem
    clearAllRowsFromDBfromMem();
    //
    raise Exception.Create('csvFileDatabase header row count incorrect ['+filename+']');
  end;


  totalRowCount:= tableRows.Count;
  result:= true;
end;

function TCsvFileDatabase.saveAllRowsFromMemIntoDB():boolean;
var
  row:TStringList;
  csvHandler:TCsvHandler;
  header:TStringList;
  I: Integer;
begin
  result:= false;
  if (tableRows = nil) then begin
    exit;
  end;

  // create header
  totalRowCount:= tableRows.Count;
  rewriteCsvFile();

  // open file and add lines
  csvHandler:= TCsvHandler.Create(filename);
  header:= csvHandler.readLine();
  // row by row
  for i := 0 to tableRows.Count-1 do begin
    row:= tableRows.Items[i];
    csvHandler.writeLine( @row );
  end;
  // close file
  csvHandler.Destroy;
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
    tableRows.clear;
  end;
end;

function TCsvFileDatabase.searchRowInMem( searchStr:string; searchCol:integer ):TStringListPtr;
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
    temp:= row.Strings[searchCol];
    if AnsiCompareStr(searchStr, temp) = 0 then begin
      result:= @row;
      exit;
    end;
  end;

  result:= nil;
end;

procedure TCsvFileDatabase.addRowToMem( row: TStringListPtr );
var newRow:TStringList;
  I: Integer;
begin
  // create a new object
  newRow:= TStringList.Create();
  // fill with strings
  for I := 0 to row.Count-1 do begin
    newRow.Add( row.Strings[i] );
  end;
  // add to list
  tableRows.Add( newRow );
end;

procedure TCsvFileDatabase.updateRowInMem( searchStr:string; searchCol:integer; newRow:TStringListPtr );
var row:TStringList;
    rowPtr:TStringListPtr;
  I: Integer;
begin
  // get the correct row
  rowPtr:= searchRowInMem( searchStr, searchCol );
  if (rowPtr = nil) then begin
  // create row if non exist
    row:= TStringList.Create;
    tableRows.Add(row);
    rowPtr:= TStringListPtr(row);
  end else begin
    row:= rowPtr^;
  end;

  // update
  row.Clear;
  for I := 0 to newRow.Count-1 do begin
    row.Add( newRow.Strings[i] );
  end;

end;

function TCsvFileDatabase.getRowFromMem( index: integer ):TStringListPtr;
var row:TStringList;
begin
  result:= nil;
  if (tableRows=nil) then begin
    exit;
  end;
  if (index>=0) and (index<tableRows.Count) then begin
    row:= tableRows.Items[index];
    result:= @row;
  end;
end;

function TCsvFileDatabase.getRowMemCount():integer;
begin
  result:= 0;
  if (tableRows=nil) then begin
    exit;
  end;
  result:= tableRows.Count;
end;

procedure TCsvFileDatabase.clearAllRows();
begin
  self.clearAllRowsFromDBfromMem();
end;

end.
