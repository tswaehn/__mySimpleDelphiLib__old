unit dataConnector;

interface
uses  SysUtils, Classes,
      localSettings, globalSettings, mytypes,
      csvFileDatabase;

type
  TDataConnector = class(TObject)
    constructor Create();
    destructor Destroy(); override;

  public
    procedure doConnect();

    // use startQuery + getRow to browse through table
    procedure startQuery( tableName:string );
    function getRowFromTable( tableName:string ):TStringList;

    // write to table
    procedure addRowToTable( tableName:string; row:TStringList );

    //
    function getDatabaseFolder(): string;

  private
    function getTable( tableName:string ): TCsvFileDatabase;

  private
    isConnected:boolean;
    localSettings:TLocalSettings;
    globalSettings:TGlobalSettings;

    tables:TStringList;

  CONST DB_GLOBAL_CACHE_FOLDER: string = 'CACHE_FOLDER';
end;

implementation

constructor TDataConnector.Create;
begin
  inherited Create();
  tables:= TStringList.Create;
  isConnected:= false;
end;

destructor TDataConnector.Destroy;
begin
  localSettings.Free;
  globalSettings.Free;
  inherited Destroy;
end;

function TDataConnector.getTable( tableName: string ): TCsvFileDatabase;
var propertyName:string;
    fileName:string;
    idx:integer;
    csvFileDatabase: TCsvFileDatabase;
begin
  propertyName:= tableName + '_table';
  idx:=  tables.indexOf(propertyName);

  if idx >= 0 then begin
    // found the table object, return it and exit
    csvFileDatabase:= TCsvFileDatabase( tables.Objects[idx] );
    result:= csvFileDatabase;
    exit;
  end;

  // ELSE ...

  // table not yet loaded - start to load
  fileName:= globalSettings.getSetting( propertyName );
  if (fileName = '') then begin
    // if table does not exist
    raise Exception.Create('Unknown table name '+tableName);
    result:= nil;
    exit;
  end;

  fileName:= localSettings.globalDatabaseFolder + fileName;
  csvFileDatabase:= TCsvFileDatabase.Create( fileName );

  // add table to list
  tables.AddObject( propertyName, csvFileDatabase );
  // return table
  result:= csvFileDatabase;
  exit;

end;


procedure TDataConnector.doConnect();
begin
  // this is the local file that holds settings per computer only
  localSettings:=  TLocalSettings.Create();

  // this is the main file for the database (shared with multiple computers)
  globalSettings:= TGlobalSettings.Create( localSettings.globalDatabaseFolder );

  // check if all subtables are present
  // todo

  isConnected:= true;
end;

procedure TDataConnector.startQuery( tableName:string );
var csvFileDatabase: TCsvFileDatabase;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.rewind;

end;

function TDataConnector.getRowFromTable( tableName:string ):TStringList;
var csvFileDatabase: TCsvFileDatabase;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  result:= csvFileDatabase.readNextLine();
end;

procedure TDataConnector.addRowToTable( tableName:string; row:TStringList );
var csvFileDatabase: TCsvFileDatabase;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.appendLine( row );
end;


function TDataConnector.getDatabaseFolder(): string;
begin
  result:= localSettings.globalDatabaseFolder;
end;

end.
