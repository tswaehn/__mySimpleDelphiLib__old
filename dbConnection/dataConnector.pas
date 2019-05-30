unit dataConnector;

interface
uses  SysUtils, Classes, Dialogs,
      localSettings, globalSettings, mytypes,
      csvFileDatabase;

type
  TDataConnector = class(TObject)
    constructor Create();
    destructor Destroy(); override;

  public
    procedure doConnect();

    // table operations
    // use startQuery + getRow to browse through table
    procedure sortTable( tableName:string );
    procedure startQuery( tableName:string );
    function getRowFromTable( tableName:string ):TStringListPtr;
    function getRowCount( tableName:string ):integer;

    // write to table
    procedure addRowToTable( tableName:string; row:TStringListPtr );
    procedure storeTable( tableName:string );
    procedure clearTable( tableName:string );
    //
    function getDatabaseFolder(): string;

    // settings
    procedure showLocalSettings();
    procedure showGlobalSettings();
    procedure setGlobalSetting(key:string; value:string);
    function getGlobalSetting(key:string): string;

  protected
    function getTable( tableName:string ): TCsvFileDatabase;
    function checkDbIntegrity(): boolean; virtual;
    function createNewDatabase():boolean; virtual;
    procedure backupAllDatabases();

  protected
    localSettings:TLocalSettings;
    globalSettings:TGlobalSettings;
    isConnected:boolean;

    tables:TStringList;
    rowIndex: integer;

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

procedure TDataConnector.doConnect();
var ret:integer;
begin
  // this is the local file that holds settings per computer only
  try
    // try to load local settings
    localSettings:=  TLocalSettings.Create();
  except
    localSettings:= nil;
    raise Exception.Create('TDataConnector.doConnect() error. Cannot create local.csv!');
  end;

  // this is the remote file that holds all the database settings
  try
    // this is the main file for the database (shared with multiple computers)
    globalSettings:= TGlobalSettings.Create( localSettings.globalDatabaseFolder );
  except
    globalSettings:= nil;
    raise Exception.Create('TDataConnector.doConnect() error. Cannot create '+ localSettings.globalDatabaseFolder+'!');
  end;

  // check if all tables of the database are present
  if (checkDbIntegrity() = false) then begin

    ret:= messageDlg('It looks like database is not existing or damaged.'+
                ' Please create a backup of your database now before continue or press CANCEL!', mtError, [mbOk,mbCancel], 0);

    // check the button of msg
    if (ret <> 1) then begin
      // user pressed CANCEL
      //
      raise Exception.Create('TDataConnector.doConnect() failed.');
    end;

    // create a backup of current set
    backupAllDatabases();

    // create a new database
    if (createNewDatabase() = false) then begin
      // if create fails
      raise Exception.Create('TDataConnector.createNewDatabase() failed');
    end;

  end;

  // now we are connected :)
  isConnected:= true;
end;

function TDataConnector.checkDbIntegrity(): boolean;
begin
  // this is a virtual function, need to overwrite!
  result:= false;
end;

function TDataConnector.createNewDatabase():boolean;
begin
  // this is a virtual function, need to overwrite!
  result:= false;
end;

procedure TDataConnector.backupAllDatabases();
begin
  //
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
    raise Exception.Create('Unknown table name "'+tableName+'"');
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

  rowIndex:= 0;
end;

function TDataConnector.getRowFromTable( tableName:string ):TStringListPtr;
var csvFileDatabase: TCsvFileDatabase;
    row:TStringListPtr;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  row:= csvFileDatabase.getRowFromMem(rowIndex);
  INC(rowIndex);

  result:= row;
end;

function TDataConnector.getRowCount( tableName:string ):integer;
var csvFileDatabase: TCsvFileDatabase;
    row:TStringListPtr;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  result:= csvFileDatabase.getRowMemCount();
end;

procedure TDataConnector.addRowToTable( tableName:string; row:TStringListPtr );
var csvFileDatabase: TCsvFileDatabase;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.addRowToMem( row );
end;

procedure TDataConnector.sortTable( tableName:string );
var csvFileDatabase: TCsvFileDatabase;
begin
  if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.sortTable();
end;

procedure TDataConnector.storeTable( tableName:string );
var csvFileDatabase: TCsvFileDatabase;
begin
   if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.storeToFile();

end;

procedure TDataConnector.clearTable( tableName:string );
var csvFileDatabase: TCsvFileDatabase;
begin
   if not isConnected then begin
    raise Exception.Create('Database not connected');
  end;

  csvFileDatabase:= getTable(tableName);
  if (csvFileDatabase = nil) then begin
    raise Exception.Create('Fehlermeldung');
  end;

  csvFileDatabase.clearAllRows();
end;

function TDataConnector.getDatabaseFolder(): string;
begin
  result:= localSettings.globalDatabaseFolder;
end;

procedure TDataConnector.showLocalSettings();
begin
  if (localSettings <> nil) then begin
    localSettings.showSettings;
  end;
end;

procedure TDataConnector.showGlobalSettings();
begin
  if (globalSettings<>nil) then begin
    globalSettings.showSettings;
  end;
end;

procedure TDataConnector.setGlobalSetting(key:string; value:string);
begin
  globalSettings.setSetting(key, value);
end;

function TDataConnector.getGlobalSetting(key:string): string;
begin
  result:= globalSettings.getSetting(key);
end;


end.
