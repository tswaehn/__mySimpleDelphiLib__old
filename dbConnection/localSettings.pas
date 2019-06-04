unit localSettings;

interface
uses settingsBase,
      logger;

type
  TLocalSettings = class(TSettings)
    constructor Create();

    public
      globalDatabaseFolder: string;
    private
      CONST GLOBAL_DATABASE_FOLDER: string= 'global database folder';
  end;

implementation

constructor TLocalSettings.Create();
begin
  inherited Create( '.\local.csv');

  if (getSettingCount() = 0) then begin
    // we have an empty settings file
    setSetting(GLOBAL_DATABASE_FOLDER,'c:\temp\');
    storeSettings();
  end;

  // debug
  debugLog.notice('global settings come from ' + getSetting(GLOBAL_DATABASE_FOLDER) );

  // load
  globalDatabaseFolder:= self.getSetting(GLOBAL_DATABASE_FOLDER);

  myName:='local settings';
end;

end.
