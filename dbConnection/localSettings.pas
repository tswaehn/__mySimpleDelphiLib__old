unit localSettings;

interface
uses settingsBase;

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

  globalDatabaseFolder:= self.getSetting(GLOBAL_DATABASE_FOLDER);

  if globalDatabaseFolder ='' then begin
    setSetting(GLOBAL_DATABASE_FOLDER,'c:\temp\');
    storeSettings();
  end;

  myName:='local settings';
end;

end.
