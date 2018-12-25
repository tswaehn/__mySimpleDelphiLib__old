unit globalSettings;

interface
uses settingsBase;

type
  TGlobalSettings = class(TSettings)
    constructor Create(databaseDir:string);

  end;

implementation

constructor TGlobalSettings.Create(databaseDir:string);
begin
  inherited Create( databaseDir+'.\global.csv');

  myName:='global settings';
end;

end.

