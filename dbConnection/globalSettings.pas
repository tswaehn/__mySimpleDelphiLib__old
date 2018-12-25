unit globalSettings;

interface
uses settings;

type
  TGlobalSettings = class(TSettings)
    constructor Create(databaseDir:string);

  end;

implementation

constructor TGlobalSettings.Create(databaseDir:string);
begin
  inherited Create( databaseDir+'.\global.csv');


end;

end.

