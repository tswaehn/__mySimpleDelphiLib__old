unit settings;

interface
uses  SysUtils, Classes, mytypes,
      csvFileDatabase;

type
  TSettings = class(TObject)
    constructor Create(csvFilename:string);

    public
      function getSetting(name:string):string;

    private
      csvFileDatabase:TCsvFileDataBase;

    private
      CONST COL_ID: integer= 0;
      CONST COL_NAME: integer= 1;
      CONST COL_VALUE: integer= 2;

  end;


implementation

constructor TSettings.Create(csvFilename:string);
begin
  inherited Create();
  csvFileDatabase:= TCsvFileDataBase.Create(csvFilename);
  csvFileDatabase.loadAllRowsFromDBintoMem();
end;

function TSettings.getSetting(name: string):string;
var row:TStringList;
begin
  row:= csvFileDatabase.searchRowInMem( name, COL_NAME);
  if (row <> nil) then begin
    result:= row.Strings[COL_VALUE];
  end else begin
    result:= '';
  end;
end;

end.
