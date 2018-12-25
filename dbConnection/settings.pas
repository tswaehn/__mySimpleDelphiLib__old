unit settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  mytypes,csvFileDatabase;

type
  TsettingsUI = class(TForm)
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
{$R *.dfm}

constructor TsettingsUI.Create(csvFilename:string);
begin
  inherited Create(nil);
  csvFileDatabase:= TCsvFileDataBase.Create(csvFilename);
  csvFileDatabase.loadAllRowsFromDBintoMem();

  self.Visible:= false;
end;

function TsettingsUI.getSetting(name: string):string;
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
