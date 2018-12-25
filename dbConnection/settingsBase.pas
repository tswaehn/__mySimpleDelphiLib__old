unit settingsBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  mytypes,csvFileDatabase, Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit;

type
  TSettings = class(TForm)
    valueList: TValueListEditor;
    CANCEL: TButton;
    OK: TButton;
    constructor Create(csvFilename:string);

    public
      function getSetting(name:string):string;
      procedure setSetting(name: string; value: string );
      function showSettings():boolean;

      procedure storeSettings();

    private
      procedure dataToTable();
      procedure tableToData();

    protected
      myName:string;
    private
      csvFileDatabase:TCsvFileDataBase;

    private
      CONST COL_ID: integer= 0;
      CONST COL_NAME: integer= 1;
      CONST COL_VALUE: integer= 2;

  end;


implementation
{$R *.dfm}

constructor TSettings.Create(csvFilename:string);
begin
  inherited Create(nil);
  csvFileDatabase:= TCsvFileDataBase.Create(csvFilename);

  self.Visible:= false;
  myName:='settings';
end;

function TSettings.getSetting(name: string):string;
var row:TStringListPtr;
begin
  row:= csvFileDatabase.searchRowInMem( name, COL_NAME);
  if (row <> nil) then begin
    result:= row.Strings[COL_VALUE];
  end else begin
    result:= '';
  end;
end;

procedure TSettings.setSetting(name: string; value: string );
var newRow:TStringList;
begin
  newRow:= TStringList.Create;
  newRow.Add(INDEX);
  newRow.Add(name);
  newRow.Add(value);
  csvFileDatabase.updateRowInMem( name, COL_NAME, @newRow );

  newRow.Free;
end;

procedure TSettings.storeSettings();
begin
  csvFileDatabase.storeToFile();
end;

function TSettings.showSettings():boolean;
begin
  dataToTable();
  if (ShowModal() = mrOK) then begin
    tableToData();
    self.storeSettings;
  end;
end;

procedure TSettings.dataToTable();
var
  I: Integer;
  key:string;
  value:string;
  row:TStringListPtr;
begin
  // clean up
  valueList.Strings.Clear;

  // fill up
  for I := 0 to (csvFileDatabase.getRowMemCount()-1) do begin
    row:= csvFileDatabase.getRowFromMem(i);
    key:= row.Strings[COL_NAME];
    value:= row.Strings[COL_VALUE];
    valueList.InsertRow(key, value, true);
  end;

end;

procedure TSettings.tableToData();
var
  i: Integer;
  key: string;
  value: string;
  newRow:TStringList;
begin
  newRow:= TStringList.Create;
  for i := 1 to valueList.RowCount-1 do begin
    key:= valueList.Keys[i];
    value:= valueList.Values[key];
    newRow.Clear;
    newRow.Add(INDEX);
    newRow.Add(key);
    newRow.Add(value);
    csvFileDatabase.updateRowInMem(key, COL_NAME, @newRow );
  end;
  newRow.Free;
end;

end.
