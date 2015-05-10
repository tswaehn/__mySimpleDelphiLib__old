unit exportToExcel;

interface
uses
  ComObj, Math, SysUtils, Variants, grids;


  function ExportStringGridToExcel(StringGrid : TStringGrid) : Boolean;

implementation

//Hilfsfunktionen
function StringToVariant(const SourceString : string) : Variant;
var
  FloatValue : Extended;
begin
  if TryStrToFloat(SourceString, FloatValue) then
    Result := FloatValue
  else
    Result := SourceString;
end;

function RefToCell(Col, Row : Integer) : string;
var
  Pos : Integer;
begin
  //Spalte bestimmen
  Result := '';
  while Col > 0 do
  begin
    Pos := Col mod 26;
    if Pos = 0 then
    begin
      Pos := 26;
      Dec(Col);
    end;
    Result := Chr(Ord('A') + Pos - 1) + Result;
    Col := Col div 26;
  end;
  //Spalte und Zeile zusammenführen
  Result := Result + IntToStr(Row);
end;

//Inhalt eines TStringGrid nach Excel exportieren und anzeigen
function ExportStringGridToExcel(StringGrid : TStringGrid) : Boolean;
var
  Col       : Integer;
  Data      : OleVariant;
  ExcelApp  : OleVariant;
  MaxCol    : Integer;
  MaxRow    : Integer;
  Range     : OleVariant;
  Row       : Integer;
  Workbook  : OleVariant;
  Worksheet : OleVariant;
  Value     : OleVariant;

  formatSettings: TFormatSettings;
  filename:string;
begin
  Result := False;
  //Verbindung zu Excel herstellen
  ExcelApp := CreateOleObject('Excel.Application');
  try
    if not VarIsNull(ExcelApp) then
    begin
      //Neues Workbook öffnen
      Workbook := ExcelApp.Workbooks.Add;
      if not VarIsNull(Workbook) then
      begin
        //Maximalen Bereich bestimmen
        MaxCol := Min(StringGrid.ColCount, ExcelApp.Columns.Count);
        MaxRow := Min(StringGrid.RowCount, ExcelApp.Rows.Count);
        if (MaxRow > 0) and (MaxCol > 0) then
        begin
          //Worksheet auswählen
          Worksheet := Workbook.ActiveSheet;
          //Bereich auswählen
          Range := Worksheet.Range[RefToCell(1, 1), RefToCell(MaxCol, MaxRow)];
          if not VarIsNull(Range) then
          begin
            //Daten aus Grid holen
            Data := VarArrayCreate([1, MaxRow, 1, MaxCol], varVariant);
            for Row := 0 to Pred(MaxRow) do
            begin
              for Col := 0 to Pred(MaxCol) do
              begin
                Value := StringToVariant(StringGrid.Cells[Col, Row]);
                Data[Succ(Row), Succ(Col)] := Value
              end;
            end;
            //Daten dem Excelsheet übergeben
            Range.Value := Data;
            Range.Columns.AutoFit;

            GetLocaleFormatSettings(0, formatSettings );
            formatSettings.TimeSeparator:='_';
            formatSettings.DateSeparator:='_';
            formatSettings.ListSeparator:='_';
            
            filename:= '.\test'+ TimeToStr( now(), formatSettings ) +'.xls';



            //Excel anzeigen
                        workbook.saveAs(filename);

            Workbook.Activate;
            //ExcelApp.Visible := True;
            Result := True;


          end;
        end;
      end;
    end;
  finally
    Value    := UnAssigned;
    Data     := UnAssigned;
    Range    := UnAssigned;
    Workbook := UnAssigned;
    ExcelApp := UnAssigned;
  end;
end;

end.
