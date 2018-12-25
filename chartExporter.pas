unit chartExporter;

interface
uses Chart, Series, TeEngine, 
    textFileWriter, versioninfo;

type
  TChartExport = class (TObject)
    constructor Create( chart_: TChart);
    destructor Destroy(); override;

    procedure go();
    
  private
    chart:TChart;
    fileWriter:TTextFileWriter;
  end;
implementation

constructor TChartExport.Create(chart_: TChart);
begin
  inherited Create();

  chart:= chart_;
  fileWriter:= TTextFileWriter.Create('text.txt');

  fileWriter.writeln('TMMS Export '+GetVersionStr() );
end;

destructor TChartExport.destroy;
begin
  fileWriter.Destroy;
  inherited Destroy;
end;

procedure TChartExport.go;
var
  i: Integer;
  series:TChartSeries;
  valueLists:TChartValueLists;
  p: Integer;

  x,y:double;
  title:string;
  c:string;
begin

  for i := 0 to chart.SeriesCount - 1 do begin
    series:= chart.Series[i];

    title:= series.Title;
    fileWriter.writeLn(title);

    for p := 0 to series.Count - 1 do begin
      x:= series.XValue[p];
      y:= series.YValue[p];
      c:= series.ValueMarkText[p];
      //c:= string( series.Labels.Items[p] );
      fileWriter.writeDouble(x,y,c);
    end;

    (*
    valueLists:= series.ValuesLists;
    for k := 0 to valueLists.Count - 1 do begin
      valueList:= valueLists[k];
      for t := 0 to valueList.Count - 1 do begin
         fileWriter.writeDouble( valueList.Value[t] );
      end;

    end;
    *)

  end;

end;


end.
