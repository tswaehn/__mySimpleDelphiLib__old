unit dbLoadingPanel_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Vcl.StdCtrls;

type
  TdbLoadingPanel = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Label2: TLabel;

    constructor Create(caption_:string; max:integer);

    procedure startWithTimer(  caption: string  );
    procedure updatePosition( value:integer );
    procedure done();


    procedure FormHide(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

constructor TdbLoadingPanel.Create(caption_:string; max:integer);
begin
  inherited Create(nil);

  self.Caption:= caption_;
  progressbar1.Position:= 0;
  progressbar1.Max:= max;
  label2.Caption:= 'total '+intToStr(progressbar1.Max);
  self.Show();
end;

procedure TdbLoadingPanel.startWithTimer( caption: string );
begin
  self.Caption:= caption;
  progressbar1.Position:= 0;
  progressbar1.Max:= 100;
  timer1.Enabled:=true;
  label2.Caption:= 'total '+intToStr(progressbar1.Max);
  self.Show();
end;

procedure TdbLoadingPanel.updatePosition(value: Integer);
begin
  progressbar1.Position:= value;
  application.ProcessMessages();
end;

procedure TdbLoadingPanel.done;
begin
  self.Hide;
end;

procedure TdbLoadingPanel.FormHide(Sender: TObject);
begin
  timer1.Enabled:= false;
end;

procedure TdbLoadingPanel.Timer1Timer(Sender: TObject);
begin
  progressbar1.Position:=  progressbar1.Position + 1;
  if progressbar1.Position = progressbar1.Max then progressbar1.Position:=0;

end;

end.
