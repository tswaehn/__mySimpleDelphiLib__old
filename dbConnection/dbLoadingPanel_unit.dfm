object dbLoadingPanel: TdbLoadingPanel
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Loading from database ...'
  ClientHeight = 125
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 24
    Top = 48
    Width = 513
    Height = 25
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 296
    Top = 88
  end
end
