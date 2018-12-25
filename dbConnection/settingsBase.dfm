object Settings: TSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Settings'
  ClientHeight = 377
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object valueList: TValueListEditor
    Left = 32
    Top = 28
    Width = 561
    Height = 277
    TabOrder = 0
    TitleCaptions.Strings = (
      'Key'
      'Value')
    ColWidths = (
      185
      370)
  end
  object CANCEL: TButton
    Left = 552
    Top = 330
    Width = 75
    Height = 25
    Caption = 'CANCEL'
    ModalResult = 2
    TabOrder = 1
  end
  object OK: TButton
    Left = 471
    Top = 330
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
