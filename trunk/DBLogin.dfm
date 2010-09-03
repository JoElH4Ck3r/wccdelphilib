object fLogin: TfLogin
  Left = 387
  Top = 391
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 199
  ClientWidth = 355
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 16
    Width = 49
    Height = 13
    Caption = 'UserName'
  end
  object Label2: TLabel
    Left = 10
    Top = 64
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label3: TLabel
    Left = 10
    Top = 112
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object eUsername: TEdit
    Left = 10
    Top = 32
    Width = 335
    Height = 21
    TabOrder = 0
  end
  object ePassword: TEdit
    Left = 10
    Top = 80
    Width = 335
    Height = 21
    PasswordChar = '#'
    TabOrder = 1
  end
  object bOK: TButton
    Left = 192
    Top = 160
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 272
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object comboDSN: TComboBox
    Left = 10
    Top = 128
    Width = 335
    Height = 21
    Style = csDropDownList
    Ctl3D = False
    ItemHeight = 0
    ParentCtl3D = False
    TabOrder = 4
  end
end
