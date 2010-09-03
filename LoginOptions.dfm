object FLoginOptions: TFLoginOptions
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'Connection Options'
  ClientHeight = 389
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 334
    Width = 366
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      366
      55)
    object bCancel: TButton
      Left = 287
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object bOK: TButton
      Left = 215
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 366
    Height = 334
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 1
    object tsGeneral: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 24
        Top = 67
        Width = 305
        Height = 94
        AutoSize = False
        Caption = 
          'When connecting to TruckMate'#8482' databases a valid schema is requir' +
          'ed. This is typically LYNX for older TruckMate'#8482' installations of' +
          ' TMWIN for newer TruckMate'#8482' installations. If you have a multi-s' +
          'chema deployment please check with your system administrator.'
        WordWrap = True
      end
      object Label2: TLabel
        Left = 24
        Top = 195
        Width = 305
        Height = 94
        AutoSize = False
        Caption = 
          'The function path is usually set to the same value as the schema' +
          '. However, unlike the schema you can specify multiple values by ' +
          'seperating them with a comma. For example to access functions an' +
          'd procedures implicitly from the schemas LYNX and TMWIN, you cou' +
          'ld specify "LYNX,TMWIN".'
        WordWrap = True
      end
      object eSchema: TLabeledEdit
        Left = 24
        Top = 40
        Width = 305
        Height = 21
        EditLabel.Width = 45
        EditLabel.Height = 13
        EditLabel.Caption = 'Schema'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -11
        EditLabel.Font.Name = 'Tahoma'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        TabOrder = 0
      end
      object ePath: TLabeledEdit
        Left = 24
        Top = 168
        Width = 305
        Height = 21
        EditLabel.Width = 77
        EditLabel.Height = 13
        EditLabel.Caption = 'Function Path'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -11
        EditLabel.Font.Name = 'Tahoma'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        TabOrder = 1
      end
    end
  end
end
