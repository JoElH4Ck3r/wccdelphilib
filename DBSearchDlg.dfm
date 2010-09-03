object FDBSearch: TFDBSearch
  Left = 0
  Top = 0
  Caption = 'Search'
  ClientHeight = 364
  ClientWidth = 598
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object topPanel: TPanel
    Left = 0
    Top = 0
    Width = 598
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 584
    object labelFilter: TLabel
      Left = 8
      Top = 4
      Width = 64
      Height = 13
      Caption = 'Search String'
    end
    object JvArrowButton1: TJvArrowButton
      Left = 400
      Top = 18
      Width = 89
      Height = 24
      Caption = '&View'
      FillFont.Charset = DEFAULT_CHARSET
      FillFont.Color = clWindowText
      FillFont.Height = -11
      FillFont.Name = 'Tahoma'
      FillFont.Style = []
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFCC6701CC6701CC6701CC6701CC6701CC6701CC
        6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701FF00FFCC6701
        FFFFFFFFFFFFFFFAF5FFF3E6FEEBD5FEE3C3FEDCB5FED7ABFED7ABFED7ABFED7
        ABFED7ABFED7ABCC6701FF00FFCC6701FFFFFFFFFFFFFFFFFFFFFAF5FFF3E6FE
        EBD5FEE3C4FEDCB5FED7ABFED7ABFED7ABFED7ABFED7ABCC6701FF00FFCC6701
        FFFFFF4571FA4571FA4571FAFFFBF5A23F08A23F08A23F08FEDBB5059ACD059A
        CD059ACDFED7ABCC6701FF00FFCC6701FFFFFF4571FA4571FA4571FAFFFFFFA2
        3F08A23F08A23F08FEE3C4059ACD059ACD059ACDFED7ABCC6701FF00FFCC6701
        FFFFFF4571FA4571FA4571FAFFFFFFA23F08A23F08A23F08FFEBD5059ACD059A
        CD059ACDFED7ABCC6701FF00FFCC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFEFAF5FEF3E7FEEBD5FFE3C3FEDCB5FED7ABCC6701FF00FFCC6701
        FFFFFFCC9A99CC9A99CC9A99FFFFFFE27E03E27E03E27E03FFFAF4029A03029A
        03029A03FEDCB5CC6701FF00FFCC6701FFFFFFCC9A99CC9A99CC9A99FFFFFFE2
        7E03E27E03E27E03FFFFFF029A03029A03029A03FEE3C4CC6701FF00FFCC6701
        FFFFFFCC9A99CC9A99CC9A99FFFFFFE27E03E27E03E27E03FFFFFF029A03029A
        03029A03FFEBD5CC6701FF00FFCC6701FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF5FFF3E6CC6701FF00FFCC6701
        CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701CC67
        01CC6701CC6701CC6701FF00FFFF00FFCC6701CC6701CC6701CC6701CC6701CC
        6701CC6701CC6701CC6701CC6701CC6701CC6701CC6701FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
    end
    object Label1: TLabel
      Left = 192
      Top = 4
      Width = 58
      Height = 13
      Caption = 'Search Field'
    end
    object eSearchFilter: TEdit
      Left = 8
      Top = 20
      Width = 177
      Height = 21
      TabOrder = 0
    end
    object cbSearchField: TComboBox
      Left = 192
      Top = 20
      Width = 193
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object cbCaseSensitive: TCheckBox
      Left = 8
      Top = 44
      Width = 177
      Height = 17
      Caption = 'Case Sensitive Search'
      TabOrder = 2
    end
  end
  object SB: TStatusBar
    Left = 0
    Top = 345
    Width = 598
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
    ExplicitWidth = 584
  end
  object panelBottom: TPanel
    Left = 0
    Top = 306
    Width = 598
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 584
    DesignSize = (
      598
      39)
    object bCancel: TButton
      Left = 518
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object bOK: TButton
      Left = 437
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object vData: TVirtualStringTree
    Left = 0
    Top = 73
    Width = 598
    Height = 233
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    TabOrder = 3
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    ExplicitWidth = 584
    Columns = <>
  end
end
