object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'On Airchive'
  ClientHeight = 307
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 456
    Height = 307
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Log'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LogRichEdit: TRichEdit
        Left = 0
        Top = 0
        Width = 448
        Height = 279
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HideScrollBars = False
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        Zoom = 100
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'File System buffer view'
      ImageIndex = 1
      object FSDataBufferStatusPrinterScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 448
        Height = 279
        Align = alClient
        BorderStyle = bsNone
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        DesignSize = (
          448
          279)
        object FSDataBufferStatusPrinterSwitcher: TSpeedButton
          Left = 2
          Top = 3
          Width = 446
          Height = 22
          AllowAllUp = True
          Anchors = [akLeft, akTop, akRight]
          GroupIndex = 1
          Caption = 'Show File System buffer state'
          Enabled = False
          OnClick = FSDataBufferStatusPrinterSwitcherClick
          ExplicitWidth = 415
        end
        object FSDataBufferStatusPrinterGroup: TGroupBox
          Left = 2
          Top = 31
          Width = 442
          Height = 55
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Visible = False
          DesignSize = (
            442
            55)
          object FSDataBufferStatusPrinterProgressStr: TLabel
            Left = 261
            Top = 35
            Width = 172
            Height = 14
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            Caption = 'FSDataBufferStatusPrinterProgress'
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ExplicitLeft = 234
          end
          object FSDataBufferStatusPrinterProgressBar: TProgressBar
            Left = 11
            Top = 17
            Width = 422
            Height = 11
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
        end
      end
    end
  end
  object PeriodicalProcTimer: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = PeriodicalProcTimerTimer
    Left = 264
    Top = 152
  end
  object RunTimer: TTimer
    OnTimer = RunTimerTimer
    Left = 152
    Top = 200
  end
  object StorageFreeSpaceNotifier: TTimer
    Enabled = False
    Interval = 43200
    OnTimer = StorageFreeSpaceNotifierTimer
    Left = 264
    Top = 200
  end
  object FSDataBufferStatusPrinter: TTimer
    Enabled = False
    Interval = 500
    OnTimer = FSDataBufferStatusPrinterTimer
    Left = 152
    Top = 152
  end
end
