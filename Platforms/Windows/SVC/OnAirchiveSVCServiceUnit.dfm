object OnAirchiveSVCService: TOnAirchiveSVCService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'OnAirchiveSVCService'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
  object PeriodicalProcTimer: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = PeriodicalProcTimerTimer
    Left = 40
    Top = 16
  end
  object StorageFreeSpaceNotifier: TTimer
    Enabled = False
    Interval = 43200
    Left = 80
    Top = 80
  end
end
