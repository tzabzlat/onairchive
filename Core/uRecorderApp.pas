unit uRecorderApp;

interface

uses
  Classes, SyncObjs, SysUtils, DateUtils,
  uAppConfig, uILogger, TZDB, uRecorder, uRecorderFileStorage;

const
  DEFAULT_CONFIG_NAME = 'config.ini';
  CRITICAL_FREE_SPACE_VALUE_MB = 10000;

type
  TStreamStatusData = record
    FSDataBufferStatus: TDataBufferStatus;
    StreamToken: string;
    StreamIndex: byte;
  end;

  TStreamStatusDataList = array of TStreamStatusData;

  TStreamRecorderData = record
    StreamRecorder: TRecorder;
    StreamRecorderFileStorage: TRecorderFileStorage;
    StreamTimeZone: TBundledTimeZone;
    StreamToken: String;
  end;

  TRecorderApp = class
  protected
    Config: TAppConfig;
    Logger: ILogger;
    StreamRecorders: array of TStreamRecorderData;
    AppIsReady: Boolean;
    procedure InitRecorderObjects();
    procedure DestroyRecorderObjects();
  public
    AppPath: string;
    constructor Create(AppPathStr: string; LoggerObj: ILogger);
    procedure Start();
    procedure Stop();
    procedure CheckTimerProc();
    destructor Destroy; override;
    function GetFreeSpaceInStorageMB: Integer;
    function GetFileStorageDataBufferStatus: TStreamStatusDataList;
  end;

implementation

uses
  StrUtils;

{$IFDEF FPC}
// Helper function for Free Pascal compatibility - converts local time to UTC
function ConvertLocalTimeToUniversal(const LocalTime: TDateTime): TDateTime;
begin
  // Uses DateUtils.LocalTimeToUniversal for proper timezone conversion
  Result := LocalTimeToUniversal(LocalTime);
end;
{$ENDIF}

{ TRecorderApp }

procedure TRecorderApp.CheckTimerProc;
var
  StreamIndex: byte;
begin
  for StreamIndex := 0 to High(Self.StreamRecorders) do
  begin
    Self.StreamRecorders[StreamIndex].StreamRecorder.CheckProcedure();
  end;
end;

constructor TRecorderApp.Create(AppPathStr: string; LoggerObj: ILogger);
begin
  inherited Create;

  Self.AppIsReady := false;

  Self.Logger := LoggerObj;
  Self.AppPath := AppPathStr;

  Self.Logger.LogInfo('Launching...');

  Self.Config := TAppConfig.Create;

  try
    Self.Config.LoadFromFile(IncludeTrailingPathDelimiter(Self.AppPath) + DEFAULT_CONFIG_NAME);
  except
    On E: Exception do
    begin
      Self.Logger.LogError('Load configuration error: ' +
        E.Message);
      exit;
    end;
  end;

  Self.Logger.LogInfo('Configuration was successfully loaded');

  Self.InitRecorderObjects();

  Self.AppIsReady := true;
end;

destructor TRecorderApp.Destroy;
begin
  if (not Self.AppIsReady) then
  begin
    inherited;

    exit;
  end;

  Self.Logger.LogInfo('Recording station is stopping...');

  Self.DestroyRecorderObjects;

  Self.Logger.LogInfo('Recording station is stopped');

  Self.Config.Destroy;

  inherited;
end;

procedure TRecorderApp.DestroyRecorderObjects;
var
  StreamIndex: byte;
  MaxStreamIndex: byte;
begin
  MaxStreamIndex := High(Self.StreamRecorders);

  for StreamIndex := 0 to MaxStreamIndex do
  begin
    Self.StreamRecorders[StreamIndex].StreamRecorder.DestroyRecorder
      (StreamIndex = MaxStreamIndex);
    Self.Logger.LogInfo('Decoder is stopped', StreamIndex,
      Self.StreamRecorders[StreamIndex].StreamToken);
    Self.StreamRecorders[StreamIndex].StreamRecorderFileStorage.Destroy;
    Self.Logger.LogInfo('File storage is stopped', StreamIndex,
      Self.StreamRecorders[StreamIndex].StreamToken);
  end;
end;

function TRecorderApp.GetFileStorageDataBufferStatus: TStreamStatusDataList;
var
  Index: byte;
begin
  SetLength(Result, Length(Self.StreamRecorders));

  for Index := 0 to High(Self.StreamRecorders) do
  begin
    Result[Index].FSDataBufferStatus := Self.StreamRecorders[Index]
      .StreamRecorderFileStorage.GetDataBufferStatus();
    Result[Index].StreamToken := Self.StreamRecorders[Index].StreamToken;
    Result[Index].StreamIndex := Index;
  end;
end;

function TRecorderApp.GetFreeSpaceInStorageMB: Integer;
begin
  Result := Self.StreamRecorders[0].StreamRecorderFileStorage.
    GetStorageFreeSpaceMB();
end;

procedure TRecorderApp.InitRecorderObjects;
var
  StreamIndex: byte;

  CurrentItemConfig: TStreamConfig;

  TempStreamDateTime: TDateTime;
  TempStreamUniversalDateTime: TDateTime;
begin

  SetLength(Self.StreamRecorders, Length(Self.Config.Streams));

  for StreamIndex := 0 to High(Self.Config.Streams) do
  begin
    CurrentItemConfig := Self.Config.Streams[StreamIndex];

    Self.StreamRecorders[StreamIndex].StreamToken :=
      CurrentItemConfig.StreamToken;

    Self.Logger.LogInfo('Stream url: ' + CurrentItemConfig.StreamUri,
      StreamIndex, CurrentItemConfig.StreamToken);

    Self.StreamRecorders[StreamIndex].StreamTimeZone :=
      TBundledTimeZone.GetTimeZone(CurrentItemConfig.StreamTz);

    Self.Logger.LogInfo('Stream time zone: ' + Self.StreamRecorders
      [StreamIndex].StreamTimeZone.DisplayName, StreamIndex,
      CurrentItemConfig.StreamToken);

    {$IFDEF FPC}
    TempStreamUniversalDateTime := ConvertLocalTimeToUniversal(Now);
    {$ELSE}
    TempStreamUniversalDateTime := TTimeZone.Local.ToUniversalTime(Now);
    {$ENDIF}
    TempStreamDateTime := Self.StreamRecorders[StreamIndex]
      .StreamTimeZone.ToLocalTime(TempStreamUniversalDateTime);

    Self.Logger.LogInfo('Stream timezone current time: ' +
      FormatDateTime('YYYY-MM-DD HH:NN:SS', TempStreamDateTime), StreamIndex,
      CurrentItemConfig.StreamToken);

    Self.StreamRecorders[StreamIndex].StreamRecorderFileStorage :=
      TRecorderFileStorage.Create(CurrentItemConfig.StreamToken,
      Self.Config.FilesPath, Self.StreamRecorders[StreamIndex].StreamTimeZone,
      Self.Logger, Self.Config.DataBufferSizeKb);

    Self.Logger.LogInfo('Path to save recordings: ' + Self.Config.FilesPath,
      StreamIndex, CurrentItemConfig.StreamToken);

    if (StreamIndex = 0) then
    begin
      try
        Self.StreamRecorders[StreamIndex].StreamRecorderFileStorage.
          CheckRootPathAccess();
      except
        On E: Exception do
        begin
          Self.Logger.LogError('An error occurred while checking file storage: '
            + E.Message, StreamIndex, CurrentItemConfig.StreamToken);
          exit;
        end;
      end;

      Self.Logger.LogInfo('File storage check completed successfully', StreamIndex,
        CurrentItemConfig.StreamToken);

    end;

    Self.Logger.LogInfo('Initializing recording functions', StreamIndex,
      CurrentItemConfig.StreamToken);

    Self.StreamRecorders[StreamIndex].StreamRecorder :=
      TRecorder.Create(Self.AppPath, Self.Logger, CurrentItemConfig.StreamUri,
      CurrentItemConfig.StreamToken, Self.StreamRecorders[StreamIndex]
      .StreamRecorderFileStorage, StreamIndex);

    Self.StreamRecorders[StreamIndex].StreamRecorder.SelfPointer :=
      Addr(Self.StreamRecorders[StreamIndex].StreamRecorder);

    if (StreamIndex = 0) then
    begin
      // Init Bass.dll if it is first stream
      Self.StreamRecorders[StreamIndex].StreamRecorder.Init;
    end;
  end;

end;

procedure TRecorderApp.Start;
var
  StreamIndex: byte;
begin
  if (not Self.AppIsReady) then
    raise Exception.Create('Recorder App is not ready');

  for StreamIndex := 0 to High(Self.StreamRecorders) do
  begin
    Self.StreamRecorders[StreamIndex].StreamRecorder.Start();
  end;
end;

procedure TRecorderApp.Stop;
var
  StreamIndex: byte;
begin
  if (not Self.AppIsReady) then
    exit;

  for StreamIndex := 0 to High(Self.StreamRecorders) do
  begin
    Self.StreamRecorders[StreamIndex].StreamRecorder.StopChannels();
  end;
end;

end.
