unit uRecorder;

interface

uses
  Classes, SysUtils, Variants, DateUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  uILogger, bass, BASSENC, uRecorderFileStorage, uCreateStreamFromURLThread;

type
  TStatusEnum = (sStopped, sWorking, sWaitStopForDestroy);
  TRecorder = class;

  PRecorder = ^TRecorder;

  TRecorder = class
  protected
    AppPath: String;
    StreamURI: String;
    StreamID: String;
    StreamIndex: Byte;
    Logger: ILogger;
    StreamChannel: HSTREAM;
    StreamEncoder: HSTREAM;
    FileStorage: TRecorderFileStorage;
    Status: TStatusEnum;

    LeftPartOfChannelSoundValue: DWORD;
    RightPartOfChannelSoundValue: DWORD;

    ErrorFound: Boolean;

    CreateStreamThread: TCreateStreamFromURLThread;
    CreateStreamThreadStarted: Boolean;
  public
    SelfPointer: PRecorder;
    constructor Create(AppPathStr: string; LoggerObj: ILogger;
      StreamURIStr, StreamIDStr: String;
      RecorderFileStorage: TRecorderFileStorage; RecorderIndex: Byte);
    procedure DestroyRecorder(DestroyBass: Boolean);
    procedure Init;
    procedure DoMeta();
    procedure HandleEndOfPlaying(TypeId: DWORD);
    procedure HandleDownloadProc(buffer: Pointer; length: DWORD);
    procedure WriteAudioBuffer(buffer: Pointer; length: DWORD);
    procedure Start();
    procedure StopChannels();
    procedure CheckProcedure();
    procedure BASSBufferingStarted();
    procedure BASSCreateStreamFromURLResult(StreamChannelResult: HSTREAM;
      ErrorCode: Integer; BeforeBufferization: Boolean);
  end;

implementation

{$IFDEF FPC}
// Helper functions for Free Pascal compatibility

function LoWORD(L: DWORD): Word;
begin
  Result := Word(L and $FFFF);
end;

function HiWORD(L: DWORD): Word;
begin
  Result := Word((L shr 16) and $FFFF);
end;

{$ENDIF}
{ TRecorder class }

procedure TRecorder.CheckProcedure;
var
  level: DWORD;
  temp1, temp2: DWORD;
begin
  if (Self.Status = sStopped) then
  begin
    Self.Status := sWorking;
    Self.Start();

    Exit;
  end;

  if (BASS_ChannelIsActive(Self.StreamChannel) = BASS_ACTIVE_PLAYING) then
  begin
    level := BASS_ChannelGetLevel(Self.StreamChannel);

    temp1 := LoWORD(level);
    temp2 := HiWORD(level);

    if ((temp1 = Self.LeftPartOfChannelSoundValue) and
      (temp2 = Self.RightPartOfChannelSoundValue)) then
    begin
      Self.Logger.LogError('Silence detected in the stream', Self.StreamIndex,
        Self.StreamID);
    end
    else
    begin
      Self.LeftPartOfChannelSoundValue := temp1;
      Self.LeftPartOfChannelSoundValue := temp2;
    end;
  end;
end;

constructor TRecorder.Create(AppPathStr: string; LoggerObj: ILogger;
  StreamURIStr, StreamIDStr: String; RecorderFileStorage: TRecorderFileStorage;
  RecorderIndex: Byte);
begin
  inherited Create();

  Self.AppPath := AppPathStr;
  Self.StreamURI := StreamURIStr;
  Self.StreamID := StreamIDStr;
  Self.Logger := LoggerObj;
  Self.StreamIndex := RecorderIndex;

  Self.CreateStreamThread := nil;
  Self.CreateStreamThreadStarted := false;

  Self.FileStorage := RecorderFileStorage;

  Self.Status := sWorking;

  Self.LeftPartOfChannelSoundValue := 0;
  Self.RightPartOfChannelSoundValue := 0;

  Self.ErrorFound := false;
end;

procedure TRecorder.DestroyRecorder(DestroyBass: Boolean);
begin
  if (DestroyBass) then
  begin
    BASS_Free;
  end;

  Self.Destroy;
end;

procedure TRecorder.DoMeta;
var
  meta: PAnsiChar;
  MetaString: String;
  p: Integer;
  StrStream, StrStream2: TStringStream;

begin
  meta := BASS_ChannelGetTags(Self.StreamChannel, BASS_TAG_META);

  if (meta <> nil) then
  begin
    p := Pos('StreamTitle=', String(AnsiString(meta)));

    if (p = 0) then
      Exit;

    p := p + 13;

    StrStream := TStringStream.Create(meta, TEncoding.ANSI);
    StrStream2 := TStringStream.Create('', TEncoding.UTF8);
    StrStream2.LoadFromStream(StrStream);

    MetaString := StrStream2.DataString;

    MetaString := Copy(MetaString, p, Pos(';', String(MetaString)) - p - 1);

    StrStream.Destroy;
    StrStream2.Destroy;

    Self.FileStorage.WriteTitle(MetaString, false);
  end;
end;

procedure TRecorder.HandleDownloadProc(buffer: Pointer; length: DWORD);
var
  connected: Byte;
begin
  if (length = 0) then
  begin
    if (buffer = nil) then
    begin
      if ((Self.Status = sWaitStopForDestroy) or (Self.Status = sStopped)) then
      begin
        // If we are stopping or stopped early
        Exit;
      end;

      Self.Logger.LogError('Buffer is empty!', Self.StreamIndex, Self.StreamID);

      connected := BASS_StreamGetFilePosition(Self.StreamChannel,
        BASS_FILEPOS_CONNECTED);

      if (connected = 0) then
      begin
        Self.StopChannels;
        Self.ErrorFound := true;
        Self.Logger.LogError('Connection lost. We will try to reconnect.',
          Self.StreamIndex, Self.StreamID);
      end;
    end;
  end;
end;

procedure TRecorder.HandleEndOfPlaying(TypeId: DWORD);
begin
  Self.Logger.LogError('Stream ended with code', Self.StreamIndex,
    Self.StreamID);
  // IntToStr(TypeId)
end;

procedure TRecorder.Init;
var
  PluginHandle: HPLUGIN;
  PluginPath: String;
begin
  if (HiWORD(BASS_GetVersion) <> BASSVERSION) then
  begin
    raise Exception.Create('An incorrect version of BASS.DLL was loaded');
  end;
{$IFDEF FPC}
  if (not BASS_Init(0, 44100, 0, nil, nil)) then
{$ELSE}
  if (not BASS_Init(0, 44100, 0, 0, nil)) then
{$ENDIF}
  begin
    raise Exception.Create('Can''t initialize device');
  end;

  BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);

  // Load AAC plugin for AAC/AACP stream support
{$IFDEF MSWINDOWS}
  PluginPath := IncludeTrailingPathDelimiter(Self.AppPath) + 'bass_aac.dll';
{$ELSE}
  PluginPath := 'libbass_aac.so';
{$ENDIF}
  PluginHandle := BASS_PluginLoad(PChar(PluginPath),
{$IFDEF MSWINDOWS}BASS_UNICODE{$ELSE}0{$ENDIF});
  if (PluginHandle = 0) then
  begin
    Self.Logger.LogWarning('AAC plugin not loaded (error: ' +
      IntToStr(BASS_ErrorGetCode()) + '). AAC/AACP streams may not work.',
      Self.StreamIndex, Self.StreamID);
  end
  else
  begin
    Self.Logger.LogInfo('AAC plugin loaded successfully', Self.StreamIndex,
      Self.StreamID);
  end;
end;

procedure StreamEndEvent(handle: HSYNC; Stream, data: DWORD; user: Pointer);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  recorderObj: PRecorder;
begin
  recorderObj := user;
  recorderObj^.HandleEndOfPlaying(data);
end;

procedure StreamMetaSync(handle: HSYNC; Stream, data: DWORD; user: Pointer);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  recorderObj: PRecorder;
begin
  recorderObj := user;
  recorderObj^.DoMeta();
end;

procedure StreamDownloadProc(buffer: Pointer; length: DWORD; user: Pointer);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  recorderObj: PRecorder;
begin
  recorderObj := user;
  recorderObj^.HandleDownloadProc(buffer, length);
end;

procedure EncodeAudio(Encoder: HENCODE; channel: HCHANNEL; buffer: Pointer;
  length: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  recorderObj: PRecorder;
begin
  recorderObj := user;
  recorderObj^.WriteAudioBuffer(buffer, length);
end;

procedure TRecorder.Start;
begin
  Self.Status := sWorking;
  Self.Logger.LogInfo('Attempting to open stream with ID ' + Self.StreamID
    + ' (uri: ' + Self.StreamURI + ')', Self.StreamIndex, Self.StreamID);

  Self.CreateStreamThread := TCreateStreamFromURLThread.Create(true);

{$IFDEF MSWINDOWS}
  Self.CreateStreamThread.FreeOnTerminate := true;
{$ELSE}
  Self.CreateStreamThread.FreeOnTerminate := false;
{$ENDIF}
  Self.CreateStreamThread.StreamURI := Self.StreamURI;
  Self.CreateStreamThread.DownloadProc := @StreamDownloadProc;
  Self.CreateStreamThread.RecorderObjectPointer := Self.SelfPointer;
{$IFDEF FPC}
  Self.CreateStreamThread.StartBufferizationCallBackProc :=
    @Self.BASSBufferingStarted;
  Self.CreateStreamThread.FinalCallBackProc :=
    @Self.BASSCreateStreamFromURLResult;
{$ELSE}
  Self.CreateStreamThread.StartBufferizationCallBackProc :=
    Self.BASSBufferingStarted;
  Self.CreateStreamThread.FinalCallBackProc :=
    Self.BASSCreateStreamFromURLResult;
{$ENDIF}
  Self.CreateStreamThread.Start();
  Self.CreateStreamThreadStarted := true;
end;

procedure TRecorder.BASSBufferingStarted;
begin
  Self.Logger.LogInfo('Stream opened successfully, buffering...',
    Self.StreamIndex, Self.StreamID);
end;

procedure TRecorder.BASSCreateStreamFromURLResult(StreamChannelResult: HSTREAM;
  ErrorCode: Integer; BeforeBufferization: Boolean);
begin
  Self.CreateStreamThreadStarted := false;

{$IFDEF MSWINDOWS}
{$ELSE}
  if Assigned(Self.CreateStreamThread) then
  begin
    Self.CreateStreamThread.Free;
    Self.CreateStreamThread := nil;
  end;
{$ENDIF}
  if ((Self.Status = sWaitStopForDestroy) or (Self.Status = sStopped)) then
  begin
    Exit;
  end;

  Self.StreamChannel := StreamChannelResult;

  if (StreamChannelResult = 0) then
  begin
    Self.Status := sStopped;
    Self.StopChannels();

    Self.Logger.LogError('Error opening the stream ' + Self.StreamURI +
      '. Error code: ' + IntToStr(ErrorCode) +
      '. The stream was not opened (StreamChannel = 0).', Self.StreamIndex,
      Self.StreamID);

    Exit;
  end;

  if (ErrorCode <> BASS_OK) then
  begin
    Self.Status := sStopped;
    Self.StopChannels();

    Self.Logger.LogError('Error opening stream ' + Self.StreamURI +
      '. Error code: ' + IntToStr(ErrorCode), Self.StreamIndex, Self.StreamID);

    Exit;
  end;

  Self.Logger.LogInfo('Recording started', Self.StreamIndex, Self.StreamID);

  DoMeta();
  BASS_ChannelSetSync(Self.StreamChannel, BASS_SYNC_META, 0, @StreamMetaSync,
    Self.SelfPointer);
  BASS_ChannelSetSync(Self.StreamChannel, BASS_SYNC_DOWNLOAD or BASS_SYNC_END or
    BASS_SYNC_FREE, 0, @StreamEndEvent, Self.SelfPointer);

{$IFDEF MSWINDOWS}
  StreamEncoder := BASS_Encode_Start(Self.StreamChannel,
    PChar(IncludeTrailingPathDelimiter(Self.AppPath) +
    'ffmpeg.exe -f s16le -ar 44100 -ac 2 -i - -c:a aac -b:a 128k -f adts -'),
    BASS_UNICODE or BASS_ENCODE_QUEUE or BASS_ENCODE_LIMIT or
    BASS_ENCODE_NOHEAD, @EncodeAudio, Self.SelfPointer);
{$ELSE}
  StreamEncoder := BASS_Encode_Start(Self.StreamChannel,
    PChar('ffmpeg -f s16le -ar 44100 -ac 2 -i - -c:a aac -b:a 128k -f adts -'),
    BASS_ENCODE_QUEUE or BASS_ENCODE_LIMIT or BASS_ENCODE_NOHEAD, @EncodeAudio,
    Self.SelfPointer);
{$ENDIF}
  if (StreamEncoder = 0) then
  begin
    Self.Logger.LogError('Failed to start encoder. Error code: ' +
      IntToStr(BASS_ErrorGetCode()), Self.StreamIndex, Self.StreamID);
  end;

  BASS_ChannelPlay(Self.StreamChannel, false);

  if (Self.ErrorFound) then
  begin
    Self.Logger.LogError
      ('The error has been resolved, stream recording resumed',
      Self.StreamIndex, Self.StreamID);
  end;

  Self.ErrorFound := false;
end;

procedure TRecorder.StopChannels;
begin
  Self.Status := sWaitStopForDestroy;

  if (Self.CreateStreamThreadStarted) then
  begin
    Self.CreateStreamThread.Terminate();
  end;

  BASS_Encode_Stop(Self.StreamEncoder);
  BASS_ChannelStop(Self.StreamChannel);
  BASS_StreamFree(Self.StreamChannel);

  Self.FileStorage.CloseFiles();
  Self.Logger.LogInfo('Processing of stream ' + Self.StreamID + ' has stopped',
    Self.StreamIndex, Self.StreamID);
  Self.Status := sStopped;
end;

procedure TRecorder.WriteAudioBuffer(buffer: Pointer; length: DWORD);
begin
  Self.FileStorage.WriteAudio(buffer, length);
end;

end.
