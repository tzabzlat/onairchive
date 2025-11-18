unit uCreateStreamFromURLThread;

interface

uses
  Classes, SysUtils, bass;

type
  TCreateStreamFromURLThreadBufferizationCallBackReference = procedure()
    of object;
  TCreateStreamFromURLThreadFinalCallBackReference = procedure
    (StreamChannel: HSTREAM; ErrorCode: Integer; BeforeBufferization: Boolean)
    of object;

  TCreateStreamFromURLThread = class(TThread)
  private
    { Private declarations }
  protected
    StreamChannel: bass.HSTREAM;
    ErrorCode: Integer;
    BeforeBufferization: Boolean;
    procedure Execute; override;
    procedure CallFinalCallBack();
    procedure CallStartBufferizationCallBack();
    constructor Create(CreateSuspended: Boolean); overload;
  public
    StreamURI: String;
    DownloadProc: bass.DownloadProc;
    RecorderObjectPointer: Pointer;
    StartBufferizationCallBackProc
      : TCreateStreamFromURLThreadBufferizationCallBackReference;
    FinalCallBackProc: TCreateStreamFromURLThreadFinalCallBackReference;
  end;

implementation

{ TCreateStreamFromURLThread }

procedure TCreateStreamFromURLThread.CallFinalCallBack;
begin
  Self.FinalCallBackProc(Self.StreamChannel, Self.ErrorCode,
    Self.BeforeBufferization);
end;

procedure TCreateStreamFromURLThread.CallStartBufferizationCallBack;
begin
  Self.StartBufferizationCallBackProc();
end;

constructor TCreateStreamFromURLThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  Self.ErrorCode := BASS_OK;
  Self.BeforeBufferization := true;
end;

procedure TCreateStreamFromURLThread.Execute;
var
  len, Progress: DWORD;
begin
  if (Self.Terminated) then
  begin
    {$IFDEF FPC}
    Queue(@CallFinalCallBack);
    {$ELSE}
    Synchronize(CallFinalCallBack);
    {$ENDIF}

    Exit;
  end;

  {$IFDEF MSWINDOWS}
  StreamChannel := BASS_StreamCreateURL(PChar(Self.StreamURI), 0,
    BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or
    BASS_UNICODE, Self.DownloadProc, Self.RecorderObjectPointer);
  {$ELSE}
  StreamChannel := BASS_StreamCreateURL(PChar(Self.StreamURI), 0,
    BASS_STREAM_BLOCK or BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE,
    Self.DownloadProc, Self.RecorderObjectPointer);
  {$ENDIF}

  if (Self.StreamChannel = 0) then
  begin
    Self.ErrorCode := BASS_ErrorGetCode();
    {$IFDEF FPC}
    Queue(@CallFinalCallBack);
    {$ELSE}
    Synchronize(CallFinalCallBack);
    {$ENDIF}

    Exit;
  end;

  if (Self.Terminated) then
  begin
    BASS_StreamFree(StreamChannel);

    {$IFDEF FPC}
    Queue(@CallFinalCallBack);
    {$ELSE}
    Synchronize(CallFinalCallBack);
    {$ENDIF}

    Exit;
  end;

  {$IFDEF FPC}
  Queue(@CallStartBufferizationCallBack);
  {$ELSE}
  Synchronize(CallStartBufferizationCallBack);
  {$ENDIF}
  BeforeBufferization := false;

  // Progress
  repeat
    if (Self.Terminated) then
    begin
      BASS_StreamFree(StreamChannel);

      {$IFDEF FPC}
      Queue(@CallFinalCallBack);
      {$ELSE}
      Synchronize(CallFinalCallBack);
      {$ENDIF}

      Exit;
    end;

    len := BASS_StreamGetFilePosition(Self.StreamChannel, BASS_FILEPOS_END);

    if (len = DW_Error) then
    begin
      Self.ErrorCode := BASS_ErrorGetCode();
      {$IFDEF FPC}
      Queue(@CallFinalCallBack);
      {$ELSE}
      Synchronize(CallFinalCallBack);
      {$ENDIF}

      Exit;
    end;

    Progress := BASS_StreamGetFilePosition(Self.StreamChannel,
      BASS_FILEPOS_BUFFER) * 100 div len;
    Sleep(100);  // Small delay to avoid busy loop
  until (Progress > 75) or (BASS_StreamGetFilePosition(Self.StreamChannel,
    BASS_FILEPOS_CONNECTED) = 0);

  {$IFDEF FPC}
  Queue(@CallFinalCallBack);
  {$ELSE}
  Synchronize(CallFinalCallBack);
  {$ENDIF}
end;

end.
