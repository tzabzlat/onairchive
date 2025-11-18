unit uSTOUNAndFileLogger;

{
  Unsecure (!) HTTP notifications to Telegram via "stoun" bot
 (@see https://doc.api.stonline.my-lessons.ru/?preset-page=TelegramNotifier)
}

interface

uses System.SyncObjs, System.SysUtils, Vcl.ComCtrls, Vcl.Graphics, uILogger,
  Quick.Logger, Quick.Logger.Provider.Files, Quick.Logger.ExceptionHook,
  Quick.Threads, uSTOUNLogProvider;

type
  TSTOUNAndFileLogger = class(TInterfacedObject, ILogger)
  protected
    CriticalSection: TCriticalSection;
    function AddStreamDataPrefix(Text: String; StreamIndex: Smallint;
      StreamToken: String): String;
  public
    constructor Create(FilesPath: String; STOUNToken: String);
    destructor Destroy(); override;
    procedure LogInfo(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogWarning(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogError(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
  protected
    procedure PushToFile(Text: String; Level: TEventType);
  end;

implementation

uses Messages, Vcl.Forms;

function TSTOUNAndFileLogger.AddStreamDataPrefix(Text: String;
  StreamIndex: Smallint; StreamToken: String): String;
begin
  Result := '[ ' + IntToStr(StreamIndex) + ' - ' + StreamToken + ' ] ' + Text;
end;

constructor TSTOUNAndFileLogger.Create(FilesPath: String; STOUNToken: String);
begin
  inherited Create;

  Self.CriticalSection := TCriticalSection.Create();

  GlobalLogFileProvider.DailyRotate := True;
  GlobalLogFileProvider.MaxRotateFiles := 30;
  GlobalLogFileProvider.MaxFileSizeInMB := 10;
  GlobalLogFileProvider.RotatedFilesPath := FilesPath;
  GlobalLogFileProvider.Enabled := True;

  GlobalSTOUNProvider.URL := ' http://stoun.my-lessons.ru/notify';
  GlobalSTOUNProvider.STOUNToken := STOUNToken;
  GlobalSTOUNProvider.Enabled := True;
  GlobalSTOUNProvider.LogLevel := [etError, etCritical, etException, etWarning];

  Logger.Providers.Add(GlobalLogFileProvider);
  Logger.Providers.Add(GlobalSTOUNProvider);
end;

destructor TSTOUNAndFileLogger.Destroy;
begin
  Self.CriticalSection.Destroy;

  inherited Destroy;
end;

procedure TSTOUNAndFileLogger.LogError(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, etError);
  CriticalSection.Leave;
end;

procedure TSTOUNAndFileLogger.LogInfo(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, etInfo);
  CriticalSection.Leave;
end;

procedure TSTOUNAndFileLogger.LogWarning(Text: String;
  StreamIndex: Smallint = -1; StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, etWarning);
  CriticalSection.Leave;
end;

procedure TSTOUNAndFileLogger.PushToFile(Text: String; Level: TEventType);
begin
  case Level of
    etInfo:
      Logger.Info(Text);
    etWarning:
      Logger.Warn(Text);
    etError:
      Logger.Error(Text);
    etException:
      Logger.Error(Text);
  else
    Logger.Info(Text);
  end;
end;

end.
