unit uFileRotateLogger;

interface

uses
  SyncObjs, SysUtils, uILogger,
  Quick.Logger, Quick.Logger.Provider.Files, Quick.Logger.Provider.Rest,
  Quick.Logger.ExceptionHook,
  Quick.Threads;

type
  TFileRotateLogger = class(TInterfacedObject, ILogger)
  protected
    CriticalSection: TCriticalSection;
    function AddStreamDataPrefix(Text: String; StreamIndex: Smallint;
      StreamToken: String): String;
  public
    constructor Create(FilesPath: String);
    destructor Destroy(); override;
    procedure LogInfo(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogWarning(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogError(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
  protected
    procedure PushToFile(Text: String; Error: Boolean);
  end;

implementation

{ TFileRotateLogger }

constructor TFileRotateLogger.Create(FilesPath: String);
begin
  inherited Create;

  Self.CriticalSection := TCriticalSection.Create();

  GlobalLogFileProvider.DailyRotate := True;
  GlobalLogFileProvider.MaxRotateFiles := 30;
  GlobalLogFileProvider.MaxFileSizeInMB := 10;
  GlobalLogFileProvider.RotatedFilesPath := FilesPath;
  Logger.Providers.Add(GlobalLogFileProvider);
  GlobalLogFileProvider.Enabled := True;
end;

destructor TFileRotateLogger.Destroy;
begin
  Self.CriticalSection.Destroy;

  inherited Destroy;
end;

function TFileRotateLogger.AddStreamDataPrefix(Text: String;
  StreamIndex: Smallint; StreamToken: String): String;
begin
  Result := '[ ' + IntToStr(StreamIndex) + ' - ' + StreamToken + ' ] ' + Text;
end;

procedure TFileRotateLogger.LogError(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, True);
  CriticalSection.Leave;
end;

procedure TFileRotateLogger.LogInfo(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, false);
  CriticalSection.Leave;
end;

procedure TFileRotateLogger.LogWarning(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  Self.PushToFile(Text, false);
  CriticalSection.Leave;
end;

procedure TFileRotateLogger.PushToFile(Text: String; Error: Boolean);
begin
  if (Error) then
  begin
    Logger.Error('[ERROR] ' + Text);
  end
  else
  begin
    Logger.Info(Text);
  end;
end;

end.
