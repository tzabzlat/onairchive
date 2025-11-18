unit OnAirchiveSVCServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, uFileRotateLogger,
  uRecorderApp, Registry, uILogger, Vcl.ExtCtrls;

const
  REGISTRY_SERVICES_PATH = '\SYSTEM\CurrentControlSet\Services\';
  REGISTRY_PATH_KEY_NAME = 'ImagePath';

type
  TOnAirchiveSVCService = class(TService)
    PeriodicalProcTimer: TTimer;
    StorageFreeSpaceNotifier: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
    procedure PeriodicalProcTimerTimer(Sender: TObject);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    Logger: ILogger;
    RecorderApp: TRecorderApp;

    procedure SaveInstallPathToRegistry(Sender: TService);
    function GetInstallPathFromRegistry(Sender: TService): string;
    function GetRegistryKey(Sender: TService): string;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  OnAirchiveSVCService: TOnAirchiveSVCService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  OnAirchiveSVCService.Controller(CtrlCode);
end;

function TOnAirchiveSVCService.GetInstallPathFromRegistry
  (Sender: TService): string;
var
  Reg: TRegistry;
  PCharString: PChar;
begin
  Reg := TRegistry.Create;

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.Access := KEY_READ;
    if (not Reg.OpenKey(Self.GetRegistryKey(Sender), false)) then
    begin
      raise Exception.Create('Cant read registry key ' +
        Self.GetRegistryKey(Sender) + ' ' + REGISTRY_PATH_KEY_NAME);
    end;

    PCharString := PChar(Reg.ReadString(REGISTRY_PATH_KEY_NAME));

    Result := AnsiExtractQuotedStr(PCharString, '"');

    Exit;
  finally
    Reg.Free;
  end;

  raise Exception.Create('Cant read registry key ' + Self.GetRegistryKey(Sender)
    + ' ' + REGISTRY_PATH_KEY_NAME);
end;

function TOnAirchiveSVCService.GetRegistryKey(Sender: TService): string;
begin
  Result := REGISTRY_SERVICES_PATH + Sender.Name;
end;

function TOnAirchiveSVCService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TOnAirchiveSVCService.PeriodicalProcTimerTimer(Sender: TObject);
begin
  RecorderApp.CheckTimerProc();
end;

procedure TOnAirchiveSVCService.SaveInstallPathToRegistry(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.Access := KEY_SET_VALUE;
    if Reg.OpenKey(Self.GetRegistryKey(Sender), false) then
      try
        Reg.WriteString(REGISTRY_PATH_KEY_NAME,
          SysUtils.AnsiQuotedStr(ParamStr(0), '"'));
      finally
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;

procedure TOnAirchiveSVCService.ServiceAfterInstall(Sender: TService);
begin
  Self.SaveInstallPathToRegistry(Sender);
end;

procedure TOnAirchiveSVCService.ServiceStart(Sender: TService;
  var Started: Boolean);
var
  InstallPath: string;
begin
  InstallPath := Self.GetInstallPathFromRegistry(Sender);

  InstallPath := ExtractFileDir(InstallPath);

  Logger := TFileRotateLogger.Create(InstallPath + '\logs\service');
  RecorderApp := TRecorderApp.Create(InstallPath, Logger);

  try
    RecorderApp.Start;

    Self.PeriodicalProcTimer.Enabled := true;
    Self.StorageFreeSpaceNotifier.Enabled := true;
  except
    on E: Exception do
      Logger.LogError(E.Message);
  end;
end;

procedure TOnAirchiveSVCService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  Self.PeriodicalProcTimer.Enabled := false;
  RecorderApp.Stop;
  RecorderApp.Destroy;
end;

end.
