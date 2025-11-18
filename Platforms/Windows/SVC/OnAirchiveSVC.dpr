program OnAirchiveSVC;

{$ifdef DEBUG}
{$APPTYPE CONSOLE}
{$endif}

uses
  SysUtils,
  Vcl.SvcMgr,
  OnAirchiveSVCServiceUnit in 'OnAirchiveSVCServiceUnit.pas' {OnAirchiveSVCService: TService},
  uFileRotateLogger in '..\..\..\Logger\uFileRotateLogger.pas',
  uILogger in '..\..\..\Logger\uILogger.pas',
  uRecorder in '..\..\..\Recorder\uRecorder.pas',
  uRecorderFileStorage in '..\..\..\RecorderFileStorage\uRecorderFileStorage.pas',
  uSTOUNLogProvider in '..\..\..\Logger\uSTOUNLogProvider.pas',
  uSTOUNAndFileLogger in '..\..\..\Logger\uSTOUNAndFileLogger.pas',
  uCreateStreamFromURLThread in '..\..\..\Recorder\uCreateStreamFromURLThread.pas',
  TZDB in '..\..\..\Core\TZDB.pas',
  uAppConfig in '..\..\..\Core\uAppConfig.pas',
  uRecorderApp in '..\..\..\Core\uRecorderApp.pas',
  bass in '..\..\..\ThirdParty\bass.pas',
  bass_aac in '..\..\..\ThirdParty\bass_aac.pas',
  bassenc in '..\..\..\ThirdParty\bassenc.pas';

{$R *.RES}

{$IFDEF DEBUG}
var
  ServerContainer1: TOnAirchiveSVCService;
  DummyBoolean: Boolean;
{$ENDIF}

begin
{$IFDEF DEBUG}
  try
    // In debug mode the server acts as a console application.
    WriteLn('Service in DEBUG mode. Press enter to exit.');

    // Create the TService descendant manually.
    ServerContainer1 := TOnAirchiveSVCService.Create(nil);

    // Simulate service start.
    ServerContainer1.ServiceStart(ServerContainer1, DummyBoolean);

    // Keep the console box running (ServerContainer1 code runs in the background)
    ReadLn;

    // On exit, destroy the service object.
    FreeAndNil(ServerContainer1);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      WriteLn('Press enter to exit.');
      ReadLn;
    end;
  end;
{$ELSE}
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TOnAirchiveSVCService, OnAirchiveSVCService);
  Application.Run;
{$ENDIF}

end.
