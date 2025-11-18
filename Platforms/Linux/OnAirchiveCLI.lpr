program OnAirchiveLinux;

{$mode objfpc}{$H+}

uses
  cthreads, BaseUnix,
  SysUtils, Classes,
  uRecorderApp in '../../Core/uRecorderApp.pas',
  uAppConfig in '../../Core/uAppConfig.pas',
  TZDB in '../../Core/TZDB.pas',
  uRecorder in '../../Recorder/uRecorder.pas',
  uCreateStreamFromURLThread in '../../Recorder/uCreateStreamFromURLThread.pas',
  uRecorderFileStorage in '../../RecorderFileStorage/uRecorderFileStorage.pas',
  uILogger in '../../Logger/uILogger.pas',
  uConsoleLogger in '../../Logger/uConsoleLogger.pas',
  bass in '../../ThirdParty/bass.pas',
  bassenc in '../../ThirdParty/bassenc.pas';

var
  ConsoleLogger: ILogger;
  RecorderApp: TRecorderApp;
  Running: Boolean = True;
  CheckInterval: Integer = 1000; // milliseconds

procedure SignalHandler(Signal: LongInt); cdecl;
begin
  WriteLn('');
  WriteLn('Received signal ', Signal, ', shutting down gracefully...');
  Running := False;
end;

function ParseArguments: Boolean;
begin
  Result := True;
end;

begin
  // Parse command line arguments
  if not ParseArguments then
    Exit;

  WriteLn('On Airchive Linux');

  // Set up signal handlers for graceful shutdown
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  FpSignal(SIGHUP, @SignalHandler);

  // Create console logger
  ConsoleLogger := TConsoleLogger.Create;
  WriteLn('Console logger initialized');
  WriteLn('Press Ctrl+C to stop');

  // Get application path
  WriteLn('Application path: ', ExtractFilePath(ParamStr(0)));

  // Create and initialize recorder app
  try
    RecorderApp := TRecorderApp.Create(
      ExtractFilePath(ParamStr(0)),
      ConsoleLogger
    );

    try
      RecorderApp.Start;

      // Main loop
      while Running do
      begin
        RecorderApp.CheckTimerProc;
        CheckSynchronize(100);  // Process queued thread callbacks
        Sleep(CheckInterval - 100);
      end;

      ConsoleLogger.LogInfo('Stopping recording...');
      RecorderApp.Stop;
      ConsoleLogger.LogInfo('Recording stopped');

    finally
      RecorderApp.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(StdErr, '');
      WriteLn(StdErr, 'FATAL ERROR: ', E.ClassName, ': ', E.Message);
      WriteLn(StdErr, '');
      ExitCode := 1;
    end;
  end;

  WriteLn('Recording Station shutdown complete');
end.
