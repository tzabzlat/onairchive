program OnAirchiveVCL;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uILogger in '..\..\..\Logger\uILogger.pas',
  uFileRotateLogger in '..\..\..\Logger\uFileRotateLogger.pas',
  uRecorder in '..\..\..\Recorder\uRecorder.pas',
  uRecorderFileStorage in '..\..\..\RecorderFileStorage\uRecorderFileStorage.pas',
  uCreateStreamFromURLThread in '..\..\..\Recorder\uCreateStreamFromURLThread.pas',
  TZDB in '..\..\..\Core\TZDB.pas',
  uAppConfig in '..\..\..\Core\uAppConfig.pas',
  uRecorderApp in '..\..\..\Core\uRecorderApp.pas',
  bass in '..\..\..\ThirdParty\bass.pas',
  bass_aac in '..\..\..\ThirdParty\bass_aac.pas',
  bassenc in '..\..\..\ThirdParty\bassenc.pas',
  uFormRichEditLogger in '..\..\..\Logger\uFormRichEditLogger.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'On Airchive';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
