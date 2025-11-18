unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFormRichEditLogger, uILogger,
  uFileRotateLogger,
  uRecorderApp,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons;

type
  TStreamVCLControls = record
    FSDataBufferProgressGroupBox: TGroupBox;
    FSDataBufferProgressBar: TProgressBar;
    FSDataBufferProgressStr: TLabel;
  end;

  TStreamVCLControlsList = array of TStreamVCLControls;

  TForm1 = class(TForm)
    PeriodicalProcTimer: TTimer;
    RunTimer: TTimer;
    StorageFreeSpaceNotifier: TTimer;
    FSDataBufferStatusPrinter: TTimer;
    MainPageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    LogRichEdit: TRichEdit;
    FSDataBufferStatusPrinterScrollBox: TScrollBox;
    FSDataBufferStatusPrinterGroup: TGroupBox;
    FSDataBufferStatusPrinterProgressStr: TLabel;
    FSDataBufferStatusPrinterProgressBar: TProgressBar;
    FSDataBufferStatusPrinterSwitcher: TSpeedButton;
    procedure PeriodicalProcTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RunTimerTimer(Sender: TObject);
    procedure StorageFreeSpaceNotifierTimer(Sender: TObject);
    procedure FSDataBufferStatusPrinterSwitcherClick(Sender: TObject);
    procedure FSDataBufferStatusPrinterTimer(Sender: TObject);
    function PrintBytes(Bytes: Int64): String;
    procedure PrepareVCLFSBufferStatusPrinterObjects();
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    StreamControlsList: TStreamVCLControlsList;
  public
    { Public declarations }
    RichEditLogger: ILogger;
    RecorderApp: TRecorderApp;
  end;

var
  Form1: TForm1;

implementation

uses uRecorderFileStorage;

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Self.RecorderApp = nil) then
  begin
    Self.RunTimer.Enabled := false;
    Self.PeriodicalProcTimer.Enabled := false;

    Exit;
  end;

  Self.PeriodicalProcTimer.Enabled := false;
  RecorderApp.Stop;
  RecorderApp.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.RecorderApp := nil;
  MainPageControl.TabIndex := 0;
end;

procedure TForm1.FSDataBufferStatusPrinterSwitcherClick(Sender: TObject);
var
  NewStatus: Boolean;

  Index: Byte;
begin
  NewStatus := (Sender as TSpeedButton).Down;

  if (NewStatus) then
  begin
    Self.FSDataBufferStatusPrinterTimer(Self.FSDataBufferStatusPrinter);
  end;

  Self.FSDataBufferStatusPrinter.Enabled := NewStatus;

  for Index := 0 to High(Self.StreamControlsList) do
  begin
    Self.StreamControlsList[Index].FSDataBufferProgressGroupBox.Visible :=
      NewStatus;
  end;
end;

procedure TForm1.FSDataBufferStatusPrinterTimer(Sender: TObject);
var
  StreamList: TStreamStatusDataList;
  PercentValue: Byte;
  Index: Byte;
  ItemsCount: Byte;
begin
  StreamList := Self.RecorderApp.GetFileStorageDataBufferStatus();
  ItemsCount := Length(StreamList);

  for Index := 0 to ItemsCount - 1 do
  begin
    PercentValue := Round((StreamList[Index].FSDataBufferStatus.BytesPosition /
      StreamList[Index].FSDataBufferStatus.BytesSize) * 100);

    Self.StreamControlsList[Index].FSDataBufferProgressBar.Position :=
      PercentValue;

    Self.StreamControlsList[Index].FSDataBufferProgressStr.Caption :=
      IntToStr(PercentValue) + '% ( ' + Self.PrintBytes
      (StreamList[Index].FSDataBufferStatus.BytesPosition) + ' / ' +
      Self.PrintBytes(StreamList[Index].FSDataBufferStatus.BytesSize) + ' )';
  end;
end;

procedure TForm1.PeriodicalProcTimerTimer(Sender: TObject);
begin
  RecorderApp.CheckTimerProc();
end;

procedure TForm1.PrepareVCLFSBufferStatusPrinterObjects;
var
  GroupBoxMS, ProgressBarMS, LabelMS: TMemoryStream;

  GroupBox: TGroupBox;
  ProgressBar: TProgressBar;
  LabelObj: TLabel;

  Index: Byte;
  StreamList: TStreamStatusDataList;
  ItemsCount: Byte;
begin
  StreamList := Self.RecorderApp.GetFileStorageDataBufferStatus();
  ItemsCount := Length(StreamList);
  SetLength(Self.StreamControlsList, ItemsCount);

  GroupBoxMS := TMemoryStream.Create;
  ProgressBarMS := TMemoryStream.Create;
  LabelMS := TMemoryStream.Create;

  GroupBoxMS.WriteComponent(FSDataBufferStatusPrinterGroup);
  ProgressBarMS.WriteComponent(FSDataBufferStatusPrinterProgressBar);
  LabelMS.WriteComponent(FSDataBufferStatusPrinterProgressStr);

  GroupBoxMS.Seek(0, soFromBeginning);
  ProgressBarMS.Seek(0, soFromBeginning);
  LabelMS.Seek(0, soFromBeginning);

  for Index := 0 to ItemsCount - 1 do
  begin
    GroupBox := TGroupBox.Create(FSDataBufferStatusPrinterScrollBox);
    GroupBox.Parent := FSDataBufferStatusPrinterScrollBox;
    GroupBoxMS.ReadComponent(GroupBox);
    GroupBox.Top := GroupBox.Top + (GroupBox.Height * Index) + (5 * Index);
    GroupBox.Name := GroupBox.Name + IntToStr(Index);
    GroupBox.Caption := IntToStr(StreamList[Index].StreamIndex) + ' - ' +
      StreamList[Index].StreamToken;
    GroupBoxMS.Seek(0, soFromBeginning);

    ProgressBar := TProgressBar.Create(GroupBox);
    ProgressBar.Parent := GroupBox;
    ProgressBarMS.ReadComponent(ProgressBar);
    ProgressBarMS.Seek(0, soFromBeginning);
    ProgressBar.Name := ProgressBar.Name + IntToStr(Index);

    LabelObj := TLabel.Create(GroupBox);
    LabelObj.Parent := GroupBox;
    LabelMS.ReadComponent(LabelObj);
    LabelMS.Seek(0, soFromBeginning);
    LabelObj.Name := LabelObj.Name + IntToStr(Index);
    LabelObj.Caption := '';

    Self.StreamControlsList[Index].FSDataBufferProgressBar := ProgressBar;
    Self.StreamControlsList[Index].FSDataBufferProgressStr := LabelObj;
    Self.StreamControlsList[Index].FSDataBufferProgressGroupBox := GroupBox;
  end;

  GroupBoxMS.Free;
  ProgressBarMS.Free;
  LabelMS.Free;
end;

function TForm1.PrintBytes(Bytes: Int64): String;
const
  B = 1; // byte
  KB = 1024 * B; // kilobyte
  MB = 1024 * KB; // megabyte
  GB = 1024 * MB; // gigabyte
begin
  if Bytes > GB then
    result := FormatFloat('#.## GB', Bytes / GB)
  else if Bytes > MB then
    result := FormatFloat('#.## MB', Bytes / MB)
  else if Bytes > KB then
    result := FormatFloat('#.## KB', Bytes / KB)
  else
    result := FormatFloat('#.## B', Bytes);
end;

procedure TForm1.RunTimerTimer(Sender: TObject);
begin
  RunTimer.Enabled := false;

  RichEditLogger := TFormRichEditLogger.Create(Self.LogRichEdit);
  RecorderApp := TRecorderApp.Create(ExtractFileDir(Application.ExeName),
    RichEditLogger);

  try
    Self.PrepareVCLFSBufferStatusPrinterObjects();
    RecorderApp.Start();

    Self.PeriodicalProcTimer.Enabled := true;
    Self.StorageFreeSpaceNotifier.Enabled := true;

    Self.FSDataBufferStatusPrinterSwitcher.Enabled := true;
  except
    on E: Exception do
      RichEditLogger.LogError(E.Message);
  end;

end;

procedure TForm1.StorageFreeSpaceNotifierTimer(Sender: TObject);
var
  FreeSpace: Integer;
begin
  FreeSpace := Self.RecorderApp.GetFreeSpaceInStorageMB;

  if (FreeSpace < CRITICAL_FREE_SPACE_VALUE_MB) then
  begin
    Self.RichEditLogger.LogError('Storage has less than '
      + IntToStr(CRITICAL_FREE_SPACE_VALUE_MB) +
      ' MB of free space. More free space is required.');
  end;
end;

end.
