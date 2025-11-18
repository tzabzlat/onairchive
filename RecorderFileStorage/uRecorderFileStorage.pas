unit uRecorderFileStorage;

interface

uses
  {$IFDEF FPC}
  SysUtils, Classes, DateUtils,
  {$ELSE}
  SysUtils, System.Classes, System.DateUtils, System.IOUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Winapi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  {$ENDIF}
  TZDB, uILogger;

const
  AUDIO_FILE_EXTENSION = '.aac';
  TEXT_FILE_EXTENSION = '.txt';
  INIT_LAST_FILE_HOUR = 200;

type
  TCurrentStreamTitle = record
    Time: TDateTime;
    Text: String;
  end;

  TDataBufferStatus = record
    BytesPosition: Int64;
    BytesSize: Int64;
  end;

  TNeedNewFileChecker = class
  protected
    LastFileHour: Byte;
  public
    constructor Create;
    function NeedNewFile: Boolean;
  end;

  TRecorderFileStorage = class
  protected
    CurrentRootPath: String;
    DiskLetter: Char;
    StreamTimezone: TBundledTimeZone;
    StreamID: String;
    Logger: ILogger;

    CurrentAudioFileHandle: TFileStream;
    CurrentTextFileHandle: TFileStream;

    DataBufferSizeKb: Integer;
    DataBuffer: TMemoryStream;

    CurrentStreamTitle: TCurrentStreamTitle;

    NeedNewFileChecker: TNeedNewFileChecker;

    function GetFilesPathForCurrentStreamIdAndDate(): String;
    function GetFileNameWithoutExtensionForCurrentTime(): String;
    function ModifyFileNameForExcludeOverwriting(FilePath: String): String;
    function FormatStreamTimezoneDateTime(): String;
    function FormatStreamTimezoneDate(): String;
    function GetStreamDateTime(): TDateTime;

    procedure OpenNewFiles();
  public
    constructor Create(StreamIDStr: String; FilesPath: String;
      StreamTZ: TBundledTimeZone; LoggerObj: ILogger;
      DataBufferSizeValueKb: Integer);
    destructor Destroy(); override;
    function GetTotalStorageSizeMB: Integer;
    function GetStorageFreeSpaceMB: Integer;
    procedure CheckRootPathAccess();

    procedure WriteAudio(buffer: Pointer; length: DWORD);
    procedure WriteAudioDataBufferToFileAndClearBuffer();
    procedure WriteTitle(NewTitle: String; WriteCertainly: Boolean);
    procedure CloseFiles();
    function GetDataBufferStatus: TDataBufferStatus;
  end;

implementation

{$IFDEF FPC}
// Helper functions for Free Pascal compatibility

function GetFileNameWithoutExtension(const FileName: String): String;
var
  BaseName: String;
begin
  BaseName := ExtractFileName(FileName);
  Result := ChangeFileExt(BaseName, '');
end;

{$ENDIF}

{ TRecorderFileStorage }

procedure TRecorderFileStorage.CheckRootPathAccess;
var
  TestFilePath: String;
  FIleStream: TFileStream;
begin
  // If folder does not exist, but it can be created - good
  if (not DirectoryExists(Self.CurrentRootPath)) then
  begin
    if ForceDirectories(Self.CurrentRootPath) then
    begin
      exit;
    end;

    raise Exception.Create('"' + Self.CurrentRootPath +
      '" not found and cannot create');
  end
  else
  begin
    // If target folder exist, let's try to create file and folder in there,
    // and delete them immediately (for check the access to write).

    // Testing ability to create subfolders
    TestFilePath := IncludeTrailingPathDelimiter(Self.CurrentRootPath) + 'TestDir';

    if (not CreateDir(TestFilePath)) then
    begin
      raise Exception.Create('Cannot create access-test-folder in path ' + '"'
        + Self.CurrentRootPath + '"');
    end;

    if (not RemoveDir(TestFilePath)) then
    begin
      raise Exception.Create('Cannot remove access-test-folder in path ' + '"'
        + Self.CurrentRootPath + '"');
    end;

    TestFilePath := IncludeTrailingPathDelimiter(Self.CurrentRootPath) + 'TestFile.txt';

    // Testing ability to create files
    try
      FIleStream := TFileStream.Create(TestFilePath, fmCreate);
      FIleStream.WriteBuffer(TestFilePath, length(TestFilePath));
      FIleStream.Free;
    except
      raise Exception.Create('Cannot write access-test-file in path ' + '"' +
        Self.CurrentRootPath + '"');
    end;

    if (not DeleteFile(PChar(TestFilePath))) then
    begin
      raise Exception.Create('Cannot delete access-test-file in path ' + '"' +
        Self.CurrentRootPath + '"');
    end;
  end;
end;

procedure TRecorderFileStorage.CloseFiles;
begin
  if (Self.DataBuffer.Position > 0) then
    Self.WriteAudioDataBufferToFileAndClearBuffer();

  if (Self.CurrentAudioFileHandle <> nil) then
  begin
    Self.CurrentAudioFileHandle.Free;
    Self.CurrentAudioFileHandle := nil;
  end;

  if (Self.CurrentTextFileHandle <> nil) then
  begin
    Self.CurrentTextFileHandle.Free;
    Self.CurrentTextFileHandle := nil;
  end;

  Self.NeedNewFileChecker.Destroy;
  Self.NeedNewFileChecker := TNeedNewFileChecker.Create();
end;

constructor TRecorderFileStorage.Create(StreamIDStr: String; FilesPath: String;
  StreamTZ: TBundledTimeZone; LoggerObj: ILogger;
  DataBufferSizeValueKb: Integer);
var
  DiskLetterTmp: String;
begin
  inherited Create;

  Self.CurrentRootPath := FilesPath;
  Self.StreamID := StreamIDStr;
  Self.StreamTimezone := StreamTZ;
  DiskLetterTmp := ExtractFileDrive(CurrentRootPath);
  {$IFDEF MSWINDOWS}
  Self.DiskLetter := DiskLetterTmp[1];
  {$ELSE}
  // On Unix systems, there are no drive letters
  Self.DiskLetter := '/';
  {$ENDIF}
  Self.Logger := LoggerObj;

  Self.CurrentAudioFileHandle := nil;
  Self.CurrentTextFileHandle := nil;

  Self.DataBufferSizeKb := DataBufferSizeValueKb;
  Self.DataBuffer := TMemoryStream.Create;
  Self.DataBuffer.SetSize(Self.DataBufferSizeKb * 1024);
  Self.DataBuffer.Seek(0, soFromBeginning);

  Self.CurrentStreamTitle.Text := '';

  Self.NeedNewFileChecker := TNeedNewFileChecker.Create;
end;

destructor TRecorderFileStorage.Destroy;
begin
  if (Self.CurrentAudioFileHandle <> nil) then
  begin
    Self.CurrentAudioFileHandle.Free;
  end;

  if (Self.CurrentTextFileHandle <> nil) then
  begin
    Self.CurrentAudioFileHandle.Free;
  end;

  Self.DataBuffer.Free;

  Self.NeedNewFileChecker.Destroy;

  inherited;
end;

function TRecorderFileStorage.FormatStreamTimezoneDate: String;
begin
  Result := FormatDateTime('yyyy-mm-dd', Self.GetStreamDateTime);
end;

function TRecorderFileStorage.FormatStreamTimezoneDateTime: String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Self.GetStreamDateTime);
end;

function TRecorderFileStorage.GetDataBufferStatus: TDataBufferStatus;
begin
  Result.BytesPosition := Self.DataBuffer.Position;
  Result.BytesSize := Self.DataBuffer.Size;
end;

function TRecorderFileStorage.GetFileNameWithoutExtensionForCurrentTime: String;
var
  CurrentTimeStr: String;
begin
  CurrentTimeStr := Self.FormatStreamTimezoneDateTime();

  Result := '[' + FormatDateTime('yyyy-mm-dd hh-nn-ss', Self.GetStreamDateTime)
    + '] ' + Self.StreamID;
end;

function TRecorderFileStorage.GetFilesPathForCurrentStreamIdAndDate: String;
begin
  Result := IncludeTrailingPathDelimiter(Self.CurrentRootPath) +
    Self.StreamID + PathDelim +
    Self.FormatStreamTimezoneDate() + PathDelim;
end;

function TRecorderFileStorage.GetStorageFreeSpaceMB: Integer;
var
  FreeSpace: Int64;
begin
  {$IFDEF MSWINDOWS}
  FreeSpace := DiskFree(ord(Self.DiskLetter) - 64);

  if (FreeSpace < 1) then
  begin
    raise Exception.Create('Cannot get drive "' + Self.DiskLetter +
      '" free space');
  end;
  {$ELSE}
  // On Unix systems, get free space for the root filesystem
  FreeSpace := DiskFree(0);

  if (FreeSpace < 1) then
  begin
    raise Exception.Create('Cannot get filesystem free space');
  end;
  {$ENDIF}

  FreeSpace := (FreeSpace div 1024) div 1024;

  Result := FreeSpace;
end;

function TRecorderFileStorage.GetStreamDateTime: TDateTime;
var
  CurrentUniversalDateTime: TDateTime;
begin
  {$IFDEF FPC}
  CurrentUniversalDateTime := LocalTimeToUniversal(Now);
  {$ELSE}
  CurrentUniversalDateTime := TTimeZone.Local.ToUniversalTime(Now);
  {$ENDIF}
  Result := Self.StreamTimezone.ToLocalTime(CurrentUniversalDateTime);
end;

function TRecorderFileStorage.GetTotalStorageSizeMB: Integer;
var
  Size: Int64;
begin
  {$IFDEF MSWINDOWS}
  Size := DiskSize(ord(Self.DiskLetter) - 64);

  if (Size = -1) then
  begin
    raise Exception.Create('Cannot get drive "' + Self.DiskLetter + '" size');
  end;
  {$ELSE}
  // On Unix systems, get size for the root filesystem
  Size := DiskSize(0);

  if (Size = -1) then
  begin
    raise Exception.Create('Cannot get filesystem size');
  end;
  {$ENDIF}

  Size := (Size div 1024) div 1024;

  Result := Size;
end;

function TRecorderFileStorage.ModifyFileNameForExcludeOverwriting
  (FilePath: String): String;
var
  FilePathWithoutExtension: String;
  FileExtension: String;
  IncrementIndex: Integer;
begin
  Result := FilePath;

  if (not FileExists(FilePath)) then
    exit;

  {$IFDEF FPC}
  FilePathWithoutExtension := ExtractFilePath(FilePath) +
    GetFileNameWithoutExtension(FilePath);
  {$ELSE}
  FilePathWithoutExtension := ExtractFilePath(FilePath) +
    TPath.GetFileNameWithoutExtension(FilePath);
  {$ENDIF}

  FileExtension := ExtractFileExt(FilePath);

  IncrementIndex := 0;

  repeat
    Inc(IncrementIndex);

    FilePath := FilePathWithoutExtension + ' (' + IntToStr(IncrementIndex) + ')'
      + FileExtension;
  until (FileExists(FilePath));
end;

procedure TRecorderFileStorage.OpenNewFiles;
var
  NewAudioFileName: string;
  NewTextFileName: string;
  FilePath: String;
begin
  if (Self.CurrentAudioFileHandle <> nil) then
  begin
    Self.CurrentAudioFileHandle.Free;
  end;

  if (Self.CurrentTextFileHandle <> nil) then
  begin
    Self.CurrentTextFileHandle.Free;
  end;

  FilePath := Self.GetFilesPathForCurrentStreamIdAndDate();

  if (not DirectoryExists(FilePath)) then
  begin
    if not ForceDirectories(FilePath) then
    begin
      raise Exception.Create('Cannot create path ' + FilePath);
    end;
  end;

  NewAudioFileName := Self.GetFilesPathForCurrentStreamIdAndDate();
  NewAudioFileName := NewAudioFileName +
    Self.GetFileNameWithoutExtensionForCurrentTime();
  NewAudioFileName := NewAudioFileName + AUDIO_FILE_EXTENSION;

  NewAudioFileName := Self.ModifyFileNameForExcludeOverwriting
    (NewAudioFileName);

  {$IFDEF FPC}
  NewTextFileName := ExtractFilePath(NewAudioFileName) +
    GetFileNameWithoutExtension(NewAudioFileName) + TEXT_FILE_EXTENSION;
  {$ELSE}
  NewTextFileName := ExtractFilePath(NewAudioFileName) +
    TPath.GetFileNameWithoutExtension(NewAudioFileName) + TEXT_FILE_EXTENSION;
  {$ENDIF}

  Self.CurrentAudioFileHandle := TFileStream.Create(NewAudioFileName,
    fmCreate or fmShareDenyWrite);

  Self.CurrentTextFileHandle := TFileStream.Create(NewTextFileName,
    fmCreate or fmShareDenyWrite);

  if (Self.CurrentStreamTitle.Text <> '') then
  begin
    Self.WriteTitle(Self.CurrentStreamTitle.Text, True);
  end;
end;

procedure TRecorderFileStorage.WriteAudio(buffer: Pointer; length: DWORD);
begin
  if (Self.NeedNewFileChecker.NeedNewFile) then
  begin
    // If need new files

    // Flush buffer data to current file
    Self.WriteAudioDataBufferToFileAndClearBuffer();

    // Let's create new files
    Self.OpenNewFiles();
  end;

  if (Self.DataBuffer.Position + length > Self.DataBuffer.Size) then
  begin
    // The buffer is full. Let's flush the data into a file.
    Self.WriteAudioDataBufferToFileAndClearBuffer();
  end;

  Self.DataBuffer.WriteBuffer(buffer^, length);
end;

procedure TRecorderFileStorage.WriteAudioDataBufferToFileAndClearBuffer;
var
  BytesCountForCopy: Int64;
begin
  // 1. Write buffer to the current file
  BytesCountForCopy := Self.DataBuffer.Position;

  Self.DataBuffer.Seek(0, soFromBeginning);

  Self.CurrentAudioFileHandle.CopyFrom(Self.DataBuffer, BytesCountForCopy);

  // 2. Move buffer pointer to the beginning
  Self.DataBuffer.Seek(0, soFromBeginning);
end;

procedure TRecorderFileStorage.WriteTitle(NewTitle: String;
  WriteCertainly: Boolean);
var
  TempStream: TStringStream;
begin
  if (Self.NeedNewFileChecker.NeedNewFile) then
  begin
    Self.OpenNewFiles();
  end;

  if ((NewTitle = Self.CurrentStreamTitle.Text) and (not WriteCertainly)) then
  begin
    exit;
  end;

  Self.CurrentStreamTitle.Text := NewTitle;

  NewTitle := '[' + Self.FormatStreamTimezoneDateTime + '] ' + NewTitle
    + #13#10;

  TempStream := TStringStream.Create(NewTitle, TEncoding.UTF8);

  Self.CurrentTextFileHandle.CopyFrom(TempStream, TempStream.Size);

  TempStream.Destroy;
end;

{ TNeedNewFileChecker }

constructor TNeedNewFileChecker.Create;
begin
  inherited Create;

  Self.LastFileHour := INIT_LAST_FILE_HOUR;
end;

function TNeedNewFileChecker.NeedNewFile: Boolean;
var
  CurrentDT: TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  Result := false;

  CurrentDT := Now;

  DecodeTime(CurrentDT, Hour, Min, Sec, MSec);

  if (Hour <> Self.LastFileHour) then
  begin
    Self.LastFileHour := Hour;

    Result := True;
  end;
end;

end.
