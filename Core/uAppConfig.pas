unit uAppConfig;

interface

uses
  IniFiles, SysUtils;

type

  TStreamConfig = record
    StreamUri: String;
    StreamToken: String;
    StreamTz: String;
  end;

  TAppConfig = class
  public
    Streams: array of TStreamConfig;
    FilesPath: String;
    DataBufferSizeKb: Integer;
    procedure LoadFromFile(FilePath: String);

  protected
    IniFile: TIniFile;
    procedure LoadStreamsData;
  end;

implementation

{ TAppConfig }

procedure TAppConfig.LoadFromFile(FilePath: String);
var
  ConfigDir: string;
  RawFilesPath: string;
  IsAbsolutePath: Boolean;
begin
  Self.IniFile := TIniFile.Create(FilePath);
  try
    Self.DataBufferSizeKb := IniFile.ReadInteger('General',
      'DataBufferSizeKb', -1);
    if (Self.DataBufferSizeKb < 1) then
      raise Exception.Create('Check DataBufferSizeKb config value!');

    // Read path from config
    RawFilesPath := IniFile.ReadString('Storage', 'FilesPath', '');
    if (RawFilesPath = '') then
      raise Exception.Create('Not found FilesPath value!');

    // Determine base directory (folder with ini-file)
    ConfigDir := ExtractFilePath(ExpandFileName(FilePath));

    // Check if path is absolute
    IsAbsolutePath := False;
{$IFDEF MSWINDOWS}
    // Windows: check for drive letter (C:\ or D:\)
    if (Length(RawFilesPath) >= 2) and (RawFilesPath[2] = ':') then
      IsAbsolutePath := True
    else if (Length(RawFilesPath) >= 1) and (RawFilesPath[1] = '\') then
      IsAbsolutePath := True;
{$ELSE}
    // Linux/Unix: absolute path starts with /
    if (Length(RawFilesPath) >= 1) and (RawFilesPath[1] = '/') then
      IsAbsolutePath := True;
{$ENDIF}
    // Convert to full path
    if IsAbsolutePath then
      Self.FilesPath := RawFilesPath
    else
      // Relative path - make it relative to config folder
      Self.FilesPath := ExpandFileName(IncludeTrailingPathDelimiter(ConfigDir) +
        RawFilesPath);

    Self.LoadStreamsData();
  finally
    Self.IniFile.Free;
  end;
end;

procedure TAppConfig.LoadStreamsData;
var
  StreamIndex: Byte;

  CurrentStreamUri, CurrentStreamToken, CurrentStreamTz: String;
begin
  StreamIndex := 0;

  while True do
  begin
    CurrentStreamUri := IniFile.ReadString('Stream',
      'StreamUri' + IntToStr(StreamIndex), '');
    CurrentStreamToken := IniFile.ReadString('Stream',
      'StreamToken' + IntToStr(StreamIndex), '');
    CurrentStreamTz := IniFile.ReadString('Stream',
      'StreamTz' + IntToStr(StreamIndex), '');

    if ((CurrentStreamUri = '') or (CurrentStreamToken = '') or
      (CurrentStreamTz = '')) then
      break;

    SetLength(Self.Streams, StreamIndex + 1);

    Self.Streams[StreamIndex].StreamUri := CurrentStreamUri;
    Self.Streams[StreamIndex].StreamToken := CurrentStreamToken;
    Self.Streams[StreamIndex].StreamTz := CurrentStreamTz;

    inc(StreamIndex);
  end;
end;

end.
