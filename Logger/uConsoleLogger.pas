unit uConsoleLogger;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  uILogger;

type
  TConsoleLogger = class(TInterfacedObject, ILogger)
  private
    function AddStreamDataPrefix(Text: String; StreamIndex: Smallint;
      StreamToken: String): String;
    function FormatLogMessage(LogLevel: String; Text: String): String;
  public
    procedure LogInfo(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogWarning(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogError(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
  end;

implementation

function TConsoleLogger.AddStreamDataPrefix(Text: String;
  StreamIndex: Smallint; StreamToken: String): String;
begin
  Result := '[ ' + IntToStr(StreamIndex) + ' - ' + StreamToken + ' ] ' + Text;
end;

function TConsoleLogger.FormatLogMessage(LogLevel: String; Text: String): String;
begin
  Result := '[' + LogLevel + '] ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ' + Text;
end;

procedure TConsoleLogger.LogInfo(Text: String; StreamIndex: Smallint;
  StreamToken: String);
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  WriteLn(FormatLogMessage('INFO', Text));
end;

procedure TConsoleLogger.LogWarning(Text: String; StreamIndex: Smallint;
  StreamToken: String);
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  WriteLn(FormatLogMessage('WARN', Text));
end;

procedure TConsoleLogger.LogError(Text: String; StreamIndex: Smallint;
  StreamToken: String);
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  // Write errors to stderr
  {$IFDEF FPC}
  WriteLn(StdErr, FormatLogMessage('ERROR', Text));
  {$ELSE}
  WriteLn(ErrOutput, FormatLogMessage('ERROR', Text));
  {$ENDIF}
end;

end.
