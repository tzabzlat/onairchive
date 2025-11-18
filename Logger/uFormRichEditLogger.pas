unit uFormRichEditLogger;

interface

uses System.SyncObjs, System.SysUtils, Vcl.ComCtrls, Vcl.Graphics, uILogger;

type
  TFormRichEditLogger = class(TInterfacedObject, ILogger)
  protected
    RichEdit: TRichEdit;
    CriticalSection: TCriticalSection;
    function AddStreamDataPrefix(Text: String; StreamIndex: Smallint;
      StreamToken: String): String;
  public
    constructor Create(RichEditObj: TRichEdit);
    destructor Destroy(); override;
    procedure LogInfo(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogWarning(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogError(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
  protected
    procedure PushToRichEdit(Text: String; Color: TColor);
  end;

implementation

uses Messages, Vcl.Forms;

{ TFormRichEditLogger }

constructor TFormRichEditLogger.Create(RichEditObj: TRichEdit);
begin
  inherited Create;
  Self.RichEdit := RichEditObj;
  Self.RichEdit.Lines.Clear;
  Self.CriticalSection := TCriticalSection.Create();
end;

function TFormRichEditLogger.AddStreamDataPrefix(Text: String;
  StreamIndex: Smallint; StreamToken: String): String;
begin
  Result := '[ ' + IntToStr(StreamIndex) + ' - ' + StreamToken + ' ] ' + Text;
end;

destructor TFormRichEditLogger.Destroy;
begin
  Self.CriticalSection.Destroy;

  inherited Destroy;
end;

procedure TFormRichEditLogger.LogError(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  PushToRichEdit(Text, clRed);
  CriticalSection.Leave;
end;

procedure TFormRichEditLogger.LogInfo(Text: String; StreamIndex: Smallint = -1;
  StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  PushToRichEdit(Text, clGray);
  CriticalSection.Leave;
end;

procedure TFormRichEditLogger.LogWarning(Text: String;
  StreamIndex: Smallint = -1; StreamToken: String = '');
begin
  if ((StreamIndex > -1) and (StreamToken <> '')) then
    Text := Self.AddStreamDataPrefix(Text, StreamIndex, StreamToken);

  CriticalSection.Enter;
  PushToRichEdit(Text, clOlive);
  CriticalSection.Leave;
end;

procedure TFormRichEditLogger.PushToRichEdit(Text: String; Color: TColor);
begin
  Self.RichEdit.SelAttributes.Color := Color;
  Self.RichEdit.Lines.Add(FormatDateTime('yyyy-mm-dd hh:mm:ss.zzz', Now()) +
    ': ' + Text);
  Self.RichEdit.SelAttributes.Color := clBlack;

  Application.ProcessMessages;
end;

end.
