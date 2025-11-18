unit uSTOUNLogProvider;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.HttpClient,
  Quick.Commons,
  Quick.Logger;

type

  TLogSTOUNProvider = class(TLogProviderBase)
  private
    fHTTPClient: TJsonHTTPClient;
    fURL: string;
    fUserAgent: string;
    fSTOUNToken: String;
    fJsonOutputOptions: TJsonOutputOptions;
    function PostData(Data: String): IHttpRequestResponse;
  public
    constructor Create; override;
    destructor Destroy; override;
    property STOUNToken: string read fSTOUNToken write fSTOUNToken;
    property URL: string read fURL write fURL;
    property UserAgent: string read fUserAgent write fUserAgent;
    property JsonOutputOptions: TJsonOutputOptions read fJsonOutputOptions
      write fJsonOutputOptions;
    procedure Init; override;
    procedure Restart; override;
    procedure WriteLog(cLogItem: TLogItem); override;
    function FormatItem(cLogItem: TLogItem): string;
  end;

var
  GlobalSTOUNProvider: TLogSTOUNProvider;

implementation

constructor TLogSTOUNProvider.Create;
begin
  inherited;
  LogLevel := LOG_ALL;
  fURL := '';
  fSTOUNToken := '';
  fUserAgent := DEF_USER_AGENT;
  IncludedInfo := [iiAppName, iiHost];
end;

destructor TLogSTOUNProvider.Destroy;
begin
  if Assigned(fHTTPClient) then
    FreeAndNil(fHTTPClient);

  inherited;
end;

function TLogSTOUNProvider.FormatItem(cLogItem: TLogItem): string;
begin
  Result := cLogItem.Msg;
end;

procedure TLogSTOUNProvider.Init;
begin
  fHTTPClient := TJsonHTTPClient.Create;
  fHTTPClient.ContentType := 'application/json';
  fHTTPClient.UserAgent := fUserAgent;
  fHTTPClient.HandleRedirects := True;
  inherited;
end;

function TLogSTOUNProvider.PostData(Data: String): IHttpRequestResponse;
begin
  Result := fHTTPClient.Post(fURL, '{"channel-token": "' + STOUNToken +
    '","message": "' + Data + '"}');
end;

procedure TLogSTOUNProvider.Restart;
begin
  Stop;
  if Assigned(fHTTPClient) then
    FreeAndNil(fHTTPClient);
  Init;
end;

procedure TLogSTOUNProvider.WriteLog(cLogItem: TLogItem);
var
  resp: IHttpRequestResponse;
begin
  resp := PostData(FormatItem(cLogItem));

  if not(resp.StatusCode in [200, 201]) then
    raise ELogger.Create
      (Format('[TLogRestProvider] : Response %d : %s trying to post event',
      [resp.StatusCode, resp.StatusText]));
end;

initialization

GlobalSTOUNProvider := TLogSTOUNProvider.Create;

finalization

if Assigned(GlobalSTOUNProvider) and (GlobalSTOUNProvider.RefCount = 0) then
  GlobalSTOUNProvider.Free;

end.
