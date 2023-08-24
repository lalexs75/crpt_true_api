{ CRPT TrueAPI interface library for FPC and Lazarus

  Copyright (C) 2023 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


unit CRPTTrueAPI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets, sslsockets, fpJSON, CRPTTrueAPI_Consts;

const
  //TrueAPI
  sAPIURL_sandbox = 'https://markirovka.sandbox.crptech.ru/api/v3/true-api/';
  sAPIURL = 'https://markirovka.crpt.ru/api/v3/true-api/';

  //SUZ
  sAPISuzURL = 'https://suzgrid.crpt.ru/';
  sAPISuzURL_sandbox1 = 'https://suz.sandbox.crptech.ru/';
  sAPISuzURL_sandbox2 = 'https://suz-integrator.sandbox.crptech.ru/';


type
  TCustomCRPTApi = class;
  TCRPTTrueAPI = class;

  THttpMethod = (hmGET, hmPOST);
  TOnHttpStatusEnevent = procedure (Sender:TCustomCRPTApi) of object;
  TOnSignDataEvent = procedure(Sender:TCustomCRPTApi; AData:string; ADetached:Boolean; out ASign:string) of object;

  { TCustomCRPTApi }

  TCustomCRPTApi = class(TComponent)
  private
    FHTTP:TFPHTTPClient;
    FServer: string;
    FAuthorizationToken:string;
    FAuthorizationTokenTimeStamp:TDateTime;
    FOnHttpStatus: TOnHttpStatusEnevent;
    FOnSignData: TOnSignDataEvent;
    FDocument:TMemoryStream;

    FResultCode: integer;
    FResultString: string;
    FResultText: TStrings;
    procedure DoHaveSocketHandler(Sender: TObject; AHandler: TSocketHandler);
    procedure DoVerifyCertificate(Sender: TObject; AHandler: TSSLSocketHandler; var aAllow: Boolean);
    function DoAnsverLogin(ALoginServer, FUID, FDATA:string):Boolean;
    procedure SetServer(AValue: string);
  protected
    function SendCommand(AMethod:THttpMethod; ACommand:string; AParams:string; AData:TStream; const AllowedResponseCodes : array of integer; AMimeType:string = ''; ANeedSign:Boolean = false):Boolean;
    function DoLogin:Boolean;
    procedure SaveHttpData(ACmdName: string);
    procedure InternalMakeLoginParams(var ALoginParams:string); virtual;
    procedure InternalMakeClientToken; virtual;
    procedure DoSignRequestData(const AData:TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Login:Boolean;
  protected
    property AuthorizationToken:string read FAuthorizationToken write FAuthorizationToken;
    property Server:string read FServer write SetServer;

    property OnHttpStatus:TOnHttpStatusEnevent read FOnHttpStatus write FOnHttpStatus;
    property OnSignData:TOnSignDataEvent read FOnSignData write FOnSignData;
  public
    property ResultText:TStrings read FResultText;
    property ResultCode : integer read FResultCode;
    property ResultString : string read FResultString;
  end;

  { TCRPTTrueAPI }

  TCRPTTrueAPI = class(TCustomCRPTApi)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAPIToken;
    procedure LoadAuthorizationToken(AFileName:string);

  public
    function ProductsInfo(ACis:string; AchildrenPage:Integer = 0; AChildrenLimit:Integer = 0):TJSONObject;
//    function
//КИ
//Номер страницы вложений в агрегат первого слоя
    property AuthorizationToken;
  published
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

  { TCRPTSuzAPI }

  TCRPTSuzAPI = class(TCustomCRPTApi)
  private
    FOmsConnection: string;
    FOmsID: string;
    procedure SetOmsConnection(AValue: string);
    procedure SetOmsID(AValue: string);
  protected
    procedure InternalMakeLoginParams(var ALoginParams:string); override;
    procedure InternalMakeClientToken; override;
  public
    function Ping(AProductGroup:TCRPTProductGroup):TJSONObject;
    function Order(AProductGroup:TCRPTProductGroup; AOrder:string {TJSONObject}):TJSONObject;
    function Providers:TJSONObject;
  public
    property AuthorizationToken;
  published
    property OmsConnection:string read FOmsConnection write SetOmsConnection;
    property OmsID:string read FOmsID write SetOmsID;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

procedure Register;

procedure AddURLParam(var S:string; AParam, AValue:string); overload;
procedure AddURLParam(var S:string; AParam:string); overload;
implementation
uses opensslsockets, rxlogging, jsonparser;

{$R crpt_true_api.res}

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TCRPTTrueAPI, TCRPTSuzAPI]);
end;

function HTTPEncode(const AStr: String): String;
const
  HTTPAllowed = ['A'..'Z','a'..'z', '*','@','.','_','-', '0'..'9', '$','!','''','(',')'];
var
  SS,S,R: PChar;
  H : String[2];
  L : Integer;
begin
  L:=Length(AStr);
  SetLength(Result,L*3); // Worst case scenario
  if (L=0) then
    exit;
  R:=PChar(Result);
  S:=PChar(AStr);
  SS:=S; // Avoid #0 limit !!
  while ((S-SS)<L) do
  begin
    if S^ in HTTPAllowed then
      R^:=S^
    else
    if (S^=' ') then
      R^:='+'
    else
    begin
      R^:='%';
      H:=HexStr(Ord(S^),2);
      Inc(R);
      R^:=H[1];
      Inc(R);
      R^:=H[2];
    end;
    Inc(R);
    Inc(S);
  end;
  SetLength(Result,R-PChar(Result));
end;

procedure AddURLParam(var S: string; AParam, AValue: string);
begin
  if S<>'' then S:=S + '&';
  if AValue <>'' then
  begin
    //AValue:=StringReplace(AValue, '#', '%23', [rfReplaceAll]);
    AValue:=StringReplace(AValue, '%', '%25', [rfReplaceAll]);
    S:=S + AParam + '=' + HTTPEncode(AValue)
  end
  else
    S:=S + AParam
end;

procedure AddURLParam(var S: string; AParam: string);
begin
  AddURLParam(S, AParam, '');
end;


{ TCRPTTrueAPI }

constructor TCRPTTrueAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FServer:=sAPIURL;
end;

destructor TCRPTTrueAPI.Destroy;
begin
  inherited Destroy;
end;

procedure TCRPTTrueAPI.ClearAPIToken;
begin
  FAuthorizationToken:='';
  FAuthorizationTokenTimeStamp:=0;
end;

procedure TCRPTTrueAPI.LoadAuthorizationToken(AFileName: string);
begin

end;

function TCRPTTrueAPI.ProductsInfo(ACis: string; AchildrenPage: Integer;
  AChildrenLimit: Integer): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'cis', ACis);

  if AchildrenPage>0 then
   AddURLParam(S, 'childrenPage', IntToStr(AchildrenPage));

  if AchildrenLimit>0 then
    AddURLParam(S, 'childrenLimit', IntToStr(AChildrenLimit));

  if SendCommand(hmGET, 'products/info', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('products_info');
end;

{ TCRPTSuzAPI }

procedure TCRPTSuzAPI.SetOmsConnection(AValue: string);
begin
  if FOMSConnection=AValue then Exit;
  FOMSConnection:=AValue;
end;

procedure TCRPTSuzAPI.SetOmsID(AValue: string);
begin
  if FOmsID=AValue then Exit;
  FOmsID:=AValue;
end;

procedure TCRPTSuzAPI.InternalMakeLoginParams(var ALoginParams: string);
begin
  ALoginParams:=FOMSConnection;
end;

procedure TCRPTSuzAPI.InternalMakeClientToken;
begin
  FHTTP.AddHeader('clientToken', FAuthorizationToken);
end;

function TCRPTSuzAPI.Ping(AProductGroup: TCRPTProductGroup): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);

  if SendCommand(hmGET, 'api/v3/ping', S, nil, [200, 400, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_ping');
end;

function TCRPTSuzAPI.Order(AProductGroup: TCRPTProductGroup; AOrder: string {TJSONObject}
  ): TJSONObject;
var
  FMS: TMemoryStream;
  S: String;
  S1: TJSONStringType;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  FMS:=TMemoryStream.Create;
  S1:=AOrder{.FormatJSON};
  FMS.Write(S1[1], Length(S1));
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/Order_json.json');
  FMS.Position:=0;
  try
    if SendCommand(hmPOST, 'api/v3/order', S, FMS, [200, 400, 404], 'application/json', true) then
    begin
      FDocument.Position:=0;
      P:=TJSONParser.Create(FDocument);
      Result:=P.Parse as TJSONObject;
      P.Free;
    end;
  except
  end;
  SaveHttpData('oms_api_v3_order');
  FMS.Free;
end;

function TCRPTSuzAPI.Providers: TJSONObject;
var
  P: TJSONParser;
  S: String;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  if SendCommand(hmGET, 'api/v3/providers', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_providers');
end;

{ TCustomCRPTApi }

procedure TCustomCRPTApi.DoHaveSocketHandler(Sender: TObject;
  AHandler: TSocketHandler);
var
  SSLHandler :  TSSLSocketHandler absolute aHandler;
begin
  if (aHandler is TSSLSocketHandler) then
  begin
    SSLHandler.CertificateData.TrustedCertsDir:='/etc/ssl/certs/';
  end
end;

procedure TCustomCRPTApi.DoVerifyCertificate(Sender: TObject;
  AHandler: TSSLSocketHandler; var aAllow: Boolean);
begin
  {  RxWriteLog(etDebug, 'SSL Certificate verification requested, allowing');
    S:=TEncoding.ASCII.GetAnsiString( aHandler.CertificateData.Certificate.Value);
    RxWriteLog(etDebug, 'Cert: %s',[S]);}
  aAllow:=True;
end;

function TCustomCRPTApi.DoAnsverLogin(ALoginServer, FUID, FDATA: string
  ): Boolean;
var
  S1, S: string;
  J, R: TJSONObject;
  M: TStringStream;
  P: TJSONParser;
begin
  if not Assigned(FOnSignData) then Exit;

  S:='';
  InternalMakeLoginParams(S);
  if S<>'' then S:='/' + S;

  FOnSignData(Self, FDATA, false, S1);

  J:=TJSONObject.Create;
  J.Add('uuid', FUID);
  J.Add('data', S1);
  M:=TStringStream.Create(J.FormatJSON);
  M.Position:=0;
  J.Free;


  if SendCommand(hmPOST, ALoginServer + 'auth/simpleSignIn' + S, '', M, [200], 'application/json') then
  begin
    SaveHttpData('auth_simpleSignIn');
    FDocument.Position:=0;
    try
      P:=TJSONParser.Create(FDocument);
      R:=P.Parse as TJSONObject;
      if FResultCode = 200 then
      begin
        FAuthorizationToken:=JSONStringToString( R.GetPath('token').AsString );
        FAuthorizationTokenTimeStamp:=Now;
      end;
    finally
      P.Free;
      R.Free;
    end;
    FDocument.Position:=0;
  end;
  M.Free;
end;

procedure TCustomCRPTApi.SetServer(AValue: string);
begin
  if FServer=AValue then Exit;
  if AValue <> '' then
  begin
    if AValue[Length(AValue)]<>'/' then
      AValue:=AValue + '/';
  end;
  FServer:=AValue;
end;

function TCustomCRPTApi.SendCommand(AMethod: THttpMethod; ACommand: string;
  AParams: string; AData: TStream;
  const AllowedResponseCodes: array of integer; AMimeType: string;
  ANeedSign: Boolean): Boolean;
var
  FSrv: String;
begin
  Clear;
  if AParams <> '' then
    AParams:='?' + AParams;

  FHTTP.KeepConnection:=true;

  if FAuthorizationToken <> '' then
  begin
    InternalMakeClientToken;
    FHTTP.AddHeader('accept', '*/*');
  end;

  if Copy(ACommand, 1, 8) <> 'https://' then
    FSrv:=FServer
  else
    FSrv:='';

  if AMethod = hmGET then
  begin
    FHTTP.HTTPMethod('GET',FSrv + ACommand + AParams,FDocument, AllowedResponseCodes);
  end
  else
  begin
   if AMimeType<>'' then
      FHTTP.AddHeader('Content-Type',AMimeType)
    else
      FHTTP.AddHeader('Content-Type','application/x-www-form-urlencoded');

    if ANeedSign and Assigned(FOnSignData) then
      DoSignRequestData(AData);

    if (not Assigned(AData)) or (AData.Size = 0) then
      FHTTP.AddHeader('Content-Length', '0');

    FHTTP.RequestBody:=AData;
    FHTTP.Post(FSrv + ACommand + AParams, FDocument);
  end;

  FResultCode := FHTTP.ResponseStatusCode;
  FResultString := FHTTP.ResponseStatusText;
  Result:=FResultCode = 200;

  if Assigned(FOnHttpStatus) then
    FOnHttpStatus(Self);
end;

function TCustomCRPTApi.DoLogin: Boolean;
var
  B: TJSONParser;
  J: TJSONData;
  FDATA, FUID: TJSONStringType;
  R: Int64;
  FLoginServer: String;
begin
  if (FAuthorizationToken <> '') and (FAuthorizationTokenTimeStamp > (Now - (1 / 20) * 10)) then Exit;
  FAuthorizationToken:='';
  Result:=false;
  if (FServer = sAPISuzURL_sandbox1) or (FServer = sAPISuzURL_sandbox2) or (FServer = sAPIURL_sandbox) then
    FLoginServer:=sAPIURL_sandbox
  else
    FLoginServer:=sAPIURL;

  if SendCommand(hmGET, FLoginServer + 'auth/key', '', nil, [200]) then
  begin
    SaveHttpData('auth_key');
    if FResultCode = 200 then
    begin
      R:=FDocument.Size;
      FDocument.Position:=0;
      B:=TJSONParser.Create(FDocument);
      J:=B.Parse;
      FUID:=J.GetPath('uuid').AsString;
      FDATA:=J.GetPath('data').AsString;
      J.Free;
      B.Free;
      if FDATA<> '' then
        Result:=DoAnsverLogin(FLoginServer, FUID, FDATA);
    end;
  end;
end;

procedure TCustomCRPTApi.SaveHttpData(ACmdName: string);
var
  F: TFileStream;
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  F:=TFileStream.Create(GetTempDir(false) + PathDelim + ACmdName, fmCreate);
  P:=FDocument.Position;
  FDocument.Position:=0;
  FDocument.SaveToStream(F);
  F.Free;
  FDocument.Position:=P;
end;

procedure TCustomCRPTApi.InternalMakeLoginParams(var ALoginParams: string);
begin

end;

procedure TCustomCRPTApi.InternalMakeClientToken;
begin
  FHTTP.AddHeader('Authorization', 'Bearer ' + FAuthorizationToken);
end;

procedure TCustomCRPTApi.DoSignRequestData(const AData: TStream);
var
  S, FSig:string;
begin
  if (not Assigned(FOnSignData)) or (AData.Size = 0) then Exit;
  SetLength(S, AData.Size);
  FOnSignData(Self, S, true, FSig);

  FHTTP.AddHeader('X-Signature',FSig)
end;

constructor TCustomCRPTApi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument:=TMemoryStream.Create;
  FResultText:=TStringList.Create;
  FHTTP:=TFPHTTPClient.Create(nil);
  FHTTP.HTTPversion:='1.1';
  FHTTP.OnVerifySSLCertificate:=@DoVerifyCertificate;
  FHTTP.AfterSocketHandlerCreate:=@DoHaveSocketHandler;
  FHTTP.VerifySSlCertificate:=false;
end;

destructor TCustomCRPTApi.Destroy;
begin
  FreeAndNil(FResultText);
  FreeAndNil(FDocument);
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TCustomCRPTApi.Clear;
begin
  FHTTP.Cookies.Clear;
  FHTTP.RequestHeaders.Clear;
  FHTTP.RequestBody:=nil;
  FResultText.Clear;
  FDocument.Clear;
end;

function TCustomCRPTApi.Login: Boolean;
begin
  Result:=DoLogin;
end;

end.
