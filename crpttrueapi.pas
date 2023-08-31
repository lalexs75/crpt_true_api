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

{$IFDEF LINUX}
{$DEFINE DebugTrueAPI}
{$ENDIF}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets, sslsockets, fpJSON,
  CRPTTrueAPI_Consts,

  //
  trueapi_cises_info
  ;

const
  //TrueAPI
  sTrueAPIURL3 = 'https://markirovka.crpt.ru/api/v3/true-api/';
  sTrueAPIURL4 = 'https://markirovka.crpt.ru/api/v4/true-api/';

  sTrueAPIURL3_sandbox = 'https://markirovka.sandbox.crptech.ru/api/v3/true-api/';
  sTrueAPIURL4_sandbox = 'https://markirovka.sandbox.crptech.ru/api/v4/true-api/';

  //SUZ
  sAPISuzURL = 'https://suzgrid.crpt.ru/';
  sAPISuzURL_sandbox1 = 'https://suz.sandbox.crptech.ru/';
  sAPISuzURL_sandbox2 = 'https://suz-integrator.sandbox.crptech.ru/';

  //Интеграция
  sAPISuzIntegrator = 'https://suzgrid.crpt.ru:16443/';
  sAPISuzIntegrator_sandbox = 'https://suz-integrator.sandbox.crptech.ru/';


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
    FErrorText: TStrings;
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
    property Document:TMemoryStream read FDocument;
  protected
    property AuthorizationToken:string read FAuthorizationToken write FAuthorizationToken;
    property Server:string read FServer write SetServer;

    property OnHttpStatus:TOnHttpStatusEnevent read FOnHttpStatus write FOnHttpStatus;
    property OnSignData:TOnSignDataEvent read FOnSignData write FOnSignData;
  public
    property ResultText:TStrings read FResultText;
    property ErrorText: TStrings read FErrorText;
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
    function CisesList(ACis:string):TJSONObject;

    function CisesInfo(ACISList:TStringArray; APG:string = ''; childsWithoutBrackets:Boolean = false):TCISInfos;
    function CisesInfo(ACIS:string; APG:string = ''; childsWithoutBrackets:Boolean = false):TCISInfos;

    function CisesShortList(ACis:string):TJSONData;
    function CisesSearch(ACis:string):TJSONData;
    function CisesAggregatedList(ACis:string; APG:string = ''; AChildrenPage:Integer = 0; AChildrenLimit:Integer = 0; childsWithoutBrackets:Boolean = false):TJSONData;

    function DocList(APG:TCRPTProductGroup):TJSONData;
    function DocInfo(ADocId:string{APG:TCRPTProductGroup}):TJSONData;
    function ReceiptList(APG:TCRPTProductGroup):TJSONData;
    function ReceiptInfo(AReceiptId:string{APG:TCRPTProductGroup}):TJSONData;

    function BalanceAll:TJSONData;
    function Balance(AProductGroupId: Integer): TJSONData;
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
    function OrderStatus(AOrderID, AGTIN: string): TJSONData;
    function OrdersList: TJSONData;
    function OrderCodes(AOrderID, AGTIN: string; AQuantity: Integer): TJSONData;
    function OrderBlocks(AOrderID, AGTIN: string): TJSONData;
    function OrderCodesRetry(ABlockID: string): TJSONData;
    function OrderClose(AOrderID, AGTIN: string): TJSONData;
    function OrderProduct(AOrderID: string): TJSONData;
    function Receipt(AResultDocId: string): TJSONData;
    function ReceiptSearch(AFilterStr: TJSONData; ALimit, ASkip: Integer
      ): TJSONData;
    function ReceiptDocument(AResultDocId, ADocId:string): TJSONData;
    function DocumentsSearch(ADdocumentType:string; ALimit, ASkip: Integer): TJSONData;
    function Providers:TJSONObject;
    function QualityInfo(AOrderId:string; ALimit, ASkip: Integer):TJSONObject;
    function QualityCisList(AReportId:string):TJSONObject;
  public
    property AuthorizationToken;
  published
    property OmsConnection:string read FOmsConnection write SetOmsConnection;
    property OmsID:string read FOmsID write SetOmsID;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

  { TCRPTSuzIntegrationAPI }

  TCRPTSuzIntegrationAPI = class(TCustomCRPTApi)
  private
    FOmsID: string;
    FRegistrationKey: string;
    procedure SetOmsID(AValue: string);
    procedure SetRegistrationKey(AValue: string);
  protected
    procedure InternalMakeClientToken; override;
  public
    property AuthorizationToken;
    function IntegrationRegister(AName, AAdress:string):TJSONObject;
  published
    property OmsID:string read FOmsID write SetOmsID;
    property RegistrationKey:string read FRegistrationKey write SetRegistrationKey;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

procedure Register;

procedure AddURLParam(var S:string; AParam, AValue:string); overload;
procedure AddURLParam(var S: string; AParam:string; AValue: Integer); inline; overload;
procedure AddURLParam(var S:string; AParam:string); overload;
implementation
uses opensslsockets, rxlogging, jsonparser, jsonscanner;

{$R crpt_true_api.res}

procedure Register;
begin
  RegisterComponents('ЦРПТ',[TCRPTTrueAPI, TCRPTSuzAPI, TCRPTSuzIntegrationAPI]);
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

procedure AddURLParam(var S: string; AParam:string; AValue: Integer); inline;
begin
  AddURLParam(S, AParam, IntToStr(AValue));
end;

procedure AddURLParam(var S: string; AParam: string); inline;
begin
  AddURLParam(S, AParam, '');
end;


{ TCRPTTrueAPI }

constructor TCRPTTrueAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FServer:=sTrueAPIURL3;
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

  if SendCommand(hmGET, 'products/info', S, nil, [200, 400, 401, 402, 403, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('products_info');
end;

function TCRPTTrueAPI.CisesList(ACis: string): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'values', ACis);
{
  if AchildrenPage>0 then
   AddURLParam(S, 'childrenPage', IntToStr(AchildrenPage));

  if AchildrenLimit>0 then
    AddURLParam(S, 'childrenLimit', IntToStr(AChildrenLimit));
}
  if SendCommand(hmPOST, 'cises/list', S, nil, [200, 400, 401, 402, 403, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('true_api_cises_list');
end;

function TCRPTTrueAPI.CisesInfo(ACIS: string; APG:string = ''; childsWithoutBrackets:Boolean = false): TCISInfos;
begin
  Result:=CisesInfo(TStringArray.Create(ACIS), APG, childsWithoutBrackets);
end;

function TCRPTTrueAPI.CisesInfo(ACISList: TStringArray; APG:string = ''; childsWithoutBrackets:Boolean = false): TCISInfos;
var
  P1: TJSONArray;
  S1: TJSONStringType;
  FMS: TMemoryStream;
  S, S2: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;

  S:='';
  if APG <> '' then
    AddURLParam(S, 'pg', APG);

  P1:=TJSONArray.Create;
  for S2 in ACISList do
    P1.Add(S2);

  S1:=P1.FormatJSON;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_cises_info.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;
  if SendCommand(hmPOST, 'cises/info', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    Result:=TCISInfos.Create;
    Result.LoadFromStream(FDocument);
{    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;}
  end;
  FMS.Free;
  SaveHttpData('true_api_cises_info');
end;

function TCRPTTrueAPI.CisesShortList(ACis: string): TJSONData;
begin
  Result:=nil;
  DoLogin;
{
  S:='';

  P1:=TJSONArray.Create;
  //P1.Add('filter', AFilterStr);
  P1.Add(ACis);

  S1:=P1.FormatJSON;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_cises_info.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;
  if SendCommand(hmPOST, 'cises/short/list', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('true_api_cises_short_list');
}
end;

function TCRPTTrueAPI.CisesSearch(ACis: string): TJSONData;
var
  P2, P1: TJSONObject;
  S: String;
  S1: TJSONStringType;
  FMS: TMemoryStream;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';

  P1:=TJSONObject.Create;
  P2:=TJSONObject.Create;
  P2.Add('gtins', TJSONArray.Create([ACis]));
  P2.Add('productGroups', TJSONArray.Create(['tires', 'milk', 'water']));

  P1.Add('filter', P2);

  S1:=P1.FormatJSON;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_cises_search.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;

  if SendCommand(hmPOST, 'cises/search', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('true_api_cises_search');
end;

function TCRPTTrueAPI.CisesAggregatedList(ACis: string; APG: string;
  AChildrenPage: Integer; AChildrenLimit: Integer;
  childsWithoutBrackets: Boolean): TJSONData;
var
  P1: TJSONArray;
  S1: TJSONStringType;
  FMS: TMemoryStream;
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'codes', ACis);

  if SendCommand(hmGET, 'cises/aggregated/list', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;

{
P1:=TJSONArray.Create([ACis]);

  S1:=P1.FormatJSON;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_cises_aggregated_list.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;

  if SendCommand(hmGET, 'cises/aggregated/list', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  FMS.Free; }
  SaveHttpData('true_api_cises_aggregated_list');
end;

function TCRPTTrueAPI.ReceiptList(APG: TCRPTProductGroup): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'pg', CRPTProductGroupStr[APG]);

  if SendCommand(hmGET, 'receipt/list', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('true_api_receipt_list');
end;

function TCRPTTrueAPI.ReceiptInfo(AReceiptId: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  //AddURLParam(S, 'pg', CRPTProductGroupStr[APG]);

  if SendCommand(hmGET, 'receipt/'+AReceiptId+'/info', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('true_api_doc_info');
end;

function TCRPTTrueAPI.DocInfo(ADocId: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  //AddURLParam(S, 'pg', CRPTProductGroupStr[APG]);

  if SendCommand(hmGET, 'doc/'+ADocId+'/info', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('true_api_doc_info');
end;

function TCRPTTrueAPI.DocList(APG: TCRPTProductGroup): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'pg', CRPTProductGroupStr[APG]);

  if SendCommand(hmGET, 'doc/list', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('true_api_doc_list');
end;

function TCRPTTrueAPI.BalanceAll: TJSONData;
var
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;

  if SendCommand(hmGET, 'elk/product-groups/balance/all', '', nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('balance_all');
end;

function TCRPTTrueAPI.Balance(AProductGroupId: Integer): TJSONData;
var
  P: TJSONParser;
  S: String;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'productGroupId', AProductGroupId);

  if SendCommand(hmGET, 'elk/product-groups/balance', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('balance_all');
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
    P:=TJSONParser.Create(FDocument, DefaultOptions);
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
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/Order_json.json');
  {$ENDIF}
  FMS.Position:=0;
  try
    if SendCommand(hmPOST, 'api/v3/order', S, FMS, [200, 400, 404], 'application/json', true) then
    begin
      FDocument.Position:=0;
      P:=TJSONParser.Create(FDocument, DefaultOptions);
      Result:=P.Parse as TJSONObject;
      P.Free;
    end;
  except
  end;
  SaveHttpData('oms_api_v3_order');
  FMS.Free;
end;

function TCRPTSuzAPI.OrderStatus(AOrderID, AGTIN:string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'orderId', AOrderID);
  if AGTIN<>'' then
    AddURLParam(S, 'gtin', AGTIN);

  if SendCommand(hmGET, 'api/v3/order/status', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_order_status');
end;

function TCRPTSuzAPI.OrdersList: TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  if SendCommand(hmGET, 'api/v3/order/list', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_order_list');
end;

function TCRPTSuzAPI.OrderCodes(AOrderID, AGTIN: string; AQuantity:Integer): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'orderId', AOrderID);
  AddURLParam(S, 'gtin', AGTIN);
  AddURLParam(S, 'quantity', AQuantity);
  if SendCommand(hmGET, 'api/v3/codes', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_codes');
end;

function TCRPTSuzAPI.OrderBlocks(AOrderID, AGTIN: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'orderId', AOrderID);
  AddURLParam(S, 'gtin', AGTIN);
  if SendCommand(hmGET, 'api/v3/order/codes/blocks', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_order_codes_blocks');
end;

function TCRPTSuzAPI.OrderCodesRetry(ABlockID: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'blockId', ABlockID);
  if SendCommand(hmGET, 'api/v3/order/codes/retry', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_order_codes_retry');
end;

function TCRPTSuzAPI.OrderClose(AOrderID, AGTIN: string): TJSONData;
var
  JP: TJSONObject;
  ST: TMemoryStream;
  S: String;
  S1: TJSONStringType;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);

  JP:=TJSONObject.Create;
  JP.Add('orderId', AOrderID);
  if AGTIN <> '' then
  JP.Add('gtin', AGTIN);
  S1:=JP.FormatJSON;
  JP.Free;

  ST:=TMemoryStream.Create;
  ST.Write(S1[1], Length(S1));

  if SendCommand(hmPOST, 'api/v3/order/close', S, ST, [200, 400, 404], 'application/json', true) then
  begin
    SaveHttpData('oms_api_v3_order_close');
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  ST.Free;
end;

function TCRPTSuzAPI.OrderProduct(AOrderID: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'orderId', AOrderID);
  if SendCommand(hmGET, 'api/v3/order/product', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_order_product');
end;

function TCRPTSuzAPI.Receipt(AResultDocId: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'resultDocId', AResultDocId);
  if SendCommand(hmGET, 'api/v3/receipts/receipt', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_receipts_receipt');
end;

function TCRPTSuzAPI.ReceiptSearch(AFilterStr: TJSONData; ALimit, ASkip: Integer
  ): TJSONData;
var
  S: String;
  P1: TJSONObject;
  S1: TJSONStringType;
  FMS: TMemoryStream;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  P1:=TJSONObject.Create;
  P1.Add('filter', AFilterStr);

  if ALimit>0 then
    P1.Add('limit', ALimit);

  if ASkip>0 then
    P1.Add('skip', ASkip);

  S1:=P1.FormatJSON;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/oms_api_v3_receipts_receipt_search_filter.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;
  if SendCommand(hmPOST, 'api/v3/receipts/receipt/search', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('oms_api_v3_receipts_receipt_search');
end;

function TCRPTSuzAPI.ReceiptDocument(AResultDocId, ADocId: string): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'resultDocId', AResultDocId);
  AddURLParam(S, 'docId', ADocId);
  if SendCommand(hmGET, 'api/v3/receipts/document', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_receipts_document');
end;

function TCRPTSuzAPI.DocumentsSearch(ADdocumentType: string; ALimit,
  ASkip: Integer): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'documentType', ADdocumentType);
  if ALimit>0 then
    AddURLParam(S, 'limit', ALimit);

  if ASkip>0 then
    AddURLParam(S, 'skip', ASkip);
  if SendCommand(hmGET, 'api/v3/documents/search', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_documents_search');
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
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_providers');
end;

function TCRPTSuzAPI.QualityInfo(AOrderId: string; ALimit, ASkip: Integer
  ): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);

  if AOrderId<>'' then
    AddURLParam(S, 'orderId', AOrderId);

  if ALimit>0 then
    AddURLParam(S, 'limit', ALimit);

  if ASkip>0 then
    AddURLParam(S, 'skip', ASkip);
  if SendCommand(hmGET, 'api/v3/quality', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_quality');
end;

function TCRPTSuzAPI.QualityCisList(AReportId: string): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);
  AddURLParam(S, 'reportId', AReportId);
  if SendCommand(hmGET, 'api/v3/quality/cisList', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_quality_cisList');
end;

{ TCRPTSuzIntegrationAPI }

procedure TCRPTSuzIntegrationAPI.SetOmsID(AValue: string);
begin
  if FOmsID=AValue then Exit;
  FOmsID:=AValue;
end;

procedure TCRPTSuzIntegrationAPI.SetRegistrationKey(AValue: string);
begin
  if FRegistrationKey=AValue then Exit;
  FRegistrationKey:=AValue;
end;

procedure TCRPTSuzIntegrationAPI.InternalMakeClientToken;
begin
  if RegistrationKey<>'' then
  begin
    FHTTP.AddHeader('X-RegistrationKey', RegistrationKey);
    RxWriteLog(etDebug, 'X-RegistrationKey: %s', [RegistrationKey]);
  end;
end;

function TCRPTSuzIntegrationAPI.IntegrationRegister(AName, AAdress: string
  ): TJSONObject;
var
  P1: TJSONObject;
  FMS: TMemoryStream;
  S, S1: String;
  P: TJSONParser;
begin
  Result:=nil;
  S:='';
  AddURLParam(S, 'omsId', FOmsID);


  P1:=TJSONObject.Create;
  P1.Add('address', AAdress);
  P1.Add('name', AName);
  S1:=P1.FormatJSON;
  P1.Free;

  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/oms_api_v3_integration_connection.json');
  {$ENDIF}
  FMS.Position:=0;

  if SendCommand(hmPOST, 'api/v3/integration/connection', S, FMS, [200, 400, 404], 'application/json', true) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('oms_api_v3_integration_connection');
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
  Result:=false;
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
      P:=TJSONParser.Create(FDocument, DefaultOptions);
      R:=P.Parse as TJSONObject;
      if FResultCode = 200 then
      begin
        FAuthorizationToken:=JSONStringToString( R.GetPath('token').AsString );
        FAuthorizationTokenTimeStamp:=Now;
      end;
      Result:=true;
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
  P: Int64;
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
    RxWriteLog(etDebug, 'GET: %s', [FSrv + ACommand + AParams]);
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
    RxWriteLog(etDebug, 'POST: %s', [FSrv + ACommand + AParams]);
    FHTTP.Post(FSrv + ACommand + AParams, FDocument);
  end;

  FResultCode := FHTTP.ResponseStatusCode;
  FResultString := FHTTP.ResponseStatusText;
  Result:=FResultCode = 200;

  if (not Result) and (FDocument.Size > 0) then
  begin
    P:=FDocument.Position;
    FDocument.Position:=0;
    FErrorText.LoadFromStream(FDocument);
    FDocument.Position:=P;
  end;

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
  if (FServer = sAPISuzURL_sandbox1) or (FServer = sAPISuzURL_sandbox2) or (FServer = sTrueAPIURL3_sandbox) or (FServer = sTrueAPIURL4_sandbox) then
    FLoginServer:=sTrueAPIURL3_sandbox
  else
    FLoginServer:=sTrueAPIURL3;

  if SendCommand(hmGET, FLoginServer + 'auth/key', '', nil, [200]) then
  begin
    SaveHttpData('auth_key');
    if FResultCode = 200 then
    begin
      R:=FDocument.Size;
      FDocument.Position:=0;
      B:=TJSONParser.Create(FDocument, DefaultOptions);
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
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  P:=FDocument.Position;
  FDocument.Position:=0;
  FDocument.SaveToFile(GetTempDir(false) + PathDelim + ACmdName);
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
  P: Int64;
begin
  if (not Assigned(FOnSignData)) or (AData.Size = 0) then Exit;
  P:=AData.Position;
  AData.Position:=0;
  SetLength(S, AData.Size);
  AData.Read(S[1], AData.Size);
  AData.Position:=P;
  FOnSignData(Self, S, true, FSig);

  FHTTP.AddHeader('X-Signature',FSig);
  RxWriteLog(etDebug, 'X-Signature: %s', [FSig]);
end;

constructor TCustomCRPTApi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument:=TMemoryStream.Create;
  FResultText:=TStringList.Create;
  FErrorText:=TStringList.Create;
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
  FreeAndNil(FErrorText);
  inherited Destroy;
end;

procedure TCustomCRPTApi.Clear;
begin
  FHTTP.Cookies.Clear;
  FHTTP.RequestHeaders.Clear;
  FHTTP.RequestBody:=nil;
  FResultText.Clear;
  FDocument.Clear;
  FErrorText.Clear;
end;

function TCustomCRPTApi.Login: Boolean;
begin
  Result:=DoLogin;
end;

end.
