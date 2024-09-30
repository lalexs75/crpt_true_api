{ CRPT TrueAPI interface library for FPC and Lazarus

  Copyright (C) 2023-2024 Lagunov Aleksey alexs75@yandex.ru

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
  AbstractSerializationObjects, CRPTTrueAPI_Consts,

  //
  CRPTTrueAPIDataObjects
  ;

const
  //TrueAPI
  sTrueAPIURL3 = 'https://markirovka.crpt.ru/api/v3/true-api/';
  sTrueAPIURL4 = 'https://markirovka.crpt.ru/api/v4/true-api/';

  sTrueAPIURL3_sandbox = 'https://markirovka.sandbox.crptech.ru/api/v3/true-api/';
  sTrueAPIURL4_sandbox = 'https://markirovka.sandbox.crptech.ru/api/v4/true-api/';

type
  TCRPTApiType = (atSandbox, atProduction);

  TCustomCRPTApi = class;
  TCRPTTrueAPI = class;

  THttpMethod = (hmGET, hmPOST);
  TOnHttpStatusEnevent = procedure (Sender:TCustomCRPTApi) of object;
  TOnSignDataEvent = procedure(Sender:TCustomCRPTApi; AData:string; ADetached:Boolean; out ASign:string) of object;

  { TCustomCRPTApi }

  TCustomCRPTApi = class(TComponent)
  private
    FCRPTApiType: TCRPTApiType;
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
    FHTTP:TFPHTTPClient;
    FServer: string;
    function SendCommand(AMethod:THttpMethod; ACommand:string; AParams:string; AData:TStream; const AllowedResponseCodes : array of integer; AMimeType:string = ''; ANeedSign:Boolean = false):Boolean;
    function DoLogin:Boolean;
    procedure SaveHttpData(ACmdName: string);
    procedure InternalMakeLoginParams(var ALoginParams:string); virtual;
    procedure InternalMakeClientToken; virtual;
    procedure DoSignRequestData(const AData:TStream); virtual;
    procedure SetCRPTApiType(AValue: TCRPTApiType); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Login:Boolean;
    property Document:TMemoryStream read FDocument;
  protected
    property AuthorizationToken:string read FAuthorizationToken write FAuthorizationToken;
    property Server:string read FServer write SetServer;

    property CRPTApiType:TCRPTApiType read FCRPTApiType write SetCRPTApiType;
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
    function InternalTrueAPIUrl4:string;inline;
    function InternalTrueAPIUrl3:string;inline;
  protected
    procedure SetCRPTApiType(AValue: TCRPTApiType); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAPIToken;
    procedure LoadAuthorizationToken(AFileName:string);

  public
    function ProductsInfo(ACis:string; AchildrenPage:Integer = 0; AChildrenLimit:Integer = 0):TJSONObject;
    function CisesList(ACis:string):TJSONData;

    function CisesInfo(ACISList:TStringArray; APG:string = ''; childsWithoutBrackets:Boolean = false):TCISInfos;
    function CisesInfo(ACIS:string; APG:string = ''; childsWithoutBrackets:Boolean = false):TCISInfos;
    function CisesCodesCheck(ACISList: TStringArray; AInn: string = ''): TJSONData;
    function CisesCheck(ACISList: TStringArray): TJSONData;

    function CisesShortList(ACis:string):TJSONData;

    function CisesSearch(ACis:string):TJSONData;
    function CisesAggregatedList(ACis:string; APG:string = ''; AChildrenPage:Integer = 0; AChildrenLimit:Integer = 0; childsWithoutBrackets:Boolean = false):TJSONData;

    function DocList(APG:TCRPTProductGroup; var AFilterRecord:TDocListFilterRecord):TJSONData;
    function DocInfo(ADocId:string{APG:TCRPTProductGroup}):TJSONData;
    function ReceiptList(APG:TCRPTProductGroup):TJSONData;
    function ReceiptInfo(AReceiptId:string{APG:TCRPTProductGroup}):TJSONData;

    function BalanceAll:TJSONData;
    function Balance(AProductGroupId: Integer): TJSONData;

    function DocCreate(const PG, ADocumentFormat, ADocumentType:string;const AProductDocument, ASignature:TStream):string;

    function CodesCheck(ACodes:TXSDStringArray; AFiscalDriveNumber:string = ''): TJSONData;
//КИ
//Номер страницы вложений в агрегат первого слоя
    property AuthorizationToken;
  published
    property CRPTApiType default atProduction;
    //property Server;
    property OnHttpStatus;
    property OnSignData;
  end;


procedure AddURLParam(var S:string; AParam, AValue:string); overload;
procedure AddURLParam(var S: string; AParam:string; AValue: Integer); inline; overload;
procedure AddURLParam(var S:string; AParam:string); overload;
implementation
uses opensslsockets, sdo_date_utils, rxlogging, jsonparser, jsonscanner;

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

function TCRPTTrueAPI.InternalTrueAPIUrl4: string;
begin
  if FCRPTApiType = atSandbox then
    Result:=sTrueAPIURL4_sandbox
  else
    Result:=sTrueAPIURL4;
end;

function TCRPTTrueAPI.InternalTrueAPIUrl3: string;
begin
  if FCRPTApiType = atSandbox then
    Result:=sTrueAPIURL3_sandbox
  else
    Result:=sTrueAPIURL3;
end;

procedure TCRPTTrueAPI.SetCRPTApiType(AValue: TCRPTApiType);
begin
  inherited SetCRPTApiType(AValue);
  FServer:=InternalTrueAPIUrl4;
end;

constructor TCRPTTrueAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CRPTApiType:=atProduction;
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

function TCRPTTrueAPI.CisesList(ACis: string): TJSONData;
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
  if SendCommand(hmPOST, InternalTrueAPIUrl3 + 'cises/list', S, nil, [200, 400, 401, 402, 403, 404], 'application/json') then
  begin
    SaveHttpData('true_api_cises_list');
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end
  else
    SaveHttpData('true_api_cises_list');
end;

function TCRPTTrueAPI.CisesInfo(ACIS: string; APG:string = ''; childsWithoutBrackets:Boolean = false): TCISInfos;
begin
  Result:=CisesInfo(TStringArray.Create(ACIS), APG, childsWithoutBrackets);
end;

function TCRPTTrueAPI.CisesCodesCheck(ACISList: TStringArray; AInn: string
  ): TJSONData;
var
  P1: TJSONObject;
  P2: TJSONArray;
  S1, S: string;
  FMS: TMemoryStream;
  P: TJSONParser;
begin
  Result:=nil;
  S:='';

  P1:=TJSONObject.Create;
  if AInn <> '' then
    P1.Add('inn', AInn);
  P2:=TJSONArray.Create;
  P1.Add('codes', P2);
  for S1 in ACISList do
    P2.Add(S1);
  S1:=P1.FormatJSON;
  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_codes_check.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;

  if SendCommand(hmPOST, InternalTrueAPIUrl4 + 'codes/check', S, FMS, [200, 400, 404], 'application/json') then
  begin
    SaveHttpData('true_api_codes_check');
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end
  else
    SaveHttpData('true_api_codes_check');
  FMS.Free;
end;

function TCRPTTrueAPI.CisesCheck(ACISList: TStringArray): TJSONData;
var
  S, S1: String;
  P1: TJSONObject;
  P2: TJSONArray;
  FMS: TMemoryStream;
  P: TJSONParser;
begin
  Result:=nil;
  S:='';

  P1:=TJSONObject.Create;
  P2:=TJSONArray.Create;
  P1.Add('codes', P2);
  for S1 in ACISList do
    P2.Add(S1);
  S1:=P1.FormatJSON;
  FMS:=TMemoryStream.Create;
  FMS.Write(S1[1], Length(S1));
  {$IFDEF DebugTrueAPI}
  FMS.Position:=0;
  FMS.SaveToFile('/tmp/true_api_cises_check.json');
  {$ENDIF}
  FMS.Position:=0;

  P1.Free;

  if SendCommand(hmPOST, InternalTrueAPIUrl3 + 'cises/check', S, FMS, [200, 400, 404], 'application/json') then
  begin
    SaveHttpData('true_api_cises_check');
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end
  else
    SaveHttpData('true_api_cises_check');
  FMS.Free;
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
  if SendCommand(hmPOST, InternalTrueAPIUrl3 + 'cises/info', S, FMS, [200, 400, 404], 'application/json') then
  begin
    SaveHttpData('true_api_cises_info');
    FDocument.Position:=0;
    Result:=TCISInfos.Create;
    Result.LoadFromStream(FDocument);
  end
  else
    SaveHttpData('true_api_cises_info');
  FMS.Free;
end;

function TCRPTTrueAPI.CisesShortList(ACis: string): TJSONData;
var
  S: String;
  P1: TJSONArray;
  S1: string;
  FMS: TMemoryStream;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;

  S:='';

  P1:=TJSONArray.Create;
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

  if SendCommand(hmPOST,  InternalTrueAPIUrl3 + 'cises/short/list', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('true_api_cises_short_list');

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

  if SendCommand(hmPOST, InternalTrueAPIUrl4 + 'cises/search', S, FMS, [200, 400, 404], 'application/json') then
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
(*
  AddURLParam(S, 'codes', ACis);

  if SendCommand(hmGET, 'cises/aggregated/list', S, nil, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
*)

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

  if SendCommand(hmPOST, InternalTrueAPIUrl3 + 'cises/aggregated/list', S, FMS, [200, 400, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  FMS.Free;
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

  if SendCommand(hmGET, InternalTrueAPIUrl4 + 'receipt/list', S, nil, [200, 400, 401, 404]) then
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

  if SendCommand(hmGET, InternalTrueAPIUrl4 + 'receipt/'+AReceiptId+'/info', S, nil, [200, 400, 401, 404]) then
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

  if SendCommand(hmGET, InternalTrueAPIUrl4 + 'doc/'+ADocId+'/info', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('true_api_doc_info');
end;

function TCRPTTrueAPI.DocList(APG: TCRPTProductGroup;
  var AFilterRecord: TDocListFilterRecord): TJSONData;
var
  S: String;
  P: TJSONParser;
begin
  Result:=nil;
  DoLogin;
  S:='';
  AddURLParam(S, 'pg', CRPTProductGroupStr[APG]);
  if AFilterRecord.FromDate >0 then
    AddURLParam(S, 'dateFrom', xsd_DateTimeToStr(AFilterRecord.FromDate, xdkDateTime));
  if AFilterRecord.FromDate >0 then
    AddURLParam(S, 'dateTo', xsd_DateTimeToStr(AFilterRecord.ToDate, xdkDateTime));

  if SendCommand(hmGET, InternalTrueAPIUrl4 + 'doc/list', S, nil, [200, 400, 401, 404]) then
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

  if SendCommand(hmGET, InternalTrueAPIUrl3 + 'elk/product-groups/balance/all', '', nil, [200, 400, 401, 404]) then
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

  if SendCommand(hmGET, InternalTrueAPIUrl3 + 'elk/product-groups/balance', S, nil, [200, 400, 401, 404]) then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end;
  SaveHttpData('balance_all');
end;

function TCRPTTrueAPI.DocCreate(const PG, ADocumentFormat,
  ADocumentType: string; const AProductDocument, ASignature: TStream): string;
var
  S: String;
  M: TMemoryStream;
  V: TTrueAPICreateDocumentData;
  P: TJSONParser;
begin
  Result:='';
  DoLogin;

  S:='';
  M:=TMemoryStream.Create;
  AddURLParam(S, 'pg', PG);
  V:=TTrueAPICreateDocumentData.Create;
  V.DocumentType:=ADocumentType;
  V.DocumentFormat:=ADocumentFormat;
  V.LoadProductDocument(AProductDocument);
  V.LoadSignature(ASignature);
  V.SaveToStream(M);
  V.Free;
  M.Position:=0;

  if SendCommand(hmPOST, InternalTrueAPIUrl3 + 'lk/documents/create', S, M, [200, 201, 400, 401, 404], 'application/json') then
  begin
{    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;}
    if FDocument.Size > 0 then
    begin
      FDocument.Position:=0;
      SetLength(Result, FDocument.Size);
      FDocument.Read(Result[1], FDocument.Size);
    end;
  end;
  SaveHttpData('documents_create');
  M.Free;
end;

function TCRPTTrueAPI.CodesCheck(ACodes: TXSDStringArray;
  AFiscalDriveNumber: string): TJSONData;
var
  Rec: TTrueAPICheckCodesRequest;
  M: TMemoryStream;
  SUrl: String;
  P: TJSONParser;
begin
  Result:=nil;
  M:=TMemoryStream.Create;
  Rec:=TTrueAPICheckCodesRequest.Create;
  Rec.fiscalDriveNumber:=AFiscalDriveNumber;
  Rec.Codes:=ACodes;
  Rec.SaveToStream(M);
  Rec.Free;
  M.Position:=0;

//"X-API-KEY: cece8458-e925-45b3-84ee-ac2c23b1332d" -d "{\"codes\":[\"0104604278003464215*p-s2H/oL9kB\"]}"
  SUrl:='https://cdn10.crpt.ru/api/v4/true-api/codes/check';
  if SendCommand(hmPOST, SUrl, '', M, [200, 201, 400, 401, 404], 'application/json') then
  begin
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
    SaveHttpData('CodesCheck');
  end;
  M.Free;
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

procedure TCustomCRPTApi.SetCRPTApiType(AValue: TCRPTApiType);
begin
  if FCRPTApiType=AValue then Exit;
  FCRPTApiType:=AValue;
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

{    if (not Assigned(AData)) or (AData.Size = 0) then
      FHTTP.AddHeader('Content-Length', '0'); }

    FHTTP.RequestBody:=AData;
    RxWriteLog(etDebug, 'POST: %s', [FSrv + ACommand + AParams]);
    FHTTP.Post(FSrv + ACommand + AParams, FDocument);
  end;

  FResultCode := FHTTP.ResponseStatusCode;
  FResultString := FHTTP.ResponseStatusText;
  Result:=(FResultCode = 200) or (FResultCode = 201) or (FResultCode = 202);

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

  if FCRPTApiType = atSandbox then
//  if (FServer = sAPISuzURL_sandbox1) or (FServer = sAPISuzURL_sandbox2) or (FServer = sTrueAPIURL3_sandbox) or (FServer = sTrueAPIURL4_sandbox) then
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
