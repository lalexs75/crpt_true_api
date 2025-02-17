unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpsend, {fphttpclient,} fpJSON;

const
  sAPIURL = 'https://suzgrid.crpt.ru/api/v2/';
//            'https://suzgrid.crpt.ru'
type
  TSuzAPIExtension =
  (
     light, // параметр URL extension для лёгкой промышленности. Использовался ранее для двух товарных групп: «Предметы одежды, белье постельное, столовое,
              //туалетное и кухонное» и «Обувные товары». Начинается переход с данного общего
              //расширения на два раздельных: lp и shoes. С 01.06.2021 г. расширение light
              //перестанет поддерживаться.
     lp,    //параметр URL extension для лёгкой промышленности, категория товарной
              //группы «Предметы одежды, белье постельное, столовое, туалетное и кухонное»;
     shoes,  //параметр URL extension для лёгкой промышленности, категория товарной
               //группы «Обувные товары»;
     pharma,   //параметр URL extension для фармацевтической промышленности.
     tobacco,  //параметр URL extension для табачной промышленности.
     tires,    //параметр URL extension для производителей шин.
     photo,    //параметр URL extension для производителей фототоваров.
     perfum,   //параметр URL extension для производителей парфюмерной продукции.
     milk,     //параметр URL extension для производителей молока;
     bicycle,  //параметр URL extension для производителей велосипедов и велосипедных рам;
     wheelchairs, //параметр URL extension для производителей кресел-колясок;
     otp,         //параметр URL extension для производителей альтернативной табачной продукции;
     water        //параметр URL extension для производителей упакованной воды.
  );

  TCRPTSuzAPI = class;

  THttpMethod = (hmGET, hmPOST);
  TOnHttpStatusEnevent = procedure (Sender:TCRPTSuzAPI) of object;
  TOnSignDataEvent = procedure(Sender:TCRPTSuzAPI; AData:string; out ASign:string) of object;

  { TCRPTSuzAPI }

  TCRPTSuzAPI = class(TComponent)
  private
    //FHTTP: TFPHTTPClient;
    FHTTP:THTTPSend;
    FAuthorizationToken:string;
    FAuthorizationTokenTimeStamp:TDateTime;
    FOmsId: string;
    FOnHttpStatus: TOnHttpStatusEnevent;
    FOnSignData: TOnSignDataEvent;
    //FDocument:TMemoryStream;

    FResultCode: integer;
    FResultString: string;
    FResultText: TStrings;
    FServer: string;
    function DoAnsverLogin(FUID, FDATA:string):Boolean;
    procedure SaveHttpData(ACmdName: string);
  protected
    function SendCommand(AMethod:THttpMethod; AExtension:TSuzAPIExtension; ACommand:string; AParams:string; AData:TStream; AMimeType:string = ''):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAPIToken;
    procedure Clear;
    function DoLogin:Boolean;
    function Login:Boolean;
    procedure LoadOrdersStatus(AExtension:TSuzAPIExtension);

  public
    property AuthorizationToken:string read FAuthorizationToken write FAuthorizationToken;
    property OmsId:string read FOmsId write FOmsId;

    property ResultText:TStrings read FResultText;
    property ResultCode : integer read FResultCode;
    property ResultString : string read FResultString;
    property HTTP:THTTPSend read FHTTP;
  published
    property Server:string read FServer write FServer;
    property OnHttpStatus:TOnHttpStatusEnevent read FOnHttpStatus write FOnHttpStatus;
    property OnSignData:TOnSignDataEvent read FOnSignData write FOnSignData;
  end;


const
  SuzAPIExtensionStr : array [TSuzAPIExtension] of string = (
     'light', // параметр URL extension для лёгкой промышленности. Использовался ранее для двух товарных групп: «Предметы одежды, белье постельное, столовое,
              //туалетное и кухонное» и «Обувные товары». Начинается переход с данного общего
              //расширения на два раздельных: lp и shoes. С 01.06.2021 г. расширение light
              //перестанет поддерживаться.
     'lp',    //параметр URL extension для лёгкой промышленности, категория товарной
              //группы «Предметы одежды, белье постельное, столовое, туалетное и кухонное»;
     'shoes',  //параметр URL extension для лёгкой промышленности, категория товарной
               //группы «Обувные товары»;
     'pharma',   //параметр URL extension для фармацевтической промышленности.
     'tobacco',  //параметр URL extension для табачной промышленности.
     'tires',    //параметр URL extension для производителей шин.
     'photo',    //параметр URL extension для производителей фототоваров.
     'perfum',   //параметр URL extension для производителей парфюмерной продукции.
     'milk',     //параметр URL extension для производителей молока;
     'bicycle',  //параметр URL extension для производителей велосипедов и велосипедных рам;
     'wheelchairs', //параметр URL extension для производителей кресел-колясок;
     'otp',         //параметр URL extension для производителей альтернативной табачной продукции;
     'water'        //параметр URL extension для производителей упакованной воды.
  );

implementation
uses crpt_cmp, jsonparser;

{ TCRPTSuzAPI }

function TCRPTSuzAPI.DoAnsverLogin(FUID, FDATA: string): Boolean;
begin

end;

procedure TCRPTSuzAPI.SaveHttpData(ACmdName: string);
var
  S: String;
  F: TFileStream;
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  S:=GetTempDir(false) + PathDelim + ACmdName;
  F:=TFileStream.Create(S, fmCreate);
  P:=FHTTP.Document.Position;
  FHTTP.Document.Position:=0;
  FHTTP.Document.SaveToStream(F);
  F.Free;
  FHTTP.Document.Position:=P;
end;

function TCRPTSuzAPI.SendCommand(AMethod: THttpMethod;
  AExtension: TSuzAPIExtension; ACommand: string; AParams: string;
  AData: TStream; AMimeType: string): Boolean;
var
  S, SMethod, FAPIURL, S1: String;
begin
  Clear;
  if AParams <> '' then
    AParams:='?' + AParams;

  //FHTTP.KeepConnection:=true;
  FHTTP.KeepAlive:=true;

  if FAuthorizationToken <> '' then
  begin
    //FHTTP.AddHeader('clientToken', FAuthorizationToken);
    //FHTTP.AddHeader('accept', '*/*');
    S:='clientToken: ' + FAuthorizationToken;
    FHTTP.Headers.Insert(0, S);
  end;

  ACommand:=SuzAPIExtensionStr[AExtension] + '/' + ACommand;

  if AMethod = hmGET then
  begin
    //FHTTP.Get(FServer + '/' + ACommand + AParams, FDocument);
    SMethod := 'GET'
  end
  else
  begin
{    if AMimeType<>'' then
      FHTTP.AddHeader('Content-Type',AMimeType)
    else
      FHTTP.AddHeader('Content-Type','application/x-www-form-urlencoded');

    if (not Assigned(AData)) or (AData.Size = 0) then
      FHTTP.AddHeader('Content-Length', '0');

    FHTTP.RequestBody:=AData;
    FHTTP.Post(FServer + '/' + ACommand + AParams, FDocument);}
    SMethod := 'POST';
    if (not Assigned(AData)) or (AData.Size = 0) then
    begin
      FHTTP.Headers.Insert(0, 'Content-Length: 0');
    end
    else
      FHTTP.Document.LoadFromStream(AData);
  end;

{  if AParams <> '' then
    AParams:='?' + AParams;}

  FHTTP.MimeType:='application/json';
  FAPIURL:=sAPIURL;//crtpVersion3 then

  S1:=FAPIURL + ACommand + AParams;
  Result := FHTTP.HTTPMethod(SMethod, FAPIURL + ACommand + AParams);
  FHTTP.Document.Position:=0;
  FResultCode := FHTTP.ResultCode;
  FResultString := FHTTP.ResultString;

  if Assigned(FOnHttpStatus) then
    FOnHttpStatus(Self);

{


  FResultCode := FHTTP.ResponseStatusCode;
  FResultString := FHTTP.ResponseStatusText;
  Result:=FResultCode = 200;

  if Assigned(FOnHttpStatus) then
    FOnHttpStatus(Self); }
end;

constructor TCRPTSuzAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FServer:=sAPIURL;
  //FDocument:=TMemoryStream.Create;
  FResultText:=TStringList.Create;

//  FHTTP:=TFPHTTPClient.Create(nil);
//  FHTTP.HTTPversion:='1.1';
  FHTTP := THTTPSend.Create;
  FHTTP.Protocol:='1.1';

end;

destructor TCRPTSuzAPI.Destroy;
begin
  FreeAndNil(FResultText);
//  FreeAndNil(FDocument);
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TCRPTSuzAPI.ClearAPIToken;
begin
  FAuthorizationToken:='';
  FAuthorizationTokenTimeStamp:=0;
end;

procedure TCRPTSuzAPI.Clear;
begin
  FHTTP.Clear;
  //FHTTP.RequestHeaders.Clear;
  //FHTTP.RequestBody:=nil;
  FResultText.Clear;
  //FDocument.Clear;
end;

function TCRPTSuzAPI.DoLogin: Boolean;
var
  B: TJSONParser;
  J: TJSONData;
  FDATA, FUID: TJSONStringType;
  R: Int64;
begin
{  if (FAuthorizationToken <> '') and (FAuthorizationTokenTimeStamp > (Now - (1 / 20) * 10)) then Exit;
  FAuthorizationToken:='';
  Result:=false;
  if SendCommand(hmGET, 'auth/key', '', nil) then
  begin
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
      begin
        Result:=DoAnsverLogin(FUID, FDATA);
      end;
    end;
  end; }
end;

function TCRPTSuzAPI.Login: Boolean;
begin
  Clear;
  Result:=DoLogin;
end;

procedure TCRPTSuzAPI.LoadOrdersStatus(AExtension: TSuzAPIExtension);
var
  S: String;
begin
  DoLogin;

  S:='';
  AddURLParam(S, 'omsId', FOmsId);

  if SendCommand(hmGET, AExtension, 'orders', S, nil) then
  begin
    FHTTP.Document.Position:=0;
    SaveHttpData('orders');
    FHTTP.Document.Position:=0;
{    P:=TJSONParser.Create(FDocument);
    Result:=P.Parse as TJSONObject;
    P.Free;}
  end;
end;

end.

