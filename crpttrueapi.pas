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
  Classes, SysUtils, fphttpclient, fpJSON;

const
  sAPIURL = 'https://markirovka.crpt.ru/api/v3/true-api';

type
  TCRPTTrueAPI = class;

  THttpMethod = (hmGET, hmPOST);
  TOnHttpStatusEnevent = procedure (Sender:TCRPTTrueAPI) of object;
  TOnSignDataEvent = procedure(Sender:TCRPTTrueAPI; AData:string; out ASign:string) of object;

  { TCRPTTrueAPI }

  TCRPTTrueAPI = class(TComponent)
  private
    FHTTP: TFPHTTPClient;
    FAuthorizationToken:string;
    FAuthorizationTokenTimeStamp:TDateTime;
    FOnHttpStatus: TOnHttpStatusEnevent;
    FOnSignData: TOnSignDataEvent;
    FDocument:TMemoryStream;

    FResultCode: integer;
    FResultString: string;
    FResultText: TStrings;
    FServer: string;
    function DoAnsverLogin(FUID, FDATA:string):Boolean;
    procedure SaveHttpData(ACmdName: string);
  protected
    function SendCommand(AMethod:THttpMethod; ACommand:string; AParams:string; AData:TStream; AMimeType:string = ''):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAPIToken;
    procedure Clear;
    function DoLogin:Boolean;
    function Login:Boolean;
    procedure LoadAuthorizationToken(AFileName:string);

    property ResultText:TStrings read FResultText;
    property ResultCode : integer read FResultCode;
    property ResultString : string read FResultString;
  public
    function ProductsInfo(ACis:string; AchildrenPage:Integer = 0; AChildrenLimit:Integer = 0):TJSONObject;
//КИ
//Номер страницы вложений в агрегат первого слоя
  published
    property Server:string read FServer write FServer;
    property OnHttpStatus:TOnHttpStatusEnevent read FOnHttpStatus write FOnHttpStatus;
    property OnSignData:TOnSignDataEvent read FOnSignData write FOnSignData;
  end;

procedure Register;

implementation
uses jsonparser;

{$R crpt_true_api.res}

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TCRPTTrueAPI]);
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


{ TCRPTTrueAPI }

function TCRPTTrueAPI.DoAnsverLogin(FUID, FDATA: string): Boolean;
var
  S1: string;
  J, R: TJSONObject;
  M: TStringStream;
  P: TJSONParser;
begin
  if not Assigned(FOnSignData) then Exit;

  FOnSignData(Self, FDATA, S1);

  J:=TJSONObject.Create;
  J.Add('uuid', FUID);
  J.Add('data', S1);
  M:=TStringStream.Create(J.FormatJSON);
  M.Position:=0;
  J.Free;


  if SendCommand(hmPOST, 'auth/simpleSignIn', '', M, 'application/json') then
  begin
    SaveHttpData('dologin_cert');
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

procedure TCRPTTrueAPI.SaveHttpData(ACmdName: string);
var
  S: String;
  F: TFileStream;
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  S:=GetTempDir(false) + PathDelim + ACmdName;
  F:=TFileStream.Create(S, fmCreate);
  P:=FDocument.Position;
  FDocument.Position:=0;
  FDocument.SaveToStream(F);
  F.Free;
  FDocument.Position:=P;
end;

function TCRPTTrueAPI.SendCommand(AMethod: THttpMethod; ACommand: string;
  AParams: string; AData: TStream; AMimeType: string): Boolean;
var
  S: String;
begin
  Clear;
  if AParams <> '' then
    AParams:='?' + AParams;

  FHTTP.KeepConnection:=true;

  if FAuthorizationToken <> '' then
  begin
    FHTTP.AddHeader('Authorization', 'Bearer' + FAuthorizationToken);
    FHTTP.AddHeader('accept', '*/*');
  end;

  if AMethod = hmGET then
  begin
    FHTTP.Get(FServer + '/' + ACommand + AParams, FDocument);
  end
  else
  begin
    if AMimeType<>'' then
      FHTTP.AddHeader('Content-Type',AMimeType)
    else
      FHTTP.AddHeader('Content-Type','application/x-www-form-urlencoded');

    if (not Assigned(AData)) or (AData.Size = 0) then
      FHTTP.AddHeader('Content-Length', '0');

    FHTTP.RequestBody:=AData;
    FHTTP.Post(FServer + '/' + ACommand + AParams, FDocument);
  end;

  FResultCode := FHTTP.ResponseStatusCode;
  FResultString := FHTTP.ResponseStatusText;
  Result:=FResultCode = 200;

  if Assigned(FOnHttpStatus) then
    FOnHttpStatus(Self);
end;

constructor TCRPTTrueAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FServer:=sAPIURL;
  FDocument:=TMemoryStream.Create;
  FResultText:=TStringList.Create;

  FHTTP:=TFPHTTPClient.Create(nil);
  FHTTP.HTTPversion:='1.1';

//  FProxyData:=TProxyData.Create;
//  FResultText:=TStringList.Create;
end;

destructor TCRPTTrueAPI.Destroy;
begin
  FreeAndNil(FResultText);
  FreeAndNil(FDocument);
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TCRPTTrueAPI.ClearAPIToken;
begin
  FAuthorizationToken:='';
  FAuthorizationTokenTimeStamp:=0;
end;

procedure TCRPTTrueAPI.Clear;
begin
  FHTTP.RequestHeaders.Clear;
  FHTTP.RequestBody:=nil;
  FResultText.Clear;
  FDocument.Clear;
end;

function TCRPTTrueAPI.DoLogin: Boolean;
var
  B: TJSONParser;
  J: TJSONData;
  FDATA, FUID: TJSONStringType;
  R: Int64;
begin
  if (FAuthorizationToken <> '') and (FAuthorizationTokenTimeStamp > (Now - (1 / 20) * 10)) then Exit;
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
  end;
end;

function TCRPTTrueAPI.Login: Boolean;
begin
  Clear;
  Result:=DoLogin;
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
  S:=HTTPEncode(StringReplace(ACis, '%', '%25', [rfReplaceAll]));

  if SendCommand(hmGET, 'products/info', 'cis='+S, nil) then
  begin
    SaveHttpData('products_info');
    FDocument.Position:=0;
    P:=TJSONParser.Create(FDocument);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

end.
