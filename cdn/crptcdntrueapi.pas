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

unit crptCDNTrueAPI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CRPTTrueAPI, fpjson, AbstractSerializationObjects, crptLocalTrueAPITypesUnit,
  crptCDNTrueAPITypes;

const
  sCDNTrueAPIURL_sandbox = 'https://markirovka.sandbox.crptech.ru'; //  Хост для тестового контура
  sCDNTrueAPIURL = 'https://cdn.crpt.ru';          //Хост для продуктивного контура

type

  { TCDNCrptAPI }

  TCDNCrptAPI = class(TCustomCRPTApi)
  protected
    procedure InternalMakeClientToken; override;
  public
    constructor Create(AOwner: TComponent); override;

    function CDNListInfo(ACheckHealth:Boolean): TCDNInfo;
  published
    property AuthorizationToken;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

  { TTrueAPICheck }

  TTrueAPICheck = class(TCustomCRPTApi)
  protected
    procedure InternalMakeClientToken; override;
  public
    constructor Create(AOwner: TComponent); override;

    function CodesCheck(ACodes:TXSDStringArray; AFiscalDriveNumber:string = ''): TJSONData;
  published
    property AuthorizationToken;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

  TLocalTrueAPICheck = class(TCustomCRPTApi)
  private
    FPassword : string;
    FUserName : string;
    procedure SetUserName(AUserName:string);
    procedure SetPassword(APassword:string);
  protected
    procedure InternalMakeClientToken; override;
  public
    constructor Create(AOwner: TComponent); override;

    function ServerStatus:TJSONData;
    function ServerStatusExt:TLCServerStatus;
    function CodeCheck(ACis:string; AFiscalDriveNumber:string = ''): TJSONData;
    function CodesCheck(ACodes:TXSDStringArray; AFiscalDriveNumber:string = ''): TJSONData;
  published
    property UserName:string read FUserName write SetUserName;
    property Password:string read FPassword write SetPassword;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

implementation
uses base64, jsonscanner, jsonparser, CRPTTrueAPIDataObjects, rxlogging, rxstrutils;

{ TCDNCrptAPI }

procedure TCDNCrptAPI.InternalMakeClientToken;
begin
  FHTTP.AddHeader('X-API-KEY', AuthorizationToken);
end;

constructor TCDNCrptAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:=sCDNTrueAPIURL;
end;

function TCDNCrptAPI.CDNListInfo(ACheckHealth: Boolean): TCDNInfo;

procedure DoCheckHealth(SI:TCDNSiteInfo);
var
  S: String;
begin
  S:=SI.Host + '/api/v4/true-api/cdn/health/check';
  SI.Code := -1;
  if SendCommand(hmGET,  S, '', nil, [200, 400, 404, 418], 'application/json') then
  begin
    Document.Position:=0;
    SI.LoadFromStream(Document);
  end;

  RxWriteLog(etDebug, 'DoCheckHealth %s :  ResultCode=%d; ResultString=%s', [SI.Host, ResultCode, ResultString]);
  Document.Position:=0;
  if Document.Size>0 then
  begin
    SetLength(S, Document.Size);
    Document.Read(S[1], Document.Size);
    RxWriteLog(etDebug, S);
  end;
end;

var
  I: Integer;
begin
  Result:=nil;

{$IFDEF LINUX}
//  raise Exception.Create('Error');
{$ENDIF}

  if SendCommand(hmGET, '/api/v4/true-api/cdn/info', '', nil, [200, 400, 401, 404], 'application/json') then
  begin
    SaveHttpData('true_api_CDNListInfo');
    Document.Position:=0;
    Result:=TCDNInfo.Create;
    Result.LoadFromStream(Document);
    if ACheckHealth and (Result.Code = 0) and (Result.Hosts.Count>0) then
    begin
      // CheckHealth
      for I:=0 to Result.Hosts.Count-1 do
        DoCheckHealth(Result.Hosts[i]);
    end;
  end
  else
    SaveHttpData('true_api_CDNListInfo');
end;

{ TTrueAPICheck }

procedure TTrueAPICheck.InternalMakeClientToken;
begin
  FHTTP.AddHeader('X-API-KEY', AuthorizationToken);
end;

constructor TTrueAPICheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer:='https://cdn10.crpt.ru/';
end;

function TTrueAPICheck.CodesCheck(ACodes: TXSDStringArray;
  AFiscalDriveNumber: string): TJSONData;
var
  M: TMemoryStream;
  Rec: TTrueAPICheckCodesRequest;
  P: TJSONParser;
begin
  Result:=nil;
{$IFDEF LINUX}
//  raise Exception.Create('Error');
{$ENDIF}

  M:=TMemoryStream.Create;
  Rec:=TTrueAPICheckCodesRequest.Create;
  Rec.fiscalDriveNumber:=AFiscalDriveNumber;
  Rec.Codes:=ACodes;
  Rec.SaveToStream(M);
  Rec.Free;
  M.Position:=0;

  if SendCommand(hmPOST, 'api/v4/true-api/codes/check', '', M, [200, 201, 400, 401, 404], 'application/json') then
  begin
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
    SaveHttpData('CodesCheck');
  end
  else
    SaveHttpData('true_api_CodesCheck');

//  M.Free;
end;

procedure TLocalTrueAPICheck.SetUserName(AUserName : string);
begin
  if AUserName <> FUserName then
  begin
    FUserName :=AUserName;
    AuthorizationToken:=EncodeStringBase64(FUserName + ':' + FPassword);
  end;
end;

procedure TLocalTrueAPICheck.SetPassword(APassword : string);
begin
  if APassword <> FPassword then
  begin
    FPassword :=APassword;
    AuthorizationToken:=EncodeStringBase64(FUserName + ':' + FPassword);
  end;
end;

procedure TLocalTrueAPICheck.InternalMakeClientToken;
begin
  FHTTP.AddHeader('Authorization', 'Basic ' + AuthorizationToken);
end;

constructor TLocalTrueAPICheck.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

function TLocalTrueAPICheck.ServerStatus : TJSONData;
var
  P : TJSONParser;
begin
  Result:=nil;
  if SendCommand(hmGET, 'api/v1/status', '', nil, [200, 201, 400, 401, 404], 'application/json') then
  begin
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
    SaveHttpData('local_server_stat');
  end
  else
    SaveHttpData('local_server_stat');
end;

function TLocalTrueAPICheck.ServerStatusExt : TLCServerStatus;
begin
  Result:=nil;
  if SendCommand(hmGET, 'api/v1/status', '', nil, [200, 201, 400, 401, 404], 'application/json') then
  begin
    SaveHttpData('local_server_stat');
    Document.Position:=0;
    Result:=TLCServerStatus.Create;
    Result.LoadFromStream(Document);
  end
  else
    SaveHttpData('local_server_stat');
end;

function TLocalTrueAPICheck.CodeCheck(ACis : string; AFiscalDriveNumber : string) : TJSONData;
var
  P : TJSONParser;
  S : String;
begin
  S:='';
  AddURLParam(S, 'cis', ACis);
  Result:=nil;
  if SendCommand(hmGET, 'api/v1/cis/check', S, nil, [200, 201, 400, 401, 404], 'application/json') then
  begin
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
    SaveHttpData('local_cis_check');
  end
  else
    SaveHttpData('local_cis_check');
end;

function TLocalTrueAPICheck.CodesCheck(ACodes : TXSDStringArray; AFiscalDriveNumber : string) : TJSONData;
var
  P : TJSONParser;
  J : TJSONArray;
  S1 : String;
  J1 : TJSONObject;
  M : TStringStream;
begin
  Result:=nil;


  J:=TJSONArray.Create;
  for S1 in ACodes do
    J.Add(S1);
  J1:=TJSONObject.Create;
  J1.Add('cis_list', J);

  M:=TStringStream.Create(J1.FormatJSON);
  J1.Free;
  M.Position:=0;

  if SendCommand(hmPOST, 'api/v1/cis/check', '', M, [200, 201, 400, 401, 404], 'application/json') then
  begin
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
    SaveHttpData('local_cises_check');
    RxWriteLog(etDebug, 'TLocalTrueAPICheck.CodesCheck - ok');
  end
  else
  begin
    SaveHttpData('local_cises_check');
    RxWriteLog(etDebug, 'TLocalTrueAPICheck.CodesCheck - error : %d %s', [ResultCode, ResultString]);
  end;
//  M.Free;
end;

end.

