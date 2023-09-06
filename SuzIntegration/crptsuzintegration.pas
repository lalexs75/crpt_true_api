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

unit CRPTSuzIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CRPTTrueAPI, fpJSON;

const
  //Интеграция
  sAPISuzIntegrator = 'https://suzgrid.crpt.ru:16443/';
  sAPISuzIntegrator_sandbox = 'https://suz-integrator.sandbox.crptech.ru/';

type

  { TCRPTSuzIntegrationAPI }

  TCRPTSuzIntegrationAPI = class(TCustomCRPTApi)
  private
    FOmsID: string;
    FRegistrationKey: string;
    procedure SetOmsID(AValue: string);
    procedure SetRegistrationKey(AValue: string);
    function InternalSuzAPIUrl:string;inline;
  protected
    procedure InternalMakeClientToken; override;
    procedure SetCRPTApiType(AValue: TCRPTApiType); override;
  public
    property AuthorizationToken;
    function IntegrationRegister(AName, AAdress:string):TJSONObject;
  published
    property CRPTApiType default atProduction;
    property OmsID:string read FOmsID write SetOmsID;
    property RegistrationKey:string read FRegistrationKey write SetRegistrationKey;
    property OnHttpStatus;
    property OnSignData;
  end;

implementation
uses rxlogging, jsonparser, jsonscanner;

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

function TCRPTSuzIntegrationAPI.InternalSuzAPIUrl: string;
begin
  if CRPTApiType = atSandbox then
    Result:=sAPISuzIntegrator_sandbox
  else
    Result:=sAPISuzIntegrator;
end;

procedure TCRPTSuzIntegrationAPI.InternalMakeClientToken;
begin
  if RegistrationKey<>'' then
  begin
    FHTTP.AddHeader('X-RegistrationKey', RegistrationKey);
    RxWriteLog(etDebug, 'X-RegistrationKey: %s', [RegistrationKey]);
  end;
end;

procedure TCRPTSuzIntegrationAPI.SetCRPTApiType(AValue: TCRPTApiType);
begin
  inherited SetCRPTApiType(AValue);
  FServer:=InternalSuzAPIUrl;
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  FMS.Free;
  SaveHttpData('oms_api_v3_integration_connection');
end;

end.

