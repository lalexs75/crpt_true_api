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
  Classes, SysUtils, CRPTTrueAPI, fpjson;

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

    function CDNListInfo: TJSONData;
  published
    property AuthorizationToken;
    property Server;
    property OnHttpStatus;
    property OnSignData;
  end;

implementation
uses jsonscanner, jsonparser;

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

function TCDNCrptAPI.CDNListInfo: TJSONData;
var
  P:TJSONParser;
begin
  Result:=nil;


  if SendCommand(hmGET, '/api/v4/true-api/cdn/info', '', nil, [200, 400, 401, 404], 'application/json') then
  begin
    SaveHttpData('true_api_CDNListInfo');
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONData;
    P.Free;
  end
  else
    SaveHttpData('true_api_CDNListInfo');
end;

end.
