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

unit CRPTSuzAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CRPTTrueAPI, fpJSON, CRPTTrueAPI_Consts;

const
  //SUZ
  sAPISuzURL = 'https://suzgrid.crpt.ru/';
  sAPISuzURL_sandbox1 = 'https://suz.sandbox.crptech.ru/';
  sAPISuzURL_sandbox2 = 'https://suz-integrator.sandbox.crptech.ru/';


type

  { TCRPTSuzAPI }

  TCRPTSuzAPI = class(TCustomCRPTApi)
  private
    FOmsConnection: string;
    FOmsID: string;
    procedure SetOmsConnection(AValue: string);
    procedure SetOmsID(AValue: string);
    function InternalSuzAPIUrl:string;inline;
  protected
    procedure InternalMakeLoginParams(var ALoginParams:string); override;
    procedure InternalMakeClientToken; override;
    procedure SetCRPTApiType(AValue: TCRPTApiType); override;
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
    property CRPTApiType default atProduction;
    property OmsConnection:string read FOmsConnection write SetOmsConnection;
    property OmsID:string read FOmsID write SetOmsID;
    property OnHttpStatus;
    property OnSignData;
  end;

implementation
uses rxlogging, jsonparser, jsonscanner;

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

function TCRPTSuzAPI.InternalSuzAPIUrl: string;
begin
  if CRPTApiType = atSandbox then
    Result:=sAPISuzURL_sandbox1
  else
    Result:=sAPISuzURL;
end;

procedure TCRPTSuzAPI.InternalMakeLoginParams(var ALoginParams: string);
begin
  ALoginParams:=FOMSConnection;
end;

procedure TCRPTSuzAPI.InternalMakeClientToken;
begin
  FHTTP.AddHeader('clientToken', AuthorizationToken);
end;

procedure TCRPTSuzAPI.SetCRPTApiType(AValue: TCRPTApiType);
begin
  inherited SetCRPTApiType(AValue);
  FServer:=InternalSuzAPIUrl;
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
      Document.Position:=0;
      P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
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
    Document.Position:=0;
    P:=TJSONParser.Create(Document, DefaultOptions);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
  SaveHttpData('oms_api_v3_quality_cisList');
end;

end.

