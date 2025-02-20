{ CRPT TrueAPI interface library for FPC and Lazarus

  Copyright (C) 2023-2025 Lagunov Aleksey alexs75@yandex.ru

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

unit crptLocalTrueAPITypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type
  TLCCodeInfo = class(TJSONSerializationObject)
  private
    Fcis : string;
    Fgtin : string;
    FisBlocked : Boolean;
    FprintView : string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
  published
    property printView:string read FprintView write FprintView;
    property isBlocked:Boolean read FisBlocked write FisBlocked;
    property gtin:string read Fgtin write Fgtin;
    property cis:string read Fcis write Fcis;
  end;
  TLCCodeInfos = specialize GJSONSerializationObjectList<TLCCodeInfo>;


  TLCResultItem = class(TJSONSerializationObject)
  private
    Fcode : Integer;
    Fcodes : TLCCodeInfos;
    Fdescription : string;
    Finst : string;
    FreqId : string;
    FreqTimestamp : Int64;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
    function reqDate:TDateTime;
  published
    property reqId:string read FreqId write FreqId;
    property reqTimestamp :Int64 read FreqTimestamp write FreqTimestamp;
    property inst:string read Finst write Finst;
    property description:string read Fdescription write Fdescription;
    property codes : TLCCodeInfos read Fcodes;
    property code:Integer read Fcode write Fcode;
  end;
  TLCResultItems = specialize GJSONSerializationObjectList<TLCResultItem>;

  TLCResult = class(TJSONSerializationObject)
  private
    Fresults : TLCResultItems;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property results:TLCResultItems read Fresults;
  end;

  //Статус сервера
  TLCStatusDocCount = class(TJSONSerializationObject)
  private
    FlocalDocCount : Integer;
    FserverDocCount : Integer;
    FtimeLag : Integer;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
  published
    property timeLag:Integer read FtimeLag write FtimeLag;
    property serverDocCount:Integer read FserverDocCount write FserverDocCount;
    property localDocCount:Integer read FlocalDocCount write FlocalDocCount;
  end;

  { TLCStatusReplicationStatus }
  TLCStatusReplicationStatus = class(TJSONSerializationObject)
  private
    Fblocked_cis : TLCStatusDocCount;
    Fblocked_gtin : TLCStatusDocCount;
    Fblocked_series : TLCStatusDocCount;
    Fcis : TLCStatusDocCount;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cis:TLCStatusDocCount read Fcis;
    property blocked_gtin:TLCStatusDocCount read Fblocked_gtin;
    property blocked_cis:TLCStatusDocCount read Fblocked_cis;
    property blocked_series:TLCStatusDocCount read Fblocked_series;
  end;

  { TLCServerStatus }
  TLCServerStatus = class(TJSONSerializationObject)
  private
    Finst : string;
    FlastSync : Int64;
    Fname : string;
    FoperationMode : string;
    FreplicationStatus : TLCStatusReplicationStatus;
    FrequiresDownload : string;
    Fstatus : string;
    Fversion : string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
    function lastSyncDate:TDateTime;
  published
    property version:string read Fversion write Fversion;
    property status:string read Fstatus write Fstatus;
    property requiresDownload:string read FrequiresDownload write FrequiresDownload;
    property replicationStatus:TLCStatusReplicationStatus read FreplicationStatus;
    property operationMode:string read FoperationMode write FoperationMode;
    property name:string read Fname write Fname;
    property lastSync : Int64 read FlastSync write FlastSync;
    property inst:string read Finst write Finst;
  end;

implementation
uses DateUtils;

procedure TLCCodeInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('printView', 'printView', [], '', -1, -1);
  RegisterProperty('isBlocked', 'isBlocked', [], '', -1, -1);
  RegisterProperty('gtin', 'gtin', [], '', -1, -1);
  RegisterProperty('cis', 'cis', [], '', -1, -1);
end;

procedure TLCCodeInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

procedure TLCResultItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('reqId', 'reqId', [], '', -1, -1);
  RegisterProperty('reqTimestamp', 'reqTimestamp', [], '', -1, -1);
  RegisterProperty('inst', 'inst', [], '', -1, -1);
  RegisterProperty('description', 'description', [], '', -1, -1);
  RegisterProperty('codes', 'codes', [], '', -1, -1);
  RegisterProperty('code', 'code', [], '', -1, -1);
end;

procedure TLCResultItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fcodes := TLCCodeInfos.Create;
end;

destructor TLCResultItem.Destroy;
begin
  FreeAndNil(Fcodes);
  inherited Destroy;
end;

function TLCResultItem.reqDate : TDateTime;
begin
  Result:=UnixToDateTime(FreqTimestamp div 1000, false);
end;

procedure TLCResult.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('results', 'results', [], '', -1, -1);
end;

procedure TLCResult.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fresults:=TLCResultItems.Create;
end;

destructor TLCResult.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Fresults);
end;

  {  TLCStatusDocCount  }
procedure TLCStatusDocCount.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('timeLag', 'timeLag', [], '', -1, -1);
  RegisterProperty('serverDocCount', 'serverDocCount', [], '', -1, -1);
  RegisterProperty('localDocCount', 'localDocCount', [], '', -1, -1);
end;

procedure TLCStatusDocCount.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

  {  TLCStatusReplicationStatus  }
procedure TLCStatusReplicationStatus.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
end;

procedure TLCStatusReplicationStatus.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fblocked_cis := TLCStatusDocCount.Create;
  Fblocked_gtin := TLCStatusDocCount.Create;
  Fblocked_series := TLCStatusDocCount.Create;
  Fcis := TLCStatusDocCount.Create;
end;

destructor TLCStatusReplicationStatus.Destroy;
begin
  FreeAndNil(Fblocked_cis);
  FreeAndNil(Fblocked_gtin);
  FreeAndNil(Fblocked_series);
  FreeAndNil(Fcis);
  inherited Destroy;
end;

procedure TLCServerStatus.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('version', 'version', [], '', -1, -1);
  RegisterProperty('status', 'status', [], '', -1, -1);
  RegisterProperty('requiresDownload', 'requiresDownload', [], '', -1, -1);
  RegisterProperty('replicationStatus', 'replicationStatus', [], '', -1, -1);
  RegisterProperty('operationMode', 'operationMode', [], '', -1, -1);
  RegisterProperty('name', 'name', [], '', -1, -1);
  RegisterProperty('lastSync', 'lastSync', [], '', -1, -1);
  RegisterProperty('inst', 'inst', [], '', -1, -1);
end;

procedure TLCServerStatus.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FreplicationStatus :=TLCStatusReplicationStatus.Create;
end;

destructor TLCServerStatus.Destroy;
begin
  FreeAndNil(FreplicationStatus);
  inherited Destroy;
end;

function TLCServerStatus.lastSyncDate : TDateTime;
begin
  Result:=UnixToDateTime(FlastSync div 1000, false);
end;


end.

