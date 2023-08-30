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

unit trueapi_cises_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TCISInfoData }

  TCISInfoData = class(TJSONSerializationObject)

  private
    FapplicationDate: string;
    Fbrand: string;
    Fcis: string;
    FcisWithoutBrackets: string;
    FemissionDate: string;
    FemissionType: string;
    FgeneralPackageType: string;
    Fgtin: string;
    FintroducedDate: string;
    FmarkWithdraw: boolean;
    FownerInn: string;
    FownerName: string;
    FpackageType: string;
    FproducedDate: string;
    FproducerInn: string;
    FproducerName: string;
    FproductGroup: string;
    FproductGroupId: Integer;
    FproductName: string;
    FrequestedCis: string;
    Fstatus: string;
    FstatusEx: string;
    FtnVedEaes: string;
    FtnVedEaesGroup: string;
    FwithdrawReason: string;
    procedure SetapplicationDate(AValue: string);
    procedure Setbrand(AValue: string);
    procedure Setcis(AValue: string);
    procedure SetcisWithoutBrackets(AValue: string);
    procedure SetemissionDate(AValue: string);
    procedure SetemissionType(AValue: string);
    procedure SetgeneralPackageType(AValue: string);
    procedure Setgtin(AValue: string);
    procedure SetintroducedDate(AValue: string);
    procedure SetmarkWithdraw(AValue: boolean);
    procedure SetownerInn(AValue: string);
    procedure SetownerName(AValue: string);
    procedure SetpackageType(AValue: string);
    procedure SetproducedDate(AValue: string);
    procedure SetproducerInn(AValue: string);
    procedure SetproducerName(AValue: string);
    procedure SetproductGroup(AValue: string);
    procedure SetproductGroupId(AValue: Integer);
    procedure SetproductName(AValue: string);
    procedure SetrequestedCis(AValue: string);
    procedure Setstatus(AValue: string);
    procedure SetstatusEx(AValue: string);
    procedure SettnVedEaes(AValue: string);
    procedure SettnVedEaesGroup(AValue: string);
    procedure SetwithdrawReason(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property applicationDate : string read FapplicationDate write SetapplicationDate;
    property introducedDate:string read FintroducedDate write SetintroducedDate;
    property requestedCis:string read FrequestedCis write SetrequestedCis;
    property cis:string read Fcis write Setcis;
    property cisWithoutBrackets:string read FcisWithoutBrackets write SetcisWithoutBrackets;
    property gtin:string read Fgtin write Setgtin;
    property tnVedEaes:string read FtnVedEaes write SettnVedEaes;
    property tnVedEaesGroup:string read FtnVedEaesGroup write SettnVedEaesGroup;
    property productName:string read FproductName write SetproductName;
    property productGroupId : Integer read FproductGroupId write SetproductGroupId;
    property productGroup:string read FproductGroup write SetproductGroup;
    property brand:string read Fbrand write Setbrand;
    property producedDate:string read FproducedDate write SetproducedDate;
    property emissionDate:string read FemissionDate write SetemissionDate;
    property emissionType:string read FemissionType write SetemissionType;
    property packageType:string read FpackageType write SetpackageType;
    property generalPackageType:string read FgeneralPackageType write SetgeneralPackageType;
    property ownerInn:string read FownerInn write SetownerInn;
    property ownerName:string read FownerName write SetownerName;
    property status:string read Fstatus write Setstatus;
    property statusEx:string read FstatusEx write SetstatusEx;
//    "child" : [    ],
    property producerInn:string read FproducerInn write SetproducerInn;
    property producerName:string read FproducerName write SetproducerName;
    property markWithdraw:boolean read FmarkWithdraw write SetmarkWithdraw;
    property withdrawReason:string read FwithdrawReason write SetwithdrawReason;
  end;

  { TCISInfo }

  TCISInfo = class(TJSONSerializationObject)
  private
    FcisInfo: TCISInfoData;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cisInfo:TCISInfoData read FcisInfo;
  end;
  TCISInfos = specialize GJSONSerializationObjectList<TCISInfo>;



implementation
uses fpjson, jsonparser;

{ TCISInfoData }

procedure TCISInfoData.SetapplicationDate(AValue: string);
begin
  if FapplicationDate=AValue then Exit;
  FapplicationDate:=AValue;
  ModifiedProperty('applicationDate');
end;

procedure TCISInfoData.Setbrand(AValue: string);
begin
  if Fbrand=AValue then Exit;
  Fbrand:=AValue;
  ModifiedProperty('brand');
end;

procedure TCISInfoData.Setcis(AValue: string);
begin
  if Fcis=AValue then Exit;
  Fcis:=AValue;
  ModifiedProperty('cis');
end;

procedure TCISInfoData.SetcisWithoutBrackets(AValue: string);
begin
  if FcisWithoutBrackets=AValue then Exit;
  FcisWithoutBrackets:=AValue;
  ModifiedProperty('cisWithoutBrackets');
end;

procedure TCISInfoData.SetemissionDate(AValue: string);
begin
  if FemissionDate=AValue then Exit;
  FemissionDate:=AValue;
  ModifiedProperty('emissionDate');
end;

procedure TCISInfoData.SetemissionType(AValue: string);
begin
  if FemissionType=AValue then Exit;
  FemissionType:=AValue;
  ModifiedProperty('emissionType');
end;

procedure TCISInfoData.SetgeneralPackageType(AValue: string);
begin
  if FgeneralPackageType=AValue then Exit;
  FgeneralPackageType:=AValue;
  ModifiedProperty('generalPackageType');
end;

procedure TCISInfoData.Setgtin(AValue: string);
begin
  if Fgtin=AValue then Exit;
  Fgtin:=AValue;
  ModifiedProperty('gtin');
end;

procedure TCISInfoData.SetintroducedDate(AValue: string);
begin
  if FintroducedDate=AValue then Exit;
  FintroducedDate:=AValue;
  ModifiedProperty('introducedDate');
end;

procedure TCISInfoData.SetmarkWithdraw(AValue: boolean);
begin
  if FmarkWithdraw=AValue then Exit;
  FmarkWithdraw:=AValue;
  ModifiedProperty('markWithdraw');
end;

procedure TCISInfoData.SetownerInn(AValue: string);
begin
  if FownerInn=AValue then Exit;
  FownerInn:=AValue;
  ModifiedProperty('ownerInn');
end;

procedure TCISInfoData.SetownerName(AValue: string);
begin
  if FownerName=AValue then Exit;
  FownerName:=AValue;
  ModifiedProperty('ownerName');
end;

procedure TCISInfoData.SetpackageType(AValue: string);
begin
  if FpackageType=AValue then Exit;
  FpackageType:=AValue;
  ModifiedProperty('packageType');
end;

procedure TCISInfoData.SetproducedDate(AValue: string);
begin
  if FproducedDate=AValue then Exit;
  FproducedDate:=AValue;
  ModifiedProperty('producedDate');
end;

procedure TCISInfoData.SetproducerInn(AValue: string);
begin
  if FproducerInn=AValue then Exit;
  FproducerInn:=AValue;
  ModifiedProperty('producerInn');
end;

procedure TCISInfoData.SetproducerName(AValue: string);
begin
  if FproducerName=AValue then Exit;
  FproducerName:=AValue;
  ModifiedProperty('producerName');
end;

procedure TCISInfoData.SetproductGroup(AValue: string);
begin
  if FproductGroup=AValue then Exit;
  FproductGroup:=AValue;
  ModifiedProperty('productGroup');
end;

procedure TCISInfoData.SetproductGroupId(AValue: Integer);
begin
  if FproductGroupId=AValue then Exit;
  FproductGroupId:=AValue;
  ModifiedProperty('productGroupId');
end;

procedure TCISInfoData.SetproductName(AValue: string);
begin
  if FproductName=AValue then Exit;
  FproductName:=AValue;
  ModifiedProperty('productName');
end;

procedure TCISInfoData.SetrequestedCis(AValue: string);
begin
  if FrequestedCis=AValue then Exit;
  FrequestedCis:=AValue;
  ModifiedProperty('requestedCis');
end;

procedure TCISInfoData.Setstatus(AValue: string);
begin
  if Fstatus=AValue then Exit;
  Fstatus:=AValue;
  ModifiedProperty('status');
end;

procedure TCISInfoData.SetstatusEx(AValue: string);
begin
  if FstatusEx=AValue then Exit;
  FstatusEx:=AValue;
  ModifiedProperty('statusEx');
end;

procedure TCISInfoData.SettnVedEaes(AValue: string);
begin
  if FtnVedEaes=AValue then Exit;
  FtnVedEaes:=AValue;
  ModifiedProperty('tnVedEaes');
end;

procedure TCISInfoData.SettnVedEaesGroup(AValue: string);
begin
  if FtnVedEaesGroup=AValue then Exit;
  FtnVedEaesGroup:=AValue;
  ModifiedProperty('tnVedEaesGroup');
end;

procedure TCISInfoData.SetwithdrawReason(AValue: string);
begin
  if FwithdrawReason=AValue then Exit;
  FwithdrawReason:=AValue;
  ModifiedProperty('withdrawReason');
end;

procedure TCISInfoData.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('applicationDate', 'applicationDate', [], '', -1, -1);
  RegisterProperty('introducedDate', 'introducedDate', [], '', -1, -1);
  RegisterProperty('requestedCis', 'requestedCis', [], '', -1, -1);
  RegisterProperty('cis', 'cis', [], '', -1, -1);
  RegisterProperty('cisWithoutBrackets', 'cisWithoutBrackets', [], '', -1, -1);
  RegisterProperty('gtin', 'gtin', [], '', -1, -1);
  RegisterProperty('tnVedEaes', 'tnVedEaes', [], '', -1, -1);
  RegisterProperty('tnVedEaesGroup', 'tnVedEaesGroup', [], '', -1, -1);
  RegisterProperty('productName', 'productName', [], '', -1, -1);
  RegisterProperty('productGroupId', 'productGroupId', [], '', -1, -1);
  RegisterProperty('productGroup', 'productGroup', [], '', -1, -1);
  RegisterProperty('brand', 'brand', [], '', -1, -1);
  RegisterProperty('producedDate', 'producedDate', [], '', -1, -1);
  RegisterProperty('emissionDate', 'emissionDate', [], '', -1, -1);
  RegisterProperty('emissionType', 'emissionType', [], '', -1, -1);
  RegisterProperty('packageType', 'packageType', [], '', -1, -1);
  RegisterProperty('generalPackageType', 'generalPackageType', [], '', -1, -1);
  RegisterProperty('ownerInn', 'ownerInn', [], '', -1, -1);
  RegisterProperty('ownerName', 'ownerName', [], '', -1, -1);
  RegisterProperty('status', 'status', [], '', -1, -1);
  RegisterProperty('statusEx', 'statusEx', [], '', -1, -1);
//    "child" : [    ],
  RegisterProperty('producerInn', 'producerInn', [], '', -1, -1);
  RegisterProperty('producerName', 'producerName', [], '', -1, -1);
  RegisterProperty('markWithdraw', 'markWithdraw', [], '', -1, -1);
  RegisterProperty('withdrawReason', 'withdrawReason', [], '', -1, -1);
end;

procedure TCISInfoData.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCISInfoData.Destroy;
begin
  inherited Destroy;
end;

{ TCISInfo }

procedure TCISInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('cisInfo', 'cisInfo', [], '', -1, -1);
end;

procedure TCISInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FcisInfo:=TCISInfoData.Create;
end;

destructor TCISInfo.Destroy;
begin
  FreeAndNil(FcisInfo);
  inherited Destroy;
end;


end.

