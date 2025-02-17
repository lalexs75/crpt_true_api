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

implementation

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

end.

