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

unit crptCDNTrueAPITypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;
type

  { TCDNSiteInfo }

  TCDNSiteInfo = class(TJSONSerializationObject)
  private
    FavgTimeMs: integer;
    FHost: string;
    FSiteErrorCode: integer;
    FSiteErrorDescription: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
  published
    property Host:string read FHost write FHost; //Адрес CDN-площадки
    //
    property Code:integer read FSiteErrorCode write FSiteErrorCode;
    property Description:string read FSiteErrorDescription write FSiteErrorDescription;
    property avgTimeMs:integer read FavgTimeMs write FavgTimeMs;
  end;
  TCDNSiteInfos = specialize GJSONSerializationObjectList<TCDNSiteInfo>;

  { TCDNInfo }

  TCDNInfo = class(TJSONSerializationObject)
  private
    FCode: Integer;
    FDescription: string;
    FHosts: TCDNSiteInfos;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Code:Integer read FCode write FCode; //Результат обработки операции
    property Description:string read FDescription write FDescription; //Текстовое описание результата выполнения метода
    property Hosts:TCDNSiteInfos read FHosts; //Список CDN-площадок
  end;

implementation

{ TCDNSiteInfo }

procedure TCDNSiteInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Host', 'host', [], '', -1, -1);
  RegisterProperty('Code', 'code', [], '', -1, -1);
  RegisterProperty('Description', 'description', [], '', -1, -1);
  RegisterProperty('avgTimeMs', 'avgTimeMs', [], '', -1, -1);
end;

procedure TCDNSiteInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

{ TCDNInfo }

procedure TCDNInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Code', 'code', [], '', -1, -1);
  RegisterProperty('Description', 'description', [], '', -1, -1);
  RegisterProperty('Hosts', 'hosts', [], '', -1, -1);
end;

procedure TCDNInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FHosts:=TCDNSiteInfos.Create;
end;

destructor TCDNInfo.Destroy;
begin
  FreeAndNil(FHosts);
  inherited Destroy;
end;

end.

