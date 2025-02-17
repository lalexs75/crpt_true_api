{ CRPT SUZ interface library demo for FPC and Lazarus

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

unit frmSUZCmdAbstractUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CRPTTrueAPI, CRPTTrueAPI_Consts, CRPTSuzAPI, IniFiles;

type
  TSUZType = (stTest, stWork);

  { TfrmSUZCmdAbstractFrame }

  TfrmSUZCmdAbstractFrame = class(TFrame)
  private
    FCRPTSuzAPI: TCRPTSuzAPI;
  protected
    function APISuzType:TSUZType;
    function SelectedGroup:TCRPTProductGroup;
    procedure SetCRPTSuzAPI(AValue: TCRPTSuzAPI); virtual;
  public
    function FrameName:string;virtual;abstract;
    procedure LoadParams(AIni:TIniFile); virtual;
    procedure SaveParams(AIni:TIniFile); virtual;
    procedure AppLogin; virtual;
    property CRPTSuzAPI: TCRPTSuzAPI read FCRPTSuzAPI write SetCRPTSuzAPI;
  end;

implementation

uses CRPTSuzTestMainUnit;

{$R *.lfm}

{ TfrmSUZCmdAbstractFrame }

procedure TfrmSUZCmdAbstractFrame.SetCRPTSuzAPI(AValue: TCRPTSuzAPI);
begin
  if FCRPTSuzAPI=AValue then Exit;
  FCRPTSuzAPI:=AValue;
end;

function TfrmSUZCmdAbstractFrame.APISuzType: TSUZType;
begin
  Result:=TSUZType((Owner as TCRPTSuzTestForm).RadioGroup1.ItemIndex)
end;

function TfrmSUZCmdAbstractFrame.SelectedGroup: TCRPTProductGroup;
begin

end;

procedure TfrmSUZCmdAbstractFrame.LoadParams(AIni: TIniFile);
begin
  //
end;

procedure TfrmSUZCmdAbstractFrame.SaveParams(AIni: TIniFile);
begin

end;

procedure TfrmSUZCmdAbstractFrame.AppLogin;
begin
  //
end;

end.

