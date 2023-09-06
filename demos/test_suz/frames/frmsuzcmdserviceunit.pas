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

unit frmSUZCmdServiceUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, CRPTSuzAPI,
  frmSUZCmdAbstractUnit, CRPTTrueAPI, CRPTSuzIntegration;

type

  { TfrmSUZCmdServiceFrame }

  TfrmSUZCmdServiceFrame = class(TfrmSUZCmdAbstractFrame)
    Button1: TButton;
    CRPTSuzIntegrationAPI1: TCRPTSuzIntegrationAPI;
    edtAdress: TEdit;
    edtName: TEdit;
    edtRegistrationKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

    procedure SetCRPTSuzAPI(AValue: TCRPTSuzAPI); override;
  protected
  public
    function FrameName:string;override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
    procedure AppLogin; override;
  end;

implementation
uses fpjson, rxlogging;

{$R *.lfm}

{ TfrmSUZCmdServiceFrame }

procedure TfrmSUZCmdServiceFrame.Button1Click(Sender: TObject);
var
  P1: TJSONObject;
begin
  CRPTSuzIntegrationAPI1.AuthorizationToken:=edtRegistrationKey.Text;
  CRPTSuzIntegrationAPI1.RegistrationKey:=edtRegistrationKey.Text;
  P1:=CRPTSuzIntegrationAPI1.IntegrationRegister(edtName.Text, edtAdress.Text);
  if Assigned(P1) then
  begin
    Memo1.Lines.Text:=P1.FormatJSON;
    RxWriteLog(etInfo, P1.FormatJSON);
    P1.Free;
  end;
end;

procedure TfrmSUZCmdServiceFrame.SetCRPTSuzAPI(AValue: TCRPTSuzAPI);
begin
  inherited SetCRPTSuzAPI(AValue);
  CRPTSuzIntegrationAPI1.OnHttpStatus:=AValue.OnHttpStatus;
  CRPTSuzIntegrationAPI1.OnSignData:=AValue.OnSignData;
end;

function TfrmSUZCmdServiceFrame.FrameName: string;
begin
  Result:='Регистрация экземпляра';
end;

procedure TfrmSUZCmdServiceFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  edtRegistrationKey.Text:=AIni.ReadString(ClassName, 'edtRegistrationKey_Text', '');
  edtName.Text:=AIni.ReadString(ClassName, 'edtName_Text', '');
  edtAdress.Text:=AIni.ReadString(ClassName, 'edtAdress_Text', '');
end;

procedure TfrmSUZCmdServiceFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteString(ClassName, 'edtRegistrationKey_Text', edtRegistrationKey.Text);
  AIni.WriteString(ClassName, 'edtName_Text', edtName.Text);
  AIni.WriteString(ClassName, 'edtAdress_Text', edtAdress.Text);
end;

procedure TfrmSUZCmdServiceFrame.AppLogin;
begin
  CRPTSuzIntegrationAPI1.OmsID:=CRPTSuzAPI.OmsID;

  if APISuzType = stTest then
    CRPTSuzIntegrationAPI1.CRPTApiType:=atSandbox
  else
    CRPTSuzIntegrationAPI1.CRPTApiType:=atProduction;
end;

end.


