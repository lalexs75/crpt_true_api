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

unit frmSUZCmdOrderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  frmSUZCmdAbstractUnit, IniFiles;

type

  { TfrmSUZCmdOrderFrame }

  TfrmSUZCmdOrderFrame = class(TfrmSUZCmdAbstractFrame)
    Button1: TButton;
    edtCnt: TEdit;
    edtServiceProviderId: TEdit;
    edtCISType: TEdit;
    edtGTIN: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
  private

  public
    function FrameName:string; override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
  end;

implementation
uses rxlogging, fpjson, CRPTTrueAPI_Consts;
{$R *.lfm}

{ TfrmSUZCmdOrderFrame }

procedure TfrmSUZCmdOrderFrame.Button1Click(Sender: TObject);
var
  P, PA, P1: TJSONObject;
  S: String;
  PAA: TJSONArray;
  S11: TJSONStringType;
begin
  P:=TJSONObject.Create;
  P.Add('productGroup', 'tires');
  P.Add('serviceProviderId', edtServiceProviderId.Text);


  PAA:=TJSONArray.Create;
  P.Add('products', PAA);
  PA:=TJSONObject.Create;
  PAA.Add(PA);

  PA.Add('gtin', edtGTIN.Text);
  PA.Add('quantity', StrToInt(edtCnt.Text));
  PA.Add('serialNumberType', 'SELF_MADE');
  PA.Add('serialNumbers', TJSONArray.Create(['QIQ8BQCXmSJJ', 'GLTP9kqZn5QR']));
  PA.Add('templateId', 7);
  PA.Add('cisType', edtCISType.Text);

  PA:=TJSONObject.Create;
  P.Add('attributes', PA);
  S:='Иванов П.А.';
  PA.Add('contactPerson',S);
  PA.Add('releaseMethodType','IMPORT');
  PA.Add('createMethodType','CM');
  PA.Add('productionOrderId','08528091-808a-41ba-a55d-d6230c64b332');

  if PageControl1.ActivePageIndex = 0 then
    S11:=P.FormatJSON
  else
    S11:=Memo1.Lines.Text;

  RxWriteLog(etInfo, S11 {P.FormatJSON});
  P1:=CRPTSuzAPI.Order(tires, S11 {P});
  if Assigned(P1) then
  begin
    RxWriteLog(etInfo, P1.FormatJSON);
    P1.Free;
  end;
  P.Free;

end;

function TfrmSUZCmdOrderFrame.FrameName: string;
begin
  Result:='Заказ на эмиссию'
end;

procedure TfrmSUZCmdOrderFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  edtGTIN.Text:=AIni.ReadString(ClassName, 'edtGTIN_Text', '');
  edtServiceProviderId.Text:=AIni.ReadString(ClassName, 'edtServiceProviderId_Text', '');
  edtCnt.Text:=AIni.ReadString(ClassName, 'edtCnt_Text', '');
  edtCISType.Text:=AIni.ReadString(ClassName, 'edtCISType_Text', '');
end;

procedure TfrmSUZCmdOrderFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteString(ClassName, 'edtGTIN_Text', edtGTIN.Text);
  AIni.WriteString(ClassName, 'edtServiceProviderId_Text', edtServiceProviderId.Text);
  AIni.WriteString(ClassName, 'edtCnt_Text', edtCnt.Text);
  AIni.WriteString(ClassName, 'edtCISType_Text', edtCISType.Text);
end;

end.

