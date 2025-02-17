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

unit frmSUZCmdQualitySearchUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, fpjson, frmSUZCmdAbstractUnit;

type

  { TfrmSUZCmdQualitySearchFrame }

  TfrmSUZCmdQualitySearchFrame = class(TfrmSUZCmdAbstractFrame)
    Button1: TButton;
    edtOrderID: TEdit;
    edtLimit: TEdit;
    edtSkip: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblOrderID: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private

  public
    function FrameName:string; override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
  end;

implementation
uses rxlogging;

{$R *.lfm}

{ TfrmSUZCmdQualitySearchFrame }

procedure TfrmSUZCmdQualitySearchFrame.Button1Click(Sender: TObject);
var
  P1: TJSONObject;
begin
  P1:=CRPTSuzAPI.QualityInfo(edtOrderID.Text, StrToInt(edtLimit.Text), StrToInt(edtSkip.Text));
  if Assigned(P1) then
  begin
    Memo1.Lines.Text:=P1.FormatJSON;
    RxWriteLog(etInfo, P1.FormatJSON);
    P1.Free;
  end;
end;

function TfrmSUZCmdQualitySearchFrame.FrameName: string;
begin
  Result:='Сведения о нанесении';
end;

procedure TfrmSUZCmdQualitySearchFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  edtOrderID.Text:=AIni.ReadString(ClassName, 'edtOrderID_Text', '');
  edtLimit.Text:=AIni.ReadString(ClassName, 'edtLimit_Text', '0');
  edtSkip.Text:=AIni.ReadString(ClassName, 'edtSkip_Text', '0');
end;

procedure TfrmSUZCmdQualitySearchFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteString(ClassName, 'edtOrderID_Text', edtOrderID.Text);
  AIni.WriteString(ClassName, 'edtLimit_Text', edtLimit.Text);
  AIni.WriteString(ClassName, 'edtSkip_Text', edtSkip.Text);
end;

end.

