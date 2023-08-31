{ CRPT TrueAPI interface library demo for FPC and Lazarus

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

unit frmTrueAPICmdDocListUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  rxtooledit, IniFiles, fpjson, frmTrueAPICmdAbstractUnit;

type

  { TfrmTrueAPICmdDocListFrame }

  TfrmTrueAPICmdDocListFrame = class(TfrmTrueAPICmdAbstractFrame)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    RxDateEdit1: TRxDateEdit;
    RxDateEdit2: TRxDateEdit;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private

  public
    function FrameName:string; override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
  end;

implementation
uses rxlogging, CRPTTrueAPI_Consts;

{$R *.lfm}

{ TfrmTrueAPICmdDocListFrame }

procedure TfrmTrueAPICmdDocListFrame.Button1Click(Sender: TObject);
var
  R: TJSONData;
  FR:TDocListFilterRecord;
begin
  FillChar(FR, SizeOf(FR), 0);
  if CheckBox1.Checked then
  begin
    FR.FromDate:=RxDateEdit1.Date;
    FR.ToDate:=RxDateEdit2.Date;
  end;

  R:=CRPTTrueAPI.DocList(SelectedGroup, FR);
  if Assigned(R) then
  begin
    Memo1.Lines.Text:=R.FormatJSON;
    RxWriteLog(etInfo, R.FormatJSON);
    R.Free;
  end;
end;

procedure TfrmTrueAPICmdDocListFrame.CheckBox1Change(Sender: TObject);
begin
  RxDateEdit1.Enabled:=CheckBox1.Checked;
  RxDateEdit2.Enabled:=CheckBox1.Checked;
  Label1.Enabled:=CheckBox1.Checked;
  Label2.Enabled:=CheckBox1.Checked;
end;

function TfrmTrueAPICmdDocListFrame.FrameName: string;
begin
  Result:='Список документов';
end;

procedure TfrmTrueAPICmdDocListFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  RxDateEdit1.Date:=AIni.ReadDate(ClassName, 'RxDateEdit1_Date', Now-7);
  RxDateEdit2.Date:=AIni.ReadDate(ClassName, 'RxDateEdit2_Date', Now);
  CheckBox1.Checked:=AIni.ReadBool(ClassName, 'CheckBox1_Checked', false);
end;

procedure TfrmTrueAPICmdDocListFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteDate(ClassName, 'RxDateEdit1_Date', RxDateEdit1.Date);
  AIni.WriteDate(ClassName, 'RxDateEdit2_Date', RxDateEdit2.Date);
  AIni.WriteBool(ClassName, 'CheckBox1_Checked', CheckBox1.Checked);
end;

end.

