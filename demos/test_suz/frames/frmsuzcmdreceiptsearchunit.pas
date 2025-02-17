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

unit frmSUZCmdReceiptSearchUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, frmSUZCmdAbstractUnit, IniFiles;

type

  { TfrmSUZCmdReceiptSearchFrame }

  TfrmSUZCmdReceiptSearchFrame = class(TfrmSUZCmdAbstractFrame)
    Button1: TButton;
    edtCISType: TEdit;
    edtCnt11: TEdit;
    edtLimit: TEdit;
    edtSkip: TEdit;
    edtGTIN: TEdit;
    edtServiceProviderId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
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
uses rxlogging, fpjson, jsonscanner, jsonparser, CRPTTrueAPI_Consts;

{$R *.lfm}

{ TfrmSUZCmdReceiptSearchFrame }

procedure TfrmSUZCmdReceiptSearchFrame.Button1Click(Sender: TObject);
var
  S11: String;
  P1: TJSONData;
  P: TJSONParser;
  PP: TJSONObject;
begin
  if PageControl1.ActivePageIndex = 0 then
    S11:=Memo1.Lines.Text//P.FormatJSON
  else
    S11:=Memo1.Lines.Text;

  P:=TJSONParser.Create(S11, DefaultOptions);
  PP:=P.Parse as TJSONObject;
  P.Free;


  RxWriteLog(etInfo, S11);
  P1:=CRPTSuzAPI.ReceiptSearch(PP, StrToInt(edtLimit.Text), StrToInt(edtSkip.Text));
  if Assigned(P1) then
  begin
    RxWriteLog(etInfo, P1.FormatJSON);
    P1.Free;
  end;
 // PP.Free;
end;

function TfrmSUZCmdReceiptSearchFrame.FrameName: string;
begin
  Result:='Поиск квитанции';
end;

procedure TfrmSUZCmdReceiptSearchFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  edtLimit.Text:=AIni.ReadString(ClassName, 'edtLimit_Text', '100');
  edtSkip.Text:=AIni.ReadString(ClassName, 'edtSkip_Text', '0');
end;

procedure TfrmSUZCmdReceiptSearchFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteString(ClassName, 'edtLimit_Text', edtLimit.Text);
  AIni.WriteString(ClassName, 'edtSkip_Text', edtSkip.Text);
end;

end.

