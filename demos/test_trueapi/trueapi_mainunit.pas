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

unit TrueAPI_MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, CRPTTrueAPI, ocrsConnectionUnit, DividerBevel,
  RxIniPropStorage, sslsockets, ssockets, fpjson, CRPTTrueAPI_Consts
  ;

type

  { TCRPTTrueAPITestMainForm }

  TCRPTTrueAPITestMainForm = class(TForm)
    btnLogin: TButton;
    ComboBox1: TComboBox;
    ConfigPanel: TPanel;
    CRPTTrueAPI1: TCRPTTrueAPI;
    edtUserKey: TEdit;
    edtCryptoProSrv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    RxIniPropStorage1: TRxIniPropStorage;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TreeView1: TTreeView;
    procedure btnLoginClick(Sender: TObject);
    procedure CRPTTrueAPI1HttpStatus(Sender: TCustomCRPTApi);
    procedure CRPTTrueAPI1SignData(Sender: TCustomCRPTApi; AData: string;
      ADetached: Boolean; out ASign: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
    procedure TreeView1Click(Sender: TObject);
  private
    OldFrame:TFrame;
    procedure DoFillProductGroup;
    function SelectedGroup:TCRPTProductGroup;
    function AddCRPTOperFrame(AGroup:string; AFrame:TFrame):TFrame;
    procedure CreatePages;
    procedure SavePages;
  public

  end;

var
  CRPTTrueAPITestMainForm: TCRPTTrueAPITestMainForm;

procedure RxLogWriter( ALogType:TEventType; const ALogMessage:string);
implementation
uses
  rxlogging, IniFiles, rxAppUtils, frmTrueAPICmdAbstractUnit,
  frmTrueAPICmdCISUnit, frmTrueAPICmdBalanceUnit, frmTrueAPICmdDocListUnit,
  frmTrueAPICmdReceiptListUnit, frmTrueAPICmdDocInfoUnit,
  frmTrueAPICmdReceiptInfoUnit, frmTrueAPICmdCheckCISUnit,
  frmTrueAPICmdOutCISUnit,

  fpopenssl,
  opensslsockets
;

procedure RxLogWriter(ALogType: TEventType; const ALogMessage: string);
const
  sEventNames : array [TEventType] of string =
    ('CUSTOM','INFO','WARNING','ERROR','DEBUG');

begin
  RxDefaultWriteLog(ALogType, ALogMessage);
  if Assigned(CRPTTrueAPITestMainForm) then
    CRPTTrueAPITestMainForm.Memo1.Lines.Add(sEventNames[ALogType] + ' : ' + ALogMessage);
end;

{$R *.lfm}

{ TCRPTTrueAPITestMainForm }

procedure TCRPTTrueAPITestMainForm.btnLoginClick(Sender: TObject);
begin
(*
  case RadioGroup1.ItemIndex of
    0:CRPTTrueAPI1.Server:=sTrueAPIURL4_sandbox;
    1:CRPTTrueAPI1.Server:=sTrueAPIURL4;
    //1:CRPTTrueAPI1.Server:=sTrueAPIURL3;
  end;
*)
  case RadioGroup1.ItemIndex of
    0:CRPTTrueAPI1.CRPTApiType:=atSandbox;
    1:CRPTTrueAPI1.CRPTApiType:=atProduction;
  end;

  CRPTTrueAPI1.Login;
  if CRPTTrueAPI1.ResultCode = 200 then
  begin
    TabSheet2.TabVisible:=true;
    PageControl1.ActivePageIndex:=1;
    RxWriteLog(etDebug, 'AuthorizationToken = %s', [CRPTTrueAPI1.AuthorizationToken]);
  end;
end;

procedure TCRPTTrueAPITestMainForm.CRPTTrueAPI1HttpStatus(Sender: TCustomCRPTApi
  );
begin
  RxWriteLog(etInfo, '%d: %s', [Sender.ResultCode, Sender.ResultString]);
  if (Sender.ResultCode<>200) and (Sender.ErrorText.Count>0) then
  begin
    Memo2.Lines.Assign(Sender.ErrorText);
    RxWriteLog(etError, 'Error: %s', [Sender.ErrorText.Text]);
  end;
end;

procedure TCRPTTrueAPITestMainForm.CRPTTrueAPI1SignData(Sender: TCustomCRPTApi;
  AData: string; ADetached: Boolean; out ASign: string);
var
  M: TStream;
begin
  OptCryptoServer1.Server:=edtCryptoProSrv.Text;
  ASign:='';
  M:=OptCryptoServer1.SignDoc(edtUserKey.Text, AData, ADetached);
  if Assigned(M) then
  begin
    if M.Size > 0 then
    begin
      M.Position:=0;
      SetLength(ASign, M.Size);
      M.Read(ASign[1], M.Size);
    end;
    M.Free;
  end;
end;

procedure TCRPTTrueAPITestMainForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SavePages;
end;

procedure TCRPTTrueAPITestMainForm.FormCreate(Sender: TObject);
begin
  DoFillProductGroup;
  CreatePages;

  PageControl1.ActivePageIndex:=0;
  TabSheet2.TabVisible:=false;
end;

procedure TCRPTTrueAPITestMainForm.OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
begin
  RxWriteLog(etDebug, 'OptCryptoServer.ResultCode=%d', [Sender.ResultCode]);//
end;

procedure TCRPTTrueAPITestMainForm.TreeView1Click(Sender: TObject);
procedure DoSelectFrame(Cfg: TfrmTrueAPICmdAbstractFrame);
begin
  if Assigned(OldFrame) then
    OldFrame.Visible:=false;
  OldFrame:=Cfg;
  OldFrame.BringToFront;
  OldFrame.Visible:=true;
end;

begin
  if Assigned(TreeView1.Selected) then
  begin
    if Assigned(TreeView1.Selected.Data) then
      DoSelectFrame(TfrmTrueAPICmdAbstractFrame(TreeView1.Selected.Data))
    else
    begin
      if (TreeView1.Selected.Count>0) and Assigned(TreeView1.Selected.GetFirstChild.Data) then
        DoSelectFrame(TfrmTrueAPICmdAbstractFrame(TreeView1.Selected.GetFirstChild.Data))
    end;
  end;
end;

procedure TCRPTTrueAPITestMainForm.DoFillProductGroup;
var
  P: TCRPTProductGroup;
begin
  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  for P in TCRPTProductGroup do
    ComboBox1.Items.Add(CRPTProductGroupStr[P] + ' : ' + CRPTProductGroupNames[P]);
  ComboBox1.Items.EndUpdate;
  ComboBox1.ItemIndex:=Ord(tires);
end;

function TCRPTTrueAPITestMainForm.SelectedGroup: TCRPTProductGroup;
begin
  Result:=TCRPTProductGroup(ComboBox1.ItemIndex);
end;

function TCRPTTrueAPITestMainForm.AddCRPTOperFrame(AGroup: string;
  AFrame: TFrame): TFrame;
procedure DoAddFrame(Cfg:TfrmTrueAPICmdAbstractFrame; RootNode:TTreeNode);
var
  Node:TTreeNode;
begin
  if not Assigned(Cfg) then exit;
  Node:=TreeView1.Items.AddChild(RootNode, Cfg.FrameName);
  Node.Data:=Cfg;
  Cfg.CRPTTrueAPI:=CRPTTrueAPI1;
  Cfg.Parent:=ConfigPanel;
  Cfg.Align:=alClient;

  if not Assigned(TreeView1.Selected) then
    TreeView1.Selected:=Node;
end;

var
  RN: TTreeNode;
begin
  Result:=AFrame;
  RN:=TreeView1.Items.FindNodeWithText(AGroup);
  if not Assigned(RN) then
    RN:=TreeView1.Items.AddChild(nil, AGroup);
  DoAddFrame(AFrame as TfrmTrueAPICmdAbstractFrame, RN);
  TreeView1Click(nil);
end;

procedure TCRPTTrueAPITestMainForm.CreatePages;
var
  Ini: TIniFile;
  P: TTreeNode;
begin
  Ini:=TIniFile.Create(GetDefaultIniName);

  AddCRPTOperFrame('КИЗ', TfrmTrueAPICmdCISFrame.Create(Self));
  AddCRPTOperFrame('КИЗ', TfrmTrueAPICmdCheckCISUnitFrame.Create(Self));
  AddCRPTOperFrame('Финансовое', TfrmTrueAPICmdBalanceFrame.Create(Self));
  AddCRPTOperFrame('Документы', TfrmTrueAPICmdDocListFrame.Create(Self));
  AddCRPTOperFrame('Документы', TfrmTrueAPICmdDocInfoFrame.Create(Self));
  AddCRPTOperFrame('Документы', TfrmTrueAPICmdReceiptListFrame.Create(Self));
  AddCRPTOperFrame('Документы', TfrmTrueAPICmdReceiptInfoFrame.Create(Self));
  AddCRPTOperFrame('Вывод', TfrmTrueAPICmdOutCISFrame.Create(Self));

  for P in TreeView1.Items do
    if Assigned(P.Data) then
      TfrmTrueAPICmdAbstractFrame(P.Data).LoadParams(Ini);
  Ini.Free;
end;

procedure TCRPTTrueAPITestMainForm.SavePages;
var
  Ini: TIniFile;
  P: TTreeNode;
begin
  Ini:=TIniFile.Create(GetDefaultIniName);
  for P in TreeView1.Items do
    if Assigned(P.Data) then
      TfrmTrueAPICmdAbstractFrame(P.Data).SaveParams(Ini);
  Ini.Free;
end;


end.

