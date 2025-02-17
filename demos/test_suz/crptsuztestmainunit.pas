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

unit CRPTSuzTestMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ocrsConnectionUnit, CRPTTrueAPI, CRPTSuzAPI,
  RxIniPropStorage, CRPTTrueAPI_Consts;

type

  { TCRPTSuzTestForm }

  TCRPTSuzTestForm = class(TForm)
    btnLogin: TButton;
    ComboBox1: TComboBox;
    CRPTSuzAPI1: TCRPTSuzAPI;
    edtOMSID: TEdit;
    edtOMSConnection: TEdit;
    edtCryptoProSrv: TEdit;
    edtUserKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    ConfigPanel: TPanel;
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
    procedure CRPTSuzAPI1HttpStatus(Sender: TCustomCRPTApi);
    procedure CRPTSuzAPI1SignData(Sender: TCustomCRPTApi; AData: string;
      ADetached: Boolean; out ASign: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    OldFrame:TFrame;
    procedure DoFillProductGroup;
    function SelectedGroup:TCRPTProductGroup;
    function AddCRPTOperFrame(AGroup:string; AFrame:TFrame):TFrame;
    procedure CreatePages;
    procedure SavePages;
    procedure AfterLogin;
  public

  end;

var
  CRPTSuzTestForm: TCRPTSuzTestForm;

procedure RxLogWriter(ALogType: TEventType; const ALogMessage: string);
implementation
uses rxlogging, IniFiles, rxAppUtils, frmSUZCmdAbstractUnit,
  frmSUZCmdServiceUnit, frmSUZCmdOrderUnit, frmSUZCmdServiceProvidersListUnit,
  frmSUZCmdOrderStatusUnit, frmSUZCmdOrderListUnit, frmSUZCmdCodesFromOrderUnit,
  frmSUZCmdCodesBlocksRetryUnit, frmSUZCmdCodesBlocksUnit,
  frmSUZCmdOrderCloseUnit, frmSUZCmdReceiptDocUnit, frmSUZCmdReceiptSearchUnit,
  frmSUZCmdReceiptGetDocUnit, frmSUZCmdDocumentsSearchUnit,
  frmSUZCmdQualitySearchUnit, frmSUZCmdQualityCisListUnit,
  frmSUZCmdOrderProductUnit, frmSUZCmdPingUnit;

{$R *.lfm}

procedure RxLogWriter(ALogType: TEventType; const ALogMessage: string);
const
  sEventNames : array [TEventType] of string =
    ('CUSTOM','INFO','WARNING','ERROR','DEBUG');

begin
  RxDefaultWriteLog(ALogType, ALogMessage);
  if Assigned(CRPTSuzTestForm) then
    CRPTSuzTestForm.Memo1.Lines.Add(sEventNames[ALogType] + ' : ' + ALogMessage);
end;


{ TCRPTSuzTestForm }

procedure TCRPTSuzTestForm.btnLoginClick(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0:CRPTSuzAPI1.CRPTApiType:=atSandbox;
    1:CRPTSuzAPI1.CRPTApiType:=atProduction;
  end;
  CRPTSuzAPI1.OMSConnection:=edtOMSConnection.Text;
  CRPTSuzAPI1.OmsID:=edtOMSID.Text;
  CRPTSuzAPI1.Login;
  if CRPTSuzAPI1.ResultCode = 200 then
  begin
    TabSheet2.TabVisible:=true;
    PageControl1.ActivePageIndex:=1;
    RxWriteLog(etDebug, 'AuthorizationToken = %s', [CRPTSuzAPI1.AuthorizationToken]);

    AfterLogin;
  end;
end;

procedure TCRPTSuzTestForm.CRPTSuzAPI1HttpStatus(Sender: TCustomCRPTApi);
begin
  RxWriteLog(etDebug, '%d: %s', [Sender.ResultCode, Sender.ResultString]);
  if (Sender.ResultCode<>200) and (Sender.ErrorText.Count>0) then
  begin
    Memo2.Lines.Assign(Sender.ErrorText);
    RxWriteLog(etError, 'Error: %s', [Sender.ErrorText.Text]);
  end
  else
    Memo2.Lines.Clear;
end;

procedure TCRPTSuzTestForm.CRPTSuzAPI1SignData(Sender: TCustomCRPTApi;
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

procedure TCRPTSuzTestForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SavePages;
end;

procedure TCRPTSuzTestForm.FormCreate(Sender: TObject);
begin
  DoFillProductGroup;
  CreatePages;

  PageControl1.ActivePageIndex:=0;
  TabSheet2.TabVisible:=false;
end;

procedure TCRPTSuzTestForm.TreeView1Click(Sender: TObject);
procedure DoSelectFrame(Cfg: TfrmSUZCmdAbstractFrame);
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
      DoSelectFrame(TfrmSUZCmdAbstractFrame(TreeView1.Selected.Data))
    else
    begin
      if (TreeView1.Selected.Count>0) and Assigned(TreeView1.Selected.GetFirstChild.Data) then
        DoSelectFrame(TfrmSUZCmdAbstractFrame(TreeView1.Selected.GetFirstChild.Data))
    end;
  end;
end;

procedure TCRPTSuzTestForm.DoFillProductGroup;
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

function TCRPTSuzTestForm.SelectedGroup: TCRPTProductGroup;
begin
  Result:=TCRPTProductGroup(ComboBox1.ItemIndex);
end;

function TCRPTSuzTestForm.AddCRPTOperFrame(AGroup: string; AFrame: TFrame
  ): TFrame;
procedure DoAddFrame(Cfg:TfrmSUZCmdAbstractFrame; RootNode:TTreeNode);
var
  Node:TTreeNode;
begin
  if not Assigned(Cfg) then exit;
  Node:=TreeView1.Items.AddChild(RootNode, Cfg.FrameName);
  Node.Data:=Cfg;
  Cfg.CRPTSuzAPI:=CRPTSuzAPI1;
  Cfg.Parent:=ConfigPanel;
  Cfg.Align:=alClient;

  if TreeView1.Selected = nil then
    TreeView1.Selected := Node;
end;

var
  RN: TTreeNode;
begin
  Result:=AFrame;
  RN:=TreeView1.Items.FindNodeWithText(AGroup);
  if not Assigned(RN) then
    RN:=TreeView1.Items.AddChild(nil, AGroup);
  DoAddFrame(AFrame as TfrmSUZCmdAbstractFrame, RN)
end;

procedure TCRPTSuzTestForm.CreatePages;
var
  Ini: TIniFile;
  P: TTreeNode;
begin
  Ini:=TIniFile.Create(GetDefaultIniName);
  AddCRPTOperFrame('Общее', TfrmSUZCmdPingFrame.Create(Self));
  AddCRPTOperFrame('Общее', TfrmSUZCmdServiceFrame.Create(Self));
  AddCRPTOperFrame('Общее', TfrmSUZCmdServiceProvidersListFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdOrderFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdOrderListFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdOrderStatusFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdCodesFromOrderFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdCodesBlocksFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdCodesBlocksRetryFrame.Create(Self));
  AddCRPTOperFrame('Заказ маркировки', TfrmSUZCmdOrderCloseFrame.Create(Self));
  AddCRPTOperFrame('Квитанции', TfrmSUZCmdReceiptDocFrame.Create(Self));
  AddCRPTOperFrame('Квитанции', TfrmSUZCmdReceiptSearchFrame.Create(Self));
  AddCRPTOperFrame('Квитанции', TfrmSUZCmdReceiptGetDocFrame.Create(Self));
  AddCRPTOperFrame('Документ', TfrmSUZCmdDocumentsSearchFrame.Create(Self));
  AddCRPTOperFrame('Отчёты', TfrmSUZCmdQualitySearchFrame.Create(Self));
  AddCRPTOperFrame('Отчёты', TfrmSUZCmdQualityCisListFrame.Create(Self));
  AddCRPTOperFrame('Отчёты', TfrmSUZCmdOrderProductFrame.Create(Self));

  for P in TreeView1.Items do
    if Assigned(P.Data) then
      TfrmSUZCmdAbstractFrame(P.Data).LoadParams(Ini);
  Ini.Free;

  TreeView1Click(nil);
end;

procedure TCRPTSuzTestForm.SavePages;
var
  Ini: TIniFile;
  P: TTreeNode;
begin
  Ini:=TIniFile.Create(GetDefaultIniName);
  for P in TreeView1.Items do
    if Assigned(P.Data) then
      TfrmSUZCmdAbstractFrame(P.Data).SaveParams(Ini);
  Ini.Free;
end;

procedure TCRPTSuzTestForm.AfterLogin;
var
  P: TTreeNode;
begin
  for P in TreeView1.Items do
    if Assigned(P.Data) then
      TfrmSUZCmdAbstractFrame(P.Data).AppLogin;
end;

end.

