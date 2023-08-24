unit TrueAPI_MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, CRPTTrueAPI, ocrsConnectionUnit, DividerBevel, crpt_cmp,
  RxIniPropStorage, sslsockets, ssockets, fpjson
  ;

type

  { TCRPTTrueAPITestMainForm }

  TCRPTTrueAPITestMainForm = class(TForm)
    btnLogin: TButton;
    Button2: TButton;
    Button3: TButton;
    CRPTTrueAPI1: TCRPTTrueAPI;
    DividerBevel1: TDividerBevel;
    edtCIS: TEdit;
    edtUserKey: TEdit;
    edtCryptoProSrv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    PageControl1: TPageControl;
    RadioGroup1: TRadioGroup;
    RxIniPropStorage1: TRxIniPropStorage;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnLoginClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
    procedure CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
      ASign: string);
    procedure FormCreate(Sender: TObject);
    procedure OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
  private
  public

  end;

var
  CRPTTrueAPITestMainForm: TCRPTTrueAPITestMainForm;

procedure RxLogWriter( ALogType:TEventType; const ALogMessage:string);
implementation
uses
  rxlogging,
  fphttpclient,
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
  case RadioGroup1.ItemIndex of
    0:CRPTTrueAPI1.Server:=sAPIURL_sandbox;
    1:CRPTTrueAPI1.Server:=sAPIURL;
  end;

  CRPTTrueAPI1.Login;
  if CRPTTrueAPI1.ResultCode = 200 then
  begin
    TabSheet2.TabVisible:=true;
    PageControl1.ActivePageIndex:=1;
    RxWriteLog(etDebug, 'AuthorizationToken = %s', [CRPTTrueAPI1.AuthorizationToken]);
  end;
end;

procedure TCRPTTrueAPITestMainForm.Button3Click(Sender: TObject);
var
  R: TJSONObject;
begin
  R:=CRPTTrueAPI1.ProductsInfo(edtCIS.Text);
  if Assigned(R) then
  begin
    RxWriteLog(etInfo, R.FormatJSON);
    R.Free;
  end;
end;

procedure TCRPTTrueAPITestMainForm.CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
begin
  Memo1.Lines.Add('%d: %s', [Sender.ResultCode, Sender.ResultString]);
end;

procedure TCRPTTrueAPITestMainForm.CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
  ASign: string);
var
  ADetached: Boolean;
  M: TStream;
begin
  OptCryptoServer1.Server:=edtCryptoProSrv.Text;
  ADetached:=false;
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

procedure TCRPTTrueAPITestMainForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  TabSheet2.TabVisible:=false;
  Memo1.Lines.Clear;
end;

procedure TCRPTTrueAPITestMainForm.OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
begin
  RxWriteLog(etDebug, 'OptCryptoServer.ResultCode=%d', [Sender.ResultCode]);//
end;


end.

