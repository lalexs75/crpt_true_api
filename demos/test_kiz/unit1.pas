unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CRPTTrueAPI, ocrsConnectionUnit, crpt_cmp, RxIniPropStorage, Unit2;

type

  { TCRPTTrueAPITestKIZMainForm }

  TCRPTTrueAPITestKIZMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CRPTComponent1: TCRPTComponent;
    CRPTTrueAPI1: TCRPTTrueAPI;
    edtOMSID: TEdit;
    edtCIS: TEdit;
    edtUserKey: TEdit;
    edtCryptoProSrv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    RxIniPropStorage1: TRxIniPropStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
    procedure CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
      ASign: string);
    procedure FormCreate(Sender: TObject);
    procedure OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
  private
    FCRPTSuzAPI:TCRPTSuzAPI;
    procedure CRPTSuzAPI1HttpStatus(Sender: TCRPTSuzAPI);
  public

  end;

var
  CRPTTrueAPITestKIZMainForm: TCRPTTrueAPITestKIZMainForm;

implementation
uses rxlogging;

{$R *.lfm}

{ TCRPTTrueAPITestKIZMainForm }

procedure TCRPTTrueAPITestKIZMainForm.Button1Click(Sender: TObject);
begin
  CRPTTrueAPI1.Login;//
end;

procedure TCRPTTrueAPITestKIZMainForm.Button2Click(Sender: TObject);
var
  J: TJSONObject;
begin
  J:=CRPTTrueAPI1.ProductsInfo(edtCIS.Text);
  if Assigned(J) then
  begin
    Memo1.Lines.Add(J.AsJSON);
    J.Free;
  end;
end;

procedure TCRPTTrueAPITestKIZMainForm.Button4Click(Sender: TObject);
begin
  CRPTTrueAPI1.Login;
  FCRPTSuzAPI.AuthorizationToken:=CRPTTrueAPI1.AuthorizationToken;
  RxWriteLog(etDebug, 'TOKEN = %s', [CRPTTrueAPI1.AuthorizationToken]);
  FCRPTSuzAPI.OmsId:=edtOMSID.Text;
  FCRPTSuzAPI.LoadOrdersStatus(tires);
end;

procedure TCRPTTrueAPITestKIZMainForm.CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
begin
  Memo1.Lines.Add('%d: %s', [Sender.ResultCode, Sender.ResultString]);
  Memo1.Lines.Add('ResultText:');
  Memo1.Lines.Add(Sender.ResultText.Text);
end;

procedure TCRPTTrueAPITestKIZMainForm.CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
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

procedure TCRPTTrueAPITestKIZMainForm.FormCreate(Sender: TObject);
begin
  FCRPTSuzAPI:=TCRPTSuzAPI.Create(Self);
  FCRPTSuzAPI.OnHttpStatus:=@CRPTSuzAPI1HttpStatus;
end;

procedure TCRPTTrueAPITestKIZMainForm.OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
begin
  //
end;

procedure TCRPTTrueAPITestKIZMainForm.CRPTSuzAPI1HttpStatus(Sender: TCRPTSuzAPI
  );
begin
  Memo1.Lines.Add('%d: %s', [Sender.ResultCode, Sender.ResultString]);
  Memo1.Lines.Add('ResultText:');
  Memo1.Lines.Add(Sender.ResultText.Text);
//  if Sender.HTTP.;
end;

end.

