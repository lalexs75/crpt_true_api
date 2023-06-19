unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CRPTTrueAPI, ocrsConnectionUnit, RxIniPropStorage;

type

  { TCRPTTrueAPITestKIZMainForm }

  TCRPTTrueAPITestKIZMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CRPTTrueAPI1: TCRPTTrueAPI;
    edtCIS: TEdit;
    edtUserKey: TEdit;
    edtCryptoProSrv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    RxIniPropStorage1: TRxIniPropStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
    procedure CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
      ASign: string);
    procedure OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
  private

  public

  end;

var
  CRPTTrueAPITestKIZMainForm: TCRPTTrueAPITestKIZMainForm;

implementation

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

procedure TCRPTTrueAPITestKIZMainForm.OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
begin
  //
end;

end.

