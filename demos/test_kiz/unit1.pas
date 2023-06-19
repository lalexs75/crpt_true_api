unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CRPTTrueAPI,
  ocrsConnectionUnit, RxIniPropStorage;

type

  { TCRPTTrueAPITestMainForm }

  TCRPTTrueAPITestMainForm = class(TForm)
    Button1: TButton;
    CRPTTrueAPI1: TCRPTTrueAPI;
    edtUserKey: TEdit;
    edtCryptoProSrv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    OptCryptoServer1: TOptCryptoServer;
    RxIniPropStorage1: TRxIniPropStorage;
    procedure Button1Click(Sender: TObject);
    procedure CRPTTrueAPI1HttpStatus(Sender: TCRPTTrueAPI);
    procedure CRPTTrueAPI1SignData(Sender: TCRPTTrueAPI; AData: string; out
      ASign: string);
    procedure OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
  private

  public

  end;

var
  CRPTTrueAPITestMainForm: TCRPTTrueAPITestMainForm;

implementation

{$R *.lfm}

{ TCRPTTrueAPITestMainForm }

procedure TCRPTTrueAPITestMainForm.Button1Click(Sender: TObject);
begin
  CRPTTrueAPI1.Login;//
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

procedure TCRPTTrueAPITestMainForm.OptCryptoServer1HttpStatus(Sender: TOptCryptoServer);
begin
  //
end;

end.

