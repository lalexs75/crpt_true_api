unit frmTrueAPICmdOutCISUnit;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, frmTrueAPICmdAbstractUnit, IniFiles;

type

  { TfrmTrueAPICmdOutCISFrame }

  TfrmTrueAPICmdOutCISFrame = class(TfrmTrueAPICmdAbstractFrame)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public
    function FrameName:string; override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
  end;

implementation
uses CRPTTrueAPIDataObjects, LazFileUtils;

{$R *.lfm}

{ TfrmTrueAPICmdOutCISFrame }

procedure TfrmTrueAPICmdOutCISFrame.Button1Click(Sender: TObject);
var
  LK: TLK_RECEIPT;
  P: TLK_RECEIPT_ITEM;
begin
  LK:=TLK_RECEIPT.Create;
  LK.inn:='1111111111';
  LK.buyer_inn:='7777777777';
  LK.action:='OTHER';
  LK.withdrawal_type_other:='Any text up to 255 characters';
  LK.action_date:=EncodeDate(2000, 1, 1);
  LK.document_type:='OTHER';
  LK.document_number:='00000';
  LK.document_date:=EncodeDate(2000, 1, 20);
  LK.primary_document_custom_name:='Also any text up to 255 characters';
  LK.fias_id:='0bcec0d0-00bc-0e00-b00c-ba0c6f0a0c00';

  P:=LK.products.AddItem;
  P.cis:='010111111111111111lBEtVuGyhA0HO';
  P.product_cost:=10000;

  P:=LK.products.AddItem;
  P.cis:='010111111111111111JUbiOuE9=NuEG';
  P.product_cost:=200000;

  P:=LK.products.AddItem;
  P.cis:='010111111111111111%IfhPcJ/fQxsS';
  P.product_cost:=300000;

  P:=LK.products.AddItem;
  P.cis:='0101111111111111110unznYOtprzN,';
  P.product_cost:=70000;

  LK.SaveToFile(AppendPathDelim(GetTempDir) + 'aaa.json');
  LK.Free;
end;

function TfrmTrueAPICmdOutCISFrame.FrameName: string;
begin
  Result:='Вывод из оборота';
end;

procedure TfrmTrueAPICmdOutCISFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
end;

procedure TfrmTrueAPICmdOutCISFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
end;

end.

