unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, CRPTTrueAPI, RxIniPropStorage, RxDBGrid, rxmemds,
  crptCDNTrueAPI, crptCDNTrueAPITypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CDNCrptAPI1: TCDNCrptAPI;
    cbCheckHealth: TCheckBox;
    dsCDNList: TDataSource;
    Edit1: TEdit;
    Edit2: TEdit;
    edtCDNToken: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rxCDNListavgTimeMs: TLongintField;
    rxCDNListHost: TStringField;
    rxCDNListSiteErrorCode: TLongintField;
    rxCDNListSiteErrorDescription: TStringField;
    RxDBGrid1: TRxDBGrid;
    RxIniPropStorage1: TRxIniPropStorage;
    rxCDNList: TRxMemoryData;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CDNCrptAPI1HttpStatus(Sender: TCustomCRPTApi);
  private

  public

  end;

var
  Form1: TForm1 = nil;

procedure AppLogEvent( ALogType:TEventType; const ALogMessage:string);
implementation
uses rxlogging, rxAppUtils;

{$R *.lfm}

procedure AppLogEvent( ALogType:TEventType; const ALogMessage:string);
const
  sEventNames : array [TEventType] of string =
    ('CUSTOM','INFO','WARNING','ERROR','DEBUG');
begin
  RxDefaultWriteLog(ALogType, ALogMessage);
  if Assigned(Form1) then
    Form1.Memo2.Lines.Add('|%s| %20s |%s', [sEventNames[ALogType], DateTimeToStr(Now), ALogMessage]);
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  J: TJSONData;
  P: TCDNInfo;
  PP: TCDNSiteInfo;
  I: Integer;
begin
  rxCDNList.CloseOpen;
  if edtCDNToken.Text <> '' then
  begin
    CDNCrptAPI1.AuthorizationToken:=edtCDNToken.Text;
    P:=CDNCrptAPI1.CDNListInfo(cbCheckHealth.Checked);
    if Assigned(P) then
    begin
      Edit1.Text:=P.Code.ToString;
      Edit2.Text:=P.Description;
      //Memo1.Lines.:=CDNCrptAPI1.Document.;
      for I:=0 to P.Hosts.Count-1 do
      begin
        PP:=P.Hosts[i];
        rxCDNList.Append;
        rxCDNListHost.AsString:=PP.Host;
        rxCDNListavgTimeMs.AsInteger:=PP.avgTimeMs;
        rxCDNListSiteErrorCode.AsInteger:=PP.Code;
        rxCDNListSiteErrorDescription.AsString:=PP.Description;
        rxCDNList.Post;
      end;
      rxCDNList.First;
      P.Free;
    end;
  end
  else
    ErrorBox('Необходимо указать API ключ');
end;

procedure TForm1.CDNCrptAPI1HttpStatus(Sender: TCustomCRPTApi);
var
  S: String;
begin
  S:=Format('%d: %s', [Sender.ResultCode, Sender.ResultString]);
  Memo2.Lines.Text:=S +  LineEnding + Sender.ErrorText.Text;
  RxWriteLog(etDebug, Memo2.Lines.Text);
end;

end.

//
