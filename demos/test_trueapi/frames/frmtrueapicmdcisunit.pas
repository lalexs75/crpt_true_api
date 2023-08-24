unit frmTrueAPICmdCISUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles,
  frmTrueAPICmdAbstractUnit;

type

  { TfrmTrueAPICmdCISFrame }

  TfrmTrueAPICmdCISFrame = class(TfrmTrueAPICmdAbstractFrame)
    Button2: TButton;
    Button3: TButton;
    edtCIS: TEdit;
    Label3: TLabel;
    procedure Button3Click(Sender: TObject);
  private

  public
    function FrameName:string; override;
    procedure LoadParams(AIni:TIniFile); override;
    procedure SaveParams(AIni:TIniFile); override;
  end;

implementation
uses rxlogging;

{$R *.lfm}

{ TfrmTrueAPICmdCISFrame }

procedure TfrmTrueAPICmdCISFrame.Button3Click(Sender: TObject);
var
  R: TJSONObject;
begin
  R:=FCRPTTrueAPI.ProductsInfo(edtCIS.Text);
  if Assigned(R) then
  begin
    RxWriteLog(etInfo, R.FormatJSON);
    R.Free;
  end;
end;

function TfrmTrueAPICmdCISFrame.FrameName: string;
begin
  Result:='КИЗ';
end;

procedure TfrmTrueAPICmdCISFrame.LoadParams(AIni: TIniFile);
begin
  inherited LoadParams(AIni);
  edtCIS.Text:=AIni.ReadString(ClassName, 'edtCIS_Text', '');
end;

procedure TfrmTrueAPICmdCISFrame.SaveParams(AIni: TIniFile);
begin
  inherited SaveParams(AIni);
  AIni.WriteString(ClassName, 'edtCIS_Text', edtCIS.Text);
end;

end.

