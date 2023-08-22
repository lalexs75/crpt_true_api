program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  opensslsockets,
  rxnew,
  ssl_openssl,
  ssl_openssl_lib,
  rxlogging,
  unit1, unit2
  { you can add units after this };

{$R *.res}

var
  B: Boolean;
begin
  B:=IsSSLloaded;

  InitRxLogs;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCRPTTrueAPITestKIZMainForm, CRPTTrueAPITestKIZMainForm
    );
  Application.Run;
end.

