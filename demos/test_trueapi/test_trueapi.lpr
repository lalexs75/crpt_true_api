program test_trueapi;

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
  rxlogging, lazcontrols,
  trueapi_mainunit
  { you can add units after this };

{$R *.res}

begin
  OnRxLoggerEvent:=@RxLogWriter;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCRPTTrueAPITestMainForm, CRPTTrueAPITestMainForm);
  Application.Run;
end.

