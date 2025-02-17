{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit crpt_true_api;

{$warn 5023 off : no warning about unused units}
interface

uses
  CRPTTrueAPI, CRPTTrueAPI_Consts, CRPTTrueAPIDataObjects, CRPTTrueAPIGlobals, CRPTSuzIntegration, LazCRPTRegister, CRPTSuzAPI, 
  crptCDNTrueAPI, crptCDNTrueAPITypes, crptLocalTrueAPITypesUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazCRPTRegister', @LazCRPTRegister.Register);
end;

initialization
  RegisterPackage('crpt_true_api', @Register);
end.
