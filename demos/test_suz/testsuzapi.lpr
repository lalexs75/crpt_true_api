{ CRPT SUZ interface library demo for FPC and Lazarus

  Copyright (C) 2023 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


program TestSUZApi;

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
  lazcontrols,
  rxlogging,

  CRPTSuzTestMainUnit,

  frmSUZCmdAbstractUnit,
  frmSUZCmdServiceUnit,
  frmSUZCmdOrderUnit,
  frmSUZCmdServiceProvidersListUnit,
  frmSUZCmdOrderStatusUnit,
  frmSUZCmdOrderListUnit,
  frmSUZCmdCodesFromOrderUnit,
  frmSUZCmdCodesBlocksRetryUnit,
  frmSUZCmdCodesBlocksUnit,
  frmSUZCmdOrderCloseUnit,
  frmSUZCmdReceiptDocUnit,
  frmSUZCmdReceiptSearchUnit,
  frmSUZCmdReceiptGetDocUnit,
  frmSUZCmdDocumentsSearchUnit,
  frmSUZCmdQualitySearchUnit,
  frmSUZCmdQualityCisListUnit,
  frmSUZCmdOrderProductUnit,
  frmSUZCmdPingUnit;

{$R *.res}

begin
  OnRxLoggerEvent:=@RxLogWriter;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCRPTSuzTestForm, CRPTSuzTestForm);
  Application.Run;
end.

