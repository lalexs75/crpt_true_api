{ CRPT TrueAPI interface library for FPC and Lazarus

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

unit CRPTTrueAPIDataObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TLicencesInfo }

  TLicencesInfo = class(TJSONSerializationObject)
  private
    FlicenceDate: string;
    FlicenceNumber: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property licenceNumber:string read FlicenceNumber write FlicenceNumber; //Номер лицензии на пользование недрами
    property licenceDate:string read FlicenceDate write FlicenceDate; //Дата выдачи лицензии
  end;
  TLicencesInfos = specialize GJSONSerializationObjectList<TLicencesInfo>;

  { TOwnerMod }

  TOwnerMod = class(TJSONSerializationObject)
  private
    Faddress: string;
    FfiasId: string;
    Fkpp: string;
    FmodId: Integer;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property modId:Integer read FmodId write FmodId; //Идентификатор текущего места осуществления деятельности
    property kpp:string read Fkpp write Fkpp; //КПП торговой точки
    property fiasId:string read FfiasId write FfiasId; //Идентификатор ФИАС
    property address:string read Faddress write Faddress; //Адрес места осуществления деятельности
  end;

  { TExpirationInfo }

  TExpirationInfo = class(TJSONSerializationObject)
  private
    FexpirationStorageDate: string;
    FstorageConditionId: Integer;
    FstorageConditionName: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property expirationStorageDate:string read FexpirationStorageDate write FexpirationStorageDate; //Дата срока годности
    property storageConditionId:Integer read FstorageConditionId write FstorageConditionId;    //Идентификатор условия хранения
    property storageConditionName:string read FstorageConditionName write FstorageConditionName; //Описание условий хранения
  end;
  TExpirationInfos = specialize GJSONSerializationObjectList<TExpirationInfo>;

  { TPartialSaleInfo }

  TPartialSaleInfo = class(TJSONSerializationObject)
  private
    FcorrectRest: Boolean;
    FinnerUnitCount: Integer;
    Frest: Integer;
    FsoldUnitCount: Integer;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property innerUnitCount:Integer read FinnerUnitCount write FinnerUnitCount; //Общее количество (объём) товара в потребительской упаковке (для товара с возможностью частичного выбытия)
    property soldUnitCount:Integer read FsoldUnitCount write FsoldUnitCount; //Количество (объём) выведенного из оборота товара по КИ, шт (мл)
    property rest:Integer read Frest write Frest; //Количество (объём) товара, доступного к выводу из оборота по КИ, шт (мл)
    property correctRest:Boolean read FcorrectRest write FcorrectRest; //Корректность остатка товара
  end;

  { TCertDoc }

  TCertDoc = class(TJSONSerializationObject)
  private
    FCertType: string;
    Fdate: string;
    Findx: string;
    Fnumber: string;
    FwellNumber: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CertType:string read FCertType write FCertType;
    property number:string read Fnumber write Fnumber;
    property date:string read Fdate write Fdate;
    property wellNumber:string read FwellNumber write FwellNumber;
    property indx:string read Findx write Findx;
  end;
  TCertDocs = specialize GJSONSerializationObjectList<TCertDoc>;

  { TCISInfoData }

  TCISInfoData = class(TJSONSerializationObject)
  private
    FapplicationDate: string;
    Fbrand: string;
    Fchild: TXSDStringArray;
    Fcis: string;
    FcisWithoutBrackets: string;
    FconnectDate: string;
    FemissionDate: string;
    FemissionType: string;
    FerrorCode: string;
    FerrorMessage: string;
    FexpirationDate: string;
    Fexpirations: TExpirationInfos;
    FexporterName: string;
    FgeneralPackageType: string;
    Fgtin: string;
    FintroducedDate: string;
    FisVarQuantity: Boolean;
    Flicences: TLicencesInfos;
    FmarkWithdraw: boolean;
    FmaxRetailPrice: string;
    FownerInn: string;
    FownerMod: TOwnerMod;
    FownerName: string;
    FpackageType: string;
    Fparent: string;
    FpartialSaleInfo: TPartialSaleInfo;
    FproducedDate: string;
    FproducerInn: string;
    FproducerName: string;
    FproductGroup: string;
    FproductGroupId: Integer;
    FproductName: string;
    FproductWeightGr: integer;
    FprVetDocument: string;
    FrequestedCis: string;
    FsertDocs: TCertDocs;
    Fstatus: string;
    FstatusEx: string;
    FtnVedEaes: string;
    FtnVedEaesGroup: string;
    FturnoverType: string;
    FvolumeSpecialPack: string;
    FwithdrawReason: string;
    FwithdrawReasonOther: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
    function EmissionDateToDate:TDateTime;
    function ProducedDateToDate:TDateTime;
  published
    property applicationDate : string read FapplicationDate write FapplicationDate; //Дата нанесения
    property introducedDate:string read FintroducedDate write FintroducedDate;      //Дата ввода воборот
    property requestedCis:string read FrequestedCis write FrequestedCis;            //КИ / КиЗ из запроса
    property cis:string read Fcis write Fcis;                                       //КИ / КиЗ из ответа
    property cisWithoutBrackets:string read FcisWithoutBrackets write FcisWithoutBrackets; //КИ без скобок
    property gtin:string read Fgtin write Fgtin;                                    //Код товара
    property tnVedEaes:string read FtnVedEaes write FtnVedEaes;                     //10-значный код ТН ВЭД
    property tnVedEaesGroup:string read FtnVedEaesGroup write FtnVedEaesGroup;      //4-значный код ТН ВЭД
    property productName:string read FproductName write FproductName;               //Наименование продукции
    property productGroupId : Integer read FproductGroupId write FproductGroupId;   //Идентификатор товарной группы
    property productGroup:string read FproductGroup write FproductGroup;            //Наименование товарной группы
    property brand:string read Fbrand write Fbrand;                                 //Бренд
    property producedDate:string read FproducedDate write FproducedDate;            //Дата производства
    property emissionDate:string read FemissionDate write FemissionDate;            //Дата эмиссии
    property emissionType:string read FemissionType write FemissionType;            //Тип эмиссии
    property packageType:string read FpackageType write FpackageType;               //Код типа упаковки
    property generalPackageType:string read FgeneralPackageType write FgeneralPackageType;
    property ownerInn:string read FownerInn write FownerInn;                        //ИНН владельца товара
    property ownerName:string read FownerName write FownerName;                     //Наименование владельца товара
    property status:string read Fstatus write Fstatus;                              //Статус КИ / КиЗ
    property statusEx:string read FstatusEx write FstatusEx;                        //Актуальное особое состояние КИ / КиЗ
    property child:TXSDStringArray read Fchild write Fchild;                        //Список дочерних КИ в агрегате
    property producerInn:string read FproducerInn write FproducerInn;               //ИНН производителя
    property producerName:string read FproducerName write FproducerName;            //Наименование производителя
    property markWithdraw:boolean read FmarkWithdraw write FmarkWithdraw;           //Признак выбытия от не владельца
    property turnoverType:string read FturnoverType write FturnoverType;            //Вид товарооборота
    property exporterName:string read FexporterName write FexporterName;            //Наименование экспортёра
    property withdrawReason:string read FwithdrawReason write FwithdrawReason;      //Причина вывода из оборота
    property sertDocs:TCertDocs read FsertDocs;                                     //Сведения о разрешительной документации
    property parent:string read Fparent write Fparent;                              //КИ агрегата
    property maxRetailPrice:string read FmaxRetailPrice write FmaxRetailPrice;      //Максимальная цена розничной продажи в копейках
    property volumeSpecialPack:string read FvolumeSpecialPack write FvolumeSpecialPack; //Фактический объём выпущенной продукции, л
    property prVetDocument:string read FprVetDocument write FprVetDocument;         //ID производственного ВСД (для продукции, произведённой в РФ) или ID транспортного ВСД (для продукции, произведённой вне РФ)
    property expirationDate:string read FexpirationDate write FexpirationDate;      //Дата срока годности
    property productWeightGr:integer read FproductWeightGr write FproductWeightGr;  //Вес выпущенной продукции
    property isVarQuantity:Boolean read FisVarQuantity write FisVarQuantity;        //Признак переменного веса
    property partialSaleInfo:TPartialSaleInfo read FpartialSaleInfo;                //Сведения о частичном выбытии
    property expirations:TExpirationInfos read Fexpirations;                        //Массив сроков годности
    property withdrawReasonOther:string read FwithdrawReasonOther write FwithdrawReasonOther; //Описание другой причины вывода из оборота
    property connectDate:string read FconnectDate write FconnectDate;               //Дата подключения кега к оборудованию для розлива
    property ownerMod:TOwnerMod read FownerMod;                                     //Сведения о текущем месте осуществления деятельности
    property licences:TLicencesInfos read Flicences;                                //Массив лицензий на пользование недрами
    property errorMessage:string read FerrorMessage write FerrorMessage;            //Сообщение об ошибке
    property errorCode:string read FerrorCode write FerrorCode;                     //Код ошибки
  end;

  { TCISInfo }

  TCISInfo = class(TJSONSerializationObject)
  private
    FcisInfo: TCISInfoData;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cisInfo:TCISInfoData read FcisInfo;
  end;
  TCISInfos = specialize GJSONSerializationObjectList<TCISInfo>;

  { TDocsItem }

  TDocsItem = class(TJSONSerializationObject)
  private
    F: string;
    FdocDate: string;
    FDocErrors: TXSDStringArray;
    FDocType: string;
    FdownloadDesc: string;
    Finput: Boolean;
    FinvoiceNumber: string;
    Fnumber: string;
    FreceivedAt: string;
    FreceiverInn: string;
    FreceiverName: string;
    FrelatedDocId: string;
    FsenderInn: string;
    FsenderName: string;
    Fstatus: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property number:string read Fnumber write Fnumber;
    property docDate:string read FdocDate write FdocDate;
    property receivedAt:string read FreceivedAt write FreceivedAt;
    property DocType:string read FDocType write FDocType;
    property status:string read Fstatus write Fstatus;
    property senderInn:string read FsenderInn write FsenderInn;
    property senderName:string read FsenderName write FsenderName;
    property receiverInn:string read FreceiverInn write FreceiverInn;
    property receiverName:string read FreceiverName write FreceiverName;
    property invoiceNumber:string read FinvoiceNumber write FinvoiceNumber;
    property relatedDocId:string read FrelatedDocId write FrelatedDocId;
    property downloadDesc:string read FdownloadDesc write FdownloadDesc;
    property input:Boolean read Finput write Finput;
    property DocErrors:TXSDStringArray read FDocErrors write FDocErrors;
    property productGroup:string read F write F;
    property productGroupId:string read F write F;
  end;

  TDocsItems = specialize GJSONSerializationObjectList<TDocsItem>;

  { TDocList }

  TDocList = class(TJSONSerializationObject)
  private
    FnextPage: Boolean;
    FResultItems: TDocsItems;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ResultItems:TDocsItems read FResultItems;
    property nextPage:Boolean read FnextPage write FnextPage;
  end;


  { TReceiptItem }

  TReceiptItem = class(TJSONSerializationObject)
  private
    Fdid: string;
    FproductGroup: string;
    FproductGroupId: string;
    FreceiptDate: string;
    FReceiptType: string;
    FreceivedAt: string;
    FsenderInn: string;
    FsenderName: string;
    Fstatus: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property did:string read Fdid write Fdid;
    property receiptDate:string read FreceiptDate write FreceiptDate;
    property receivedAt:string read FreceivedAt write FreceivedAt;
    property ReceiptType:string read FReceiptType write FReceiptType;
    property status:string read Fstatus write Fstatus;
    property senderInn:string read FsenderInn write FsenderInn;
    property senderName:string read FsenderName write FsenderName;
    property productGroup:string read FproductGroup write FproductGroup;
    property productGroupId:string read FproductGroupId write FproductGroupId;
  end;
  TReceiptItems = specialize GJSONSerializationObjectList<TReceiptItem>;

  { TReceiptList }

  TReceiptList = class(TJSONSerializationObject)
  private
    FnextPage: Boolean;
    FResultItems: TReceiptItems;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ResultItems:TReceiptItems read FResultItems;
    property nextPage:Boolean read FnextPage write FnextPage;
  end;

implementation
uses {fpjson, jsonparser, }sdo_date_utils;

{ TLicencesInfo }

procedure TLicencesInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('licenceNumber');
  RegisterProperty('licenceDate');
end;

procedure TLicencesInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLicencesInfo.Destroy;
begin
  inherited Destroy;
end;

{ TOwnerMod }

procedure TOwnerMod.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('modId');
  RegisterProperty('kpp');
  RegisterProperty('fiasId');
  RegisterProperty('address');
end;

procedure TOwnerMod.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TOwnerMod.Destroy;
begin
  inherited Destroy;
end;

{ TExpirationInfo }

procedure TExpirationInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('expirationStorageDate');
  RegisterProperty('storageConditionId');
  RegisterProperty('storageConditionName');
end;

procedure TExpirationInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TExpirationInfo.Destroy;
begin
  inherited Destroy;
end;

{ TPartialSaleInfo }

procedure TPartialSaleInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('innerUnitCount');
  RegisterProperty('soldUnitCount');
  RegisterProperty('rest');
  RegisterProperty('correctRest');
end;

procedure TPartialSaleInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPartialSaleInfo.Destroy;
begin
  inherited Destroy;
end;

{ TCertDoc }

procedure TCertDoc.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('CertType', 'type', [], '', -1, -1);
  RegisterProperty('number');
  RegisterProperty('date');
  RegisterProperty('wellNumber');
  RegisterProperty('indx');
end;

procedure TCertDoc.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCertDoc.Destroy;
begin
  inherited Destroy;
end;

{ TCISInfoData }
procedure TCISInfoData.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('applicationDate', 'applicationDate', [], '', -1, -1);
  RegisterProperty('introducedDate', 'introducedDate', [], '', -1, -1);
  RegisterProperty('requestedCis', 'requestedCis', [], '', -1, -1);
  RegisterProperty('cis', 'cis', [], '', -1, -1);
  RegisterProperty('cisWithoutBrackets', 'cisWithoutBrackets', [], '', -1, -1);
  RegisterProperty('gtin', 'gtin', [], '', -1, -1);
  RegisterProperty('tnVedEaes', 'tnVedEaes', [], '', -1, -1);
  RegisterProperty('tnVedEaesGroup', 'tnVedEaesGroup', [], '', -1, -1);
  RegisterProperty('productName', 'productName', [], '', -1, -1);
  RegisterProperty('productGroupId', 'productGroupId', [], '', -1, -1);
  RegisterProperty('productGroup', 'productGroup', [], '', -1, -1);
  RegisterProperty('brand', 'brand', [], '', -1, -1);
  RegisterProperty('producedDate', 'producedDate', [], '', -1, -1);
  RegisterProperty('emissionDate', 'emissionDate', [], '', -1, -1);
  RegisterProperty('emissionType', 'emissionType', [], '', -1, -1);
  RegisterProperty('packageType', 'packageType', [], '', -1, -1);
  RegisterProperty('generalPackageType', 'generalPackageType', [], '', -1, -1);
  RegisterProperty('ownerInn', 'ownerInn', [], '', -1, -1);
  RegisterProperty('ownerName', 'ownerName', [], '', -1, -1);
  RegisterProperty('status', 'status', [], '', -1, -1);
  RegisterProperty('statusEx', 'statusEx', [], '', -1, -1);
  RegisterProperty('child');
  RegisterProperty('producerInn', 'producerInn', [], '', -1, -1);
  RegisterProperty('producerName', 'producerName', [], '', -1, -1);
  RegisterProperty('markWithdraw', 'markWithdraw', [], '', -1, -1);
  RegisterProperty('withdrawReason', 'withdrawReason', [], '', -1, -1);
  RegisterProperty('exporterName');
  RegisterProperty('turnoverType');
  RegisterProperty('sertDocs');

  RegisterProperty('parent');
  RegisterProperty('maxRetailPrice');
  RegisterProperty('volumeSpecialPack');
  RegisterProperty('prVetDocument');
  RegisterProperty('expirationDate');
  RegisterProperty('productWeightGr');
  RegisterProperty('isVarQuantity');
  RegisterProperty('partialSaleInfo');
  RegisterProperty('expirations');
  RegisterProperty('withdrawReasonOther');
  RegisterProperty('connectDate');
  RegisterProperty('ownerMod');
  RegisterProperty('licences');
  RegisterProperty('errorMessage');
  RegisterProperty('errorCode');
end;

procedure TCISInfoData.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FsertDocs:=TCertDocs.Create;
end;

destructor TCISInfoData.Destroy;
begin
  FreeAndNil(FsertDocs);
  inherited Destroy;
end;

function TCISInfoData.EmissionDateToDate: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FemissionDate, R, xdkDateTime) then
    Result:=R.Date
  else
    Result:=0;
end;

function TCISInfoData.ProducedDateToDate: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FproducedDate, R, xdkDateTime) then
    Result:=R.Date
  else
    Result:=0;
end;

{ TCISInfo }

procedure TCISInfo.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('cisInfo', 'cisInfo', [], '', -1, -1);
end;

procedure TCISInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FcisInfo:=TCISInfoData.Create;
end;

destructor TCISInfo.Destroy;
begin
  FreeAndNil(FcisInfo);
  inherited Destroy;
end;

{ TDocsItem }

procedure TDocsItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty( 'number', 'number', [], '', -1, -1);
  RegisterProperty( 'docDate', 'docDate', [], '', -1, -1);
  RegisterProperty( 'receivedAt','receivedAt', [], '', -1, -1);
  RegisterProperty( 'DocType', 'type', [], '', -1, -1);
  RegisterProperty( 'status', 'status', [], '', -1, -1);
  RegisterProperty( 'senderInn','senderInn', [], '', -1, -1);
  RegisterProperty( 'senderName','senderName', [], '', -1, -1);
  RegisterProperty( 'receiverInn', 'receiverInn', [], '', -1, -1);
  RegisterProperty( 'receiverName', 'receiverName', [], '', -1, -1);
  RegisterProperty( 'invoiceNumber', 'invoiceNumber', [], '', -1, -1);
  RegisterProperty( 'relatedDocId', 'relatedDocId', [], '', -1, -1);
  RegisterProperty( 'downloadDesc', 'downloadDesc', [], '', -1, -1);
  RegisterProperty( 'input', 'input', [], '', -1, -1);
  RegisterProperty( 'DocErrors', 'errors', [], '', -1, -1);
  RegisterProperty( 'productGroup', 'productGroup', [], '', -1, -1);
  RegisterProperty( 'productGroupId', 'productGroupId', [], '', -1, -1);
end;

procedure TDocsItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TDocsItem.Destroy;
begin
  inherited Destroy;
end;

{ TDocList }

procedure TDocList.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ResultItems', 'results', [], '', -1, -1);
  RegisterProperty('nextPage', 'nextPage', [], '', -1, -1);
end;

procedure TDocList.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FResultItems:=TDocsItems.Create;
end;

destructor TDocList.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FResultItems);
end;

{ TReceiptItem }

procedure TReceiptItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('did');
  RegisterProperty('receiptDate');
  RegisterProperty('receivedAt');
  RegisterProperty('ReceiptType', 'type', [], '', -1, -1);
  RegisterProperty('status');
  RegisterProperty('senderInn');
  RegisterProperty('senderName');
  RegisterProperty('productGroup');
  RegisterProperty('productGroupId');
end;

procedure TReceiptItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TReceiptItem.Destroy;
begin
  inherited Destroy;
end;

{ TReceiptList }

procedure TReceiptList.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ResultItems', 'results', [], '', -1, -1);
  RegisterProperty('nextPage', 'nextPage', [], '', -1, -1);
end;

procedure TReceiptList.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FResultItems:=TReceiptItems.Create;
end;

destructor TReceiptList.Destroy;
begin
  FreeAndNil(FResultItems);
  inherited Destroy;
end;

end.

