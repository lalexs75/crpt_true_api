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
  Classes, SysUtils, xmlobject, JSONObjects, AbstractSerializationObjects;

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
    constructor Create; override;
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
    FerrorCode: string;
    FerrorMessage: string;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cisInfo:TCISInfoData read FcisInfo;
    property errorMessage:string read FerrorMessage write FerrorMessage;            //Сообщение об ошибке
    property errorCode:string read FerrorCode write FerrorCode;                     //Код ошибки
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

  { TLK_RECEIPT_ITEM }

  TLK_RECEIPT_ITEM = class(TJSONSerializationObject)
  private
    Fcis: string;
    Fprimary_document_custom_name: string;
    Fprimary_document_date: TDateTime;
    Fprimary_document_number: string;
    Fprimary_document_type: string;
    Fproduct_cost: Cardinal;
    procedure Setcis(AValue: string);
    procedure Setprimary_document_custom_name(AValue: string);
    procedure Setprimary_document_date(AValue: TDateTime);
    procedure Setprimary_document_number(AValue: string);
    procedure Setprimary_document_type(AValue: string);
    procedure Setproduct_cost(AValue: Cardinal);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cis:string read Fcis write Setcis;
    property product_cost:Cardinal read Fproduct_cost write Setproduct_cost;
    property primary_document_type:string read Fprimary_document_type write Setprimary_document_type;
    property primary_document_number:string read Fprimary_document_number write Setprimary_document_number;
    property primary_document_date:TDateTime read Fprimary_document_date write Setprimary_document_date;
    property primary_document_custom_name:string read Fprimary_document_custom_name write Setprimary_document_custom_name;
  end;

  TLK_RECEIPT_ITEMs = specialize GJSONSerializationObjectList<TLK_RECEIPT_ITEM>;

  { TLK_RECEIPT }

  TLK_RECEIPT = class(TJSONSerializationObject)
  private
    Faction: string;
    Faction_date: TDate;
    Fbuyer_inn: string;
    Fdestination_country_code: string;
    Fdocument_date: TDate;
    Fdocument_number: string;
    Fdocument_type: string;
    Ffias_id: string;
    Fimporter_id: string;
    Finn: string;
    Fprimary_document_custom_name: string;
    Fproducts: TLK_RECEIPT_ITEMs;
    Fstate_contract_id: string;
    Fwithdrawal_type_other: string;
    procedure Setaction(AValue: string);
    procedure Setaction_date(AValue: TDate);
    procedure SetBuyer_inn(AValue: string);
    procedure Setdestination_country_code(AValue: string);
    procedure Setdocument_date(AValue: TDate);
    procedure Setdocument_number(AValue: string);
    procedure Setdocument_type(AValue: string);
    procedure Setfias_id(AValue: string);
    procedure Setimporter_id(AValue: string);
    procedure SetInn(AValue: string);
    procedure Setprimary_document_custom_name(AValue: string);
    procedure Setstate_contract_id(AValue: string);
    procedure Setwithdrawal_type_other(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property inn:string read Finn write SetInn;
    property buyer_inn:string read Fbuyer_inn write SetBuyer_inn;
    property action:string read Faction  write Setaction;
    property withdrawal_type_other:string read Fwithdrawal_type_other write Setwithdrawal_type_other;
    property action_date:TDate read Faction_date write Setaction_date;
    property document_type:string read Fdocument_type write Setdocument_type;
    property document_number:string read Fdocument_number write Setdocument_number;
    property document_date:TDate read Fdocument_date write Setdocument_date;
    property primary_document_custom_name:string read Fprimary_document_custom_name write Setprimary_document_custom_name;
    property fias_id:string read Ffias_id write Setfias_id;
    //
    property state_contract_id:string read Fstate_contract_id write Setstate_contract_id;
    property destination_country_code:string read Fdestination_country_code write Setdestination_country_code;
    property importer_id:string read Fimporter_id write Setimporter_id;

    property products:TLK_RECEIPT_ITEMs read Fproducts;
  end;
(*
  { TLK_RECEIPT_ITEM_XML }

  TLK_RECEIPT_ITEM_XML = class(TXmlSerializationObject)
  private
    Fcis: string;
    Fprimary_document_custom_name: string;
    Fprimary_document_date: TDateTime;
    Fprimary_document_number: string;
    Fprimary_document_type: string;
    Fproduct_cost: Cardinal;
    procedure Setcis(AValue: string);
    procedure Setprimary_document_custom_name(AValue: string);
    procedure Setprimary_document_date(AValue: TDateTime);
    procedure Setprimary_document_number(AValue: string);
    procedure Setprimary_document_type(AValue: string);
    procedure Setproduct_cost(AValue: Cardinal);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property cis:string read Fcis write Setcis;
    property product_cost:Cardinal read Fproduct_cost write Setproduct_cost;
    property primary_document_type:string read Fprimary_document_type write Setprimary_document_type;
    property primary_document_number:string read Fprimary_document_number write Setprimary_document_number;
    property primary_document_date:TDateTime read Fprimary_document_date write Setprimary_document_date;
    property primary_document_custom_name:string read Fprimary_document_custom_name write Setprimary_document_custom_name;
  end;

  TLK_RECEIPT_ITEM_XMLs = specialize GXMLSerializationObjectList<TLK_RECEIPT_ITEM_XML>;

  { TLK_RECEIPT }

  { TLK_RECEIPT_XML }

  TLK_RECEIPT_XML = class(TXmlSerializationObject)
  private
    Faction: string;
    Faction_date: TDate;
    Fbuyer_inn: string;
    Fdestination_country_code: string;
    Fdocument_date: TDate;
    Fdocument_number: string;
    Fdocument_type: string;
    Ffias_id: string;
    Fimporter_id: string;
    Finn: string;
    Fprimary_document_custom_name: string;
    Fproducts: TLK_RECEIPT_ITEM_XMLs;
    Fstate_contract_id: string;
    Fwithdrawal_type_other: string;
    procedure Setaction(AValue: string);
    procedure Setaction_date(AValue: TDate);
    procedure SetBuyer_inn(AValue: string);
    procedure Setdestination_country_code(AValue: string);
    procedure Setdocument_date(AValue: TDate);
    procedure Setdocument_number(AValue: string);
    procedure Setdocument_type(AValue: string);
    procedure Setfias_id(AValue: string);
    procedure Setimporter_id(AValue: string);
    procedure SetInn(AValue: string);
    procedure Setprimary_document_custom_name(AValue: string);
    procedure Setstate_contract_id(AValue: string);
    procedure Setwithdrawal_type_other(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property inn:string read Finn write SetInn;
    property buyer_inn:string read Fbuyer_inn write SetBuyer_inn;
    property action:string read Faction  write Setaction;
    property withdrawal_type_other:string read Fwithdrawal_type_other write Setwithdrawal_type_other;
    property action_date:TDate read Faction_date write Setaction_date;
    property document_type:string read Fdocument_type write Setdocument_type;
    property document_number:string read Fdocument_number write Setdocument_number;
    property document_date:TDate read Fdocument_date write Setdocument_date;
    property primary_document_custom_name:string read Fprimary_document_custom_name write Setprimary_document_custom_name;
    property fias_id:string read Ffias_id write Setfias_id;
    //
    property state_contract_id:string read Fstate_contract_id write Setstate_contract_id;
    property destination_country_code:string read Fdestination_country_code write Setdestination_country_code;
    property importer_id:string read Fimporter_id write Setimporter_id;

    property products:TLK_RECEIPT_ITEM_XMLs read Fproducts;
  end;
*)

type

  { TTrueAPICreateDocumentData }

  TTrueAPICreateDocumentData =  class(TJSONSerializationObject)
  private
    FDocumentFormat: string;
    FDocumentType: string;
    FProductDocument: string;
    FSignature: string;
    procedure SetDocumentFormat(AValue: string);
    procedure SetDocumentType(AValue: string);
    procedure SetProductDocument(AValue: string);
    procedure SetSignature(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadProductDocument(AStream:TStream);
    procedure LoadSignature(AStream:TStream);
  published
    property DocumentFormat:string read FDocumentFormat write SetDocumentFormat;
    property ProductDocument:string read FProductDocument write SetProductDocument;
    property DocumentType:string read FDocumentType write SetDocumentType;
    property Signature:string read FSignature write SetSignature;
  end;

type

  { TTrueAPICheckCodesRequest }

  TTrueAPICheckCodesRequest = class(TJSONSerializationObject)
  private
    FCodes: TXSDStringArray;
    FFiscalDriveNumber: string;
    procedure SetCodes(AValue: TXSDStringArray);
    procedure SetFiscalDriveNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property fiscalDriveNumber:string read FFiscalDriveNumber write SetFiscalDriveNumber;
    property Codes:TXSDStringArray read FCodes write SetCodes;
  end;

  { TTrueAPICheckCodesItem }

  TTrueAPICheckCodesItem = class(TJSONSerializationObject)
  private
    FCis: string;
    FeliminationState: Double;
    FerrorCode: Integer;
    FexpireDate: string;
    Ffound: Boolean;
    FgrayZone: Boolean;
    FgroupIds: TXSDIntegerArray;
    Fgtin: string;
    FinnerUnitCount: Integer;
    FisBlocked: Boolean;
    FisOwner: Boolean;
    FisTracking: Boolean;
    Fmessage: string;
    Fmrp: Double;
    Fogvs: TXSDStringArray;
    FpackageType: string;
    Fparent: string;
    FprintView: string;
    FproducerInn: string;
    FproductionDate: string;
    FproductWeight: Double;
    FprVetDocument: string;
    Frealizable: Boolean;
    Fsmp: Double;
    Fsold: Boolean;
    FsoldUnitCount: Integer;
    Futilised: Boolean;
    Fvalid: Boolean;
    Fverified: Boolean;
    procedure SetCis(AValue: string);
    procedure SeteliminationState(AValue: Double);
    procedure SeterrorCode(AValue: Integer);
    procedure SetexpireDate(AValue: string);
    procedure Setfound(AValue: Boolean);
    procedure SetgrayZone(AValue: Boolean);
    procedure SetgroupIds(AValue: TXSDIntegerArray);
    procedure Setgtin(AValue: string);
    procedure SetinnerUnitCount(AValue: Integer);
    procedure SetisBlocked(AValue: Boolean);
    procedure SetisOwner(AValue: Boolean);
    procedure SetisTracking(AValue: Boolean);
    procedure Setmessage(AValue: string);
    procedure Setmrp(AValue: Double);
    procedure Setogvs(AValue: TXSDStringArray);
    procedure SetpackageType(AValue: string);
    procedure Setparent(AValue: string);
    procedure SetprintView(AValue: string);
    procedure SetproducerInn(AValue: string);
    procedure SetproductionDate(AValue: string);
    procedure SetproductWeight(AValue: Double);
    procedure SetprVetDocument(AValue: string);
    procedure Setrealizable(AValue: Boolean);
    procedure Setsmp(AValue: Double);
    procedure Setsold(AValue: Boolean);
    procedure SetsoldUnitCount(AValue: Integer);
    procedure Setutilised(AValue: Boolean);
    procedure Setvalid(AValue: Boolean);
    procedure Setverified(AValue: Boolean);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ExpireDateToDate:TDateTime;
    function ProductionDateToDate:TDateTime;
  published
    property Cis:string read FCis write SetCis;                //КМ из запроса
    property Valid:Boolean read Fvalid write Setvalid;             //Результат проверки валидности структуры КМ Возможные значения: true — «Структура валидная»; false — «Структура не валидная»
    property PrintView:string read FprintView write SetprintView;          //КМ без крипто-подписи
    property GTIN:string read Fgtin write Setgtin;               //Код товара
    property GroupIds:TXSDIntegerArray read FgroupIds write SetgroupIds; //Массив идентификаторов товарных групп
    property Verified:Boolean read Fverified write Setverified;          //Результат проверки криптоподписи КМ
    property Found:Boolean read Ffound write Setfound;             //Признак наличия кода Возможные  значения: true — «Код найден»; false — «Код не найден»
    property Realizable:Boolean read Frealizable write Setrealizable;        //Признак ввода в оборот
    property Utilised:Boolean read Futilised write Setutilised;          //Признак нанесения КИ на упаковку
    property ExpireDate:string read FexpireDate write SetexpireDate;         //Дата и время истечения срока годности
    //variableExpirations
    property ProductionDate:string read FproductionDate write SetproductionDate;     //Дата производства продукции
    property ProductWeight:Double read FproductWeight write SetproductWeight;      //Переменный вес продукции (в граммах)
    property PrVetDocument:string read FprVetDocument write SetprVetDocument;      //Производственный ветеринарный сопроводительный документ
    property IsBlocked:Boolean read FisBlocked write SetisBlocked;         //Признак того, что розничная продажа продукции заблокирована по решению ОГВ
    property IsOwner:Boolean read FisOwner write SetisOwner;           //Признак, определяющий что запрос направлен владельцем кода (определяется по аутентификационному токену)
    property OGVS:TXSDStringArray read Fogvs write Setogvs;     //Органы государственной власти, установившие блокировку на КИ
    property ErrorCode:Integer read FerrorCode write SeterrorCode;         //Код ошибки
    property Message:string read Fmessage write Setmessage;            //Сообщение об ошибке
    property IsTracking:Boolean read FisTracking write SetisTracking;        //Признак контроля прослеживаемости в товарной группе
    property Sold:Boolean read Fsold write Setsold;              //Признак вывода из оборота товара
    property EliminationState:Double read FeliminationState write SeteliminationState;   //Дополнительная информация по КМ
    property Mrp:Double read Fmrp write Setmrp;                //Максимальная розничная цена
    property Smp:Double read Fsmp write Setsmp;                //Минимальная из возможных единых минимальных цен
    property PackageType:string read FpackageType write SetpackageType;        //Тип упаковки
    property ProducerInn:string read FproducerInn write SetproducerInn;        //ИНН производителя
    property GrayZone:Boolean read FgrayZone write SetgrayZone;          //Признак принадлежности табачной продукции к «серой зоне»
    property InnerUnitCount:Integer read FinnerUnitCount write SetinnerUnitCount;    //Количество единиц товара в потребительской упаковке / Фактический объём / Фактический вес
    property SoldUnitCount:Integer read FsoldUnitCount write SetsoldUnitCount;     //Счётчик проданного и возвращённого товара
    property Parent:string read Fparent write Setparent;             //КИ агрегата
  end;
  TTrueAPICheckCodesItems = specialize GJSONSerializationObjectList<TTrueAPICheckCodesItem>;


  { TTrueAPICheckCodesResponse }

  TTrueAPICheckCodesResponse = class(TJSONSerializationObject)
  private
    Fcode: Integer;
    Fcodes: TTrueAPICheckCodesItems;
    Fdescription: string;
    FreqId: string;
    FreqTimestamp: Int64;
    procedure Setcode(AValue: Integer);
    procedure Setdescription(AValue: string);
    procedure SetreqId(AValue: string);
    procedure SetreqTimestamp(AValue: Int64);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function reqDate:TDateTime;
  published
    property code:Integer read Fcode write Setcode; //Результат обработки операции
    property description:string read Fdescription write Setdescription; //Текстовое описание результата выполнения метода
    property codes:TTrueAPICheckCodesItems read Fcodes; //Список КМ
    property reqId:string read FreqId write SetreqId;  //Уникальный идентификатор запроса
    property reqTimestamp:Int64 read FreqTimestamp write SetreqTimestamp; //Дата и время регистрации запроса (в UTC)
  end;

implementation
uses base64, DateUtils, sdo_date_utils;


function EncodeStringBase64W(const S:TStream):String;
var
  OutStream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  if not Assigned(S) then
    Exit('');

  OutStream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(OutStream);
    try
      Encoder.CopyFrom(S, S.Size);
    finally
      Encoder.Free;
    end;
    Result:=OutStream.DataString;
  finally
    OutStream.free;
  end;
end;


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
  FpartialSaleInfo:=TPartialSaleInfo.Create;
end;

constructor TCISInfoData.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=true;
end;

destructor TCISInfoData.Destroy;
begin
  FreeAndNil(FsertDocs);
  FreeAndNil(FpartialSaleInfo );
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
  RegisterProperty('errorMessage');
  RegisterProperty('errorCode');
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

{ TLK_RECEIPT_ITEM }

procedure TLK_RECEIPT_ITEM.Setcis(AValue: string);
begin
  if Fcis=AValue then Exit;
  Fcis:=AValue;
  ModifiedProperty('cis');
end;

procedure TLK_RECEIPT_ITEM.Setprimary_document_custom_name(AValue: string);
begin
  if Fprimary_document_custom_name=AValue then Exit;
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure TLK_RECEIPT_ITEM.Setprimary_document_date(AValue: TDateTime);
begin
  if Fprimary_document_date=AValue then Exit;
  Fprimary_document_date:=AValue;
  ModifiedProperty('primary_document_date');
end;

procedure TLK_RECEIPT_ITEM.Setprimary_document_number(AValue: string);
begin
  if Fprimary_document_number=AValue then Exit;
  Fprimary_document_number:=AValue;
  ModifiedProperty('primary_document_number');
end;

procedure TLK_RECEIPT_ITEM.Setprimary_document_type(AValue: string);
begin
  if Fprimary_document_type=AValue then Exit;
  Fprimary_document_type:=AValue;
  ModifiedProperty('primary_document_type');
end;

procedure TLK_RECEIPT_ITEM.Setproduct_cost(AValue: Cardinal);
begin
  if Fproduct_cost=AValue then Exit;
  Fproduct_cost:=AValue;
  ModifiedProperty('product_cost');
end;

procedure TLK_RECEIPT_ITEM.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('cis', 'cis', [], '', -1, -1);
  RegisterProperty('product_cost', 'product_cost', [], '', -1, -1);

  RegisterProperty('primary_document_type', 'primary_document_type', [], '', -1, -1);
  RegisterProperty('primary_document_number', 'primary_document_number', [], '', -1, -1);
  RegisterProperty('primary_document_date', 'primary_document_date', [], '', -1, -1);
  RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [], '', -1, -1);
end;

procedure TLK_RECEIPT_ITEM.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLK_RECEIPT_ITEM.Destroy;
begin
  inherited Destroy;
end;

{ TLK_RECEIPT }

procedure TLK_RECEIPT.SetInn(AValue: string);
begin
  if Finn=AValue then Exit;
  Finn:=AValue;
  ModifiedProperty('inn');
end;

procedure TLK_RECEIPT.Setprimary_document_custom_name(AValue: string);
begin
  if Fprimary_document_custom_name=AValue then Exit;
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure TLK_RECEIPT.Setstate_contract_id(AValue: string);
begin
  if Fstate_contract_id=AValue then Exit;
  Fstate_contract_id:=AValue;
  ModifiedProperty('state_contract_id');
end;

procedure TLK_RECEIPT.Setwithdrawal_type_other(AValue: string);
begin
  if Fwithdrawal_type_other=AValue then Exit;
  Fwithdrawal_type_other:=AValue;
  ModifiedProperty('withdrawal_type_other');
end;

procedure TLK_RECEIPT.SetBuyer_inn(AValue: string);
begin
  if Fbuyer_inn=AValue then Exit;
  Fbuyer_inn:=AValue;
  ModifiedProperty('buyer_inn');
end;

procedure TLK_RECEIPT.Setdestination_country_code(AValue: string);
begin
  if Fdestination_country_code=AValue then Exit;
  Fdestination_country_code:=AValue;
  ModifiedProperty('destination_country_code');
end;

procedure TLK_RECEIPT.Setdocument_date(AValue: TDate);
begin
  if Fdocument_date=AValue then Exit;
  Fdocument_date:=AValue;
  ModifiedProperty('document_date');
end;

procedure TLK_RECEIPT.Setdocument_number(AValue: string);
begin
  if Fdocument_number=AValue then Exit;
  Fdocument_number:=AValue;
  ModifiedProperty('document_number');
end;

procedure TLK_RECEIPT.Setdocument_type(AValue: string);
begin
  if Fdocument_type=AValue then Exit;
  Fdocument_type:=AValue;
  ModifiedProperty('document_type');
end;

procedure TLK_RECEIPT.Setfias_id(AValue: string);
begin
  if Ffias_id=AValue then Exit;
  Ffias_id:=AValue;
  ModifiedProperty('fias_id');
end;

procedure TLK_RECEIPT.Setimporter_id(AValue: string);
begin
  if Fimporter_id=AValue then Exit;
  Fimporter_id:=AValue;
  ModifiedProperty('importer_id');
end;

procedure TLK_RECEIPT.Setaction(AValue: string);
begin
  if Faction=AValue then Exit;
  Faction:=AValue;
  ModifiedProperty('action');
end;

procedure TLK_RECEIPT.Setaction_date(AValue: TDate);
begin
  if Faction_date=AValue then Exit;
  Faction_date:=AValue;
  ModifiedProperty('action_date');
end;

procedure TLK_RECEIPT.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('inn', 'inn', [], '', -1, -1);
  RegisterProperty('buyer_inn', 'buyer_inn', [], '', -1, -1);
  RegisterProperty('action', 'action', [], '', -1, -1);
  RegisterProperty('withdrawal_type_other', 'withdrawal_type_other', [], '', -1, -1);
  RegisterProperty('action_date', 'action_date', [], '', -1, -1);
  RegisterProperty('document_type', 'document_type', [], '', -1, -1);
  RegisterProperty('document_number', 'document_number', [], '', -1, -1);
  RegisterProperty('document_date', 'document_date', [], '', -1, -1);
  RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [], '', -1, -1);
  RegisterProperty('fias_id', 'fias_id', [], '', -1, -1);

  RegisterProperty('state_contract_id', 'state_contract_id', [], '', -1, -1);
  RegisterProperty('destination_country_code', 'destination_country_code', [], '', -1, -1);
  RegisterProperty('importer_id', 'importer_id', [], '', -1, -1);

  RegisterProperty('products', 'products', [], '', -1, -1);
end;

procedure TLK_RECEIPT.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts:=TLK_RECEIPT_ITEMs.Create;
end;

destructor TLK_RECEIPT.Destroy;
begin
  FreeAndNil(Fproducts);
  inherited Destroy;
end;

{ TTrueAPICreateDocumentData }

procedure TTrueAPICreateDocumentData.SetDocumentFormat(AValue: string);
begin
  if FDocumentFormat=AValue then Exit;
  CheckLockupValue('DocumentFormat', AValue);
  FDocumentFormat:=AValue;
  ModifiedProperty('DocumentFormat');
end;

procedure TTrueAPICreateDocumentData.SetDocumentType(AValue: string);
begin
  if FDocumentType=AValue then Exit;
  FDocumentType:=AValue;
  ModifiedProperty('DocumentType');
end;

procedure TTrueAPICreateDocumentData.SetProductDocument(AValue: string);
begin
  if FProductDocument=AValue then Exit;
  FProductDocument:=AValue;
  ModifiedProperty('ProductDocument');
end;

procedure TTrueAPICreateDocumentData.SetSignature(AValue: string);
begin
  if FSignature=AValue then Exit;
  FSignature:=AValue;
  ModifiedProperty('Signature');
end;

procedure TTrueAPICreateDocumentData.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('DocumentFormat', 'document_format', [], '', -1, -1);
    P.ValidList.Add('MANUAL');
    P.ValidList.Add('XML');
    P.ValidList.Add('CSV');
  RegisterProperty('ProductDocument', 'product_document', [], '', -1, -1);
  RegisterProperty('DocumentType', 'type', [], '', -1, -1);
  RegisterProperty('Signature', 'signature', [], '', -1, -1);
end;

procedure TTrueAPICreateDocumentData.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TTrueAPICreateDocumentData.Create;
begin
  inherited Create;
end;

destructor TTrueAPICreateDocumentData.Destroy;
begin
  inherited Destroy;
end;

procedure TTrueAPICreateDocumentData.LoadProductDocument(AStream: TStream);
begin
  ProductDocument:=EncodeStringBase64W(AStream)
end;

procedure TTrueAPICreateDocumentData.LoadSignature(AStream: TStream);
var
  S: String;
begin
  //Signature:=EncodeStringBase64W(AStream)
  Signature:='';
  if not Assigned(AStream) then Exit;
  if (AStream.Size > 0) then
  begin
    S:='';
    SetLength(S, AStream.Size);
    AStream.Read(S[1], AStream.Size);
    Signature:=S;
  end;
end;

{ TTrueAPICheckCodesRequest }

procedure TTrueAPICheckCodesRequest.SetFiscalDriveNumber(AValue: string);
begin
  if FFiscalDriveNumber=AValue then Exit;
  FFiscalDriveNumber:=AValue;
  ModifiedProperty('fiscalDriveNumber');
end;

procedure TTrueAPICheckCodesRequest.SetCodes(AValue: TXSDStringArray);
begin
  if FCodes=AValue then Exit;
  FCodes:=AValue;

  //CheckLockupValue('Codes', AValue);
  ModifiedProperty('Codes');
end;

procedure TTrueAPICheckCodesRequest.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('fiscalDriveNumber', 'fiscalDriveNumber', [], '', -1, -1);
  RegisterProperty('Codes', 'codes', [xsaRequared], '', -1, -1);
end;

procedure TTrueAPICheckCodesRequest.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TTrueAPICheckCodesRequest.Create;
begin
  inherited Create;
end;

destructor TTrueAPICheckCodesRequest.Destroy;
begin
  inherited Destroy;
end;

{ TTrueAPICheckCodesItem }

procedure TTrueAPICheckCodesItem.SetCis(AValue: string);
begin
  if FCis=AValue then Exit;
  FCis:=AValue;
  ModifiedProperty('cis');
end;

procedure TTrueAPICheckCodesItem.SeteliminationState(AValue: Double);
begin
  if FeliminationState=AValue then Exit;
  FeliminationState:=AValue;
  ModifiedProperty('eliminationState');
end;

procedure TTrueAPICheckCodesItem.SeterrorCode(AValue: Integer);
begin
  if FerrorCode=AValue then Exit;
  FerrorCode:=AValue;
  ModifiedProperty('errorCode');
end;

procedure TTrueAPICheckCodesItem.SetexpireDate(AValue: string);
begin
  if FexpireDate=AValue then Exit;
  FexpireDate:=AValue;
  ModifiedProperty('expireDate');
end;

procedure TTrueAPICheckCodesItem.Setfound(AValue: Boolean);
begin
  if Ffound=AValue then Exit;
  Ffound:=AValue;
  ModifiedProperty('found');
end;

procedure TTrueAPICheckCodesItem.SetgrayZone(AValue: Boolean);
begin
  if FgrayZone=AValue then Exit;
  FgrayZone:=AValue;
  ModifiedProperty('grayZone');
end;

procedure TTrueAPICheckCodesItem.SetgroupIds(AValue: TXSDIntegerArray);
begin
  if FgroupIds=AValue then Exit;
  FgroupIds:=AValue;
  ModifiedProperty('groupIds');
end;

procedure TTrueAPICheckCodesItem.Setgtin(AValue: string);
begin
  if Fgtin=AValue then Exit;
  Fgtin:=AValue;
  ModifiedProperty('gtin');
end;

procedure TTrueAPICheckCodesItem.SetinnerUnitCount(AValue: Integer);
begin
  if FinnerUnitCount=AValue then Exit;
  FinnerUnitCount:=AValue;
  ModifiedProperty('innerUnitCount');
end;

procedure TTrueAPICheckCodesItem.SetisBlocked(AValue: Boolean);
begin
  if FisBlocked=AValue then Exit;
  FisBlocked:=AValue;
  ModifiedProperty('isBlocked');
end;

procedure TTrueAPICheckCodesItem.SetisOwner(AValue: Boolean);
begin
  if FisOwner=AValue then Exit;
  FisOwner:=AValue;
  ModifiedProperty('isOwner');
end;

procedure TTrueAPICheckCodesItem.SetisTracking(AValue: Boolean);
begin
  if FisTracking=AValue then Exit;
  FisTracking:=AValue;
  ModifiedProperty('isTracking');
end;

procedure TTrueAPICheckCodesItem.Setmessage(AValue: string);
begin
  if Fmessage=AValue then Exit;
  Fmessage:=AValue;
  ModifiedProperty('message');
end;

procedure TTrueAPICheckCodesItem.Setmrp(AValue: Double);
begin
  if Fmrp=AValue then Exit;
  Fmrp:=AValue;
  ModifiedProperty('mrp');
end;

procedure TTrueAPICheckCodesItem.Setogvs(AValue: TXSDStringArray);
begin
  if Fogvs=AValue then Exit;
  Fogvs:=AValue;
  ModifiedProperty('ogvs');
end;

procedure TTrueAPICheckCodesItem.SetpackageType(AValue: string);
begin
  if FpackageType=AValue then Exit;
  FpackageType:=AValue;
  ModifiedProperty('packageType');
end;

procedure TTrueAPICheckCodesItem.Setparent(AValue: string);
begin
  if Fparent=AValue then Exit;
  Fparent:=AValue;
  ModifiedProperty('parent');
end;

procedure TTrueAPICheckCodesItem.SetprintView(AValue: string);
begin
  if FprintView=AValue then Exit;
  FprintView:=AValue;
  ModifiedProperty('printView');
end;

procedure TTrueAPICheckCodesItem.SetproducerInn(AValue: string);
begin
  if FproducerInn=AValue then Exit;
  FproducerInn:=AValue;
  ModifiedProperty('producerInn');
end;

procedure TTrueAPICheckCodesItem.SetproductionDate(AValue: string);
begin
  if FproductionDate=AValue then Exit;
  FproductionDate:=AValue;
  ModifiedProperty('productionDate');
end;

procedure TTrueAPICheckCodesItem.SetproductWeight(AValue: Double);
begin
  if FproductWeight=AValue then Exit;
  FproductWeight:=AValue;
  ModifiedProperty('productWeight');
end;

procedure TTrueAPICheckCodesItem.SetprVetDocument(AValue: string);
begin
  if FprVetDocument=AValue then Exit;
  FprVetDocument:=AValue;
  ModifiedProperty('prVetDocument');
end;

procedure TTrueAPICheckCodesItem.Setrealizable(AValue: Boolean);
begin
  if Frealizable=AValue then Exit;
  Frealizable:=AValue;
  ModifiedProperty('realizable');
end;

procedure TTrueAPICheckCodesItem.Setsmp(AValue: Double);
begin
  if Fsmp=AValue then Exit;
  Fsmp:=AValue;
  ModifiedProperty('smp');
end;

procedure TTrueAPICheckCodesItem.Setsold(AValue: Boolean);
begin
  if Fsold=AValue then Exit;
  Fsold:=AValue;
  ModifiedProperty('sold');
end;

procedure TTrueAPICheckCodesItem.SetsoldUnitCount(AValue: Integer);
begin
  if FsoldUnitCount=AValue then Exit;
  FsoldUnitCount:=AValue;
  ModifiedProperty('soldUnitCount');
end;

procedure TTrueAPICheckCodesItem.Setutilised(AValue: Boolean);
begin
  if Futilised=AValue then Exit;
  Futilised:=AValue;
  ModifiedProperty('utilised');
end;

procedure TTrueAPICheckCodesItem.Setvalid(AValue: Boolean);
begin
  if Fvalid=AValue then Exit;
  Fvalid:=AValue;
  ModifiedProperty('valid');
end;

procedure TTrueAPICheckCodesItem.Setverified(AValue: Boolean);
begin
  if Fverified=AValue then Exit;
  Fverified:=AValue;
  ModifiedProperty('verified');
end;

procedure TTrueAPICheckCodesItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('cis', 'cis', [], '', -1, -1);
  RegisterProperty('valid', 'valid', [], '', -1, -1);
  RegisterProperty('printView', 'printView', [], '', -1, -1);
  RegisterProperty('gtin', 'gtin', [], '', -1, -1);
  RegisterProperty('groupIds', 'groupIds', [], '', -1, -1);
  RegisterProperty('verified', 'verified', [], '', -1, -1);
  RegisterProperty('found', 'found', [], '', -1, -1);
  RegisterProperty('realizable', 'realizable', [], '', -1, -1);
  RegisterProperty('utilised', 'utilised', [], '', -1, -1);
  RegisterProperty('isBlocked', 'isBlocked', [], '', -1, -1);
  RegisterProperty('isOwner', 'isOwner', [], '', -1, -1);
  RegisterProperty('errorCode', 'errorCode', [], '', -1, -1);
  RegisterProperty('isTracking', 'isTracking', [], '', -1, -1);
  RegisterProperty('sold', 'sold', [], '', -1, -1);
  RegisterProperty('packageType', 'packageType', [], '', -1, -1);
  RegisterProperty('producerInn', 'producerInn', [], '', -1, -1);
  RegisterProperty('grayZone', 'grayZone', [], '', -1, -1);
  RegisterProperty('expireDate', 'expireDate', [], '', -1, -1);
  RegisterProperty('productionDate', 'productionDate', [], '', -1, -1);
  RegisterProperty('productWeight', 'productWeight', [], '', -1, -1);
  RegisterProperty('prVetDocument', 'prVetDocument', [], '', -1, -1);
  RegisterProperty('ogvs', 'ogvs', [], '', -1, -1);
  RegisterProperty('errorCode', 'errorCode', [], '', -1, -1);
  RegisterProperty('message', 'message', [], '', -1, -1);
  RegisterProperty('isTracking', 'isTracking', [], '', -1, -1);
  RegisterProperty('eliminationState', 'eliminationState', [], '', -1, -1);
  RegisterProperty('sold', 'sold', [], '', -1, -1);
  RegisterProperty('mrp', 'mrp', [], '', -1, -1);
  RegisterProperty('smp', 'smp', [], '', -1, -1);
  RegisterProperty('innerUnitCount', 'innerUnitCount', [], '', -1, -1);
  RegisterProperty('soldUnitCount', 'soldUnitCount', [], '', -1, -1);
  RegisterProperty('parent', 'parent', [], '', -1, -1);
end;

procedure TTrueAPICheckCodesItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TTrueAPICheckCodesItem.Create;
begin
  inherited Create;
end;

destructor TTrueAPICheckCodesItem.Destroy;
begin
  inherited Destroy;
end;

function TTrueAPICheckCodesItem.ExpireDateToDate: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FexpireDate, R, xdkDateTime) then
    Result:=R.Date
  else
    Result:=0;
end;

function TTrueAPICheckCodesItem.ProductionDateToDate: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FproductionDate, R, xdkDateTime) then
    Result:=R.Date
  else
    Result:=0;
end;

{ TTrueAPICheckCodesResponse }

procedure TTrueAPICheckCodesResponse.Setcode(AValue: Integer);
begin
  if Fcode=AValue then Exit;
  Fcode:=AValue;
end;

procedure TTrueAPICheckCodesResponse.Setdescription(AValue: string);
begin
  if Fdescription=AValue then Exit;
  Fdescription:=AValue;
end;

procedure TTrueAPICheckCodesResponse.SetreqId(AValue: string);
begin
  if FreqId=AValue then Exit;
  FreqId:=AValue;
end;

procedure TTrueAPICheckCodesResponse.SetreqTimestamp(AValue: Int64);
begin
  if FreqTimestamp=AValue then Exit;
  FreqTimestamp:=AValue;
end;

procedure TTrueAPICheckCodesResponse.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('code', 'code', [], '', -1, -1);
  RegisterProperty('description', 'description', [], '', -1, -1);
  RegisterProperty('codes', 'codes', [], '', -1, -1);
  RegisterProperty('reqId', 'reqId', [], '', -1, -1);
  RegisterProperty('reqTimestamp', 'reqTimestamp', [], '', -1, -1);
end;

procedure TTrueAPICheckCodesResponse.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fcodes:=TTrueAPICheckCodesItems.Create;
end;

constructor TTrueAPICheckCodesResponse.Create;
begin
  inherited Create;
end;

destructor TTrueAPICheckCodesResponse.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Fcodes);
end;

function TTrueAPICheckCodesResponse.reqDate: TDateTime;
begin
  Result:=UnixToDateTime(FreqTimestamp div 1000, false);
end;

(*
{ TLK_RECEIPT_ITEM_XML }

procedure TLK_RECEIPT_ITEM_XML.Setcis(AValue: string);
begin
  if Fcis=AValue then Exit;
  Fcis:=AValue;
  ModifiedProperty('cis');
end;

procedure TLK_RECEIPT_ITEM_XML.Setprimary_document_custom_name(AValue: string);
begin
  if Fprimary_document_custom_name=AValue then Exit;
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure TLK_RECEIPT_ITEM_XML.Setprimary_document_date(AValue: TDateTime);
begin
  if Fprimary_document_date=AValue then Exit;
  Fprimary_document_date:=AValue;
  ModifiedProperty('primary_document_date');
end;

procedure TLK_RECEIPT_ITEM_XML.Setprimary_document_number(AValue: string);
begin
  if Fprimary_document_number=AValue then Exit;
  Fprimary_document_number:=AValue;
  ModifiedProperty('primary_document_number');
end;

procedure TLK_RECEIPT_ITEM_XML.Setprimary_document_type(AValue: string);
begin
  if Fprimary_document_type=AValue then Exit;
  Fprimary_document_type:=AValue;
  ModifiedProperty('primary_document_type');
end;

procedure TLK_RECEIPT_ITEM_XML.Setproduct_cost(AValue: Cardinal);
begin
  if Fproduct_cost=AValue then Exit;
  Fproduct_cost:=AValue;
  ModifiedProperty('product_cost');
end;

procedure TLK_RECEIPT_ITEM_XML.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;

  RegisterProperty('cis', 'cis', [], '', -1, -1);
  RegisterProperty('product_cost', 'product_cost', [], '', -1, -1);

  RegisterProperty('primary_document_type', 'primary_document_type', [], '', -1, -1);
  RegisterProperty('primary_document_number', 'primary_document_number', [], '', -1, -1);
  RegisterProperty('primary_document_date', 'primary_document_date', [], '', -1, -1);
  RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [], '', -1, -1);
end;

procedure TLK_RECEIPT_ITEM_XML.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLK_RECEIPT_ITEM_XML.Destroy;
begin
  inherited Destroy;
end;

{ TLK_RECEIPT_XML }

procedure TLK_RECEIPT_XML.Setaction(AValue: string);
begin
  if Faction=AValue then Exit;
  Faction:=AValue;
  ModifiedProperty('action');
end;

procedure TLK_RECEIPT_XML.Setaction_date(AValue: TDate);
begin
  if Faction_date=AValue then Exit;
  Faction_date:=AValue;
  ModifiedProperty('action_date');
end;

procedure TLK_RECEIPT_XML.SetBuyer_inn(AValue: string);
begin
  if Fbuyer_inn=AValue then Exit;
  Fbuyer_inn:=AValue;
  ModifiedProperty('buyer_inn');
end;

procedure TLK_RECEIPT_XML.Setdestination_country_code(AValue: string);
begin
  if Fdestination_country_code=AValue then Exit;
  Fdestination_country_code:=AValue;
  ModifiedProperty('destination_country_code');
end;

procedure TLK_RECEIPT_XML.Setdocument_date(AValue: TDate);
begin
  if Fdocument_date=AValue then Exit;
  Fdocument_date:=AValue;
  ModifiedProperty('document_date');
end;

procedure TLK_RECEIPT_XML.Setdocument_number(AValue: string);
begin
  if Fdocument_number=AValue then Exit;
  Fdocument_number:=AValue;
  ModifiedProperty('document_number');
end;

procedure TLK_RECEIPT_XML.Setdocument_type(AValue: string);
begin
  if Fdocument_type=AValue then Exit;
  Fdocument_type:=AValue;
  ModifiedProperty('document_type');
end;

procedure TLK_RECEIPT_XML.Setfias_id(AValue: string);
begin
  if Ffias_id=AValue then Exit;
  Ffias_id:=AValue;
  ModifiedProperty('fias_id');
end;

procedure TLK_RECEIPT_XML.Setimporter_id(AValue: string);
begin
  if Fimporter_id=AValue then Exit;
  Fimporter_id:=AValue;
  ModifiedProperty('importer_id');
end;

procedure TLK_RECEIPT_XML.SetInn(AValue: string);
begin
  if Finn=AValue then Exit;
  Finn:=AValue;
  ModifiedProperty('inn');
end;

procedure TLK_RECEIPT_XML.Setprimary_document_custom_name(AValue: string);
begin
  if Fprimary_document_custom_name=AValue then Exit;
  Fprimary_document_custom_name:=AValue;
  ModifiedProperty('primary_document_custom_name');
end;

procedure TLK_RECEIPT_XML.Setstate_contract_id(AValue: string);
begin
  if Fstate_contract_id=AValue then Exit;
  Fstate_contract_id:=AValue;
  ModifiedProperty('state_contract_id');
end;

procedure TLK_RECEIPT_XML.Setwithdrawal_type_other(AValue: string);
begin
  if Fwithdrawal_type_other=AValue then Exit;
  Fwithdrawal_type_other:=AValue;
  ModifiedProperty('withdrawal_type_other');
end;

procedure TLK_RECEIPT_XML.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('inn', 'inn', [], '', -1, -1);
  RegisterProperty('buyer_inn', 'buyer_inn', [], '', -1, -1);
  RegisterProperty('action', 'action', [], '', -1, -1);
  RegisterProperty('withdrawal_type_other', 'withdrawal_type_other', [], '', -1, -1);
  RegisterProperty('action_date', 'action_date', [], '', -1, -1);
  RegisterProperty('document_type', 'document_type', [], '', -1, -1);
  RegisterProperty('document_number', 'document_number', [], '', -1, -1);
  RegisterProperty('document_date', 'document_date', [], '', -1, -1);
  RegisterProperty('primary_document_custom_name', 'primary_document_custom_name', [], '', -1, -1);
  RegisterProperty('fias_id', 'fias_id', [], '', -1, -1);

  RegisterProperty('state_contract_id', 'state_contract_id', [], '', -1, -1);
  RegisterProperty('destination_country_code', 'destination_country_code', [], '', -1, -1);
  RegisterProperty('importer_id', 'importer_id', [], '', -1, -1);

  RegisterProperty('products', 'products', [], '', -1, -1);
end;

procedure TLK_RECEIPT_XML.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts:=TLK_RECEIPT_ITEM_XMLs.Create;
end;

destructor TLK_RECEIPT_XML.Destroy;
begin
  FreeAndNil(Fproducts);
  inherited Destroy;
end;
*)
end.

