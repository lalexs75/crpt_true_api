unit CRPTTrueAPI_Consts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  TCRPTProductGroup = (
    lp,          //Предметы одежды, белье постельное, столовое, туалетное и кухонное
    shoes,       //Обувные товары
    tobacco,     //Табачная продукция
    perfumery,   //Духи и туалетная вода
    tires,       //Шины и покрышки пневматические резиновые новые
    electronics, //Фотокамеры (кроме кинокамер), фотовспышки и лампы-вспышки
    pharma,      //Лекарственные препараты для медицинского применения
    milk,        //Молочная продукция
    bicycle,     //Велосипеды и велосипедные рамы
    wheelchairs, //Медицинские изделия
    otp,         //Альтернативная табачная продукция
    water,       //Питьевая вода
    furs,        //Товары из натурального меха
    beer,        //Пиво, напитки, изготавливаемые на основе пива и слабоалкогольные напитки
    ncp,         //Никотиносодержащая продукция
    bio,         //Биологически активные добавки к пище
    antiseptic,  //Антисептики и дезинфицирующие средства
    nabeer,      //Безалкогольное пиво
    softdrinks   //Соковая продукция и безалкогольные напитки}
    );

const
  CRPTProductGroupStr : array [TCRPTProductGroup] of string =
    ('lp', 'shoes', 'tobacco', 'perfumery', 'tires',
     'electronics', 'pharma', 'milk', 'bicycle', 'wheelchairs',
     'otp', 'water', 'furs', 'beer', 'ncp', 'bio', 'antiseptic',
     'nabeer', 'softdrinks');

implementation

end.

