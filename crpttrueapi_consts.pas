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

type
  TcrptCISStatus = (csEMITTED, csAPPLIED, csINTRODUCED, csWRITTEN_OFF,
    csRETIRED, csWITHDRAWN, csDISAGGREGATION, csDISAGGREGATED, csAPPLIED_NOT_PAID);

const
  CRPTProductGroupStr : array [TCRPTProductGroup] of string =
    ('lp', 'shoes', 'tobacco', 'perfumery', 'tires',
     'electronics', 'pharma', 'milk', 'bicycle', 'wheelchairs',
     'otp', 'water', 'furs', 'beer', 'ncp', 'bio', 'antiseptic',
     'nabeer', 'softdrinks');

  CRPTProductGroupNames : array [TCRPTProductGroup] of string =
    (
     'Предметы одежды, белье постельное, столовое, туалетное и кухонное',
     'Обувные товары',
     'Табачная продукция',
     'Духи и туалетная вода',
     'Шины и покрышки пневматические резиновые новые',
     'Фотокамеры (кроме кинокамер), фотовспышки и лампы-вспышки',
     'Лекарственные препараты для медицинского применения',
     'Молочная продукция',
     'Велосипеды и велосипедные рамы',
     'Медицинские изделия',
     'Альтернативная табачная продукция',
     'Питьевая вода',
     'Товары из натурального меха',
     'Пиво, напитки, изготавливаемые на основе пива и слабоалкогольные напитки',
     'Никотиносодержащая продукция',
     'Биологически активные добавки к пище',
     'Антисептики и дезинфицирующие средства',
     'Безалкогольное пиво',
     'Соковая продукция и безалкогольные напитки'
    );

type
  TDocListFilterRecord = record
    FromDate:TDateTime;
    ToDate:TDateTime;
  end;

implementation

end.

