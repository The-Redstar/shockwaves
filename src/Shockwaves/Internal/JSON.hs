
{-# LANGUAGE FlexibleInstances #-}

module Shockwaves.Internal.JSON where

-- depricated, but used to store some mock rust code

{-

{
  signals: {
    <sig>: <type>
  }
  types: {
    <type>: {
    }
  }
  luts: {
    <lut>: {
      <bin>: [
        []
      ]
    }
  }
}






use serde::{Deserialize, Serialize};

// use serde_json::Result;
use std::collections::HashMap;

#[derive(Serialize,Deserialize,Debug)]
struct Data{
    signals: SigMap,
    types: TypeMap,
    luts: LUTMap,
}

type SigMap = HashMap<String,String>;
type TypeMap = HashMap<String,TypeMeta>;
type LUTMap = HashMap<String,LUT>;
type LUT = HashMap<String,Translation>;

#[derive(Serialize,Deserialize,Debug)]
struct TypeMeta {
    translator: Translator,
    structure: Structure
}

#[derive(Serialize,Deserialize,Debug)]
struct Translator {
    width: u32,
    trans: TranslatorVariant
}

#[derive(Serialize,Deserialize,Debug)]
enum TranslatorVariant {
    Ref(String),
    A,B,C
}

#[derive(Serialize,Deserialize,Debug)]
enum WaveStyle {
    WSNormal, WSWarn, WSError, WSColor(u8)
}

#[derive(Serialize,Deserialize,Debug)]
struct Structure(Vec<(String,Structure)>);

#[derive(Serialize,Deserialize,Debug)]
struct Translation(Render,Vec<(String,Translation)>);

type Render = Option<(String,WaveStyle,u32)>;

//, TypeMeta, Structure, Translator, LUTMap, LUT, Translation, Render



fn main() {
    println!("Hello, world!");

    // Some data structure.
    let data = Data{
        signals: HashMap::from([
            (String::from("x"), String::from("Unsigned 5")),
            (String::from("enable"), String::from("Bool"))
        ]),
        types: HashMap::from([
            (String::from("Unsigned 5"),TypeMeta{
                structure: Structure(vec![(String::from("0"),Structure(vec![]))]),
                translator: Translator{
                    width: 5,
                    trans: TranslatorVariant::Ref(String::from("Signed 5"))
                }
            }),
        ]),
        luts: HashMap::from([
            (String::from("Custom"),HashMap::from([
                (String::from("0"),Translation(None,vec![])),
                (String::from("1"),Translation(Some((String::from("custom"),WaveStyle::WSColor(5),11)),vec![])),
            ])),
        ]),
    };

    // Serialize it to a JSON string.
    let j = serde_json::to_string_pretty(&data).unwrap();
    
    let k:Translator = serde_json::from_str("[5,\"A\"]").unwrap();
    // let k: Option<(u32,u32,Option<u32>)> = serde_json::from_str("[1,2]").unwrap();

    // Print, write to a file, or send to an HTTP server.
    println!("{}", j);
    println!("{:?}", k)

/*

{
  "signals": {
    "enable": "Bool",
    "x": "Unsigned 5"
  },
  "types": {
    "Unsigned 5": {
      "translator": {
        "width": 5,
        "trans": {
          "Ref": "Signed 5"
        }
      },
      "structure": [
        [
          "0",
          []
        ]
      ]
    }
  },
  "luts": {
    "Custom": {
      "1": [
        [
          "custom",
          "WSNormal",
          11
        ],
        []
      ],
      "0": [
        null,
        []
      ]
    }
  }
}

*/


}

-}