module Model exposing (..)

import Http


type alias Icon =
    { name : String
    , raw : String
    }


type alias Model =
    { custom : Maybe String
    , target : Maybe Icon
    , icons : List Icon
    }


type Msg
    = NoOp
    | Edit String
    | Select Icon
    | FetchFeather String
    | FetchResult String (Result Http.Error String)
