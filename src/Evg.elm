module Evg exposing (..)

import Evg.Elm as E
import Evg.String as S
import Svg exposing (Svg)


toString : String -> String
toString raw =
    S.translate raw


toElm : String -> Svg a
toElm raw =
    E.translate raw
