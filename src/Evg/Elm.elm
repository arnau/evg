module Evg.Elm exposing (..)

import Char
import Dict
import String
import Svg exposing (Svg)
import VirtualDom exposing (attribute, attributeNS)
import Xml exposing (Value(..))
import Xml.Decode exposing (decode)
import Xml.Encode exposing (null)


translate : String -> Svg a
translate raw =
    toElm <| fromString raw


fromString : String -> Value
fromString raw =
    raw
        |> decode
        |> Result.toMaybe
        |> Maybe.withDefault null


toElm : Value -> Svg a
toElm ast =
    toNode ast


toNode : Value -> Svg a
toNode value =
    case value of
        Object [ nextValue ] ->
            toNode nextValue

        Tag name dict values ->
            Svg.node name (toAttributes (Dict.toList (Dict.remove "xmlns" dict))) (toChildren values)

        _ ->
            Svg.text "Malformed SVG"


toAttributes : List ( String, Value ) -> List (Svg.Attribute a)
toAttributes xs =
    List.map toAttribute xs


toChildren : Value -> List (Svg a)
toChildren value =
    case value of
        Object xs ->
            List.map toNode xs

        _ ->
            []


toAttribute : ( String, Value ) -> Svg.Attribute a
toAttribute ( name, value ) =
    attribute name (propToString value)


toCapital : String -> String
toCapital value =
    case String.uncons value of
        Just ( letter, rest ) ->
            String.cons (Char.toUpper letter) rest

        Nothing ->
            value


propToString : Value -> String
propToString value =
    case value of
        StrNode str ->
            str

        IntNode n ->
            toString n

        BoolNode b ->
            toString b
                |> String.toLower

        FloatNode f ->
            toString f

        _ ->
            ""
