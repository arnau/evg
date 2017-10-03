module Evg.String exposing (..)

import Char
import Dict
import String
import Xml exposing (Value(..))
import Xml.Decode exposing (decode)
import Xml.Encode exposing (encode, null)


translate : String -> String
translate raw =
    toElm <| fromString raw


fromString : String -> Value
fromString raw =
    raw
        |> decode
        |> Result.toMaybe
        |> Maybe.withDefault null


toElm : Value -> String
toElm ast =
    toNode 1 ast


toNode : Int -> Value -> String
toNode depth value =
    case value of
        Object [ nextValue ] ->
            toNode (depth + 1) nextValue

        Tag name dict values ->
            "Svg." ++ name ++ " " ++ toAttributes depth (Dict.toList (Dict.remove "xmlns" dict)) ++ " " ++ toChildren depth values

        _ ->
            "Malformed SVG"


toAttributes : Int -> List ( String, Value ) -> String
toAttributes depth xs =
    let
        pad =
            String.repeat (depth * 4) " "
    in
    "[ " ++ String.join ("\n" ++ pad ++ ", ") (List.map toAttribute xs) ++ " ]"


toChildren : Int -> Value -> String
toChildren depth value =
    let
        pad =
            String.repeat (depth * 4) " "
    in
    case value of
        Object xs ->
            "[ " ++ String.join ("\n" ++ pad ++ ", ") (List.map (toNode (depth + 1)) xs) ++ " ]"

        _ ->
            "[]"


toAttribute : ( String, Value ) -> String
toAttribute ( name, value ) =
    toCamelCase name ++ " \"" ++ propToString value ++ "\""


toCamelCase : String -> String
toCamelCase value =
    let
        xs =
            String.split "-" value
    in
    case xs of
        [] ->
            ""

        x :: xs ->
            (x :: List.map toCapital xs)
                |> String.join ""


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
