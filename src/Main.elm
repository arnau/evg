module Main exposing (..)

import Evg exposing (..)
import Feather exposing (names)
import Html exposing (..)
import Html.Attributes exposing (defaultValue, href, rows, style)
import Html.Events exposing (Options, onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Model exposing (Icon, Model, Msg(..))
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { custom = Nothing
      , target = Nothing
      , icons = []
      }
    , Cmd.batch (List.map fetchFeather Feather.names)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Edit value ->
            if String.isEmpty value then
                ( { model | custom = Nothing }, Cmd.none )
            else
                ( { model | custom = Just value }, Cmd.none )

        Select icon ->
            ( { model
                | target = Just icon
                , custom = Just icon.raw
              }
            , Cmd.none
            )

        FetchFeather name ->
            ( model, fetchFeather name )

        FetchResult name (Ok raw) ->
            let
                icon =
                    Icon name raw

                icons =
                    if List.member icon model.icons then
                        model.icons
                    else
                        icon :: model.icons
            in
            ( { model | icons = icons }, Cmd.none )

        FetchResult name e ->
            let
                _ =
                    Debug.log name e
            in
            ( model, Cmd.none )


fetchFeather : String -> Cmd Msg
fetchFeather name =
    Feather.fetch (FetchResult name) name


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "grid" )
            , ( "grid-template-columns", "1fr 1fr" )
            , ( "grid-template-rows", "5vh 200px auto auto" )
            ]
        ]
        [ useTheme monokai
        , header "evg : Svg -> Elm"
        , menuPane model
        , codePane model
        ]


onClick_ : msg -> Attribute msg
onClick_ msg =
    onWithOptions "click" (Options True True) (Decode.succeed msg)


codePane : Model -> Html Msg
codePane model =
    div [ style [ ( "padding", "0 10px" ) ] ]
        [ editor model
        , elmPreview (svgValue model)

        -- , Maybe.map elmPreview model.target
        --     |> Maybe.withDefault (text "")
        ]


svgValue : Model -> String
svgValue model =
    let
        value =
            model.custom
                |> Maybe.withDefault ""
                |> Debug.log "x"
    in
    value


editor : Model -> Html Msg
editor model =
    let
        value =
            Maybe.withDefault "" model.custom
    in
    div []
        [ h2 [] [ text "SVG" ]
        , textarea
            [ onInput Edit
            , style [ ( "width", "100%" ) ]
            , rows 20
            , Html.Attributes.value value
            ]
            []
        ]


elmPreview : String -> Html Msg
elmPreview value =
    div []
        [ h2 [] [ text "Elm (", code [] [ text "elm-lang/svg" ], text ")" ]
        , node "style" [] [ text ".elmsh { overflow-x: auto; max-width: 50vw; padding: 5px }" ]
        , value
            |> Evg.toString
            |> elm
            |> Result.map (toBlockHtml (Just 1))
            |> Result.withDefault
                (pre [] [ code [] [ text value ] ])
        ]


menuPane : Model -> Html Msg
menuPane model =
    nav
        [ style
            [ ( "grid-column-start", "1" )
            , ( "grid-column-end", "2" )
            , ( "grid-row-start", "2" )
            , ( "grid-row-end", "4" )
            , ( "max-height", "95vh" )
            , ( "overflow", "auto" )
            , ( "padding", "0 10px" )
            ]
        ]
        [ h2 [ style [ ( "font-size", "1rem" ) ] ] [ text "Feather" ]
        , p [] [ a [ href "https://feathericons.com/" ] [ text "https://feathericons.com/" ] ]
        , iconList model
        ]


iconList : Model -> Html Msg
iconList model =
    ul
        [ style
            [ ( "display", "grid" )
            , ( "grid-template-columns", "repeat(auto-fill, minmax(14rem, 1fr))" )
            , ( "grid-gap", "1rem" )
            , ( "margin", "0" )
            , ( "list-style", "none" )
            , ( "padding", "0" )
            ]
        ]
        (List.map (iconItem model.target) (List.sortBy .name model.icons))


iconItem : Maybe Icon -> Icon -> Html Msg
iconItem target icon =
    let
        icon_ =
            [ span [] [ Evg.toElm icon.raw ]
            , span
                [ style
                    [ ( "line-height", "1.5" )
                    , ( "margin-left", "10px" )
                    , ( "vertical-align", "top" )
                    ]
                ]
                [ text icon.name ]
            ]

        color =
            case target of
                Just icon_ ->
                    if icon_.name == icon.name then
                        "lightyellow"
                    else
                        "#EEE"

                Nothing ->
                    "#EEE"
    in
    li [ style [ ( "padding", "0" ) ] ]
        [ a
            [ style
                [ ( "background-color", color )
                , ( "display", "block" )
                , ( "padding", "20px" )
                , ( "color", "black" )
                , ( "text-decoration", "none" )
                , ( "align-items", "center" )
                ]
            , href ("#" ++ icon.name)
            , onClick (Select icon)
            ]
            icon_
        ]


header : String -> Html Msg
header value =
    h1
        [ style
            [ ( "grid-column-start", "span 2" )
            , ( "background-color", "lightblue" )
            , ( "margin", "0" )
            , ( "font-size", "1.2rem" )
            , ( "padding", "10px" )
            ]
        ]
        [ text value ]
