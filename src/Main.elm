module Main exposing (main)

import Random
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Browser
import Html exposing (Html)

-- PRIMARY DECLARATION


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model = List ( List Int )


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( List.repeat
        4
        <|List.repeat 4 0
    , Cmd.none )


-- UPDATE


type Msg
    = Reset
    -- | Up
    -- | Down
    -- | Left
    -- | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    model
    |>  List.map
        (\row ->
            row
            |>  List.map
                (\cell ->
                    cell
                    |>  String.fromInt
                    |>  Element.text 
                    |>  Element.el
                        [   Element.width Element.fill
                        ,   Font.center
                        -- ,   Element.height Element.fill
                        ,   Element.centerX
                        ,   Element.centerY
                        ]
                )
            |>  Element.row
                [   Element.width Element.fill
                ,   Element.height Element.fill
                ,   Element.centerX
                ,   Element.centerY
                ]
        )
        |>  Element.column
            [   Element.width <| Element.px 400
            ,   Element.height <| Element.px 400
            ,   Element.centerX
            ,   Element.centerY
            ,   Border.color <| Element.rgb 0.5 0.5 0.5
            ,   Border.width 1
            ]
    |>  Element.layout
        [   Element.width Element.fill
        ,   Element.height Element.fill
        ,   Element.centerX
        ,   Element.centerY
        ]

