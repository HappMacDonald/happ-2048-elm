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

type Offset = Offset Int Int
type Cell
    =   EmptyCell
    |   StaticCell Int
    |   DynamicCell
        {   merged : Int
        ,   incumbent : Int
        ,   moving : Int
        ,   from : Offset
        }
type alias Model = List ( List Cell )


-- INIT


sprinkle : Int -> Cmd Msg
sprinkle emptyCells =
    Random.generate
        Sprinkle
        <|  Random.map2
                (\emptyIndex value ->
                    {   emptyIndex = emptyIndex
                    ,   value = value
                    }
                )
                ( Random.int 0 15 )
                (   Random.weighted
                        (   90, 2 )
                        [   (   10, 4 ) ]
                )


init : () -> ( Model, Cmd Msg )
init _ =
    (   List.repeat
            4
            <|  List.repeat 4 EmptyCell
    ,   Cmd.batch [ sprinkle 16, sprinkle 15 ]
    )


-- UPDATE


type Msg
    =   Reset
    |   Sprinkle
        {   emptyIndex : Int
        ,   value : Int
        }
    
    -- |   Up
    -- |   Down
    -- |   Left
    -- |   Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        Sprinkle { emptyIndex, value } ->
            (   model
                |>  List.indexedMap
                    (\rowIndex row ->
                        row
                        |>  List.indexedMap
                            (\columnIndex cell ->
                                if x == rowIndex && y == columnIndex
                                    then StaticCell value
                                    else cell
                            )
                    )
            ,   Cmd.none
            )
        

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
                    let
                        display = case cell of
                            EmptyCell ->
                                ""

                            StaticCell current ->
                                String.fromInt current

                            DynamicCell { merged } ->
                                String.fromInt merged

                    in
                    display
                    |>  Element.text 
                    |>  Element.el
                        [   Element.width Element.fill
                        ,   Font.center
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

