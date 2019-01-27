module Main exposing (main)

{-|
HappMacDonald [1:52 PM]
Say, in Elm, what's the easiest way to say "find the index of the Nth item that matches a given filter" or "find how many items match a given filter" so that I can decrement N by that amount and then start checking the next array in a queue of them?


lysergia [1:54 PM]
for the first, I'd write a function that recurses until it hits the end of the list or finds a match. for the second, you can just use List.filter & List.length
e.g.
```findNthMatch : Int -> (a -> Bool) -> List a -> Maybe Int
findNthMatch n doesMatch items =
  let
    recurse : (Int, Maybe Int) -> List a -> (Int, Maybe Int)
  in
    Tuple.second <| recurse (0, Nothing) items```
(edited)
With a `case ... of` on the list in `recurse`

-}

import Random
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Browser
import Html exposing (Html)
import Array exposing (Array)


-- TOP LEVEL CONSTANTS

boardWidth : Int
boardWidth =
    4


    
boardHeight : Int
boardHeight =
    4


sprinkleValueGenerator : Random.Generator Int
sprinkleValueGenerator =
    Random.weighted
        (   90, 2 )
        [   (   10, 4 ) ]



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


type alias GameBoard =
    Array Cell


type alias Model =
    {   board : GameBoard
    ,   currentSeed : Random.Seed
    }


-- INIT


countEmptyCells : GameBoard -> Int
countEmptyCells board =
    Array.foldl
        (\cell empties ->
            case cell of
                EmptyCell ->
                    empties + 1
                    
                StaticCell _ ->
                    empties

                DynamicCell _ ->
                    empties

        )
        0
        board


injectNthEmpty : Int -> Cell -> GameBoard -> GameBoard
injectNthEmpty injectIndex injectCell board =
    board
    |> Array.foldr
        (\cell (emptyIndex, resultList) ->
            case cell of
                EmptyCell ->
                    if emptyIndex == 0
                        then (-1, injectCell :: resultList)
                        else (emptyIndex-1, cell :: resultList)
                    
                StaticCell _ ->
                    (emptyIndex, cell :: resultList)

                DynamicCell _ ->
                    (emptyIndex, cell :: resultList)

        )
        (injectIndex, [])
    |> Tuple.second
    |> Array.fromList


incrementBy : Int -> Int -> Int
incrementBy offset base =
    base + offset


decrementBy : Int -> Int -> Int
decrementBy offset base =
    base - offset


sprinkle : Model -> Model
sprinkle ({board, currentSeed} as model) =
    let
        ( emptyIndex, seed0 ) =
            Random.step
                (   countEmptyCells board
                -- |>  Debug.log "empty cells"
                |>  decrementBy 1
                |>  Random.int 0
                )
                currentSeed

        ( cellValue, seed1 ) =
            Random.step
                (   sprinkleValueGenerator
                )
                seed0

        -- x = Debug.log "sprinkle to " emptyIndex
        
    in
        {   model
        |   board =
                injectNthEmpty emptyIndex ( StaticCell cellValue ) board
        ,   currentSeed =
                seed1
        }



init : () -> ( Model, Cmd Msg )
init _ =
    (   {   board =
                Array.repeat ( boardWidth * boardHeight ) EmptyCell
        ,   currentSeed =
                Random.initialSeed 0 -- just a placeholder
        }
    ,   Random.generate Seed Random.independentSeed
    )


-- UPDATE


type Msg
    =   Reset
    |   Seed Random.Seed
    -- |   Sprinkle
    --     {   emptyIndex : Int
    --     ,   value : Int
    --     }
    
    -- |   Up
    -- |   Down
    -- |   Left
    -- |   Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        Seed seed ->
            (   {   model
                |   currentSeed = seed
                }
                |> sprinkle 
                |> sprinkle
                -- |> Debug.log "post sprinkle model"
            ,   Cmd.none
            )        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view {board} =
    board
    |>  Array.foldr
            (\cell {cellIndex, resultCells, resultRows} ->
                let
                    displayValue =
                        case cell of
                            EmptyCell ->
                                " "

                            StaticCell current ->
                                String.fromInt current

                            DynamicCell { merged } ->
                                String.fromInt merged

                    displayCell =
                        displayValue
                        |>  Element.text 
                        |>  Element.el
                            [   Element.width Element.fill
                            ,   Font.center
                            ,   Font.size 48
                            ,   Element.centerX
                            ,   Element.centerY
                            ]
                        |>  Element.el
                            [   Element.width Element.fill
                            ,   Element.height Element.fill
                            ,   Font.center
                            ,   Element.centerX
                            ,   Element.centerY
                            ,   Border.color <| Element.rgb 0.5 0.5 0.5
                            ,   Border.width 1
                            ]

                in
                    if modBy boardWidth cellIndex == 0
                        then
                            {   cellIndex = cellIndex - 1
                            ,   resultCells = []
                            ,   resultRows =
                                (   ( displayCell :: resultCells )
                                |>  Element.row
                                    [   Element.width Element.fill
                                    ,   Element.height Element.fill
                                    ,   Element.centerX
                                    ,   Element.centerY
                                    ,   Element.padding 2
                                    ,   Element.spacingXY 2 0
                                    ]
                                )
                                :: resultRows
                            }
                        else
                            {   cellIndex = cellIndex - 1
                            ,   resultCells = displayCell :: resultCells
                            ,   resultRows = resultRows
                            }
            )
            {   cellIndex = (Array.length board) - 1
            ,   resultCells = []
            ,   resultRows = []
            }
    |> .resultRows
    |>  Element.column
        [   Element.width <| Element.px 400
        ,   Element.height <| Element.px 400
        ,   Element.centerX
        ,   Element.centerY
        ,   Element.padding 2
        ,   Element.spacingXY 0 2
        ,   Border.color <| Element.rgb 0.5 0.5 0.5
        ,   Border.width 1
        ]
    |>  Element.layout
        [   Element.width Element.fill
        ,   Element.height Element.fill
        ,   Element.centerX
        ,   Element.centerY
        ]

