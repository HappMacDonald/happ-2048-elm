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
import Browser.Events
import Html exposing (Html)
import Array exposing (Array)
import Html.Events
import Json.Decode as Decode



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
        -- (   10, 2 )
        [   (   10, 4 )
        -- ,   (   10, 8 )
        -- ,   (   10, 16 )
        -- ,   (   10, 32 )
        -- ,   (   10, 64 )
        -- ,   (   10, 128 )
        -- ,   (   10, 256 )
        -- ,   (   10, 512 )
        -- ,   (   10, 1024 )
        -- ,   (   10, 2048 )
        -- ,   (   10, 4096 )
        ]

interCellX = 15
interCellY = 15
cellEdge = 15
cellRoundingRadius = 3
cellWidth = 106
cellHeight = 106
gameRoundingRadius = 6
gameFontColor = (Element.rgb255 0x77 0x6e 0x65)
reverseFontColor = (Element.rgb255 0xf9 0xf6 0xf2)
gameBackground = (Element.rgb255 0xfa 0xf8 0xef)
gameBorderColor = (Element.rgb255 0xbb 0xad 0xa0)


polynomial : List Float -> Float -> Float
polynomial coefficients variable =
    case coefficients of
        [] ->
            0 -- addititve identity
      
        zeroCoefficient :: tail ->
            zeroCoefficient + variable * ( polynomial tail variable )


cellFontSize : String -> Int
cellFontSize value =
    if value == " " then 55
    else
        value
        |>  String.length
        |>  toFloat
        |>  (\x -> 180 / (x+1) )
        |>  round
        |>  min 55


cellFontColor : String -> Element.Color
cellFontColor value =
    let
        valueLog =
            value
            |>  String.toFloat
            |>  Maybe.withDefault 0
            |>  logBase 2

    in
    if valueLog<3 then gameFontColor
    else reverseFontColor

cellBackgroundColorRed : Float -> Int
cellBackgroundColorRed valueLog = 
    (   if valueLog < 6.9 then
        polynomial [255.1667, -30.14815, 15.71528, -2.949074, 0.1875] valueLog
        else polynomial [237] valueLog
    )
    |>  round


cellBackgroundColorGreen : Float -> Int
cellBackgroundColorGreen valueLog =
    (   if valueLog < 6.9
        -- then polynomial [254.8, -26.8] valueLog
        then polynomial [120.6667, 199.2487, -111.8472, 21.73148, -1.458333] valueLog
        else polynomial [229.75, -3.25] valueLog
    )
    |>  round


cellBackgroundColorBlue : Float -> Int
cellBackgroundColorBlue valueLog =
    (   if valueLog < 6.9
        then
            polynomial
                [   1314.34545454545537433326
                ,   -3115.96692190163013333414
                ,   3462.88659765071792963762
                ,   -1951.21196581196733660399
                ,   602.94741075917596278009
                ,   -103.89124183006545178397
                ,   9.38143790849674109432
                ,   -0.34659197012138225229
                ]
                valueLog
        else polynomial [233, -17] valueLog
    )
    |>  round


cellBackgroundColor : String -> Element.Color
cellBackgroundColor value =
    let
        valueLog =
            value
            |>  String.toFloat
            |>  Maybe.withDefault 0
            |>  logBase 2

    in
        if value == " "
        then Element.rgba255 238 228 218 0.35
        else
            Element.rgb255
                ( cellBackgroundColorRed valueLog )
                ( cellBackgroundColorGreen valueLog )
                ( cellBackgroundColorBlue valueLog )



sixtyThirds : String -> Int -> Float
sixtyThirds value multiplier =
    if value == " " then 0
    else
        value
        |>  String.length
        |>  (*) multiplier
        |>  toFloat
        |>  (\x -> x / 63)
        |>  max 0


outerGlowRadius = 30
outerGlowColor : String -> Element.Color
outerGlowColor value =
    sixtyThirds value 5
    |> Element.rgba 243 215 116


innerGlowRadius = 1
innerGlowColor : String -> Element.Color
innerGlowColor value =
    sixtyThirds value 5
    |> Element.rgba 0xFF 0xFF 0xFF



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
        -- ,   createdOn: Time..?
        }
    |   OutOfBounds


type alias GameBoard =
    Array Cell

type alias BoardIterator =
    { boardSoFar : GameBoard, index : Int }



type alias Model =
    {   board : GameBoard
    ,   currentSeed : Random.Seed
    }


-- HELPER FUNCTIONS


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

                OutOfBounds ->
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

                OutOfBounds ->
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


playOffset : Offset -> Model -> Model
playOffset (Offset x y) ({board, currentSeed} as model) =
    let
        offset1D =
            y*4 + x
        
        walkDirection =
            if offset1D > 0
            then -1 -- you're walking against the direction tiles slide
            else 1
        
        walkerFunction =
            if offset1D > 0
            then Array.foldr
            else Array.foldl

        maxPosition =
            boardWidth * boardHeight - 1

        firstPosition =
            if offset1D > 0
            then maxPosition
            else 0

        walkFunction : Cell -> BoardIterator -> BoardIterator
        walkFunction cell {boardSoFar, index} =
            let
                targetIndexMaybe : Maybe Int -- "Nothing" means leaving bounds
                targetIndexMaybe =
                    let
                        uncookedTargetIndex =
                            index + offset1D
                    in
                        (   --  check top bounds
                            if  uncookedTargetIndex < 0

                            --  check bottom bounds
                            ||  uncookedTargetIndex > maxPosition

                            --  check left bounds
                            --  clever trick to measure distance from left side
                            --  -x represents "how far we're supposed to move left"
                            ||  modBy boardWidth index < -x

                            --  check right bounds
                            --  even cleverer trick to measure distance from right side
                            --  x represents "how far we're supposed to move right"
                            ||  modBy boardWidth (-1-index) < x

                            then Nothing
                            else Just uncookedTargetIndex
                        )

                targetCellOld =
                    case targetIndexMaybe of
                        Nothing ->
                            OutOfBounds

                        Just targetIndex ->
                            boardSoFar
                            |>  Array.get targetIndex
                            -- "Nothing" should be unobtainable, but still..
                            |>  Maybe.withDefault OutOfBounds

                targetCellNew =
                    case targetCellOld of
                        EmptyCell ->
                            cell -- replace empty with what we're moving

                        StaticCell _ ->
                            targetCellOld -- no movement

                        DynamicCell _ ->
                            targetCellOld -- no movement

                        OutOfBounds ->
                            targetCellOld -- no movement

                currentCellNew =
                    case targetCellOld of
                        EmptyCell ->
                            EmptyCell -- Our tile moved away

                        StaticCell _ ->
                            cell -- no movement

                        DynamicCell _ ->
                            cell -- no movement

                        OutOfBounds ->
                            cell -- no movement

                boardNew =
                    case targetIndexMaybe of
                        Nothing ->
                            boardSoFar -- guaranteed no movement

                        Just targetIndex ->
                            boardSoFar -- still possibly no movement
                            |>  Array.set index currentCellNew
                            |>  Array.set targetIndex targetCellNew

            in
            {   boardSoFar = boardNew
            ,   index = index + walkDirection
            }


    in
        {   model
        |   board =
                board
                |>  walkerFunction
                        walkFunction
                        {   boardSoFar = board
                        ,   index = firstPosition
                        }
                |>  .boardSoFar
        }


sprinkle : Model -> Model
sprinkle ({board, currentSeed} as model) =
    let
        ( emptyIndex, seed0 ) =
            Random.step
                (   countEmptyCells board
                |>  decrementBy 1
                |>  Random.int 0
                )
                currentSeed

        ( cellValue, seed1 ) =
            Random.step
                (   sprinkleValueGenerator
                )
                seed0
        
    in
        {   model
        |   board =
                injectNthEmpty emptyIndex ( StaticCell cellValue ) board
        ,   currentSeed =
                seed1
        }


-- INIT


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
    |   KeyDown Offset
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
            ,   Cmd.none
            )

        KeyDown offset ->
            (   model
            |>  playOffset offset
            |>  sprinkle
            ,   Cmd.none
            )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    -- By itself, `Events.onKeyDown keyDecoder` will create a subscription
    -- of keys, not messages. The `Sub.map` here tells Elm to wrap all values
    -- from that subscription in our `KeyDown` message
    Sub.map KeyDown (Browser.Events.onKeyDown keyDecoder)


keyDecoder : Decode.Decoder Offset
keyDecoder =
    -- Decode a custom type as describe here:
    -- https://thoughtbot.com/blog/5-common-json-decoders#1---decoding-union-types
    Decode.field "key" Decode.string
        |> Decode.andThen offsetFromString


offsetFromString : String -> Decode.Decoder Offset
offsetFromString string =
    case string of
        "ArrowUp" ->
            Offset 0 -1
            |>  Decode.succeed

        "ArrowDown" ->
            Offset 0 1
            |>  Decode.succeed

        "ArrowLeft" ->
            Offset -1 0
            |>  Decode.succeed

        "ArrowRight" ->
            Offset 1 0
            |>  Decode.succeed

        _ ->
            -- Fail the decoder if not an arrow key. When used in an event
            -- handler events that failed to decode are ignored
            Decode.fail (string ++ " is not an arrow key")


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

                            OutOfBounds ->
                                " "

                    displayCell =
                        displayValue
                        |>  Element.text 
                        |>  Element.el
                            [   Element.width Element.fill
                            ,   Font.center
                            ,   displayValue
                                |>  cellFontSize
                                |>  Font.size 
                            ,   Font.bold
                            ,   displayValue
                                |>  cellFontColor
                                |>  Font.color
                            ,   Element.centerX
                            ,   Element.centerY
                            ]
                        |>  Element.el
                            [   Element.width (Element.px cellWidth)
                            ,   Element.height (Element.px cellHeight)
                            ,   Font.center
                            ,   Element.centerX
                            ,   Element.centerY
                            ,   displayValue
                                |>  cellBackgroundColor
                                |>  Background.color
                            ,   Border.rounded cellRoundingRadius
                            ,   Border.glow
                                    ( outerGlowColor displayValue )
                                    outerGlowRadius
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
                                    ,   Element.spacing interCellY
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
        [   Element.centerX
        ,   Element.centerY
        ,   Element.spacing interCellX
        ,   Element.padding cellEdge
        ,   Background.color gameBorderColor
        ,   Border.rounded gameRoundingRadius
        ]
    |>  Element.layout
        [   Element.width Element.fill
        ,   Element.height Element.fill
        ,   Element.centerX
        ,   Element.centerY
        ,   Background.color gameBackground
        ,   Font.color gameFontColor
        ,   Font.family
            [   Font.typeface "Clear Sans"
            ,   Font.typeface "Helvetica Neue"
            ,   Font.typeface "Arial"
            ,   Font.sansSerif
            ]
        ]

