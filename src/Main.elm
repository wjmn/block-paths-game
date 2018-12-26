module Main exposing (BBox, Board, BoardGenMethod(..), BoardNum, BoardParams, BoardShape(..), BoardType(..), Coord, Level, Model, Msg(..), Path, Screen(..), WinType(..), boardOnClick, boardOnMouseMove, boardToSvg, coordDecoder, genLevel, genNewBoard, genNewBoardParams, genNewWinBoard, genPathFromLength, gridSvg, initialModel, isIntersect, listToString, main, pathDecoder, pathNodes, pathPolyline, pathToSvgString, randBool, transformBoard, update, view, winScreen)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (map2)
import List.Extra exposing (scanl, zip)
import Maybe exposing (..)
import Random
import String
import Svg exposing (Svg, circle, polyline, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Events as Events
import Time exposing (..)



--------------------------------------------------------------------------------
-- TODOS
--------------------------------------------------------------------------------
{-
   - Timer
   - Colourise paths and nodes
   - Aesthetics
   - Score calculation
   - Random board generation algorithms

-}
--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------


type Screen
    = StartScreen
    | BoardScreen Level
    | ScoreScreen


type alias BoardNum =
    Int


type BoardShape
    = Line
    | Square
    | Triangle


type BoardGenMethod
    = Ordered BoardShape
    | Random


type WinType
    = LessNodes
    | MoreNodes


type BoardType
    = Play
    | Goal


type alias BoardParams =
    { coordLim : Int
    , numPaths : Int
    , numNodes : Int
    , boardGenMethod : BoardGenMethod
    , winType : WinType
    , boxSize : Int
    }


type alias Level =
    { currentBoard : Board
    , winBoard : Board
    , boxMinPos : Maybe Coord
    , boardParams : BoardParams
    , playerMinPos : Maybe Coord
    , hasWonBoard : Bool
    }


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Path =
    { coords : List Coord
    , blocked : Bool
    }


type alias Board =
    List Path


type alias BBox =
    { min : Coord
    , max : Coord
    }


type alias Model =
    { screen : Screen
    , boardNum : BoardNum
    , numMoves : Int
    , clockTime : Int
    , hasWonGame : Bool
    , boardSize : Int
    }


type Msg
    = MouseMove Int Int
    | BoardClick Int Int
    | TickClock
    | SeeBoardScreenAction
    | SeeScoreScreenAction
    | SeeStartScreenAction



--------------------------------------------------------------------------------
-- GAME FUNCTIONS
--------------------------------------------------------------------------------


isIntersect : BBox -> List Coord -> Bool
isIntersect bbox path =
    let
        checkAcrossX c1 c2 =
            (c1.y >= bbox.min.y)
                && (c1.y <= bbox.max.y)
                && (c1.y == c2.y)
                && ((c1.x <= bbox.min.x && c2.x > bbox.max.x)
                        || (c2.x <= bbox.min.x && c1.x > bbox.max.x)
                   )

        checkAcrossY c1 c2 =
            (c1.x >= bbox.min.x)
                && (c1.x <= bbox.max.x)
                && (c1.x == c2.x)
                && ((c1.y <= bbox.min.y && c2.y > bbox.max.y)
                        || (c2.y <= bbox.min.y && c1.y > bbox.max.y)
                   )

        checkWithin c1 =
            (c1.x >= bbox.min.x)
                && (c1.x <= bbox.max.x)
                && (c1.y >= bbox.min.y)
                && (c1.y <= bbox.max.y)

        checkApply f vs =
            case vs of
                [] ->
                    False

                x :: xs ->
                    List.foldl (||) False (List.map2 f (x :: xs) xs)
    in
    checkApply checkAcrossX path
        || checkApply checkAcrossY path
        || List.foldl (||) False (List.map checkWithin path)



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


initialModel : Model
initialModel =
    { screen = StartScreen
    , boardNum = 0
    , numMoves = 0
    , clockTime = 0
    , hasWonGame = False
    , boardSize = 300
    }



--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MouseMove x y ->
            case model.screen of
                BoardScreen level ->
                    case level.hasWonBoard of
                        True ->
                            ( model, Cmd.none )

                        False ->
                            let
                                modifier =
                                    toFloat level.boardParams.coordLim / toFloat model.boardSize

                                halfBoxSize =
                                    toFloat level.boardParams.boxSize / 2

                                newPlayerMinLoc =
                                    Coord (round (toFloat x * modifier - halfBoxSize)) (round (toFloat y * modifier - halfBoxSize))
                            in
                            ( { model
                                | screen =
                                    BoardScreen
                                        { level
                                            | playerMinPos = Just newPlayerMinLoc
                                        }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        BoardClick x y ->
            case model.screen of
                BoardScreen level ->
                    case level.hasWonBoard of
                        True ->
                            ( model, Cmd.none )

                        False ->
                            let
                                modifier =
                                    toFloat level.boardParams.coordLim / toFloat model.boardSize

                                halfBoxSize =
                                    toFloat level.boardParams.boxSize / 2

                                newBoxMinLoc =
                                    Coord (round (toFloat x * modifier - halfBoxSize)) (round (toFloat y * modifier - halfBoxSize))

                                newBoard =
                                    transformBoard level newBoxMinLoc

                                newHasWonBoard =
                                    newBoard == level.winBoard

                                newNumMoves =
                                    model.numMoves + 1
                            in
                            ( { model
                                | screen =
                                    BoardScreen
                                        { level
                                            | boxMinPos = Just newBoxMinLoc
                                            , currentBoard = newBoard
                                            , hasWonBoard = newHasWonBoard
                                        }
                                , numMoves = newNumMoves
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        SeeBoardScreenAction ->
            let
                seed =
                    Random.initialSeed model.clockTime
            in
            case model.screen of
                StartScreen ->
                    let
                        newLevel =
                            genLevel seed model.boardNum
                    in
                    ( { model | screen = BoardScreen newLevel }, Cmd.none )

                BoardScreen level ->
                    case model.boardNum of
                        6 ->
                            ( { model | screen = ScoreScreen, hasWonGame = True }, Cmd.none )

                        _ ->
                            let
                                newBoardNum =
                                    model.boardNum + 1

                                newLevel =
                                    genLevel seed newBoardNum
                            in
                            ( { model | screen = BoardScreen newLevel, boardNum = newBoardNum }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SeeScoreScreenAction ->
            ( { model | screen = ScoreScreen, hasWonGame = True }, Cmd.none )

        SeeStartScreenAction ->
            ( initialModel, Cmd.none )

        TickClock ->
            ( { model | clockTime = model.clockTime + 1 }, Cmd.none )


genNewBoardParams : Random.Seed -> BoardNum -> BoardParams
genNewBoardParams _ boardNum =
    case boardNum of
        0 ->
            { coordLim = 16
            , numPaths = 2
            , numNodes = 2
            , boardGenMethod = Ordered Line
            , winType = LessNodes
            , boxSize = 2
            }

        1 ->
            { coordLim = 16
            , numPaths = 3
            , numNodes = 2
            , boardGenMethod = Ordered Square
            , winType = LessNodes
            , boxSize = 2
            }

        2 ->
            { coordLim = 16
            , numPaths = 4
            , numNodes = 2
            , boardGenMethod = Ordered Line
            , winType = MoreNodes
            , boxSize = 2
            }

        3 ->
            { coordLim = 16
            , numPaths = 5
            , numNodes = 3
            , boardGenMethod = Ordered Triangle
            , winType = MoreNodes
            , boxSize = 2
            }

        4 ->
            { coordLim = 16
            , numPaths = 6
            , numNodes = 3
            , boardGenMethod = Ordered Square
            , winType = MoreNodes
            , boxSize = 2
            }

        5 ->
            { coordLim = 16
            , numPaths = 7
            , numNodes = 3
            , boardGenMethod = Random
            , winType = LessNodes
            , boxSize = 4
            }

        6 ->
            { coordLim = 20
            , numPaths = 8
            , numNodes = 4
            , boardGenMethod = Random
            , winType = MoreNodes
            , boxSize = 4
            }

        _ ->
            { coordLim = 0, numPaths = 0, numNodes = 0, boardGenMethod = Random, winType = MoreNodes, boxSize = 4 }


genPathFromLength : Random.Seed -> Int -> Int -> ( Path, Random.Seed )
genPathFromLength s0 length lim =
    let
        randomList seed =
            Random.step (Random.list length (Random.int 2 (lim - 2))) seed

        ( xRList, s1 ) =
            randomList s0

        ( yRList, s2 ) =
            randomList s1

        repList list =
            List.foldr (++) [] (List.map (\x -> [ x, x ]) list)

        xList =
            repList xRList

        yList =
            case List.tail (repList yRList) of
                Just a ->
                    a

                Nothing ->
                    []

        coordsCandidate =
            zip xList yList

        ( yFirst, s3 ) =
            Random.step randBool s2
    in
    case yFirst of
        True ->
            ( { coords = List.map (\( c1, c2 ) -> { x = c1, y = c2 }) coordsCandidate, blocked = False }, s3 )

        False ->
            ( { coords = List.map (\( c2, c1 ) -> { x = c1, y = c2 }) coordsCandidate, blocked = False }, s3 )


randBool : Random.Generator Bool
randBool =
    Random.map ((==) 1) (Random.int 0 1)


genNewBoard : Random.Seed -> BoardParams -> Board
genNewBoard s0 params =
    let
        ( pathLengths, s1 ) =
            Random.step (Random.list params.numPaths (Random.float 0.5 1.5)) s0

        intPathLengths =
            List.map (\x -> ceiling (x * toFloat params.numNodes)) pathLengths
    in
    case params.boardGenMethod of
        _ ->
            -- Need to add other board Gen Methods later
            case intPathLengths of
                x :: xs ->
                    let
                        initial =
                            genPathFromLength s1 x params.coordLim
                    in
                    List.map (\( p, s ) -> p) (scanl (\a ( _, b ) -> genPathFromLength b a params.coordLim) initial xs)

                [] ->
                    [ { coords = [ { x = 0, y = 0 } ], blocked = False } ]


genNewWinBoard : Random.Seed -> Board -> BoardParams -> ( Board, Random.Seed )
genNewWinBoard s0 board params =
    let
        ( ( x, y ), s1 ) =
            Random.step (Random.pair (Random.int 2 (params.coordLim - 2)) (Random.int 2 (params.coordLim - 2))) s0

        bbox =
            { min = Coord x y, max = Coord (x + params.boxSize) (y + params.boxSize) }

        candidateBoard =
            List.map (\p -> { p | blocked = isIntersect bbox p.coords }) board

        intersections =
            List.foldr (||) False (List.map (\b -> b.blocked) candidateBoard)
    in
    case intersections of
        True ->
            ( candidateBoard, s1 )

        False ->
            genNewWinBoard s1 board params



{-
   False ->
       genNewWinBoard s0 board params

-}


genLevel : Random.Seed -> BoardNum -> Level
genLevel s0 boardNum =
    let
        newBoardParams =
            genNewBoardParams s0 boardNum

        newBoard =
            genNewBoard s0 newBoardParams

        ( newWinBoard, s1 ) =
            genNewWinBoard s0 newBoard newBoardParams
    in
    { currentBoard = newBoard
    , winBoard = newWinBoard
    , boxMinPos = Nothing
    , boardParams = newBoardParams
    , playerMinPos = Nothing
    , hasWonBoard = False
    }


transformBoard : Level -> Coord -> Board
transformBoard level newBoxMinLoc =
    let
        bbox =
            { min = newBoxMinLoc
            , max = Coord (newBoxMinLoc.x + level.boardParams.boxSize) (newBoxMinLoc.y + level.boardParams.boxSize)
            }
    in
    List.map (\path -> { path | blocked = isIntersect bbox path.coords }) level.currentBoard


boardOnClick : Svg.Attribute Msg
boardOnClick =
    let
        decoder =
            Decode.map2 (\x y -> BoardClick x y) (Decode.field "offsetX" Decode.int) (Decode.field "offsetY" Decode.int)
    in
    Events.on "click" decoder


boardOnMouseMove : Svg.Attribute Msg
boardOnMouseMove =
    let
        decoder =
            Decode.map2 (\x y -> MouseMove x y) (Decode.field "offsetX" Decode.int) (Decode.field "offsetY" Decode.int)
    in
    Events.on "mousemove" decoder


coordDecoder : Decode.Decoder Coord
coordDecoder =
    Decode.map2 Coord (Decode.field "x" Decode.int) (Decode.field "y" Decode.int)


pathDecoder : Decode.Decoder Path
pathDecoder =
    Decode.map2 Path (Decode.field "coords" (Decode.list coordDecoder)) (Decode.field "blocked" Decode.bool)



--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


pathToSvgString : List Coord -> String
pathToSvgString path =
    List.foldl (\s1 s2 -> s1 ++ " " ++ s2)
        ""
        (List.map (\c -> String.fromInt c.x ++ "," ++ String.fromInt c.y)
            path
        )


pathPolyline : Path -> Int -> Svg msg
pathPolyline path hue =
    case path.blocked of
        False ->
            polyline
                [ fill "none"
                , stroke (makeHslString hue)
                , strokeWidth "0.3"
                , strokeOpacity "0.4"
                , points (pathToSvgString path.coords)
                ]
                []

        True ->
            polyline
                [ fill "none"
                , stroke "#666666"
                , strokeWidth "0.3"
                , strokeOpacity "0.5"
                , points (pathToSvgString path.coords)
                ]
                []


makeHslString : Int -> String
makeHslString hue =
    "hsl(" ++ String.fromInt hue ++ ", 80%, 50%)"


pathNodes : Path -> BoardType -> Int -> List (Svg msg)
pathNodes path boardType hue =
    let
        firstNode =
            List.head path.coords

        lastNode =
            List.head (List.reverse path.coords)

        colourState blocked =
            case blocked of
                False ->
                    makeHslString hue

                True ->
                    "black"

        nodeSize =
            case boardType of
                Play ->
                    1

                Goal ->
                    1

        nodeMake fn =
            case fn of
                Just node ->
                    rect
                        [ x (String.fromFloat (toFloat node.x - (nodeSize / 2)))
                        , y (String.fromFloat (toFloat node.y - (nodeSize / 2)))
                        , width (String.fromInt nodeSize)
                        , height (String.fromInt nodeSize)
                        , opacity "0.6"
                        , fill (colourState path.blocked)
                        ]
                        []

                Nothing ->
                    rect [] []
    in
    [ nodeMake firstNode, nodeMake lastNode ]


gridSvg : Int -> List (Svg msg)
gridSvg size =
    let
        gridUnit xloc yloc =
            circle
                [ cx (String.fromInt xloc)
                , cy (String.fromInt yloc)
                , r "0.2"
                , fill "#aaaaaa"
                , opacity "0.5"
                ]
                []

        gridRow row =
            List.map (\n -> gridUnit (2 * n + 1) (2 * row + 1)) (List.range 0 (size // 2))
    in
    List.foldr (++) [] (List.map gridRow (List.range 0 size))


listToString : String -> List Int -> String
listToString sep xs =
    let
        strList =
            List.map String.fromInt xs
    in
    case strList of
        n :: ns ->
            List.foldl (\s1 s2 -> s1 ++ sep ++ s2) n ns

        [] ->
            ""


boardToSvg : BoardType -> Level -> Int -> Html Msg
boardToSvg boardType level boardSize =
    let
        boardParams =
            level.boardParams

        boxMinPos =
            level.boxMinPos

        playerMinPos =
            level.playerMinPos

        bleed =
            0.5

        borderColour =
            case level.hasWonBoard of
                True ->
                    "#62d052"

                False ->
                    "#fe7b95ff"

        boxToSvg objMinPos fillString opacityString =
            case objMinPos of
                Just boxMinLoc ->
                    rect
                        [ x (String.fromFloat (toFloat boxMinLoc.x - (bleed / 2)))
                        , y (String.fromFloat (toFloat boxMinLoc.y - (bleed / 2)))
                        , width (String.fromFloat (toFloat boardParams.boxSize + bleed))
                        , height (String.fromFloat (toFloat boardParams.boxSize + bleed))
                        , fill fillString
                        , opacity opacityString
                        ]
                        []

                Nothing ->
                    rect [ x "0", y "0", fill "transparent", width "0", height "0" ]
                        []

        bgRect =
            rect
                [ x "0.1"
                , y "0.1"
                , rx "1"
                , ry "1"
                , width (String.fromFloat (toFloat boardParams.coordLim - 0.2))
                , height (String.fromFloat (toFloat boardParams.coordLim - 0.2))
                , fill "white"
                , stroke borderColour
                , strokeWidth "0.25"
                ]
                []

        fullBoardRect =
            rect
                [ x "0"
                , y "0"
                , rx "1"
                , ry "1"
                , width (String.fromInt boardParams.coordLim)
                , height (String.fromInt boardParams.coordLim)
                , fill "transparent"
                , stroke "transparent"
                , strokeWidth "0"
                , boardOnClick
                , boardOnMouseMove
                ]
                []

        seed =
            Random.initialSeed 1

        ( hues, s1 ) =
            Random.step (Random.list (List.length level.currentBoard) (Random.int 0 255)) seed
    in
    case boardType of
        Play ->
            let
                board =
                    level.currentBoard
            in
            svg
                [ width (String.fromInt boardSize)
                , height (String.fromInt boardSize)
                , viewBox (listToString " " [ boardParams.coordLim, boardParams.coordLim, 0, 0 ])
                ]
                ([ bgRect ]
                    ++ [ boxToSvg playerMinPos "#bbbbbb" "1" ]
                    ++ List.map2 pathPolyline board hues
                    ++ gridSvg boardParams.coordLim
                    ++ List.foldr (++) [] (List.map2 (\x y -> pathNodes x boardType y) board hues)
                    ++ [ boxToSvg boxMinPos "#222222" "0.7" ]
                    ++ [ fullBoardRect ]
                )

        Goal ->
            let
                board =
                    level.winBoard
            in
            svg
                [ width (String.fromInt (round (toFloat boardSize / 2)))
                , height (String.fromInt (round (toFloat boardSize / 2)))
                , viewBox (listToString " " [ boardParams.coordLim, boardParams.coordLim, 0, 0 ])
                ]
                ([ bgRect ]
                    ++ List.foldr (++) [] (List.map2 (\x y -> pathNodes x boardType y) board hues)
                )


winScreen : Bool -> Html msg
winScreen hasWon =
    case hasWon of
        True ->
            div []
                [ h1 []
                    [ text "LEVEL COMPLETE" ]
                ]

        False ->
            div [] []


startScreenView : Html Msg
startScreenView =
    div [ class "screen" ]
        [ h2 [] [ text "Localise" ]
        , div [ class "block-text-container" ] [ div [ class "block-text" ] [ text "A game of tangled wires." ] ]
        , div [ id "start-button" ]
            [ button [ onClick SeeBoardScreenAction ] [ text "Start" ]
            ]
        ]


boardScreenView : Model -> Level -> Html Msg
boardScreenView model level =
    let
        finishVisible =
            case level.hasWonBoard of
                True ->
                    "visibility: visible;"

                False ->
                    "visibility: hidden;"
    in
    div [ class "screen" ]
        [ h2 [ id "board-title" ] [ text "Localise" ]
        , div [ id "play-board" ]
            [ boardToSvg Play level model.boardSize ]
        , div [ id "win-board" ]
            [ boardToSvg Goal level model.boardSize ]
        , div [ id "controls" ]
            [ div [ id "control-container" ]
                [ div [] [ text ("Time: " ++ String.fromInt model.clockTime) ]
                , div [] [ text ("Moves: " ++ String.fromInt model.numMoves) ]
                , div [] [ text ("Level: " ++ String.fromInt model.boardNum ++ "/6") ]
                ]
            , button [ id "next-level-button", onClick SeeBoardScreenAction, style finishVisible ] [ text "Next" ]
            ]
        ]


scoreScreenView : Model -> Html Msg
scoreScreenView model =
    let
        totalScore = (7 * 140 * 1000) // (model.numMoves * model.clockTime)
    in
    
    div [ class "screen" ]
        [ h2 []
            [ text "Score" ]
        , div [ id "score-contrib-container" ]
            [ div [ class "sc-row" ]
                [ div [ class "sc-left" ]
                    [ text "Time" ]
                , div [ class "sc-right" ]
                    [ text (String.fromInt model.clockTime) ]
                ]
            , div [class "sc-row"] 
                [div [class "sc-left"]
                    [text "Moves"]
                , div [class "sc-right"]
                    [text (String.fromInt model.numMoves)]]
            ]
        , div [id "total-score"] [text (String.fromInt totalScore)]
        , button [onClick SeeStartScreenAction] [text "Restart"]
        ]


view : Model -> Html Msg
view model =
    case model.screen of
        StartScreen ->
            startScreenView

        BoardScreen level ->
            boardScreenView model level

        ScoreScreen ->
            scoreScreenView model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        BoardScreen _ ->
            every 1000 (\x -> TickClock)

        _ ->
            Sub.none



-- MAIN ------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
