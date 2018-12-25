module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import List exposing (map2)
import Svg exposing (polyline, rect, svg, Svg)
import Svg.Attributes exposing (..)
import Svg.Events as Events
import String
import Maybe exposing (..)
import Json.Decode as Decode

-- EXAMPLE BOARD ---------------------------------------------------------------

exampleBoard = [ { coords = [ Coord 4 4
                            , Coord 8 4
                            , Coord 8 18
                            , Coord 26 18
                            , Coord 26 28
                            , Coord 28 28]
                 , on = True}
               , { coords = [ Coord 4 28
                            , Coord 7 28
                            , Coord 7 7
                            , Coord 28 7
                            , Coord 28 4]
                 , on = True}
               , { coords = [ Coord 12 20
                            , Coord 12 17
                            , Coord 9 17
                            , Coord 9 12
                            , Coord 12 12]
                 , on = True}
               , { coords = [ Coord 20 20
                            , Coord 20 24
                            , Coord 10 24
                            , Coord 10 8
                            , Coord 20 8
                            , Coord 20 12]
                 , on = True }
               , { coords = [ Coord 12 28
                            , Coord 12 30
                            , Coord 2 30
                            , Coord 2 12
                            , Coord 4 12]
                 , on = True}
               , { coords = [ Coord 4 20
                            , Coord 4 25
                            , Coord 20 25
                            , Coord 20 28]
                 , on = True}
               , { coords = [ Coord 12 4
                            , Coord 12 9
                            , Coord 16 9
                            , Coord 16 16
                            , Coord 28 16
                            , Coord 28 20]
                 , on = True}
               , { coords = [ Coord 20 4
                            , Coord 20 6
                            , Coord 24 6
                            , Coord 24 12
                            , Coord 28 12]
                 , on = True}]

exampleWin = List.map2 (\x y -> {x | on = y}) exampleBoard [False, False, False, False, True, True, True, True]

-- LOGIC -----------------------------------------------------------------------

type alias Coord = { x : Int
                   , y : Int}

type alias Path = { coords : List Coord
                  , on : Bool}

type alias Board = List Path

type alias BBox = { min : Coord
                  , max : Coord}

isIntersect : BBox -> List Coord -> Bool
isIntersect bbox path =
    let
        checkAcrossX c1 c2 = (c1.y >= bbox.min.y) &&
                             (c1.y <= bbox.max.y) &&
                             (c1.y == c2.y) &&
                             ((c1.x <= bbox.min.x && c2.x > bbox.max.x) ||
                              (c2.x <= bbox.min.x && c1.x > bbox.max.x))
        checkAcrossY c1 c2 = (c1.x >= bbox.min.x) &&
                             (c1.x <= bbox.max.x) &&
                             (c1.x == c2.x) &&
                             ((c1.y <= bbox.min.y && c2.y > bbox.max.y) ||
                              (c2.y <= bbox.min.y && c1.y > bbox.max.y))
        checkWithin c1 = (c1.x >= bbox.min.x) &&
                         (c1.x <= bbox.max.x) &&
                         (c1.y >= bbox.min.y) &&
                         (c1.y <= bbox.max.y)
        checkApply f vs = case vs of
                                [] -> False
                                (x::xs) -> List.foldl (||) False (List.map2 f (x::xs) (xs))
    in
        (checkApply checkAcrossX path) ||
        (checkApply checkAcrossY path) ||
        (List.foldl (||) False (List.map checkWithin path))

-- MODEL -----------------------------------------------------------------------

type alias Model = { currentBoard : Board
                   , winBoard : Board
                   , boxOn : Bool
                   , boxSize : Int
                   , boxMinLoc : Coord
                   , playerHoverLoc : Coord
                   , playerOnBoard : Bool
                   , numMoves : Int
                   , hasWon : Bool }

initialModel = { currentBoard = exampleBoard
                , winBoard = exampleWin
                , boxOn = False
                , boxSize = 3
                , boxMinLoc = Coord 0 0
                , playerHoverLoc = Coord 0 0
                , playerOnBoard = False
                , numMoves = 0
                , hasWon = False}

-- UPDDATE ---------------------------------------------------------------------

type Msg = MouseMove Int Int Int Int
         | BoardClick Int Int Int Int

update msg model =
  case msg of
    MouseMove x y size lim ->
        let
            _ = Debug.log "x is" x
            modifier = (toFloat lim) / (toFloat size)
            newPlayerHoverLoc = Coord (round ((toFloat x) * modifier) - 2) (round ((toFloat y) * modifier) - 2)
        in
            ({ model
                | playerOnBoard = True
                , playerHoverLoc = newPlayerHoverLoc
            }, Cmd.none)

    BoardClick x y size lim ->
        let
            _ = Debug.log "x is" x
            modifier = (toFloat lim) / (toFloat size)
            newBoxMinLoc = Coord (round ((toFloat x) * modifier) - 2) (round ((toFloat y) * modifier) - 2)
            newBoard = transformBoard model newBoxMinLoc
            newHasWon = (newBoard == model.winBoard)
            newNumMoves = model.numMoves + 1
        in
            ({ model
                | boxOn = True
                , boxMinLoc = newBoxMinLoc
                , currentBoard = newBoard
                , hasWon = newHasWon
                , numMoves = newNumMoves
            },
                 Cmd.none
                 )

transformBoard : Model -> Coord -> Board
transformBoard model newBoxMinLoc =
    let
        bbox = { min = newBoxMinLoc
               , max = Coord (newBoxMinLoc.x + model.boxSize) (newBoxMinLoc.y + model.boxSize)}
    in
        List.map (\path -> {path | on = not (isIntersect bbox path.coords)}) model.currentBoard

boardOnClick : Int -> Int -> Svg.Attribute Msg
boardOnClick boardSize coordLim =
  let
    decoder = Decode.map2 (\x y -> BoardClick x y boardSize coordLim) (Decode.field "offsetX" Decode.int) (Decode.field "offsetY" Decode.int)
  in
    Events.on "click" decoder

boardOnMouseMove : Int -> Int -> Svg.Attribute Msg
boardOnMouseMove boardSize coordLim =
  let
    decoder = Decode.map2 (\x y -> MouseMove x y boardSize coordLim) (Decode.field "offsetX" Decode.int) (Decode.field "offsetY" Decode.int)
  in
    Events.on "mousemove" decoder


coordDecoder : Decode.Decoder Coord
coordDecoder = Decode.map2 Coord (Decode.field "x" Decode.int) (Decode.field "y" Decode.int)

pathDecoder : Decode.Decoder Path
pathDecoder = Decode.map2 Path (Decode.field "coords" (Decode.list coordDecoder)) (Decode.field "on" Decode.bool)


-- VIEW ------------------------------------------------------------------------

type BoardType = Play | Goal

pathToSvgString : List Coord -> String
pathToSvgString path = List.foldl (\s1 s2 -> s1 ++ " " ++ s2) ""
                       (List.map (\c -> String.fromInt(c.x) ++ "," ++ String.fromInt(c.y))
                            path)

pathPolyline : Path -> Svg msg
pathPolyline path = case path.on of
                        True -> polyline [ fill  "none"
                                         , stroke "red"
                                         , strokeWidth "0.4"
                                         , strokeOpacity "0.4"
                                         , points (pathToSvgString path.coords)]
                                []
                        False -> polyline [ fill "none"
                                          , stroke "#666666"
                                          , strokeWidth "0.3"
                                          , strokeOpacity "0.5"
                                          , points (pathToSvgString path.coords)]
                                 []

pathNodes : Path -> List (Svg msg)
pathNodes path =
    let
        firstNode = List.head path.coords
        lastNode = List.head (List.reverse path.coords)
        colourState on = case on of
                             True -> "red"
                             False -> "black"
        nodeSize = 3
        nodeMake fn = case fn of
                        Just node -> rect [ x (String.fromFloat (toFloat(node.x) - (nodeSize / 2)))
                                          , y (String.fromFloat (toFloat(node.y) - (nodeSize / 2)))
                                          , width (String.fromInt nodeSize)
                                          , height (String.fromInt nodeSize)
                                          , fill (colourState path.on)] []
                        Nothing -> rect [] []
    in
        [nodeMake firstNode, nodeMake lastNode]

gridSvg : Int -> List (Svg msg)
gridSvg size =
    let
        gridUnit xloc yloc = rect [ x (String.fromInt xloc)
                            , y (String.fromInt yloc)
                            , width "2"
                            , height "2"
                            , fill "transparent"
                            , stroke "#aaaaaa"
                            , strokeWidth "0.05"
                            , strokeOpacity "0.5"]
                       []
        gridRow row = List.map (\n -> gridUnit n row) (List.range 0 size)
    in
        List.foldr (++) [] (List.map gridRow (List.range 0 size))

listToString : String -> List Int -> String
listToString sep xs =
    let
        strList = (List.map String.fromInt xs)
    in
        case strList of
            (n::ns) -> 
                List.foldl (\s1 s2 -> s1 ++ sep ++ s2) n ns
            [] -> ""

boardToSvg : BoardType -> Int -> Board -> Bool -> Int -> Coord -> Bool -> Coord -> Html Msg
boardToSvg boardType boardSize board boxOn boxSize boxMinLoc playerOnBoard playerHoverLoc=
    let
        coordLimit = 32
        bleed = 0.5
        boxToSvg on fillString opacityString = case on of
                          True -> rect [ x (String.fromFloat ((toFloat boxMinLoc.x) - (bleed / 2)))
                                       , y (String.fromFloat ((toFloat boxMinLoc.y) - (bleed / 2)))
                                       , width (String.fromFloat ((toFloat boxSize) + bleed))
                                       , height (String.fromFloat ((toFloat boxSize) + bleed))
                                       , fill fillString
                                       , opacity opacityString]
                                  []
                          False -> rect [x "0", y "0", fill "transparent", width "0", height "0"]
                                   []
        fullBoardRect  = case boardType of
                             Play -> rect [ x "0"
                                          , y "0"
                                          , width (String.fromInt (coordLimit))
                                          , height (String.fromInt (coordLimit))
                                          , fill "transparent"
                                          , stroke "black"
                                          , strokeWidth "2"
                                          , boardOnClick boardSize coordLimit] []
                             Goal -> rect [ x "0"
                                          , y "0"
                                          , width (String.fromInt (coordLimit))
                                          , height (String.fromInt (coordLimit))
                                          , fill "transparent"
                                          , stroke "yellow"
                                          , strokeWidth "2"] []
    in
        case boardType of
                Play -> svg [ width (String.fromInt boardSize)
                            , height (String.fromInt boardSize)
                            , viewBox (listToString " " [coordLimit, coordLimit, 0, 0])]
                        ((List.map pathPolyline board) ++
                             [(boxToSvg playerOnBoard "#bbbbbb" "1")] ++
                             (gridSvg coordLimit) ++
                             (List.foldr (++) [] (List.map pathNodes board)) ++ 
                              [(boxToSvg boxOn "blue" "1")] ++ 
                             [fullBoardRect])

                Goal -> svg [ width (String.fromInt boardSize)
                                 , height (String.fromInt boardSize)
                                 , viewBox (listToString " " [coordLimit, coordLimit, 0, 0])]
                             ([fullBoardRect] ++ 
                                  (List.foldr (++) [] (List.map pathNodes board)) )

winScreen : Bool -> Html msg
winScreen hasWon =
    case hasWon of
        True ->
            div []
                [h1 []
                     [text "LEVEL COMPLETE"]]
        False ->
            div [] []

view model =
  div [class "main-frame"]
    [ (boardToSvg Play 300 model.currentBoard model.boxOn model.boxSize model.boxMinLoc
       model.playerOnBoard model.playerHoverLoc)
    , (boardToSvg Goal 100 model.winBoard model.boxOn model.boxSize model.boxMinLoc
      model.playerOnBoard model.playerHoverLoc)
    , (winScreen model.hasWon)
    , div [] [text (String.fromInt model.numMoves)]
    ]

-- MAIN ------------------------------------------------------------------------
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> (initialModel, Cmd.none)
        , update = update
        , subscriptions = always Sub.none
        }
