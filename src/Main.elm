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
                   , hasWon : Bool }

testPath = [ Coord 5 5
           , Coord 10 5
           , Coord 10 8
           , Coord 15 8]
testPath2 = [ Coord 5 15
           , Coord 10 15
           , Coord 10 12
           , Coord 15 12]
testBoard = [ {coords = testPath, on = True}
            , {coords = testPath2, on = True}]
testWin = [ {coords = testPath, on = True}
            , {coords = testPath2, on = False}]

initialModel = { currentBoard = testBoard
                , winBoard = testWin
                , boxOn = False
                , boxSize = 3
                , boxMinLoc = Coord 0 0
                , playerHoverLoc = Coord 0 0
                , playerOnBoard = False
                , hasWon = False}

-- UPDDATE ---------------------------------------------------------------------

type Msg = MouseMove | BoardClick Int Int Int Int

update msg model =
  case msg of
    MouseMove ->
      model

    BoardClick x y size lim ->
        let
            modifier = (toFloat lim) / (toFloat size)
            newBoxMinLoc = Coord (round ((toFloat x) * modifier) - 2) (round ((toFloat y) * modifier) - 2)
            newBoard = transformBoard model newBoxMinLoc
            newHasWon = (newBoard == model.winBoard)
        in
            { model
                | boxOn = True
                , boxMinLoc = newBoxMinLoc
                , currentBoard = newBoard
                , hasWon = newHasWon
            }

transformBoard : Model -> Coord -> Board
transformBoard model newBoxMinLoc =
    let
        bbox = { min = newBoxMinLoc
               , max = Coord (newBoxMinLoc.x + model.boxSize) (newBoxMinLoc.y + model.boxSize)}
        _ = Debug.log "bbox" bbox
        _ = Debug.log "model" model
    in
        List.map (\path -> {path | on = not (isIntersect bbox path.coords)}) model.currentBoard

boardOnClick : Int -> Int -> Svg.Attribute Msg
boardOnClick boardSize coordLim =
  let
    decoder = Decode.map2 (\x y -> BoardClick x y boardSize coordLim) (Decode.at ["pageX"] Decode.int) (Decode.at ["pageY"] Decode.int)
  in
    Events.on "click" decoder

-- VIEW ------------------------------------------------------------------------

type BoardType = Play | Goal

pathToSvgString : List Coord -> String
pathToSvgString path = List.foldl (\s1 s2 -> s1 ++ " " ++ s2) ""
                       (List.map (\c -> String.fromInt(c.x) ++ "," ++ String.fromInt(c.y))
                            path)

pathPolyline : Path -> Svg msg
pathPolyline path = case path.on of
                        True -> polyline [ fill  "none"
                                         , stroke "#ffa0a0"
                                         , points (pathToSvgString path.coords)]
                                []
                        False -> polyline [ fill "none"
                                          , stroke "#a5a5a5"
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
        nodeSize = 2
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
                            , strokeWidth "0.1"]
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

boardToSvg : BoardType -> Int -> Board -> Bool -> Int -> Coord -> Html Msg
boardToSvg boardType boardSize board boxOn boxSize boxMinLoc =
    let
        coordLimit = 20
        bleed = 0.5
        boxToSvg on = case on of
                          True -> rect [ x (String.fromFloat ((toFloat boxMinLoc.x) - (bleed / 2)))
                                       , y (String.fromFloat ((toFloat boxMinLoc.y) - (bleed / 2)))
                                       , width (String.fromFloat ((toFloat boxSize) + bleed))
                                       , height (String.fromFloat ((toFloat boxSize) + bleed))
                                       , fill "blue"]
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
                             (List.foldr (++) [] (List.map pathNodes board)) ++ 
                             (gridSvg coordLimit) ++
                             [(boxToSvg boxOn)] ++ 
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
  div []
    [ (boardToSvg Play 300 model.currentBoard model.boxOn model.boxSize model.boxMinLoc)
    , (boardToSvg Goal 300 model.winBoard model.boxOn model.boxSize model.boxMinLoc)
    , (winScreen model.hasWon)
    ]

-- MAIN ------------------------------------------------------------------------
main =
  Browser.sandbox { init = initialModel, update = update, view = view }
