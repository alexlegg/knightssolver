import Html
import Html.Attributes
import Html.Events
import Html.App
import Color
import Array exposing (Array)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Http
import Json.Decode as JD
import Task
import Debug exposing (crash)

main = 
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \s -> Sub.none
        }

type Square = Blocked | Blank | BlueGoal | RedGoal

type Knight = Red | Blue | Yellow

type MoveType = UpLeft | UpRight | RightUp | RightDown | DownRight | DownLeft | LeftDown | LeftUp

type alias Solution = Maybe (List (Int, Int, MoveType))

type alias Board =
    { maxX : Int
    , maxY : Int
    , squares : Array (Array Square)
    , knights : List (Int, Int, Knight)
    }

type alias Model =
    { board : Board
    , squareBrush : Maybe Square
    , knightBrush : Maybe Knight
    , solution : Solution
    , solutionPage : Int
    }

type Msg = 
      Noop 
    | Click Int Int 
    | SquareBrush Square 
    | KnightBrush (Maybe Knight) 
    | Solve
    | Reset
    | Error Http.Error
    | GotSolution Solution
    | NextPage
    | LastPage

applyMove : (Int, Int, MoveType) -> Board -> Board
applyMove (x, y, m) board =
    let
        f (i, j, k) = if x == i && y == j then (tx, ty, k) else (i, j, k)
        (tx, ty) = 
            case m of
                UpLeft    -> (x-1, y+2)
                UpRight   -> (x+1, y+2)
                RightUp   -> (x+2, y+1)
                RightDown -> (x+2, y-1)
                DownRight -> (x+1, y-2)
                DownLeft  -> (x-1, y-2)
                LeftDown  -> (x-2, y-1)
                LeftUp    -> (x-2, y+1)

    in
    { board | knights = List.map f board.knights }

decodeMoveType : JD.Decoder MoveType
decodeMoveType =
    let
        f s = case s of
            "UpLeft" -> UpLeft
            "UpRight" -> UpRight
            "RightUp" -> RightUp
            "RightDown" -> RightDown
            "DownRight" -> DownRight
            "DownLeft" -> DownLeft
            "LeftDown" -> LeftDown
            "LeftUp" -> LeftUp
            _ -> crash "Bad data from server"
    in
    JD.map f JD.string

decodeSolution : JD.Decoder Solution
decodeSolution =
    JD.maybe (JD.list (JD.tuple3 (,,) JD.int JD.int decodeMoveType))

solve : Platform.Task Http.Error Solution
solve =
    Http.post decodeSolution "http://localhost:8080/solve" Http.empty

init : (Model, Cmd.Cmd Msg)
init =
    ( { board =
        { maxX = 3
        , maxY = 3
        , squares =
            Array.fromList
                [ Array.fromList [Blank, RedGoal, BlueGoal]
                , Array.fromList [Blank, Blocked, Blank]
                , Array.fromList [Blank, Blank, Blank]
                ]
        , knights =
            [ (0, 0, Red)
            , (2, 2, Blue)
            ]
        }
    , squareBrush = Nothing
    , knightBrush = Nothing
    , solution = Nothing
    , solutionPage = 0
    }
    , Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Noop ->
            (model, Cmd.none)
        Click x y ->
            ({ model | board = updateBoard x y model.squareBrush model.knightBrush model.board }, Cmd.none)
        SquareBrush s ->
            ({ model | squareBrush = Just s, knightBrush = Nothing}, Cmd.none)
        KnightBrush k ->
            ({ model | squareBrush = Nothing, knightBrush = k}, Cmd.none)
        Solve ->
            ( model
            , Task.perform Error GotSolution solve
            )
        Reset ->
            ( { model | solution = Nothing, solutionPage = 0 }
            , Cmd.none
            )
        GotSolution sol ->
            ({ model | solution = sol }, Cmd.none )
        NextPage ->
            ( { model | solutionPage = min (model.solutionPage+1) (List.length (maybeList model.solution)) }
            , Cmd.none
            )
        LastPage ->
            ( { model | solutionPage = max (model.solutionPage-1) 0 }
            , Cmd.none
            )
        Error s ->
            crash (toString s) ( model, Cmd.none )

updateBoard : Int -> Int -> Maybe Square -> Maybe Knight -> Board -> Board
updateBoard x y sb kb board =
    let
        clearKnight = List.filter (\(i, j, _) -> not (i == x && j == y)) board.knights
    in
    case sb of
        Just s ->
            { board | squares = modifyArray y (Array.set x s) board.squares }
        Nothing ->
            case kb of
                Just k ->
                    { board | knights = [(x, y, k)] ++ clearKnight }
                Nothing ->
                    { board | knights = clearKnight }

modifyArray : Int -> (a -> a) -> Array a -> Array a
modifyArray i f arr =
    case Array.get i arr of
        Just x ->
            Array.set i (f x) arr
        Nothing ->
            arr

squareSize = 64

isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Just _ -> False
        Nothing -> True

isJust : Maybe a -> Bool
isJust x =
    not (isNothing x)

maybeList : Maybe (List a) -> List a
maybeList m =
    case m of
        Just xs -> xs
        Nothing -> []

view : Model -> Html.Html Msg
view model =
    let
        title =
            if isNothing model.solution then "Edit Puzzle" else "Solution"
        currMoves =
            List.take model.solutionPage (maybeList model.solution)
    in
    Html.div 
        [ Html.Attributes.style
            [ ("width", "500px")
            , ("margin", "auto")
            , ("padding-top", "100px")
            ]
        ]
        ([ Html.h1 [] [Html.text title]
        , viewBoard (List.foldl applyMove model.board currMoves)
        , Html.hr [] []
        ] ++ if isNothing model.solution
            then 
                [ viewPalette
                , Html.br [] []
                , Html.button 
                    [Html.Events.onClick Solve] 
                    [Html.text "Solve"]
                ]
            else
                [ Html.button
                    [Html.Events.onClick LastPage] 
                    [Html.text "<"]
                , Html.text 
                    (" " 
                    ++ (toString model.solutionPage) 
                    ++ " / " 
                    ++ (toString (List.length (maybeList model.solution))) 
                    ++ " ")
                , Html.button
                    [Html.Events.onClick NextPage] 
                    [Html.text ">"]
                , Html.br [] []
                , Html.button 
                    [Html.Events.onClick Reset] 
                    [Html.text "Reset"]
                ]
        )

viewPalette : Html.Html Msg
viewPalette =
    let 
        clickBrush b _ _ _ = b
    in
    S.svg 
        [ SA.width "320", SA.height "128", SA.viewBox "0 0 320 128" ]
        [ viewSquare (clickBrush (SquareBrush Blank)) Blank 0 0
        , viewSquare (clickBrush (SquareBrush Blocked)) Blocked 1 0
        , viewSquare (clickBrush (SquareBrush RedGoal)) RedGoal 2 0
        , viewSquare (clickBrush (SquareBrush BlueGoal)) BlueGoal 3 0
        , viewSquare (clickBrush (KnightBrush Nothing)) Blank 0 1
        , viewSquare (clickBrush (KnightBrush (Just Red))) Blank 1 1
        , viewSquare (clickBrush (KnightBrush (Just Blue))) Blank 2 1
        , viewSquare (clickBrush (KnightBrush (Just Yellow))) Blank 3 1
        , viewKnight (\_ _ -> KnightBrush (Just Red)) (1, 1, Red)
        , viewKnight (\_ _ -> KnightBrush (Just Blue)) (2, 1, Blue)
        , viewKnight (\_ _ -> KnightBrush (Just Yellow)) (3, 1, Yellow)
        ]

viewBoard : Board -> Html.Html Msg
viewBoard board =
    let 
        clickBoard s x y = Click x y
        f (y, sr) =
            List.map (\(x, s) -> viewSquare clickBoard s x y) (Array.toIndexedList sr)
    in
    S.svg 
        [ SA.width "320", SA.height "320", SA.viewBox "0 0 320 320" ]
        ( List.concat (List.map f (Array.toIndexedList board.squares))
        ++ List.map (viewKnight Click) board.knights
        )


viewSquare : (Square -> Int -> Int -> Msg) -> Square -> Int -> Int -> S.Svg Msg
viewSquare f s x y =
    let 
        c =
            case s of
                Blank -> "#FFFFFF"
                Blocked -> "#333333"
                RedGoal -> "red"
                BlueGoal -> "blue"
    in
        S.rect 
            [ SA.x (toString (x * squareSize))
            , SA.y (toString (y * squareSize))
            , SA.fill c
            , SA.width (toString squareSize)
            , SA.height (toString squareSize)
            , SE.onClick (f s x y)
            , SA.stroke "black"
            , SA.strokeWidth "1"
            ] 
            []

knightSize = squareSize - 16

viewKnight : (Int -> Int -> Msg) -> (Int, Int, Knight) -> S.Svg Msg
viewKnight f (x, y, k) =
    let
        c =
            case k of
                Red -> "red"
                Blue -> "blue"
                Yellow -> "yellow"
    in
        S.circle
            [ SA.cx (toString ((toFloat x + 0.5) * squareSize))
            , SA.cy (toString ((toFloat y + 0.5) * squareSize))
            , SA.fill c
            , SA.r (toString (knightSize / 2))
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SE.onClick (f x y)
            ] 
            []
