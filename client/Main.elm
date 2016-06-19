import Html
import Html.Attributes
import Html.App
import Color
import Array exposing (Array)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

main = 
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \s -> Sub.none
        }

type Square = Blocked | Blank | BlueGoal | RedGoal

type Knight = Red | Blue | Yellow

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
    }

type Msg = Noop | Click Int Int | SquareBrush Square | KnightBrush (Maybe Knight) | Solve

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

view : Model -> Html.Html Msg
view model =
    Html.div 
        [ Html.Attributes.style
            [ ("width", "500px")
            , ("margin", "auto")
            , ("padding-top", "100px")
            ]
        ]
        [ viewBoard model.board
        , Html.button 
            [Html.Attributes.onClick Solve] 
            [Html.text "Solve"]
        ]

viewBoard : Board -> Html.Html Msg
viewBoard board =
    let 
        clickBoard s x y = Click x y
        clickBrush b _ _ _ = b
        f (y, sr) =
            List.map (\(x, s) -> viewSquare clickBoard s x y) (Array.toIndexedList sr)
    in
        S.svg 
            [ SA.width "500", SA.height "600", SA.viewBox "0 0 500 500" ]
            ( List.concat (List.map f (Array.toIndexedList board.squares))
            ++ List.map (viewKnight Click) board.knights
            ++ [ viewSquare (clickBrush (SquareBrush Blank)) Blank 0 6
                , viewSquare (clickBrush (SquareBrush Blocked)) Blocked 1 6
                , viewSquare (clickBrush (SquareBrush RedGoal)) RedGoal 2 6
                , viewSquare (clickBrush (SquareBrush BlueGoal)) BlueGoal 3 6
                , viewSquare (clickBrush (KnightBrush Nothing)) Blank 0 7
                , viewSquare (clickBrush (KnightBrush (Just Red))) Blank 1 7
                , viewSquare (clickBrush (KnightBrush (Just Blue))) Blank 2 7
                , viewSquare (clickBrush (KnightBrush (Just Yellow))) Blank 3 7
                , viewKnight (\_ _ -> KnightBrush (Just Red)) (1, 7, Red)
                , viewKnight (\_ _ -> KnightBrush (Just Blue)) (2, 7, Blue)
                , viewKnight (\_ _ -> KnightBrush (Just Yellow)) (3, 7, Yellow)
                ]
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
