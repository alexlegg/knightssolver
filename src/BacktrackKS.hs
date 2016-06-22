module BacktrackKS
    ( btSolveN
    ) where

import KnightSolver
import Data.List
import Data.Maybe

btSolveN :: Int -> Board -> Maybe [(Int, Int, MoveType)]
btSolveN n board = solve board n []

solve :: Board -> Int -> [(Int, Int, MoveType)] -> Maybe [(Int, Int, MoveType)]
solve board n cand
    | isSolved board'   = Just cand
    | length cand == n  = Nothing
    | otherwise         = firstJust (map (\x -> solve board n (cand ++ [x])) (possibleMoves board'))
    where
        board' = foldl doMove board cand

isSolved :: Board -> Bool
isSolved b@(mx, my, sqs, ks) = all (\(x, y, k) -> colourMatch k (squareAt x y b)) ks

colourMatch :: Knight -> Square -> Bool
colourMatch Red RedGoal     = True
colourMatch Blue BlueGoal   = True
colourMatch Yellow _        = True
colourMatch _ _             = False

allDirections :: [MoveType]
allDirections = [ UpLeft .. ]

possibleMoves :: Board -> [(Int, Int, MoveType)]
possibleMoves b@(mx, my, sqs, ks) = filter (possible . move) allMoves
    where
        possible (x, y)
            | x < 0 || y < 0            = False
            | x >= mx || y >= my        = False
            | squareAt x y b == Blocked = False
            | isJust (knightAt x y b)   = False
            | otherwise                 = True
        allMoves = [ (x, y, m) | (x, y) <- map stripThd ks, m <- allDirections ]

move :: (Int, Int, MoveType) -> (Int, Int)
move (x, y, m) = 
    case m of
        UpLeft      -> (x-1, y-2)
        UpRight     -> (x+1, y-2)
        RightUp     -> (x+2, y-1)
        RightDown   -> (x+2, y+1)
        DownRight   -> (x+1, y+2)
        DownLeft    -> (x-1, y+2)
        LeftDown    -> (x-2, y+1)
        LeftUp      -> (x-2, y-1)

moveKnight :: (Int, Int, Knight) -> MoveType -> (Int, Int, Knight)
moveKnight (x, y, k) m = let (tx, ty) = move (x, y, m) in (tx, ty, k)

doMove :: Board -> (Int, Int, MoveType) -> Board
doMove (mx, my, sqs, ks) (x, y, m) = (mx, my, sqs, ks')
    where
        ks' = map (\(i, j, k) -> if i == x && j == y then moveKnight (i, j, k) m else (i, j, k)) ks

firstJust :: [Maybe a] -> Maybe a
firstJust []                = Nothing
firstJust (Just x : _)      = Just x
firstJust (Nothing : xs)    = firstJust xs

squareAt :: Int -> Int -> Board -> Square
squareAt x y (mx, _, sqs, _) = sqs !! ((y * mx) + x)

knightAt :: Int -> Int -> Board -> Maybe Knight
knightAt x y (_, _, _, ks) = fmap thd3 $ find (\(i, j, _) -> i == x && j == y) ks

stripThd :: (a, b, c) -> (a, b)
stripThd (x, y, _) = (x, y)

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x
