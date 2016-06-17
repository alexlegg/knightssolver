{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass #-}
module Main where

import Data.SBV
import Data.Generics
import Control.Monad (unless)

type Coord = (Int, Int)

data Square = Blocked | Blank | BlueGoal | RedGoal
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[blocked, blank, blueGoal, redGoal] = map literal [Blocked, Blank, BlueGoal, RedGoal]

type SSquare = SBV Square

data Knight = Red | Blue | Yellow 
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[red, blue, yellow] = map literal [Red, Blue, Yellow]

type SKnight = SBV Knight

type Board = (Int, Int, [SSquare], [(SWord8, SWord8, SKnight)])

initBoard :: Board
initBoard =
    ( 3
    , 3
    , [ blank, redGoal, blueGoal
      , blank, blocked, blank
      , blank, blank, blank
      ]
    , [ (0, 0, red), (3, 3, blue) ]
    )

type Solution = [Move]

type Move = (SWord8, SWord8, SMoveType)

data MoveType = UpLeft | UpRight | RightUp | RightDown | DownRight | DownLeft | LeftDown | LeftUp
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[upLeft, upRight, rightUp, rightDown, downRight, downLeft, leftDown, leftUp] =
    map literal [UpLeft, UpRight, RightUp, RightDown, DownRight, DownLeft, LeftDown, LeftUp]

type SMoveType = SBV MoveType

knightAt :: SWord8 -> SWord8 -> (SWord8, SWord8, SKnight) -> SBool
knightAt x y (kx, ky, kc) = x .== kx &&& y .== ky

squareAt :: SWord8 -> SWord8 -> Board -> (SSquare -> SBool) -> SBool
squareAt x y (maxX, maxY, squares, _) f = at sqs
    where
        sqs = [ (i, j, squares !! ((j * maxX) + i)) | i <- [0..(maxX-1)], j <- [0..(maxY-1)] ]
        at []               = false
        at ((i, j, s):ss)   = ite (x .== fromIntegral i &&& y .== fromIntegral j) (f s) (at ss)

move :: SWord8 -> SWord8 -> SMoveType -> (SWord8, SWord8)
move x y m = 
      ite (m .== upLeft)    (x-1, y+2)
    $ ite (m .== upRight)   (x+1, y+2)
    $ ite (m .== rightUp)   (x+2, y+1)
    $ ite (m .== rightDown) (x+2, y-1)
    $ ite (m .== downRight) (x+1, y-2)
    $ ite (m .== downLeft)  (x-1, y-2)
    $ ite (m .== leftDown)  (x-2, y-1)
    $ {--(m .== leftUp)--}  (x-2, y+1)

moveKnight :: (SWord8, SWord8, SKnight) -> SMoveType -> (SWord8, SWord8, SKnight)
moveKnight (x, y, k) m = let (tx, ty) = move x y m in (tx, ty, k)

validMove :: Board -> Move -> [SBool]
validMove board@(maxX, maxY, squares, knights) (x, y, m) =
    [ x .<= fromIntegral maxX &&& x .>= 0 
    , y .<= fromIntegral maxY &&& y .>= 0
    , bAny (knightAt x y) knights
    , tx .<= fromIntegral maxX &&& tx .>= 0 
    , ty .<= fromIntegral maxY &&& ty .>= 0
    , squareAt tx ty board (\s -> s ./= blocked)
    , bnot (bAny (knightAt tx ty) knights)
    ]
    where (tx, ty) = move x y m

doMove :: Board -> Move -> Board
doMove (maxX, maxY, squares, knights) (x, y, m) = (maxX, maxY, squares, knights')
    where
        knights' = map (\k -> ite (knightAt x y k) (moveKnight k m) k) knights

colourMatch :: SKnight -> SSquare -> SBool
colourMatch k s = ite (k .== red)   (s .== redGoal)
                $ ite (k .== blue)  (s .== blueGoal) false

isSolved :: Board -> SBool
isSolved board@(_, _, _, knights) = 
    bAll (\(kx, ky, kc) -> squareAt kx ky board (colourMatch kc)) knights

isValid :: Board -> Solution -> SBool
isValid board []        = isSolved board
isValid board (m:ms)    = ite (bAnd (validMove board m)) (isValid (doMove board m) ms) false

solveN :: Int -> Board -> IO Bool
solveN n board = do
    putStrLn $ "Checking for solution of length " ++ show n
    res <- sat $ do
        xs <- sWord8s $ map (\i -> "x" ++ show i) [0..n]
        ys <- sWord8s $ map (\i -> "y" ++ show i) [0..n]
        ms <- symbolics $ map (\i -> "m" ++ show i) [0..n]
        return $ isValid initBoard (zip3 xs ys ms)

    print res
    return $ modelExists res

main :: IO ()
main = try 1
    where try i = do r <- solveN i initBoard
                     unless r $ try (i+1)
