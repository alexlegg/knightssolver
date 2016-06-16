{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass #-}
module Main where

import Data.SBV
import Data.Generics

type Coord = (Int, Int)

data Square = Blocked | Blank | BlueGoal | RedGoal
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[blocked, blank, blueGoal, redGoal] = map literal [Blocked, Blank, BlueGoal, RedGoal]

type SSquare = SBV Square

data Knight = Red | Blue | Yellow 
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[red, blue, yellow] = map literal [Red, Blue, Yellow]

type SKnight = SBV Knight

type Board = (SWord8, SWord8, [SSquare], [(SWord8, SWord8, SKnight)])

initBoard :: Board
initBoard =
    ( 4
    , 4
    , [ blank, blank, blank, blank
      , blank, blank, blueGoal, blank
      , blank, redGoal, blank, blank
      , blank, blank, blank, blank
      ]
    , [ (0, 0, blue) ]
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

squareAt :: SWord8 -> SWord8 -> [SSquare] -> (SSquare -> SBool) -> SBool
squareAt x y squares f = at sqs
    where
        sqs = [ (i, j, squares !! ((j * 4) + i)) | i <- [0..3], j <- [0..3] ]
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
validMove (maxX, maxY, squares, knights) (x, y, m) =
    [ x .<= maxX &&& x .>= 0 
    , y .<= maxY &&& y .>= 0
    , bAny (knightAt x y) knights
    , tx .<= maxX &&& tx .>= 0 
    , ty .<= maxY &&& ty .>= 0
    , squareAt tx ty squares (\s -> s ./= blocked)
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
isSolved (maxX, maxY, squares, knights) = 
    bAll (\(kx, ky, kc) -> squareAt kx ky squares (colourMatch kc)) knights

isValid :: Board -> Solution -> SBool
isValid board []        = isSolved board
isValid board (m:ms)    = ite (bAnd (validMove board m)) (isValid (doMove board m) ms) false

main :: IO ()
main = do
    res <- allSat $ do
        x <- sWord8 "x1"
        y <- sWord8 "y1"
        m <- symbolic "m1"
        return $ isValid initBoard [(x, y, m)]
    print res
