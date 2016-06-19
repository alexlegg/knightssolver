{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, DeriveGeneric #-}
module KnightSolver 
    ( Square(..)
    , Knight(..)
    , Board(..)
    , MoveType(..)
    , solveN
    ) where

import Data.SBV
import Data.Generics
import qualified GHC.Generics as G
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Aeson

type Coord = (Int, Int)

data Square = Blocked | Blank | BlueGoal | RedGoal
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[blocked, blank, blueGoal, redGoal] = map literal [Blocked, Blank, BlueGoal, RedGoal]

type SSquare = SBV Square

data Knight = Red | Blue | Yellow 
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

[red, blue, yellow] = map literal [Red, Blue, Yellow]

type SKnight = SBV Knight

type Board = (Int, Int, [Square], [(Int, Int, Knight)])

type SBoard = (Int, Int, [SSquare], [(SWord8, SWord8, SKnight)])

makeSymbolic :: Board -> SBoard
makeSymbolic (maxX, maxY, squares, knights) =
    (maxX, maxY, map literal squares, map (\(x, y, k) -> (fromIntegral x, fromIntegral y, literal k)) knights)

type Move = (SWord8, SWord8, SMoveType)

data MoveType = UpLeft | UpRight | RightUp | RightDown | DownRight | DownLeft | LeftDown | LeftUp
    deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel, G.Generic, ToJSON)

[upLeft, upRight, rightUp, rightDown, downRight, downLeft, leftDown, leftUp] =
    map literal [UpLeft, UpRight, RightUp, RightDown, DownRight, DownLeft, LeftDown, LeftUp]

type SMoveType = SBV MoveType

knightAt :: SWord8 -> SWord8 -> (SWord8, SWord8, SKnight) -> SBool
knightAt x y (kx, ky, kc) = x .== kx &&& y .== ky

squareAt :: SWord8 -> SWord8 -> SBoard -> (SSquare -> SBool) -> SBool
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
      {--(m .== leftUp)--}  (x-2, y+1)

moveKnight :: (SWord8, SWord8, SKnight) -> SMoveType -> (SWord8, SWord8, SKnight)
moveKnight (x, y, k) m = let (tx, ty) = move x y m in (tx, ty, k)

validMove :: SBoard -> Move -> [SBool]
validMove board@(maxX, maxY, squares, knights) (x, y, m) =
    [ x .<= fromIntegral maxX &&& x .>= 0 
    , y .<= fromIntegral maxY &&& y .>= 0
    , bAny (knightAt x y) knights
    , tx .<= fromIntegral maxX &&& tx .>= 0 
    , ty .<= fromIntegral maxY &&& ty .>= 0
    , squareAt tx ty board (blocked ./=)
    , bnot (bAny (knightAt tx ty) knights)
    ]
    where (tx, ty) = move x y m

doMove :: SBoard -> Move -> SBoard
doMove (maxX, maxY, squares, knights) (x, y, m) = (maxX, maxY, squares, knights')
    where
        knights' = map (\k -> ite (knightAt x y k) (moveKnight k m) k) knights

colourMatch :: SKnight -> SSquare -> SBool
colourMatch k s = ite (k .== red)   (s .== redGoal)
                $ ite (k .== blue)  (s .== blueGoal) false

isSolved :: SBoard -> SBool
isSolved board@(_, _, _, knights) = 
    bAll (\(kx, ky, kc) -> squareAt kx ky board (colourMatch kc)) knights

isValid :: SBoard -> [Move] -> SBool
isValid board []        = isSolved board
isValid board (m:ms)    = ite (bAnd (validMove board m)) (isValid (doMove board m) ms) false

solveN :: Int -> Board -> IO (Maybe [(Int, Int, MoveType)])
solveN n board = do
    putStrLn $ "Checking for solution of length " ++ show n

    let xs = map ((:) 'x' . show) [0..n-1]
    let ys = map ((:) 'y' . show) [0..n-1]
    let ms = map ((:) 'm' . show) [0..n-1]

    res <- sat $ do
        sxs <- sWord8s xs
        sys <- sWord8s ys
        sms <- symbolics ms
        return $ isValid (makeSymbolic board) (zip3 sxs sys sms)

    if modelExists res
    then do
        let dict = getModelDictionary res
        let xs' = map (fmap fromCW . (`Map.lookup` dict)) xs :: [Maybe Word8]
        let ys' = map (fmap fromCW . (`Map.lookup` dict)) ys :: [Maybe Word8]
        let ms' = map (fmap fromCW . (`Map.lookup` dict)) ms :: [Maybe MoveType]

        if any isNothing xs' || any isNothing ys' || any isNothing ms'
        then do
            putStrLn "error"
            return Nothing
        else
            return $ Just (zip3 (map fromIntegral (catMaybes xs')) (map fromIntegral (catMaybes ys')) (catMaybes ms'))
    else return Nothing
