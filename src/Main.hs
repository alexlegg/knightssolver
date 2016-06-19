{-# LANGUAGE OverloadedStrings #-}
module Main where

import KnightSolver
import Data.Maybe
import qualified Data.Text as T
import Web.Spock.Safe
import Control.Monad.IO.Class
import System.IO

testBoard :: Board
testBoard =
    ( 3
    , 3
    , [ Blank, RedGoal, BlueGoal
      , Blank, Blocked, Blank
      , Blank, Blank, Blank
      ]
    , [ (0, 0, Red), (2, 2, Blue) ]
    )

solve :: Int -> Board -> IO (Maybe [(Int, Int, MoveType)])
solve maxN board = try 1
    where
        try i = do
            r <- solveN i testBoard
            if isJust r 
                then return r 
                else if i == maxN then return Nothing else try (i+1)

app :: SpockT IO ()
app = do
    get "/" $ do
        file "text/html" "static/index.html"

    get "/app.js" $ do
        file "text/javascript" "static/app.js"

    post "/solve" $ do
        liftIO $ putStrLn "solve"
        sol <- liftIO $ solve 25 testBoard
        liftIO $ putStrLn (show sol)
        json sol

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Running on port 8080"
    runSpock 8080 $ spockT id app
