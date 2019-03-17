module Main where

import Lib
import System.IO
import Game

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Do you wish to play first? (y/n)"
          x <- getChar
          getChar
          putChar '\n'
          let s = (empty, PlayerO)
          play (gametree s) (if x == 'y' then PlayerO else PlayerX)
