module Lib where
    
import Data.Char


import System.IO
import System.Random (randomRIO)
import Game
import Utils
import Interactions


-- Human vs human
tictactoe :: IO ()
tictactoe = run (empty, PlayerO)

run :: (Grid, Player) -> IO ()
run (g, p) = do
    cls
    goto (1,1)
    putGrid g
    run' (g, p)

run' :: (Grid, Player) -> IO ()
run' s@(g, p)   | wins PlayerO g = putStrLn "Player O wins!\n"
                | wins PlayerX g = putStrLn "Player X wins!\n"
                | full g   = putStrLn "It's a draw!\n"
                | otherwise = do i <- getNat (prompt p)
                                 case move s i of
                                     Nothing -> do putStrLn "Error: Invalid move"
                                                   run' s
                                     Just s' -> run s'

prompt :: Player -> String
prompt p = showPlayer p ++ ", enter your move:"







play :: GameTree -> Player -> IO ()
play t h =
             do cls
                goto (1,1)
                putGrid g
                play' t h
  where
    s@(g, _) = root t

lookupMaybe :: Int -> [Maybe a] -> Maybe a
lookupMaybe _ [] = Nothing
lookupMaybe 0 (x:_) = x
lookupMaybe n (_:xs) =
  if n > 0 then lookupMaybe (n-1) xs
           else Nothing

play' :: GameTree -> Player -> IO ()
play' t@(GtNode (g,p) ts) h
  | wins PlayerO g  = putStrLn "Player O wins!\n"
  | wins PlayerX g  = putStrLn "Player X wins!\n"
  | full g    = putStrLn "It's a draw!\n"
  | p == h    = do i <- getNat (prompt p)
                   case lookupMaybe i ts of 
                     Nothing ->
                           do putStrLn "Error: Invalid move"
                              play' t h
                     Just t' -> play t' h
  | otherwise = do putStr (showPlayer (next h) ++ " is thinking...")
                   t' <- selectRandom $ bestmoves t
                   play t' h

selectRandom :: [a] -> IO a
selectRandom xs = do
  n <- randomRIO (0, (length xs) - 1)
  return (xs !! n)

                    

