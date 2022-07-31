module Main where

import Game (Gamestate (..), game, words)
import System.Random

main :: IO ()
main = do
  putStrLn "Welcome to hangman!"
  num <- randomIO :: IO Float
  game (Gamestate (solution num) 0 (2+length (solution num)) [])
  where
    solution num = Game.words !! floor (num * (fromIntegral . length $ Game.words))
