module Main where

import Game ( game, Gamestate(..))

main :: IO ()
main = do
  putStrLn "Welcome to hangman!"
  game ( Gamestate "halla" 0 5 [] )

