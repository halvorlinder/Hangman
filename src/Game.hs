{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Game (game, Gamestate (..)) where
import Data.List (sort, nub)

game :: Gamestate -> IO ()
game state = do
  print state
  let res = getResult state 
  case res of 
    Win -> putStrLn "w" 
    Loss -> putStrLn "l" 
    _ -> putStrLn ""
  g <- getGuess

  game $ guess g state

getGuess :: IO Char
getGuess = do
  putStrLn "Guess a single letter: "
  l <- getLine
  if length l <= 2 && length l > 0
    then return (head l)
    else do
      putStrLn "Invalid guess!"
      getGuess

data Gamestate = Gamestate {solution :: String, guesses :: Int, allowed :: Int, guessed :: [Char]}

instance Show Gamestate where
  show (Gamestate solution gusses allowed guessed) = map (\c -> if c `elem` guessed then c else '*') solution

data Result = Ongoing | Win | Loss

guess :: Char -> Gamestate -> Gamestate
guess g state = state

getResult :: Gamestate -> Result
getResult Gamestate {..}
 | ( sort . nub ) solution == ( sort . nub ) guessed = Win
 | guesses < allowed = Ongoing
 | otherwise = Loss