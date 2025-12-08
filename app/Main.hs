module Main (main) where

import Data.Text (pack)
import Fryxbots.Example.RandomController
import Fryxbots.Simulator
import Controller
import Language
import Parser

-- Loads programs, creates bot controllers, and runs
-- the simulator.

{- 
main :: IO ()
main = do
  blueProgram <- parseProgram "programs/simplebot.exl"
  let goldController = mkRandomController
  case mkBLController blueProgram of
    Left err -> putStrLn $ "Type error in program: " ++ err
    Right blueController -> runSimulator "worlds/example1.world" blueController goldController
-}

main :: IO ()
main = do
  blueProgram <- parseProgram "programs/simplebot.exl"
  goldProgram <- parseProgram "programs/simplebot.exl"
  case mkBLController blueProgram of
    Left err -> putStrLn $ "Type error in programfor blue team: " ++ err
    Right blueController -> 
      case mkBLController goldProgram of
        Left err -> putStrLn $ "Type error in program for gold team: " ++ err
        Right goldController ->
          runSimulator "worlds/example1.world" blueController goldController


parseProgram :: String -> IO BotLang
parseProgram path = do
  programStr <- readFile path
  parse <- parseBotLang $ pack programStr
  case parse of
      Left err -> error err
      Right ast -> return ast
