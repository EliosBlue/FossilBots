module Main (main) where

import Data.Text (pack)
import Fryxbots.Example.RandomController
import Fryxbots.Simulator
import Controller
import Language
import Parser

-- Loads programs, creates bot controllers, and runs
-- the simulator.

main :: IO ()
main = do
  blueProgram <- parseProgram "programs/spinbot.exl"
  let blueController = mkELController blueProgram
  let goldController = mkRandomController
  runSimulator "worlds/example1.world" blueController goldController

parseProgram :: String -> IO ExampleLang
parseProgram path = do
  programStr <- readFile path
  parse <- parseExampleLang $ pack programStr
  case parse of
      Left err -> error err
      Right ast -> return ast
