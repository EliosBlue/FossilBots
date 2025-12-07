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
  blueProgram <- parseProgram "programs/simplebot.exl"
  let blueController = mkBLController blueProgram
  let goldController = mkRandomController
  runSimulator "worlds/example1.world" blueController goldController

parseProgram :: String -> IO BotLang
parseProgram path = do
  programStr <- readFile path
  parse <- parseBotLang $ pack programStr
  case parse of
      Left err -> error err
      Right ast -> return ast
