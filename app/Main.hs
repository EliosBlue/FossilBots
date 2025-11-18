module Main (main) where

import Data.Text (pack)
import Fryxbots.Example.RandomController
import Fryxbots.Simulator
import Interpreter
import Language
import Parser

main :: IO ()
main = do
  blueProgram <- parseProgram "programs/spinbot.exl"
  let blueController = mkELController blueProgram
  let goldController = mkRandomController
  runSimulator blueController goldController

parseProgram :: String -> IO ExampleLang
parseProgram path = do
  programStr <- readFile path
  parse <- parseExampleLang $ pack programStr
  case parse of
      Left err -> error err
      Right ast -> return ast
