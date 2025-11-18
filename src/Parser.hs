{-# LANGUAGE OverloadedStrings #-}

-- Uses Megaparsec to parse ExampleLang terms.
--
-- Grammar:
--   modes ::= mode name { term }
--           | modes*
--   term  ::= forever { term }
--           | turnLeft
--           | turnRight
--           | term; term
--
-- where "modes" is the top-level production (i.e.,
-- an ExampleLang AST root is a "modes" production).

module Parser
  ( parseExampleLang
  ) where

import Data.Text(Text)
import Data.Void
import Language
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseExampleLang :: Text -> IO (Either String ExampleLang)
parseExampleLang str = do
  case parse parseModes "" str of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right ast   -> return $ Right ast

parseModes :: Parser ExampleLang
parseModes = do
  modes <- some parseMode
  return $ Modes modes

parseMode :: Parser (String, ELTerm)
parseMode = do
  space
  _ <- string "mode"
  space
  name <- some alphaNumChar
  space
  body <- between (space >> char '{')
                  (space >> char '}')
                  parseTerm
  return $ (name, body)

parseTermLhs :: Parser ELTerm
parseTermLhs = choice
  [ try parseForever
  , try $ space >> string "turnLeft"  >> return TurnLeft
  , try $ space >> string "turnRight" >> return TurnRight
  ]

parseTerm :: Parser ELTerm
parseTerm = choice
  [ try parseSeq
  , parseTermLhs
  ]

parseSeq :: Parser ELTerm
parseSeq = do
  space
  lhs <- parseTermLhs
  space
  _ <- char ';'
  space
  rhs <- parseTerm
  return $ Seq lhs rhs

parseForever :: Parser ELTerm
parseForever = do
  space
  _ <- string "forever"
  body <- between (space >> char '{')
                  (space >> char '}')
                  parseTerm
  return $ Forever body
