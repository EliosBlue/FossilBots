{-# LANGUAGE OverloadedStrings #-}

-- Uses Megaparsec to parse BotLang terms.
--
-- The BotLang grammar:
--   lang  ::= modes | term
--   modes ::= mode name { term }
--           | modes modes
--   term  ::= turnLeft
--           | turnRight
--           | moveForward
--           | dropBeacon beaconKind
--           | destroyBeacon
--           | pickUpFossil
--           | dropFossil
--           | term; term
--           | noop
--
-- where "lang" is the top-level production (i.e.,
-- an BotLang AST root is a "lang" production).
--
-- This parser does not parse noops, which are used
-- internally by the interpreter. (Noops could be
-- added to the parsed language later if needed.)

module Parser
  ( parseBotLang
  ) where

import           Data.Text(Text)
import           Data.Void
import           Language
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Fryxbots.Beacon 

type Parser = Parsec Void Text

-- I am using Megaparsec's Lexer package here to
-- consume whitespace and comments. If you don't
-- want to do things this way, you can consume
-- whitespace as we did in the Megaparsec lab.

sc :: Parser ()  -- sc = "space consumer"
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- This function wraps a parser into one that
-- consumes trailing whitespace and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- The main entry point to the parser. Returns a sum type,
-- either a String error message or an BotLang AST.
parseBotLang :: Text -> IO (Either String BotLang)
parseBotLang str = do
  case parse (choice [parseModes, parseBLTerm]) "" str of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right ast   -> return $ Right ast

parseModes :: Parser BotLang
parseModes = do
  modes <- some parseMode
  return $ Modes modes

parseMode :: Parser (String, BLTerm)
parseMode = do
  sc
  _ <- L.symbol sc "mode"
  name <- lexeme $ some alphaNumChar
  body <- between (lexeme $ char '{')
                  (lexeme $ char '}')
                  parseTerm
  return $ (name, body)

parseTermLhs :: Parser BLTerm
parseTermLhs = choice
  [ try $ L.symbol sc "turnLeft"  >> return TurnLeft
  , try $ L.symbol sc "turnRight" >> return TurnRight
  , try $ L.symbol sc "moveForward" >> return MoveForward
  , try $ L.symbol sc "dropBeacon" >> return (DropBeacon Kind1)
  , try $ L.symbol sc "destroyBeacon" >> return DestroyBeacon
  , try $ L.symbol sc "pickUpFossil" >> return PickUpFossil
  , try $ L.symbol sc "dropFossil" >> return DropFossil
  ]

parseBLTerm :: Parser BotLang
parseBLTerm = parseTerm >>= return . Term

parseTerm :: Parser BLTerm
parseTerm = choice
  [ try parseSeq
  , try parseFossilCond
  , parseTermLhs
  ]

parseSeq :: Parser BLTerm
parseSeq = do
  lhs <- parseTermLhs
  _ <- lexeme $ char ';'
  rhs <- parseTerm
  return $ Seq lhs rhs

parseFossilCond :: Parser BLTerm
parseFossilCond = do
  _ <- L.symbol sc "if"
  th <- parseTerm
  el <- parseTerm
  return $ FossilCond th el

{- parseForever :: Parser BLTerm
parseForever = do
  _ <- L.symbol sc "forever"
  body <- between (lexeme $ char '{')
                  (lexeme $ char '}')
                  parseTerm
  return $ Forever body -}
