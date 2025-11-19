{-# LANGUAGE OverloadedStrings #-}

-- Uses Megaparsec to parse ExampleLang terms.
--
-- The ExampleLang grammar:
--   lang  ::= modes | term
--   modes ::= mode name { term }
--           | modes*
--   term  ::= forever { term }
--           | turnLeft
--           | turnRight
--           | term; term
--           | noop
--
-- where "lang" is the top-level production (i.e.,
-- an ExampleLang AST root is a "lang" production).
--
-- This parser does not parse noops, which are used
-- internally by the interpreter. (Noops could be
-- added to the parsed language later if needed.)

module Parser
  ( parseExampleLang
  ) where

import           Data.Text(Text)
import           Data.Void
import           Language
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
-- either a String error message or an ExampleLang AST.
parseExampleLang :: Text -> IO (Either String ExampleLang)
parseExampleLang str = do
  case parse (choice [parseModes, parseELTerm]) "" str of
    Left bundle -> return $ Left (errorBundlePretty bundle)
    Right ast   -> return $ Right ast

parseModes :: Parser ExampleLang
parseModes = do
  modes <- some parseMode
  return $ Modes modes

parseMode :: Parser (String, ELTerm)
parseMode = do
  sc
  _ <- L.symbol sc "mode"
  name <- lexeme $ some alphaNumChar
  body <- between (lexeme $ char '{')
                  (lexeme $ char '}')
                  parseTerm
  return $ (name, body)

parseTermLhs :: Parser ELTerm
parseTermLhs = choice
  [ try parseForever
  , try $ L.symbol sc "turnLeft"  >> return TurnLeft
  , try $ L.symbol sc "turnRight" >> return TurnRight
  ]

parseELTerm :: Parser ExampleLang
parseELTerm = parseTerm >>= return . Term

parseTerm :: Parser ELTerm
parseTerm = choice
  [ try parseSeq
  , parseTermLhs
  ]

parseSeq :: Parser ELTerm
parseSeq = do
  lhs <- parseTermLhs
  _ <- lexeme $ char ';'
  rhs <- parseTerm
  return $ Seq lhs rhs

parseForever :: Parser ELTerm
parseForever = do
  _ <- L.symbol sc "forever"
  body <- between (lexeme $ char '{')
                  (lexeme $ char '}')
                  parseTerm
  return $ Forever body
