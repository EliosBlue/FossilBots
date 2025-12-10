{-# LANGUAGE OverloadedStrings #-}

-- Uses Megaparsec to parse BotLang terms.

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
                  parseBlock
  return $ (name, body)

parseTermLhs :: Parser BLTerm
parseTermLhs = choice
  [
    try $ L.symbol sc "turnLeft"  >> return TurnLeft
  , try $ L.symbol sc "turnRight" >> return TurnRight
  , try $ L.symbol sc "turnAround" >> return TurnAround
  , try $ L.symbol sc "randomTurns" >> return RandomTurns
  , try $ L.symbol sc "moveForward" >> return MoveForward
  , try (do
          _ <- L.symbol sc "dropBeacon" 
          kindstr <- lexeme $ some alphaNumChar
          let thisKind = case kindstr of
                "kind1" -> Kind1
                "kind2" -> Kind2
                "Kind3" -> Kind3
                "Kind4" -> Kind4
                "Kind5" -> Kind5
                "Kind6" -> Kind6
                _       -> Kind1
          return (DropBeacon thisKind))
  , try $ L.symbol sc "destroyBeacon" >> return DestroyBeacon
  , try $ L.symbol sc "pickUpFossil" >> return PickUpFossil
  , try $ L.symbol sc "dropFossil" >> return DropFossil
  , try (do
          _ <- L.symbol sc "search"
          object <- lexeme $ some alphaNumChar
          return $ Search object 1)
  , try $ L.symbol sc "idle" >> return Idle
  , try (do
          _ <- L.symbol sc "setMode"
          n <- lexeme L.decimal
          return $ SetMode n)
  , try (do
          _ <- L.symbol sc "ifMode"
          n <- lexeme L.decimal
          th <- parseTerm
          el <- parseTerm
          return $ IfMode n th el)
  , try (do
          _ <- L.symbol sc "ifFossil"
          th <- parseTerm
          el <- parseTerm 
          return $ IfFossil th el)
  , try (do
          _ <- L.symbol sc "ifBase"
          th <- parseTerm
          el <- parseTerm
          return $ IfBase th el)
  , try (do
          _ <- L.symbol sc "ifBeacon"
          kindstr <- lexeme $ some alphaNumChar
          let thisKind = case kindstr of
                "kind1" -> Kind1
                "kind2" -> Kind2
                "Kind3" -> Kind3
                "Kind4" -> Kind4
                "Kind5" -> Kind5
                "Kind6" -> Kind6
                _ -> Kind1
          th <- parseTerm
          el <- parseTerm
          return $ IfBeacon thisKind th el)
  , try (do
          _ <- L.symbol sc "ifBeaconDir"
          kindstr <- lexeme $ some alphaNumChar
          let thisKind = case kindstr of
                "kind1" -> Kind1
                "kind2" -> Kind2
                "Kind3" -> Kind3
                "Kind4" -> Kind4
                "Kind5" -> Kind5
                "Kind6" -> Kind6
                _ -> Kind1
          idx <- lexeme L.decimal
          th <- parseTerm
          el <- parseTerm
          return $ IfBeaconDir thisKind idx th el)
  , try (do
          _ <- L.symbol sc "ifNearby"
          th <- parseTerm
          el <- parseTerm
          return $ IfNearby th el)
  , try (do
          _ <- L.symbol sc "ifBaseDir"
          idx <- lexeme L.decimal
          th <- parseTerm
          el <- parseTerm
          return $ IfBaseDir idx th el)
  , try (do
          _ <- L.symbol sc "for"
          num <- lexeme L.decimal
          body <- between (lexeme $ char '{')
                          (lexeme $ char '}')
                          parseBlock
          return $ For num body)
  , try (do
          _ <- L.symbol sc "choose"
          lhs <- between (lexeme $ char '{') (lexeme $ char '}') parseBlock
          rhs <- between (lexeme $ char '{') (lexeme $ char '}') parseBlock
          return $ Choose lhs rhs)
  , try $ between (lexeme $ char '(') (lexeme $ char ')') parseBlock
  ]

parseBLTerm :: Parser BotLang
parseBLTerm = parseTerm >>= return . Term

parseTerm :: Parser BLTerm
parseTerm = choice
  [ try parseSeq
	, parseTermLhs
	]

parseBlock :: Parser BLTerm
parseBlock = do
	terms <- sepBy1 parseTermLhs (lexeme $ char ';')
	return $ foldr1 Seq terms

parseSeq :: Parser BLTerm
parseSeq = do
	lhs <- parseTermLhs
	_ <- optional sc
	_ <- lexeme $ char ';'
	_ <- optional sc
	rhs <- parseTerm
	return $ Seq lhs rhs