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
                  parseTerm
  return $ (name, body)

parseTermLhs :: Parser BLTerm
parseTermLhs = choice
  [ try $ L.symbol sc "turnLeft"  >> return TurnLeft
  , try $ L.symbol sc "turnRight" >> return TurnRight
  , try $ L.symbol sc "moveForward" >> return MoveForward
  , try $ do
        _ <- L.symbol sc "dropBeacon" 
        kindstr <- lexeme $ some alphaNumChar
        let kind = case kindstr of
                    "kind1" -> Kind1
                    "kind2" -> Kind2
                    "Kind3" -> Kind3
                    "Kind4" -> Kind4
                    "Kind5" -> Kind5
                    "Kind6" -> Kind6
                    _ -> Kind1
        return (DropBeacon kind)
  , try $ L.symbol sc "destroyBeacon" >> return DestroyBeacon
  , try $ L.symbol sc "pickUpFossil" >> return PickUpFossil
  , try $ L.symbol sc "dropFossil" >> return DropFossil
  , try $ between (lexeme $ char '(') (lexeme $ char ')') parseTerm
  ]

parseBLTerm :: Parser BotLang
parseBLTerm = parseTerm >>= return . Term

parseTerm :: Parser BLTerm
parseTerm = choice
  [ try parseSeq <|> parseNonSeq
  , try parseBaseCond
  , try parseBaseDir
  , try parseBeaconCond
  , try parseBeaconDir
  , try parseNearbyCond
  , try parseFossilCond
  , try parseFor
  , try parseChoose
  , parseTermLhs
  ]

parseNonSeq :: Parser BLTerm
parseNonSeq = choice
  [ try parseFor
  , try parseChoose
  , try parseBaseDir
  , try parseBaseCond
  , try parseBeaconDir
  , try parseBeaconCond
  , try parseNearbyCond
  , try parseFossilCond
  , parseTermLhs
  ]

parseSeq :: Parser BLTerm
parseSeq = do
  lhs <- parseNonSeq
  _ <- optional sc
  _ <- lexeme $ char ';'
  _ <- optional sc
  rhs <- parseTerm
  return $ Seq lhs rhs

parseFossilCond :: Parser BLTerm
parseFossilCond = do
  _ <- L.symbol sc "ifFossil"
  th <- parseTerm
  el <- parseTerm
  return $ FossilCond th el

parseBaseCond :: Parser BLTerm
parseBaseCond = do
  _ <- L.symbol sc "ifBase"
  th <- parseTerm
  el <- parseTerm
  return $ BaseCond th el

parseBeaconCond :: Parser BLTerm
parseBeaconCond = do
  _ <- L.symbol sc "ifBeacon"
  kindstr <- lexeme $ some alphaNumChar
  let kind = case kindstr of
              "kind1" -> Kind1
              "kind2" -> Kind2
              "Kind3" -> Kind3
              "Kind4" -> Kind4
              "Kind5" -> Kind5
              "Kind6" -> Kind6
              _ -> Kind1
  th <- parseTerm
  el <- parseTerm
  return $ BeaconCond kind th el

parseBeaconDir :: Parser BLTerm
parseBeaconDir = do
  _ <- L.symbol sc "ifBeaconDir"
  kindstr <- lexeme $ some alphaNumChar
  let kind = case kindstr of
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
  return $ IfBeaconDir kind idx th el

parseNearbyCond :: Parser BLTerm
parseNearbyCond = do
  _ <- L.symbol sc "ifNearby"
  th <- parseTerm
  el <- parseTerm
  return $ NearbyCond th el

parseBaseDir :: Parser BLTerm
parseBaseDir = do
  _ <- L.symbol sc "ifBaseDir"
  idx <- lexeme L.decimal
  th <- parseTerm
  el <- parseTerm
  return $ IfBaseDir idx th el

parseFor :: Parser BLTerm
parseFor = do
  _ <- L.symbol sc "for"
  num <- lexeme L.decimal
  body <- between (lexeme $ char '{')
                  (lexeme $ char '}')
                  parseTerm
  return $ For num body
{-
parseWhile :: Parser BLTerm
parseWhile = do
  _ <- L.symbol sc "while"
  cond <- parseTerm
  body <- between (lexeme $ char '{') (lexeme $ char '}') parseTerm
  return $ While cond body

parseRepeatUntil :: Parser BLTerm
parseRepeatUntil = do
  _ <- L.symbol sc "repeat"
  body <- between (lexeme $ char '{') (lexeme $ char '}') parseTerm
  _ <- L.symbol sc "until"
  cond <- parseTerm
  return $ RepeatUntil body cond
-}

parseChoose :: Parser BLTerm
parseChoose = do
  _ <- L.symbol sc "choose"
  lhs <- between (lexeme $ char '{') (lexeme $ char '}') parseTerm
  rhs <- between (lexeme $ char '{') (lexeme $ char '}') parseTerm
  return $ Choose lhs rhs