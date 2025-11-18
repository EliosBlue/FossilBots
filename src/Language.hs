-- Defines the AST for an example language called ExampleLang.

module Language
  ( ExampleLang(..) -- The (..) means "also export all constructors".
  , ELTerm(..)
  ) where

-- For efficiency, we represent productions of the mode grammar:
--
--   modes ::= mode string { term }
--           | modes modes
--
-- as a list of (string, term) pairs.

data ExampleLang = Modes [(String, ELTerm)]
                 | Term ELTerm
                 deriving (Eq, Ord, Show)

data ELTerm = Forever ELTerm
            | TurnLeft
            | TurnRight
            | Seq ELTerm ELTerm
            | Noop
            deriving (Eq, Ord, Show)
