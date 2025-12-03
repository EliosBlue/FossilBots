-- Defines the AST for an example language called BotLang.
--
-- The grammar of BotLang is as follows:
--
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

module Language
  ( BotLang(..) -- The (..) means "also export all constructors".
  , BLTerm(..)
  ) where

-- For efficiency, we represent productions of the mode grammar:
--
--   modes ::= mode string { term }
--           | modes modes
--
-- as a list of (string, term) pairs.

data BotLang = Modes [(String, ELTerm)]
                 | Term BLTerm
                 deriving (Eq, Ord, Show)

data BLTerm = TurnLeft
            | TurnRight
            | MoveForward
            | DropBeacon BeaconKind
            | DestroyBeacon
            | PickUpFossil
            | DropFossil
            | Seq BLTerm BLTerm
            | Noop
            deriving (Eq, Ord, Show)
