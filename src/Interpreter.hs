-- The ExampleLang interpreter.
--
-- Semantics are defined in terms of a step relation -> that
-- relates <term, state> pairs:
--
--   <term, state> -> <term', state'>
--
-- A small-step operational semantics of ExampleLang is given
-- by the following evaluation rules:
--
--  ------------------------------------------- ExlModeSingle
--   <mode name {t}, s> -> <t, s[cmd := idle]>
--
--
--  --------------------------------------------- ExlModeL
--   <mode name {t} m, s> -> <t, s[cmd := idle]>
--
--
--  --------------------------------------------- ExlModeR
--   <mode name {t} m, s> -> <m, s[cmd := idle]>
--
--
--  ----------------------------------------------- ExlTurnLeft
--   <turnLeft, s> -> <noop, s[cmd := RotateLeft]>
--
--
--  ------------------------------------------------ ExlTurnRight
--   <turnRight, s> -> <noop, s[cmd := turnRight]>
--
--
--  --------------------------------------------------- ExlMoveForward
--   <moveForward, s> -> <noop, s[cmd := moveForward]>
--
--
--  ----------------------------------------------------------------------- ExlDropBeacon
--   <dropBeacon BeaconKind, s> -> <noop, s[cmd := dropBeacon BeaconKind]>
--
--
--  ------------------------------------------------------- ExlDestroyBeacon
--   <destroyBeacon, s> -> <noop, s[cmd := destroyBeacon]>
--
--
--  ----------------------------------------------------- ExlPickUpFossil
--   <pickUpFossil, s> -> <noop, s[cmd := pickUpFossil]>
--
--
--  ------------------------------------------------- ExlDropFossil
--   <dropFossil, s> -> <noop, s[cmd := dropFossil]>
--
--
--         <t1, s> -> <t1', s'>      t1 /= noop
--  ------------------------------------------------ ExlSeqL
--             <t1; t2, s> -> <t1'; t2, s'>
--
--
--         <t2, s> -> <t2', s'>      t1 = noop
--  ------------------------------------------------ ExlSeqL
--             <t1; t2, s> -> <t2', s'>

module Interpreter
  ( Interpreter(..)
  ,  evalBL
  ) where

import qualified Fryxbots.Bot.Command as Cmd
import           Fryxbots.Bot.State
import           Language (BotLang, BLTerm)
import qualified Language as Lang
import           System.Random

data Interpreter = Interpreter { stdGen :: StdGen }

-- Evaluate a top-level ExampleLang AST.
evalEL :: StdGen -> BotLang -> State -> (StdGen, BotLang, State)
evalEL stdGen expr state =
  case expr of
    Lang.Modes [] -> (stdGen, expr, state)
    Lang.Modes modes ->
      -- The semantics of ExampleLang doesn't specify a distribution of
      -- mode selection, it just allows implementors to step to any mode.
      -- We will randomly select a mode with a uniform distribution.
      let len = length modes
          (index, stdGen') = randomR (0, len - 1) stdGen :: (Int, StdGen)
          term = snd $ modes !! index
          state' = setCommand Cmd.Idle state
      in (stdGen', Lang.Term term, state')
    Lang.Term term ->
      let (term', state') = evalTerm term state
      in (stdGen, Lang.Term term', state')

-- Evaluate an ExampleLang Term.
evalTerm :: BLTerm -> State -> (BLTerm, State)
evalTerm term state =
  case term of
    Lang.TurnLeft  -> (Lang.Noop, setCommand Cmd.RotateLeft state)
    Lang.TurnRight -> (Lang.Noop, setCommand Cmd.RotateRight state)
    Lang.MoveForward -> (Lang.Noop, setCommand Cmd.MoveForward state)
    Lang.DropBeacon Beacon -> (Lang.Noop, setCommand Cmd.DropBeacon Beacon state)
    Lang.DestroyBeacon -> (Lang.Noop, setCommand Cmd.DestroyBeacon state)
    Lang.PickUpFossil -> (Lang.Noop, setCommand Cmd.PickUpFossil state)
    Lang.DropFossil -> (Lang.Noop, setCommand Cmd.DropFossil)
    Lang.Cond ifTerm thTerm ElTerm -> (if evalTerm ifTerm state == 
      (Lang.Noop, Lang.Bool True) then evalTerm thTerm state else evalTerm ElTerm state)
    Lang.Seq lhs rhs -> let (termLhs', stateLhs') = evalTerm lhs state
                            (termRhs', stateRhs') = evalTerm rhs state
                        in if lhs == Lang.Noop
                           then (termRhs', stateRhs')
                           else (Lang.Seq termLhs' rhs, stateLhs')
    Lang.IsZero numFossils -> (if numFossils == 0 then (Lang.Noop, Lang.Bool True) else (Lang.Noop, Lang.Bool False))
    Lang.Bool bool -> (Lang.Noop, Lang.Bool bool)