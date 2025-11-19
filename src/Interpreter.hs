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
--  ------------------------------------------ ExlModeSingle
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
--  ------------------------------------------------------ ExlForever
--   <forever {t}, s> -> <t; forever {t}, s[cmd := idle]>
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
  ,  evalEL
  ) where

import qualified Fryxbots.Bot.Command as Cmd
import           Fryxbots.Bot.State
import           Language (ExampleLang, ELTerm)
import qualified Language as Lang
import           System.Random

data Interpreter = Interpreter { stdGen :: StdGen }

-- Evaluate a top-level ExampleLang AST.
evalEL :: StdGen -> ExampleLang -> State -> (StdGen, ExampleLang, State)
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
evalTerm :: ELTerm -> State -> (ELTerm, State)
evalTerm term state =
  case term of
    Lang.Forever body -> ( Lang.Seq body term
                         , setCommand Cmd.Idle state )
    Lang.TurnLeft  -> (Lang.Noop, setCommand Cmd.RotateLeft state)
    Lang.TurnRight -> (Lang.Noop, setCommand Cmd.RotateRight state)
    Lang.Seq lhs rhs -> let (termLhs', stateLhs') = evalTerm lhs state
                            (termRhs', stateRhs') = evalTerm rhs state
                        in if lhs == Lang.Noop
                           then (termRhs', stateRhs')
                           else (Lang.Seq termLhs' rhs, stateLhs')
