-- The ExampleLang interpreter.

module Interpreter
  ( ELController
  , mkELController
  ) where

import           Fryxbots.Bot.Controller
import qualified Fryxbots.Bot.Command as Cmd
import           Fryxbots.Bot.State
import           Language (ExampleLang, ELTerm)
import qualified Language as Lang
import           System.Random

data Interpreter = Interpreter { stdGen :: StdGen }

evalEL :: StdGen -> ExampleLang -> State -> (StdGen, ExampleLang, State)
evalEL stdGen expr state =
  case expr of
    Lang.Modes [] -> (stdGen, expr, state)
    Lang.Modes modes ->
      let len = length modes
          (index, stdGen') = randomR (0, len - 1) stdGen :: (Int, StdGen)
          term = snd $ modes !! index
          state' = setCommand Cmd.Idle state
      in (stdGen', Lang.Term term, state')
    Lang.Term term ->
      let (term', state') = evalTerm term state
      in (stdGen, Lang.Term term', state')

evalTerm :: ELTerm -> State -> (ELTerm, State)
evalTerm term state =
  case term of
    Lang.Forever body -> ( Lang.Seq body term
                         , setCommand Cmd.Idle state )
    Lang.TurnLeft  -> (Lang.Noop, setCommand Cmd.RotateLeft state)
    Lang.TurnRight -> (Lang.Noop, setCommand Cmd.RotateRight state)
    Lang.Seq lhs rhs -> let (term', state') = evalTerm lhs state
                         in if term' == Lang.Noop
                            then (rhs, state')
                            else (term', state')


data ELController = ELController
  { interpreter :: Interpreter
  , currentExpr :: ExampleLang
  }

mkELController :: ExampleLang -> ELController
mkELController expr = ELController
  { interpreter = Interpreter { stdGen = mkStdGen 0 }
  , currentExpr = expr
  }

instance Controller ELController where

  initialize cont botId _ =
    let interp' = (interpreter cont) { stdGen = mkStdGen botId }
    in cont { interpreter = interp' }

  stepBot cont _ _ _ state =
      let expr = currentExpr cont
          gen = stdGen . interpreter $ cont
          (gen', expr', state') = evalEL gen expr state
          interp' = (interpreter cont) { stdGen = gen' }
          cont' = cont { interpreter = interp', currentExpr = expr' }
      in (cont', state')
