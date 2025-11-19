-- A bot controller which operates by running an ExampleLang program.

module Controller
  ( ELController
  , mkELController
  ) where

import Fryxbots.Bot.Controller
import Interpreter
import Language
import System.Random

data ELController = ELController
  { interpreter :: Interpreter
  , program :: ExampleLang
  }

mkELController :: ExampleLang -> ELController
mkELController prog = ELController
  { interpreter = Interpreter { stdGen = mkStdGen 0 }
  , program = prog
  }

--
-- The Fryxbot controller for ExampleLang programs.
--
instance Controller ELController where

  -- Initializing a controller seeds a new random generator
  -- in the interpreter.
  initialize cont botId _ =
    let interp' = (interpreter cont) { stdGen = mkStdGen botId }
    in cont { interpreter = interp' }

  -- Stepping a bot to a new state works by:
  --   1. Passing the controller's program to the ExampleLang interpreter
  --   2. Updating the controller with the program the evaluator stepped to
  --   3. Updating the random number generator in the interpreter
  --   4. Returning the updated controller and the newly evaluated state
  stepBot cont _ _ _ state =
      let prog = program cont
          gen = stdGen . interpreter $ cont
          (gen', prog', state') = evalEL gen prog state
          interp' = (interpreter cont) { stdGen = gen' }
          cont' = cont { interpreter = interp', program = prog' }
      in (cont', state')
