-- A bot controller which operates by running an BotLang program.

module Controller
  ( BLController
  , mkBLController
  ) where

import Fryxbots.Bot.Controller
import Fryxbots.Team
import Interpreter
import Language
import System.Random

data BLController = BLController
  { interpreter :: Interpreter
  , program :: BotLang
  , team :: Team
  }

mkBLController :: BotLang -> BLController
mkBLController prog = BLController
  { interpreter = Interpreter { stdGen = mkStdGen 0 }
  , program = prog
  , team = Blue
  }

--
-- The Fryxbot controller for ExampleLang programs.
--
instance Controller BLController where

  -- Initializing a controller seeds a new random generator
  -- in the interpreter.
  initialize cont botId botTeam =
    let interp' = (interpreter cont) { stdGen = mkStdGen botId }
    in cont { interpreter = interp', team = botTeam }

  -- Stepping a bot to a new state works by:
  --   1. Passing the controller's program to the BotLang interpreter
  --   2. Updating the controller with the program the evaluator stepped to
  --   3. Updating the random number generator in the interpreter
  --   4. Returning the updated controller and the newly evaluated state
  stepBot cont facing sensing hasFossil state =
      let prog = program cont
          gen = stdGen . interpreter $ cont
          (gen', prog', state') = evalEL gen prog state
          interp' = (interpreter cont) { stdGen = gen' }
          cont' = cont { interpreter = interp', program = prog' }
      in (cont', state')

  -- Arguments to stepBot give:
  --
  --  + The controller
  --  + The direction the bot is facing
  --  + The hex scans for the current and adjacent positions
  --  + Whether or not the bot is carrying a fossile
  --  + The current state
  --
  -- The sensing data is given as:
  --
  --   data Sensing = Sensing
  --     { current :: HexScan
  --     , adjacent :: Facing -> HexScan
  --     }
  --
  --   data HexScan = OffMap | HexScan
  --     { beacon :: Team -> Maybe BeaconKind
  --     , isBuilding :: Bool
  --     , isBase :: Team -> Bool
  --     , hasBot :: Team -> Bool
  --     , numFossils :: Int
  --     }
  --
  -- See the Fryxbot library for full details.
