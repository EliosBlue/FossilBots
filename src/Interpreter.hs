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
import           Fryxbots.Bot.Sensing
import           Fryxbots.Bot.Facing (Facing(..))
import           Fryxbots.Team (Team(..))
import           System.Random (randomR)
import           Fryxbots.Beacon

data Interpreter = Interpreter { stdGen :: StdGen, modeTable :: [(String, BLTerm)]}

-- Evaluate a top-level ExampleLang AST.
evalBL :: StdGen -> [(String, BLTerm)] -> Sensing -> Team -> Bool -> BotLang -> State -> (StdGen, BotLang, State)
evalBL stdGen modes sense team hasFossil expr state =
  case expr of
    Lang.Modes [] -> (stdGen, expr, state)
    Lang.Modes modeList ->
      let len = length modeList
          (index, stdGen') = randomR (0, len - 1) stdGen :: (Int, StdGen)
          term = snd (modeList !! index)
          state' = setCommand Cmd.Idle state
      in (stdGen', Lang.Term term, state')
    Lang.Term term ->
      let (term', stdGen', state') = evalTerm stdGen modes term sense team hasFossil state
      in (stdGen', Lang.Term term', state')

-- Evaluate an ExampleLang Term. Thread StdGen through to avoid unsafePerformIO.
evalTerm :: StdGen -> [(String, BLTerm)] -> BLTerm -> Sensing -> Team -> Bool -> State -> (BLTerm, StdGen, State)
evalTerm stdGen modes term sense team hasFossil state =
  case term of
    Lang.TurnLeft  -> (Lang.Noop, stdGen, setCommand Cmd.RotateLeft state)
    Lang.TurnRight -> (Lang.Noop, stdGen, setCommand Cmd.RotateRight state)
    Lang.MoveForward -> (Lang.Noop, stdGen, setCommand Cmd.MoveForward state)
    Lang.DropBeacon beacon -> (Lang.Noop, stdGen, setCommand (Cmd.DropBeacon beacon) state)
    Lang.DestroyBeacon -> (Lang.Noop, stdGen, setCommand Cmd.DestroyBeacon state)
    Lang.PickUpFossil -> (Lang.Noop, stdGen, setCommand Cmd.PickUpFossil state)
    Lang.DropFossil -> (Lang.Noop, stdGen, setCommand Cmd.DropFossil state)
    Lang.FossilCond thTerm elTerm ->
      if numFossils (current sense) > 0 && (isBase (current sense) team)
        then evalTerm stdGen modes thTerm sense team hasFossil state
        else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.BaseCond thTerm elTerm ->
      if (isBase (current sense) team)
        then evalTerm stdGen modes thTerm sense team hasFossil state
        else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.BeaconCond beaconKind thTerm elTerm ->
      let beaconPresent = case beacon (current sense) team of
            Just k -> k == beaconKind
            Nothing -> False
      in if beaconPresent
           then evalTerm stdGen modes thTerm sense team hasFossil state
           else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.NearbyCond thTerm elTerm ->
      let fossilNearby = numFossils (current sense) > 0 in
      if fossilNearby
        then evalTerm stdGen modes thTerm sense team hasFossil state
        else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.IfBeaconDir beaconKind idx thTerm elTerm ->
      let valid = idx >= 0 && idx <= 5
      in if not valid
           then evalTerm stdGen modes elTerm sense team hasFossil state
           else let facing = idxToFacing idx
                    hex = adjacent sense facing
                    beaconPresent = case beacon hex team of
                      Just k -> k == beaconKind
                      Nothing -> False
                in if beaconPresent
                     then evalTerm stdGen modes thTerm sense team hasFossil state
                     else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.IfBaseDir idx thTerm elTerm ->
      let valid = idx >= 0 && idx <= 5
      in if not valid
           then evalTerm stdGen modes elTerm sense team hasFossil state
           else let facing = idxToFacing idx
                    hex = adjacent sense facing
                    isbase = isBase hex team
                in if isbase
                     then evalTerm stdGen modes thTerm sense team hasFossil state
                     else evalTerm stdGen modes elTerm sense team hasFossil state
    Lang.CallMode name ->
      case lookup name modes of
        Just body -> (body, stdGen, state)
        Nothing -> (Lang.Noop, stdGen, state)
    Lang.For num body ->
      if num <= 0
        then (Lang.Noop, stdGen, state)
        else
          -- Expand the loop into a sequence: execute the body once, then continue with For (num-1) body
          (Lang.Seq body (Lang.For (num - 1) body), stdGen, state)
    Lang.While cond body ->
      let (cond', stdGen', state') = evalTerm stdGen modes cond sense team hasFossil state
      in if cond' /= Lang.Noop  -- If condition true
           then let (body', stdGen'', state'') = evalTerm stdGen' modes body sense team hasFossil state'
                 in (Lang.While cond body', stdGen'', state'')
           else (Lang.Noop, stdGen', state')
    Lang.RepeatUntil body cond ->
      if body == Lang.Noop
        then let (cond', stdGen', state') = evalTerm stdGen modes cond sense team hasFossil state
             in if cond' /= Lang.Noop
                then (Lang.Noop, stdGen', state')
                else (Lang.RepeatUntil body cond, stdGen', state')
        else let (body', stdGen', state') = evalTerm stdGen modes body sense team hasFossil state
             in (Lang.RepeatUntil body' cond, stdGen', state')
    Lang.Choose lhs rhs ->
      let (pick, stdGen') = randomR (0 :: Int, 1 :: Int) stdGen in
      if pick == 0 then evalTerm stdGen' modes lhs sense team hasFossil state
                   else evalTerm stdGen' modes rhs sense team hasFossil state
    Lang.Seq lhs rhs -> if lhs == Lang.Noop
                          then evalTerm stdGen modes rhs sense team hasFossil state
                          else 
                            let (termLhs', stdGen', stateLhs') = evalTerm stdGen modes lhs sense team hasFossil state
                            in (Lang.Seq termLhs' rhs, stdGen', stateLhs')
    Lang.Noop -> (Lang.Noop, stdGen, state)
  where
    idxToFacing :: Int -> Facing
    idxToFacing 0 = NorthEast
    idxToFacing 1 = East
    idxToFacing 2 = SouthEast
    idxToFacing 3 = SouthWest
    idxToFacing 4 = West
    idxToFacing 5 = NorthWest
    idxToFacing _ = NorthEast