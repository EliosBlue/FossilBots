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
--   <turnLeft, s> -> <idle, s[cmd := RotateLeft]>
--
--
--  ------------------------------------------------ ExlTurnRight
--   <turnRight, s> -> <idle, s[cmd := turnRight]>
--
--
--  --------------------------------------------------- ExlMoveForward
--   <moveForward, s> -> <idle, s[cmd := moveForward]>
--
--
--  ----------------------------------------------------------------------- ExlDropBeacon
--   <dropBeacon BeaconKind, s> -> <idle, s[cmd := dropBeacon BeaconKind]>
--
--
--  ------------------------------------------------------- ExlDestroyBeacon
--   <destroyBeacon, s> -> <idle, s[cmd := destroyBeacon]>
--
--
--  ----------------------------------------------------- ExlPickUpFossil
--   <pickUpFossil, s> -> <idle, s[cmd := pickUpFossil]>
--
--
--  ------------------------------------------------- ExlDropFossil
--   <dropFossil, s> -> <idle, s[cmd := dropFossil]>
--
--
--         <t1, s> -> <t1', s'>      t1 /= idle
--  ------------------------------------------------ ExlSeqL
--             <t1; t2, s> -> <t1'; t2, s'>
--
--
--         <t2, s> -> <t2', s'>      t1 = idle
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
import qualified Fryxbots.Bot.Sensing as S
import qualified Fryxbots.Bot.Facing as F
import           Fryxbots.Team (Team(..))
import           System.Random (randomR)
import           Fryxbots.Beacon

data Interpreter = Interpreter { stdGen :: StdGen}

-- Evaluate a top-level ExampleLang AST.
evalBL :: StdGen -> S.Sensing -> F.Facing -> Team -> Bool -> BotLang -> State -> (StdGen, BotLang, State)
evalBL stdGen sense face team hasFossil expr state =
  case expr of
    Lang.Modes [] -> (stdGen, expr, state)
    Lang.Modes modes ->
      let len = length modes
          (index, stdGen') = randomR (0, len - 1) stdGen :: (Int, StdGen)
          term = snd (modes !! index)
          state' = setCommand Cmd.Idle state
      in (stdGen', Lang.Term term, state')
    Lang.Term term ->
      let (term', stdGen', state') = evalTerm stdGen term sense face team hasFossil state
      in (stdGen', Lang.Term term', state')

-- Evaluate an ExampleLang Term. Thread StdGen through to avoid unsafePerformIO.
evalTerm :: StdGen -> BLTerm -> S.Sensing -> F.Facing -> Team -> Bool -> State -> (BLTerm, StdGen, State)
evalTerm stdGen term sense face team hasFossil state =
  case term of
    Lang.TurnLeft  -> (Lang.Idle, stdGen, setCommand Cmd.RotateLeft state) -- 60 deg left
    Lang.TurnRight -> (Lang.Idle, stdGen, setCommand Cmd.RotateRight state) -- 60 deg right
    Lang.TurnAround -> (Lang.Seq Lang.TurnLeft (Lang.Seq Lang.TurnLeft Lang.TurnLeft), stdGen, state) -- 180 deg
    Lang.RandomTurns -> 
      let (pick, stdGen') = randomR (0 :: Int, 5 :: Int) stdGen in
      case pick of
        0 -> (Lang.Idle, stdGen', state)
        1 -> (Lang.TurnRight, stdGen', state)
        2 -> (Lang.Seq Lang.TurnRight Lang.TurnRight, stdGen', state)
        3 -> (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight Lang.TurnRight), stdGen', state)
        4 -> (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight Lang.TurnRight)), stdGen', state)
        5 -> (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight (Lang.Seq Lang.TurnRight Lang.TurnRight))), stdGen', state)
    Lang.MoveForward ->
      let nextHex   = S.adjacent sense face
          isTheirBase = case team of
            Gold -> S.isBase nextHex Blue
            Blue -> S.isBase nextHex Gold
      in if isTheirBase || S.hasBot (S.adjacent sense face) Blue || S.hasBot (S.adjacent sense face) Gold
          then
            (Lang.TurnAround, stdGen, state)  -- bot can not moveforward if base is enemy or if there is a bot in front so it turns
          else (Lang.Idle, stdGen, setCommand Cmd.MoveForward state)
    Lang.DropBeacon beacon -> (Lang.Idle, stdGen, setCommand (Cmd.DropBeacon beacon) state)
    Lang.DestroyBeacon -> (Lang.Idle, stdGen, setCommand Cmd.DestroyBeacon state)
    Lang.PickUpFossil -> 
      if S.isBase (S.current sense) team 
        then (Lang.Idle, stdGen, state)   -- bot can not remove fossils form own base
        else (Lang.Idle, stdGen, setCommand Cmd.PickUpFossil state)
    Lang.DropFossil -> (Lang.Idle, stdGen, setCommand Cmd.DropFossil state)
    Lang.Search object num ->
      let frontHex = S.adjacent sense face
          fossilHere = S.numFossils frontHex > 0
          baseHere   = S.isBase frontHex team
          beaconHere = case S.beacon frontHex team of
                         Just _  -> True
                         Nothing -> False
          found =
            case object of
              "fossil" -> fossilHere
              "base"   -> baseHere
              "beacon" -> beaconHere
              _        -> False
      in
        if found
          then (Lang.Idle, stdGen, setCommand Cmd.MoveForward state) --object found
          else if num >= 6
            then (Lang.Idle, stdGen, state) -- has done a full roatation without finding anything
            else (Lang.Seq Lang.TurnLeft (Lang.Search object (num + 1)), stdGen, state) -- has not found object and has more directions to check
    Lang.IfFossil thTerm elTerm ->
      if S.numFossils (S.current sense) > 0
        then evalTerm stdGen thTerm sense face team hasFossil state
        else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.IfBase thTerm elTerm ->
      if (S.isBase (S.current sense) team)
        then evalTerm stdGen thTerm sense face team hasFossil state
        else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.IfBeacon beaconKind thTerm elTerm ->
      let beaconPresent = case S.beacon (S.current sense) team of
            Just k -> k == beaconKind
            Nothing -> False
      in if beaconPresent
           then evalTerm stdGen thTerm sense face team hasFossil state
           else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.IfNearby thTerm elTerm ->
      let fossilNearby = S.numFossils (S.current sense) > 0 in
      if fossilNearby
        then evalTerm stdGen thTerm sense face team hasFossil state
        else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.IfBeaconDir beaconKind idx thTerm elTerm ->
      let valid = idx >= 0 && idx <= 5
      in if not valid
           then evalTerm stdGen elTerm sense face team hasFossil state
           else let facing = idxToFacing idx
                    hex = S.adjacent sense facing
                    beaconPresent = case S.beacon hex team of
                      Just k -> k == beaconKind
                      Nothing -> False
                in if beaconPresent
                     then evalTerm stdGen thTerm sense face team hasFossil state
                     else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.IfBaseDir idx thTerm elTerm ->
      let valid = idx >= 0 && idx <= 5
      in if not valid
           then evalTerm stdGen elTerm sense face team hasFossil state
           else let facing = idxToFacing idx
                    hex = S.adjacent sense facing
                    isbase = S.isBase hex team
                in if isbase
                     then evalTerm stdGen thTerm sense face team hasFossil state
                     else evalTerm stdGen elTerm sense face team hasFossil state
    Lang.SetMode num -> 
      let st' = memStore "mode" num state
      in (Lang.Idle, stdGen, st')
    Lang.IfMode num tThen tElse ->
      case memRead "mode" state of
        Just m | m == num -> evalTerm stdGen tThen sense face team hasFossil state
        _ -> evalTerm stdGen tElse sense face team hasFossil state
    Lang.For num body ->
      if num <= 0
        then (Lang.Idle, stdGen, state)
        else
          -- For loop: execute body once, then loop again with num-1
          -- Don't expand the full loop structure; just step once per turn
          (Lang.Seq body (Lang.For (num - 1) body), stdGen, state)
    Lang.Choose lhs rhs ->
      let (pick, stdGen') = randomR (0 :: Int, 1 :: Int) stdGen in
      if pick == 0 then evalTerm stdGen' lhs sense face team hasFossil state
                   else evalTerm stdGen' rhs sense face team hasFossil state
    Lang.Seq lhs rhs -> 
      case lhs of
        Lang.Idle -> evalTerm stdGen rhs sense face team hasFossil state
        _ ->  let (lhs', stdGen', state') = evalTerm stdGen lhs sense face team hasFossil state
              in (Lang.Seq lhs' rhs, stdGen', state')
    Lang.Idle -> (Lang.Idle, stdGen, state)
  where
    idxToFacing :: Int -> F.Facing
    idxToFacing 0 = F.NorthEast
    idxToFacing 1 = F.East
    idxToFacing 2 = F.SouthEast
    idxToFacing 3 = F.SouthWest
    idxToFacing 4 = F.West
    idxToFacing 5 = F.NorthWest
    idxToFacing _ = F.NorthEast