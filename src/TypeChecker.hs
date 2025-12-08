module TypeChecker
  ( Types(..)
  , typeCheckBL
  , typeCheckTerm
  ) where

import Language

-- Simple type system
data Types =
    TArrow Types Types
  | TBool
  | TCommand
  deriving (Eq)

instance Show Types where
    show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show TBool = "bool"
    show TCommand = "command"

-- Returns the type of a term or an error string
typeCheckTerm :: BLTerm -> Either String Types
typeCheckTerm term = case term of
    TurnLeft       -> Right TCommand
    TurnRight      -> Right TCommand
    MoveForward    -> Right TCommand
    DropBeacon _   -> Right TCommand
    DestroyBeacon  -> Right TCommand
    PickUpFossil   -> Right TCommand
    DropFossil     -> Right TCommand
    Idle           -> Right TCommand
    Seq t1 t2 -> do
        ty1 <- typeCheckTerm t1
        ty2 <- typeCheckTerm t2
        if ty1 == TCommand && ty2 == TCommand
           then Right TCommand
           else Left $ "Sequence expects command types, got: " ++ show ty1 ++ " ; " ++ show ty2
    FossilCond tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
           then Right TCommand
           else Left $ "FossilCond branches must be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    BaseCond tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "BaseCond branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    BeaconCond _ tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "BeaconCond branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    NearbyCond tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "NearbyCond branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    For _ body -> do
        tybody <- typeCheckTerm body
        if tybody == TCommand
            then Right TCommand
            else Left $ "For loop body expects a command"
    Choose a b -> do
        tyA <- typeCheckTerm a
        tyB <- typeCheckTerm b
        if tyA == TCommand && tyB == TCommand
           then Right TCommand
           else Left $ "Choose branches must be commands, got: " ++ show tyA ++ " / " ++ show tyB
    IfBeaconDir _ _ tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
           then Right TCommand
           else Left $ "IfBeaconDir branches must be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    IfBaseDir _ tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
           then Right TCommand
           else Left $ "IfBaseDir branches must be commands, got: " ++ show tyThen ++ " / " ++ show tyElse


typeCheckBL :: BotLang -> Either String ()
typeCheckBL bl =
  case bl of
    Term t ->
      typeCheckTerm t >> Right ()

    Modes modes ->
      sequence_ [ case typeCheckTerm t of
                    Left err -> Left ("In mode '" ++ name ++ "': " ++ err)
                    Right _  -> Right ()
                | (name, t) <- modes ]
