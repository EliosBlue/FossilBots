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
    TurnLeft -> Right TCommand
    TurnRight -> Right TCommand
    TurnAround -> Right TCommand
    RandomTurns -> Right TCommand
    MoveForward -> Right TCommand
    DropBeacon _ -> Right TCommand
    DestroyBeacon -> Right TCommand
    PickUpFossil -> Right TCommand
    DropFossil -> Right TCommand
    Idle -> Right TCommand
    Search _ _ -> Right TCommand
    SetMode _ -> Right TCommand
    IfMode _ t1 t2 -> do
        ty1 <- typeCheckTerm t1
        ty2 <- typeCheckTerm t2
        if ty1 == TCommand && ty2 == TCommand
            then Right TCommand
            else Left $ "IfMode branches must be commands, got: " ++ show ty1 ++ " ; " ++ show ty2
    Seq t1 t2 -> do
        ty1 <- typeCheckTerm t1
        ty2 <- typeCheckTerm t2
        if ty1 == TCommand && ty2 == TCommand
           then Right TCommand
           else Left $ "Sequence expects command types, got: " ++ show ty1 ++ " ; " ++ show ty2
    IfFossil tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
           then Right TCommand
           else Left $ "IfFossil branches must be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    IfBase tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "IfBase branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    IfBeacon _ tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "IfBeacon branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
    IfNearby tThen tElse -> do
        tyThen <- typeCheckTerm tThen
        tyElse <- typeCheckTerm tElse
        if tyThen == TCommand && tyElse == TCommand
            then Right TCommand
            else Left $ "IfNearby branches must both be commands, got: " ++ show tyThen ++ " / " ++ show tyElse
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
