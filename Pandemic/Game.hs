
module Pandemic.Game where

import Data.IORef
import Control.Monad.Except
import Control.Monad.State

data GameError = LosingCondition String
               | OtherError String
    deriving (Show)

type Play s = StateT s (ExceptT GameError IO)

lose :: String -> Play s a
lose = throwError . LosingCondition

block :: String -> Play s a
block = throwError . OtherError

play :: IORef s -> Play s x -> IO x
play ref act = do
    s <- readIORef ref
    r <- runExceptT (runStateT act s)
    case r of
        Right (a,s') -> writeIORef ref s' >> return a
        Left err -> error (show err)
        
