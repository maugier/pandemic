
module Pandemic.Game where

import Data.IORef
import Control.Monad.Except
import Control.Monad.State

data GameError = LosingCondition String
               | WinningCondition String
               | OtherError String
    deriving (Show)

type Play s = StateT s (ExceptT GameError IO)

win, lose, block :: String -> Play s a
win = throwError . WinningCondition
lose = throwError . LosingCondition
block = throwError . OtherError

announce :: String -> Play s ()
announce = liftIO . putStrLn 

play :: IORef s -> Play s x -> IO x
play ref act = do
    s <- readIORef ref
    r <- runExceptT (runStateT act s)
    case r of
        Right (a,s') -> writeIORef ref s' >> return a
        Left err -> error (show err)


