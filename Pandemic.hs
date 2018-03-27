module Pandemic where

import Control.Lens
import Control.Monad.State
import Data.IORef
import Pandemic.Actions
import Pandemic.Game
import Pandemic.Rules
import Pandemic.Map
import Pandemic.Pretty
import Pandemic.World

new :: Difficulty -> [(String,Maybe Role)] -> IO (IORef Game)
new difficulty players = do
    ps <- assignRoles players
    g <- newIORef (newGame atlanta ps)
    play g $ setupGame difficulty
    return g


disp :: IORef Game -> IO ()
disp = (>>= colorPrint8 . display) . readIORef

showHand :: Play Game ()
showHand = do
    g <- get
    let hand = g ^.. (currentPlayer . cards . folded)
    liftIO . putStrLn $ "You have " ++ show (length hand) ++ " cards."
    mapM_ (liftIO.pPrint) hand
    liftIO $ putStrLn ""
