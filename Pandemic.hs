module Pandemic where

import Control.Lens
import Data.IORef
import Pandemic.Actions
import Pandemic.Game
import Pandemic.Rules
import Pandemic.Map
import Pandemic.World

new = newIORef (newGame atlanta [("Max", Medic), ("Noosh", Operations)])

disp :: IORef Game -> IO ()
disp = (>>= colorPrint8 . display) . readIORef

