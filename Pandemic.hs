module Pandemic where

import Control.Lens
import Data.IORef
import Pandemic.Actions
import Pandemic.Game
import Pandemic.Rules
import Pandemic.Pretty
import Pandemic.World

new = newIORef (newGame atlanta [("Max", Medic), ("Noosh", Operations)])

disp :: Pretty a => IORef a -> IO ()
disp = (>>= pPrint) . readIORef

