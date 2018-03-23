module Pandemic where

import Control.Lens
import Data.IORef
import Pandemic.Game
import Pandemic.Rules
import Pandemic.World

new = newIORef . newGame $ atlanta

disp :: Show a => IORef a -> IO ()
disp = (>>= print) . readIORef

