module Pandemic.Actions where

import Control.Lens
import Control.Monad
import Data.Monoid
import Pandemic.Game
import Pandemic.Rules

relocate :: City -> Play Game ()
relocate city = do
    r <- zoom currentPlayer $ do
            location .= city
            use role
    
    when (r == Medic) $ autocure city

moveTo :: City -> Play Game ()
moveTo city = do
    loc <- use (currentPlayer . location)
    ok <- city `inRangeOf` loc
    when (not ok) $ block "This city is not in range of a simple move"
    relocate city
