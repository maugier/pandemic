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
    useAction
    loc <- use (currentPlayer . location)
    ok <- city `inRangeOf` loc
    when (not ok) $ block "This city is not in range of a simple move"
    relocate city

directFlightTo :: City -> Play Game ()
directFlightTo city = do
    useAction
    useCard (CityCard city)
    relocate city

charterTo :: City -> Play Game ()
charterTo city = do
    useAction
    loc <- use (currentPlayer . location)
    useCard (CityCard loc)
    relocate city

buildStation :: Play Game ()
buildStation = do
    useAction
    loc <- use (currentPlayer . location)
    useCard (CityCard loc)

treatDisease :: Color -> Play Game ()
treatDisease color = do
    useAction
    loc <- use (currentPlayer . location)
    isMedic <- use (currentPlayer . role . to (== Medic))
    erad <- use (disease color . cured)
    cleanCity color (isMedic || erad) loc
