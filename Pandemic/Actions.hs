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
    isCured <- use (disease color . cured)
    cleanCity color (isMedic || isCured) loc

cureDisease :: Color -> [City] -> Play Game ()
cureDisease color hand = do
    useAction
    (when.has (folded . nativeColor . filtered (/= color))) hand $ block "Cards are not of the correct color"
    r <- use (currentPlayer . role)
    let l = length hand
    (when.not) (l == 5 || (r == Scientist && l == 4)) $ block "Not the correct number of cards for a cure"
    forM_ hand (useCard . CityCard)
    disease color . cured .= True

takeCardFrom :: String -> City -> Play Game ()
takeCardFrom name city = do
    useAction
    loc <- use (currentPlayer . location)
    withPlayer name $ do
        otherloc <- use location
        when (loc /= otherloc) $ block "You must be in the same city to exchange cards"
        r <- use role
        when (r /= Researcher && otherloc /= city) $ block "Only the researcher can offer cards from remote cities"
        takeCard (CityCard city)
    zoom currentPlayer $ giveCard (CityCard city)

giveCardTo :: String -> City -> Play Game ()
giveCardTo name city = do
    useAction
    zoom currentPlayer $ takeCard (CityCard city)
    loc <- use (currentPlayer . location)
    r <- use (currentPlayer . role)
    withPlayer name $ do
        otherloc <- use location
        when (loc /= otherloc) $ block "You must be in the same city to exchange cards"
        when (r /= Researcher && otherloc /= city) $ block "Only the researcher can offer cards from remote cities"
        giveCard (CityCard city)
