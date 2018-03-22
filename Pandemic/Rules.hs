{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Pandemic.Rules where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Control.Monad.Random
import Data.Function (on)
import qualified Data.Map as M
import Data.Set (Set, member, insert, empty, elems)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Pandemic.Deck
import Pandemic.Util

data Role = Scientist
          | Researcher
          | Operations
          | Dispatcher
          | EmergencyPlanner
          | QuarantinePlanner
          | Medic
    deriving (Eq,Ord,Show)

data Color = Red | Blue | Yellow | Black
    deriving (Show,Eq,Ord,Enum,Bounded)

type Infection = M.Map Color Int
type Cures = Set Color


type Outbreaks = Set City
type Neighbors = Set City

data City = City {
    _cityName :: String,
    _nativeColor :: Color,
    _neighbors :: Neighbors
}

instance Ord City where
    compare = comparing _cityName

instance Eq City where
    (==) = (==) `on` _cityName

instance Show City where
    show = show . _cityName

makeLenses ''City

data Event = Airlift
           | PublicSubvention
    deriving (Eq,Ord,Show)

data PlayerCard = CityCard City
                | EventCard Event
                | Epidemic
    deriving (Eq,Ord,Show)

newtype InfectionCard = InfectionCard City

data Player = Player {
    _name :: String,
    _role :: Role,
    _cards :: [PlayerCard],
    _location :: City
}

makeLenses ''Player


data Game = Game {
    _players :: [Player],
    _cities :: Set City,
    _infection :: M.Map City Infection,
    _centers :: Set City,
    _cures :: Cures,
    _epidemics :: Int,
    _outbreaks :: Int,
    _infectionDeck :: Deck InfectionCard,
    _playerDeck :: Deck PlayerCard
}

makeLenses ''Game


game :: City -> Game
game city = let cities = closure city _neighbors
                clean = M.fromList [(color, 0) | color <- [minBound..maxBound]]
                infects = M.fromList [(city, clean) | city <- elems cities ]
            in Game [] cities infects empty empty 0 0 emptyDeck emptyDeck


intensity n | n < 3 = 2
            | n < 5 = 3
            | otherwise = 4


addPlayer :: (Monad m, MonadRandom m, MonadState Game m) => String -> Role -> m ()
addPlayer name role = do
    return ()
    

epidemic :: (Monad m, MonadRandom m, MonadState Game m) => m ()
epidemic = do
    epidemics += 1
        



infect :: Color -> City -> State Game Outbreaks
infect color city = execStateT (infect' city) empty where
    infect' :: City -> StateT Outbreaks (State Game) ()
    infect' city = do
        outbreak <- lift . zoom (infection . ix city . ix color) $ do
            i <- get
            if i < 3
                then modify (+1) >> return Nothing
                else return $ Just ()
        when (isJust outbreak) $ do
            outs <- get
            (when . not) (city `member` outs) $ do
                modify (insert city)
                forM_ (city ^. neighbors) infect'


            
            
