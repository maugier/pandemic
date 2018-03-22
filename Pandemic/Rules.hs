{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Pandemic.Rules where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Data.Function (on)
import qualified Data.Map as M
import Data.Set (Set, member, insert, empty)
import Data.Maybe
import Data.Ord

data Role = Scientist
          | Researcher
          | Operations
          | Dispatcher
          | EmergencyPlanner
          | QuarantinePlanner
          | Medic
    deriving (Eq,Ord,Show)

data Color = Red | Blue | Yellow | Black
    deriving (Show,Eq,Ord,Enum)

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

data Card = CityCard City
          | EventCard Event
    deriving (Eq,Ord,Show)

data Player = Player {
    _name :: String,
    _role :: Role,
    _cards :: [Card],
    _location :: City
}

makeLenses ''Player


data Game = Game {
    _infection :: M.Map City Infection,
    _centers :: Set City,
    _cures :: Cures
}

makeLenses ''Game


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


            
            
