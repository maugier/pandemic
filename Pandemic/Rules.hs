{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Pandemic.Rules where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Control.Monad.Random
import Data.Function (on)
import qualified Data.Map as M
import Data.Set (Set, member, insert, empty, singleton, elems)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Pandemic.Deck
import Pandemic.Game
import Pandemic.Util
import System.Random.Shuffle (shuffleM)

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


data City = City {
    _cityName :: String,
    _nativeColor :: Color,
    _neighbors :: Set City
}
makeLenses ''City

instance Ord City where
    compare = comparing _cityName

instance Eq City where
    (==) = (==) `on` _cityName

instance Show City where
    show = show . _cityName

type Neighbors = Set City

type Infection = M.Map City Int

data Disease = Disease {
    _cured :: Bool,
    _reserve :: Int,
    _infection :: Infection
} deriving (Eq,Ord,Show)
makeLenses ''Disease

infectionAt :: City -> Simple Lens Disease Int
infectionAt city = infection . at city . non 0 

eradicated :: Disease -> Bool
eradicated (Disease True _ m) = M.null m
eradicated _ = False

emptyDisease = Disease True 24 M.empty

data Event = Airlift
           | PublicSubvention
    deriving (Eq,Ord,Show,Enum,Bounded)

data HandCard = CityCard City
              | EventCard Event
    deriving (Eq,Ord,Show)

data PlayerCard = HandCard HandCard
                | EpidemicCard
    deriving (Eq,Ord,Show)

eventCards = map EventCard allOfThem

newtype InfectionCard = InfectionCard City
    deriving (Eq,Ord,Show)

data Player = Player {
    _name :: String,
    _role :: Role,
    _cards :: [HandCard],
    _location :: City
} deriving (Show)

instance Ord Player where
    compare = comparing _name

instance Eq Player where
    (==) = (==) `on` _name

makeLenses ''Player


data Game = Game {
    _home :: City,
    _players :: M.Map String Player,
    _cities :: Set City,
    _diseases :: M.Map Color Disease,
    _centers :: Set City,
    _epidemics :: Int,
    _outbreaks :: Int,
    _infectionDeck :: Deck InfectionCard,
    _playerDeck :: Deck PlayerCard
} deriving (Show)

makeLenses ''Game

disease :: Color -> Simple Lens Game Disease
disease color = diseases . at color . anon emptyDisease eradicated

give :: String -> HandCard -> Play Game ()
give player card = players . ix player . cards %= (card :)


newGame :: City -> Game
newGame home = Game {
                        _home = home,
                        _players = M.empty,
                        _cities = closure _neighbors home,
                        _diseases = M.fromList [(color, cured .~ False $ emptyDisease) | color <- allOfThem ],
                        _centers = singleton home,
                        _epidemics = 0,
                        _outbreaks = 0,
                        _infectionDeck = emptyDeck,
                        _playerDeck = emptyDeck
               }

initialInfection :: Play Game ()
initialInfection = forM_ [3,2,1] $ \n -> replicateM_ 3 $ do
    InfectionCard city <- zoom infectionDeck drawAndDiscard
    infect n city


prepareInfectionDeck :: Play Game ()
prepareInfectionDeck = do
    cities <- use (cities . _Wrapped)
    zoom infectionDeck $ do
        put . deck . map InfectionCard $ cities
        zoom drawPile shuffle


setupGame :: Int -> Play Game ()
setupGame difficulty = do
    prepareInfectionDeck
    initialInfection
    setupPlayerDeck difficulty


cardsPerPlayer :: Game -> Int
cardsPerPlayer game = do
    case M.size $ game ^. players of
        2 -> 4
        3 -> 3
        4 -> 2
        
drawCard :: String -> Play Game ()
drawCard player = do
    card <- zoom playerDeck draw
    case card of
        EpidemicCard -> epidemic
        HandCard hand -> give player hand


dealPlayerHands :: StateT [HandCard] (Play Game) ()
dealPlayerHands = do
    g <- lift get
    let n = cardsPerPlayer g
    replicateM_ n . forM_ (g ^. players . to M.keys) $ \p -> do
        next <- get
        case next of
            [] -> lift $ block "Not enough cards to deal to player"
            (c:cs) -> lift (give p c) >> put cs

setupPlayerDeck :: Int -> Play Game ()
setupPlayerDeck difficulty = do
    cityCards <- (map CityCard . elems) <$> use cities
    cards <- shuffleM (eventCards ++ cityCards)
    rest <- execStateT dealPlayerHands cards
    let packets = (HandCard <$>) <$> (sparse difficulty rest)
    shuffled <- mapM shuffleM packets
    playerDeck .= deck (concat shuffled)


intensity :: Int -> Int
intensity n | n < 3 = 2
            | n < 5 = 3
            | otherwise = 4



addPlayer :: String -> Role -> Play Game ()
addPlayer name role = do
    h <- use home
    players %= M.insert name (Player name role [] h)
    

epidemicTarget :: Play Game City
epidemicTarget = zoom infectionDeck $ do
    card@(InfectionCard city) <- drawLast
    discard card
    zoom discardPile shuffle
    modify resetDiscard
    return city

epidemic :: Play Game ()
epidemic = do
    epidemics += 1
    city <- epidemicTarget
    infect 3 city 


placeCubes :: Int -> City -> Play Disease Bool
placeCubes amount city = do
    (overflow, consumed) <- zoom (infectionAt city) $ do
        i <- get
        let new = i + amount
        if new > 3
            then put 3 >> return (True, 3 - i)
            else put new >> return (False, amount)

    zoom reserve $ do
        r <- get
        let remain = r - consumed
        if remain < 0
            then lose "Disease reserve was insufficient"
            else put remain

    return overflow

removeFromCity :: Bool -> City -> Play Disease ()
removeFromCity all city = do
    spare <- zoom (infectionAt city) $ do
        i <- get
        case (i, all) of
            (0, _) -> return 0
            (_, True) -> put 0 >> return i
            (_, False) -> put (i-1) >> return 1
    reserve += spare
    

cleanCity :: Color -> Bool -> City -> Play Game ()
cleanCity color all city = zoom diseases $ do
    zoom (ix color) $ removeFromCity all city
    at color %= checkEradication


checkEradication :: Maybe Disease -> Maybe Disease
checkEradication Nothing = Nothing
checkEradication (Just d) | eradicated d = Nothing
                          | otherwise = Just d


propagate :: Int -> City -> StateT (Set City) (Play Disease) ()
propagate n city = do
        overflow <- lift (placeCubes n city)
        when overflow . once city $ 
                forM_ (city ^. neighbors) 
                    (propagate 1)  
        
infect :: Int -> City -> Play Game ()
infect n city = do
    let color = city ^. nativeColor
    chain <- length <$> zoom (diseases . ix color) (execStateT (propagate n city) empty)
    return ()
    zoom outbreaks $ do
        o <- get
        let o' = o + chain
        if o' > 8
            then lose "Exceeded number of maximum outbreaks. Global panic !"
            else put o' 
