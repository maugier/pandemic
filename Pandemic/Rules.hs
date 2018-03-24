{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, LambdaCase #-}

module Pandemic.Rules where

import Control.Lens
import Control.Lens.TH
import Control.Lens.At
import Control.Monad.State
import Control.Monad.Random hiding (next)
import Data.Function (on)
import qualified Data.Map as M
import Data.Set (Set, member, insert, empty, singleton, elems, delete)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Pandemic.Deck
import Pandemic.Game
import Pandemic.MapLens
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

isEradicated :: Disease -> Bool
isEradicated (Disease True _ m) = M.null m
isEradicated _ = False

emptyDisease = Disease True 24 M.empty

data Event = Airlift
           | PublicFunding
           | Prevision
           | QuietNight
           | Resilience
    deriving (Eq,Ord,Show,Enum,Bounded)

data HandCard = CityCard City
              | EventCard Event
    deriving (Eq,Ord,Show)

makePrisms ''HandCard

data PlayerCard = HandCard HandCard
                | EpidemicCard
    deriving (Eq,Ord,Show)

eventCards = map EventCard allOfThem

newtype InfectionCard = InfectionCard City
    deriving (Eq,Ord,Show)

data Player = Player {
    _name :: String,
    _role :: Role,
    _cards :: Set HandCard,
    _location :: City
} deriving (Show)

instance Ord Player where
    compare = comparing _name

instance Eq Player where
    (==) = (==) `on` _name

makeLenses ''Player

type Players = CycleZip Player


data Game = Game {
    _home :: City,
    _players :: Players,
    _actions :: Int,
    _cities :: Set City,
    _diseases :: M.Map Color Disease,
    _centers :: Set City,
    _epidemics :: Int,
    _outbreaks :: Int,
    _infectionDeck :: Deck InfectionCard,
    _playerDeck :: Deck PlayerCard
} deriving (Show)

makeLenses ''Game

data Difficulty = Tutorial
                | Normal
                | Epic

totalEpidemics :: Difficulty -> Int
totalEpidemics Tutorial = 4
totalEpidemics Normal = 5
totalEpidemics Epic = 6

disease :: Color -> Simple Lens Game Disease
disease color = diseases . at color . anon emptyDisease isEradicated

activeDisease :: Color -> Simple Traversal Game Disease
activeDisease color = diseases . at color . removing isEradicated

eradicated :: Color -> Getter Game Bool
eradicated color = diseases . at color . to isNothing

currentPlayer :: Simple Lens Game Player
currentPlayer = players . current

give :: HandCard -> Play Game ()
give card = currentPlayer . cards %= insert card

newGame :: City -> [(String,Role)] -> Game
newGame home players = Game {
                        _home = home,
                        _players = cycleCZ [Player name role empty home | (name,role) <- players],
                        _actions = 4,
                        _cities = closure _neighbors home,
                        _diseases = M.fromList [(color, cured .~ False $ emptyDisease) | color <- allOfThem ],
                        _centers = singleton home,
                        _epidemics = 0,
                        _outbreaks = 0,
                        _infectionDeck = emptyDeck,
                        _playerDeck = emptyDeck
               }

infectionTarget :: Play Game City
infectionTarget = do
    InfectionCard city <- zoom infectionDeck drawAndDiscard
    return city

initialInfection :: Play Game ()
initialInfection = forM_ [3,2,1] $ \n -> replicateM_ 3 $ do
    infectionTarget >>= infect n

doInfection :: Play Game ()
doInfection = do
    i <- use intensity
    replicateM_ i $ infectionTarget >>= infect 1

prepareInfectionDeck :: Play Game ()
prepareInfectionDeck = do
    cities <- use (cities . to elems)
    zoom infectionDeck $ do
        put . deck . map InfectionCard $ cities
        zoom drawPile shuffle


setupGame :: Difficulty -> Play Game ()
setupGame difficulty = do
    prepareInfectionDeck
    initialInfection
    setupPlayerDeck difficulty


cardsPerPlayer :: Game -> Int
cardsPerPlayer game =
    case game ^. players . to length of
        2 -> 4
        3 -> 3
        4 -> 2
        _ -> error "Unsopported player number"
        
drawCard :: Play Game ()
drawCard = do
    card <- zoom playerDeck draw
    case card of
        EpidemicCard -> epidemic
        HandCard hand -> give hand

useAction :: Play Game ()
useAction = zoom actions $ do
    a <- get
    if a > 0
        then put (a-1)
        else block "You are out of actions for this turn"

useCard :: HandCard -> Play Game ()
useCard card = zoom (currentPlayer . cards) $ do
        hand <- get
        if card `elem` hand
            then modify $ delete card
            else block "You do not have that card"


dealPlayerHands :: StateT [HandCard] (Play Game) ()
dealPlayerHands = do
    count <- lift . use $ to cardsPerPlayer
    hoist' (zoom (players . traverse)) $ replicateM_ count dealOneCard

        
dealOneCard :: StateT [HandCard] (Play Player) ()
dealOneCard = do
    pile <- get
    case pile of
        []          -> lift . block $ "Not enough city cards for all players"
        (card:rest) -> put rest >> lift (cards %= insert card)

setupPlayerDeck :: Difficulty -> Play Game ()
setupPlayerDeck difficulty  = do
    cityCards <- (map CityCard . elems) <$> use cities
    cards <- shuffleM (eventCards ++ cityCards)
    rest <- execStateT dealPlayerHands cards
    let n = totalEpidemics difficulty
    let packets = ((EpidemicCard:) . (HandCard <$>)) <$> (sparse n rest)
    shuffled <- mapM shuffleM packets
    playerDeck .= deck (concat shuffled)


intensity :: Getter Game Int
intensity = epidemics . to intensity' where
    intensity' n | n < 3 = 2
                 | n < 5 = 3
                 | otherwise = 4


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
            (0, _) -> block "This city is not infected"
            (_, True) -> put 0 >> return i
            (_, False) -> put (i-1) >> return 1
    reserve += spare
    

cleanCity :: Color -> Bool -> City -> Play Game ()
cleanCity color all city = zoom (activeDisease color) $ removeFromCity all city

buildCenter :: City -> Play Game ()
buildCenter city = centers %= insert city


endTurn :: Play Game ()
endTurn = do
    drawCard
    drawCard
    doInfection
    players %= next
    actions .= 4


    

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
    zoom outbreaks $ do
        o <- get
        let o' = o + chain
        if o' > 8
            then lose "Exceeded number of maximum outbreaks. Global panic !"
            else put o' 

inRangeOf :: City -> City -> Play Game Bool
inRangeOf a b = if b `member` _neighbors a
        then return True
        else do
            cs <- use centers
            return $ (a `member` cs) && (b `member` cs)

autocure :: City -> Play Game ()
autocure city = forM_ allOfThem $ \color -> zoom (disease color) $ do
    c <- use cured
    when c $ removeFromCity True city
        
    
