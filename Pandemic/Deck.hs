{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, LambdaCase #-}

module Pandemic.Deck where

import Control.Lens
import Control.Lens.TH
import Control.Lens.Zoom
import Control.Monad.State
import Control.Monad.Random
import Data.Functor ((<$>))
import Pandemic.Game
import System.Random.Shuffle (shuffleM)

data Deck a = Deck {
    _drawPile :: [a],
    _discardPile :: [a]
} deriving Show

makeLenses ''Deck

emptyDeck = Deck [] []

deck :: [a] -> Deck a
deck xs = Deck xs []

pop :: Play [a] a
pop = do
    pile <- get
    case pile of
        [] -> lose "Drawing from empty deck"
        (x:xs) -> put xs >> return x

push :: a -> Play [a] ()
push a = modify (a:) 

draw :: Play (Deck a) a
draw = zoom drawPile pop

drawLast :: Play (Deck a) a
drawLast = zoom (drawPile . reversed) $ pop

discard :: a -> Play (Deck a) ()
discard a = zoom discardPile $ push a

drawAndDiscard :: Play (Deck a) a
drawAndDiscard = do
    card <- draw
    discard card
    return card

shuffle :: Play [a] ()
shuffle = get >>= shuffleM >>= put

resetDiscard :: Deck a -> Deck a
resetDiscard (Deck xs ys) = Deck (ys++xs) []

remainingCards :: Play (Deck a) Int
remainingCards = length <$> use drawPile
