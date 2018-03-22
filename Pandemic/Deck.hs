{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, LambdaCase #-}

module Pandemic.Deck where

import Control.Lens
import Control.Lens.TH
import Control.Lens.Zoom
import Control.Monad.State
import Control.Monad.Random
import Data.Functor ((<$>))
import System.Random.Shuffle (shuffleM)

data Deck a = Deck {
    _drawPile :: [a],
    _discardPile :: [a]
} deriving Show

makeLenses ''Deck

emptyDeck = Deck [] []

pop :: State [a] (Maybe a)
pop = state $ \case (x:xs) -> (Just x, xs)
                    []     -> (Nothing, [])

push :: a -> State [a] ()
push x = state $ \xs -> ((), x:xs)

draw :: State (Deck a) (Maybe a)
--draw = state $ \(Deck (x:xs) ys) -> (x, Deck xs ys)
draw = zoom drawPile $ pop

drawLast :: State (Deck a) (Maybe a)
drawLast = zoom (drawPile . reversed) $ pop

discard :: a -> State (Deck a) ()
--discard a = state $ \(Deck xs ys) -> ((), Deck xs (a:ys))
discard a = zoom discardPile $ push a

shuffle :: (MonadState (Deck a) m, MonadRandom m) => m ()
shuffle = do
    Deck xs ys <- get
    xs' <- shuffleM xs
    put (Deck xs' ys)

remainingCards :: State (Deck a) Int
remainingCards = length <$> use drawPile
