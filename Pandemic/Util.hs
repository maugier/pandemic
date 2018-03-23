{-# LANGUAGE RankNTypes #-}

module Pandemic.Util where

import Control.Lens
import Control.Monad.Identity (runIdentity)
import Control.Monad.State
import Data.IORef
import Data.Functor ((<$>))
import Data.Maybe
import Data.Set


closure :: Ord k => k -> (k -> Set k) -> Set k
closure init next = closure' empty (singleton init) where
    closure' r a | a == empty  = r
                 | otherwise   = let r' = (union r a)
                                 in closure' r' (unions (next <$> elems a) \\ r')



once :: (Ord a, Monad m) => a -> StateT (Set a) m () -> StateT (Set a) m ()
once key action = do
    bl <- get
    (when . not) (key `member` bl) $ do
        modify (insert key)
        action

allOfThem :: (Bounded t, Enum t) => [t]
allOfThem = [minBound..maxBound]
