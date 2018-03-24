{-# LANGUAGE RankNTypes #-}

module Pandemic.Util where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.Identity (runIdentity)
import Control.Monad.State
import Data.IORef
import Data.List (transpose)
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Data.Set


closure :: Ord k => (k -> Set k) -> k -> Set k
closure next init = closure' empty (singleton init) where
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

group :: Int -> [a] -> [[a]]
group n = Prelude.map (Prelude.take n) . takeWhile (not . Prelude.null) . iterate (Prelude.drop n)

sparse :: Int -> [a] -> [[a]]
sparse = (transpose .) . group

data CycleZip a = CycleZip [a] a [a] 
    deriving Show

singleCZ :: a -> CycleZip a
singleCZ a = CycleZip [] a []

cycleCZ :: [a] -> CycleZip a
cycleCZ [] = error "Cannot create empty CZ"
cycleCZ (x:xs) = CycleZip [] x xs

next :: CycleZip a -> CycleZip a 
next as@(CycleZip [] a []) = as
next (CycleZip ps a (n:ns)) = CycleZip (a:ps) n ns
next as@(CycleZip ps a []) = next (CycleZip [] a (reverse ps))

current :: Simple Lens (CycleZip a) a
current f (CycleZip ps a ns) = (\a' -> CycleZip ps a' ns) <$> f a

instance Functor CycleZip where
    fmap f (CycleZip ps a ns) = CycleZip (fmap f ps) (f a) (fmap f ps)

instance Foldable CycleZip where
    foldMap f (CycleZip ps a ns) = f a <> foldMap f ns <> getDual (foldMap (Dual . f) ps)

instance Traversable CycleZip where
    traverse f (CycleZip ps a ns) = (\a' ns' ps' -> CycleZip (reverse ps') a' ns') <$> f a <*> traverse f ns <*> traverse f (reverse ps)


removing :: (b -> Bool) -> Traversal (Maybe a) (Maybe b) a b
removing pred f Nothing = pure Nothing
removing pred f (Just x) = (\x' -> if pred x' then Nothing else Just x') <$> f x

swapState :: Functor f => StateT s1 (StateT s2 f) a -> StateT s2 (StateT s1 f) a
swapState m = StateT (\s2 -> StateT (\s1 -> fmap swap (runStateT (runStateT m s1) s2))) where
    swap ((r,s1),s2) = ((r,s2),s1)

hoist' f = swapState . f . swapState

