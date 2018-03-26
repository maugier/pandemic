{-# LANGUAGE FlexibleInstances #-}

module Pandemic.RangeMap
    ( RangeMap
    , empty
    , singleton
    , fromList
    , insert
    , pad
    , squash
    , squashPad
    , toList
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import Data.Ord
import qualified Data.Text as T 
import Prelude hiding (span)
import Pandemic.Sliceable

data Span = Span Int Int
    deriving (Show)
span :: (Int,Int) -> Span
span (a,b) | a > b = Span b a
           | otherwise = Span a b

instance Ord Span where
    compare (Span a b) (Span a' b') = case (b <= a', a >= b') of
        (True,_) -> LT
        (_,True) -> GT
        _        -> EQ

instance Eq Span where
    a == b = (compare a b == EQ)

newtype RangeMap r = RangeMap (Map Span (Span,r))

empty :: RangeMap r
empty = RangeMap (M.empty)

singleton :: Sliceable r => Int -> r -> RangeMap r
singleton offset r = insert offset r empty

insert :: Sliceable r => Int -> r -> RangeMap r -> RangeMap r
insert offset r = insert' (Span offset (offset + sLength r), r)

insert' :: Sliceable r => (Span,r) -> RangeMap r -> RangeMap r
insert' e@(s,_) (RangeMap m) = case M.lookup s m of
    Nothing -> RangeMap (M.insert s e m)
    Just e'@(s',_) -> L.foldr (insert') (RangeMap (M.delete s' m)) (breakApart e e')


breakApart :: Sliceable r => (Span,r) -> (Span,r) -> [(Span,r)]
breakApart x@((Span a b),r) y@((Span a' b'),r') = case (a <= a', b >= b') of
    (True,True)   -> [x]
    (True,False)  -> [x, (Span b b', sDrop (b-a') r')]
    (False,True)  -> [x, (Span a' a, sTake (a-a') r')]
    (False,False) -> [x, (Span a' a, sTake (a-a') r'), (Span b b', sDrop (b-a') r')]

fromList :: Sliceable r => [(Int,r)] -> RangeMap r
fromList = L.foldr (uncurry insert) empty

toList :: RangeMap r -> [(Int,r)]
toList (RangeMap m) = [(a,r) | (Span a b, r) <- M.elems m ]

squash :: Sliceable r => [(Int,r)] -> [(Int,r)]
squash = toList . fromList

pad :: Paddable r => [(Int,r)] -> [r]
pad = pad' 0 . squash where
    pad' _ [] = []
    pad' n ((o,x):xs) = let rest = x : pad' (o + sLength x) xs in if (o-n) > 0 then sPad (o-n) : rest else rest

squashPad :: Paddable r => [(Int,r)] -> [r]
squashPad = pad . squash

instance Show r => Show (RangeMap r) where
    show rm = "fromList " ++ show (toList rm)
