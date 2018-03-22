module Pandemic.Util where

import Data.IORef
import Control.Monad.Identity (runIdentity)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.Set


closure :: Ord k => k -> (k -> Set k) -> Set k
closure init next = closure' empty (singleton init) where
    closure' r a | a == empty  = r
                 | otherwise   = let r' = (union r a)
                                 in closure' r' (unions (next <$> elems a) \\ r')


