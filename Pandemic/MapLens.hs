{-# LANGUAGE TypeFamilies #-}

module Pandemic.MapLens where

import Control.Applicative
import Control.Lens.At
import Data.Map as Map
        
type instance Index (Map k a) = k
type instance IxValue (Map k a) = a
instance Ord k => Ixed (Map k a) where
    ix k f m = case Map.lookup k m of
        Just v  -> (\v' -> Map.insert k v' m) <$> f v
        Nothing -> pure m
    {-# INLINE ix #-}

instance Ord k => At (Map k a) where
    at k f = Map.alterF f k
    {-# INLINE at #-}
