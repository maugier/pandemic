{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Pandemic.Sliceable
    ---( Sliceable,
    ---spliceAt
    ---)
    where

import Data.Text (Text)
import qualified Data.Text as T

class Sliceable t where
    sTake :: Int -> t -> t
    sDrop :: Int -> t -> t
    sLength :: t -> Int

class Sliceable t => Paddable t where
    sPad :: Int -> t

instance Sliceable Text where
    sTake = T.take
    sDrop = T.drop
    sLength = T.length

instance Paddable Text where
    sPad n = T.replicate n " "

instance Sliceable [t] where
    sTake = Prelude.take
    sDrop = Prelude.drop
    sLength = Prelude.length

instance Monoid t => Paddable [t] where
    sPad = flip Prelude.replicate mempty

