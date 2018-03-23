module Pandemic.Test where

import Control.Monad
import Data.Set
import Pandemic.Rules
import Pandemic.World
import Pandemic.Util

testCities :: City -> [(City, City)]

testCities root = do
    let allCities = closure _neighbors root
    city <- [root]
    neighbor <- toList (_neighbors city)
    guard . not $ city `member` _neighbors neighbor
    return (neighbor, city)

