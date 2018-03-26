{-# LANGUAGE FlexibleInstances #-}

module Pandemic.Map where

import Prelude
import Data.Monoid
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T 
import Control.Monad.Writer
import Control.Monad.List
import Pandemic.RangeMap
import Pandemic.Sliceable
import Rainbow
import Rainbow.Types

type Render = WriterT (Sum Int,Sum Int)  ([])

move :: Int -> Int -> Render ()
move y x = tell (Sum y, Sum x)

centered :: Text -> Render Text
centered t = move 0 (-(T.length t) `div` 2) >> return t

multiline :: Text -> Render Text
multiline t = WriterT [ (l,(Sum n, Sum 0)) | (n,l) <- zip [0..] $ T.lines t ]

render :: Paddable t => Render t -> [[t]]
render object = paddedScreen 0 lineBuffer where
    inputLines = [(y,[(x,r)]) | (r, (Sum y,Sum x)) <- runWriterT object ]
    lineBuffer = IM.toList . IM.map squashPad . IM.fromListWith (++) $ inputLines
    paddedScreen y [] = []
    paddedScreen y ((y',l):rest) = replicate (y'-y-1) [] ++ (l : paddedScreen y' rest)
