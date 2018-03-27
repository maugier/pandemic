{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Pandemic.Map where

import Control.Applicative as A
import Control.Monad.Writer
import Control.Monad.List
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Map as M
import Data.Monoid
import Data.Set as S (toList)
import Data.String
import qualified Data.IntMap as IM
import Data.List (intersperse)
import qualified Data.Set
import Data.Text (Text)
import qualified Data.Text as T 
import Data.Text.IO as T (putStrLn)
import Lens.Simple
import Pandemic.RangeMap as RM
import Pandemic.Sliceable
import Pandemic.Pretty
import Pandemic.Rules
import Pandemic.Util
import Rainbow
import Rainbow.Types (yarn)

type Render = WriterT (Sum Int,Sum Int)  ([])

move :: Int -> Int -> Render ()
move y x = tell (Sum y, Sum x)

centered :: Text -> Render Text
centered t = move 0 (-(T.length t) `div` 2) >> return t

multiline :: Text -> Render Text
multiline t = WriterT [ (l,(Sum n, Sum 0)) | (n,l) <- zip [0..] $ T.lines t ]

choice :: [t] -> Render t
choice = foldr (<|>) A.empty . map return

render :: Paddable t => Render t -> [[t]]
render object = paddedScreen 0 lineBuffer where
    inputLines = [(y,[(x,r)]) | (r, (Sum y,Sum x)) <- runWriterT object ]
    lineBuffer = IM.toList . IM.map squashPad . IM.fromListWith (++) $ inputLines
    paddedScreen y [] = []
    paddedScreen y ((y',l):rest) = replicate (y'-y-1) [] ++ (l : paddedScreen y' rest)


bwRender :: Render Text -> Text
bwRender = T.unlines . map T.concat . render 

bwPrint :: Render Text -> IO ()
bwPrint = T.putStrLn . bwRender

instance Sliceable a => Sliceable (Chunk a) where
    sTake = over yarn . sTake
    sDrop = over yarn . sDrop
    sLength = sLength . view yarn 

instance Paddable a => Paddable (Chunk a) where
    sPad = chunk . sPad 


drawLine :: [(Int,Int)] -> Render Text
drawLine [] = A.empty
drawLine [(y,x)] = move y x >> return "+"
drawLine ((y,x):(r@((y',x'):_))) = (move y x >> (return "+" <|> drawSegment (y'-y) (x'-x))) <|> drawLine r

drawSegment 0 0 = A.empty
drawSegment 0 x | x > 0  = move 0 1 >> return (T.replicate (x-1) "-")
                | x < 0  = move 0 x >> drawSegment 0 (-x)
drawSegment y 0 | y > 0  = choice [1..(y-1)] >>= flip move 0 >> return "|"
                | y < 0  = move y 0 >> drawSegment (-y) 0


colorRender8 :: (Renderable t, Paddable t) => Render (Chunk t) -> [ByteString]
colorRender8 = concat . map (++[ pack "\n" ]) . map colorize . render where
    colorize = chunksToByteStrings toByteStringsColors8 

colorPrint8 :: Render (Chunk Text) -> IO ()
colorPrint8 = mapM_ BS.putStr . colorRender8


{-
 -  Koch's curve demo
 -
 -  fractal n mix = return x <|> (choice [(-n,0),(0,n),(n,0),(0,-n)] >>= uncurry move >> return (mix x) )
 -  colorPrint8 $ move 20 20 >> fractal 1 (fore red) "*" >>= fractal 3 (fore green) >>= fractal 9 (fore blue)
 -
 -}

class Display t where
    display :: t -> Render (Chunk Text)

instance Display City where
    display city = uncurry move loc >> fmap (color . chunk) (centered name) where
        loc   = city ^. coordinates
        color = city ^. (nativeColor . to diseaseColor)
        name  = city ^. cityName

instance Display Player where
    display player = return . color . chunk $ label where
        r = player ^. role
        color = roleColor r
        label = fromString (take 1 (show r))

instance Display Game where
    display g = dPlayers <|> dDisease <|> dCities where
        dCities = choice (g ^. cities . to S.toList) >>= display
        dDisease = choice allOfThem >>= \color -> move (-1) (fromEnum color - 2) >> fmap (diseaseColor color) (display (g ^. disease color))
        dPlayers = (choice . zip [0..]) (g ^.. players . traverse) >>= \(i,p) -> uncurry move (p ^. location . coordinates) >> move 1 (i-2) >> display p

instance Display Disease where
    display dis = choice (dis ^.. infection . to M.toList) >>= choice >>= \(c,i) -> uncurry move (c ^. coordinates) >> return (fromString (show i))
