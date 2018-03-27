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

move :: (Int,Int) -> Render ()
move (y,x) = tell (Sum y, Sum x)

moveY y = move (y,0)
moveX x = move (0,x)

drawAt :: (Int,Int) -> t -> Render t
drawAt (y,x) t = writer (t,(Sum y,Sum x)) 

centered :: Text -> Render Text
centered t = t <$ moveX (-((T.length t - 1) `div` 2))

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
drawLine [p] = "+" <$ move p
drawLine ((y,x):(r@((y',x'):_))) = (move (y,x) >> (return "+" <|> drawSegment (y'-y) (x'-x))) <|> drawLine r

drawSegment 0 0 = A.empty
drawSegment 0 x | x > 0  = moveX 1 >> return (T.replicate (x-1) "-")
                | x < 0  = moveX x >> drawSegment 0 (-x)
drawSegment y 0 | y > 0  = choice [1..(y-1)] >>= moveY >> return "|"
                | y < 0  = moveY y >> drawSegment (-y) 0


colorRender8 :: (Renderable t, Paddable t) => Render (Chunk t) -> [ByteString]
colorRender8 = concat . map (++[ pack "\n" ]) . map colorize . render where
    colorize = chunksToByteStrings toByteStringsColors8 

colorPrint8 :: Render (Chunk Text) -> IO ()
colorPrint8 = mapM_ BS.putStr . colorRender8


{-
 -  Koch's curve demo
 -
 -  fractal n mix = return x <|> (choice [(-n,0),(0,n),(n,0),(0,-n)] >>= move >> return (mix x) )
 -  colorPrint8 $ move (20,20) >> fractal 1 (fore red) "*" >>= fractal 3 (fore green) >>= fractal 9 (fore blue)
 -
 -}

class Display t where
    display :: t -> Render (Chunk Text)

instance Display City where
    display city = move loc >> fmap (color . chunk) (centered name) where
        loc   = city ^. coordinates
        color = city ^. (nativeColor . to diseaseColor)
        name  = city ^. cityName

instance Display Player where
    display player = return . color . chunk $ label where
        r = player ^. role
        color = roleColor r
        label = fromString (take 1 (show r))

instance Display Game where
    display g = (chunk <$> backdrop) <|> (curPlayer <$ move (26,1)) <|> counters <|> dPlayers <|> dDisease <|> dCities where
        dCities = choice (g ^. cities . to S.toList) >>= display
        dDisease = choice allOfThem >>= \color -> move (-1, fromEnum color - 2) >> fmap (diseaseColor color) (display (g ^. disease color))
        dPlayers = (choice . zip [0..]) (g ^.. players . traverse) >>= \(i,p) -> (move (p ^. location . coordinates) >> move (1,(i-2)) >> display p)
        counters = (move (21,88) >> fmap (fore green . chunk) (multiline (fromString ("Outbreaks: " ++ show (g ^. outbreaks) ++ "\nIntensity: " ++ show (g ^. intensity))))) 
        curPlayer = (g ^. currentPlayer . role . to roleColor) ("Current player: " <> fromString (g ^. currentPlayer . name))

instance Display Disease where
    display dis = choice (dis ^.. infection . to M.toList) >>= choice >>= \(c,i) -> move (c ^. coordinates) >> return (fromString (show i))

backdrop :: Render Text
backdrop = move (1,1) >> multiline "     \n                                                                                                                           \n<--San Francisco---Chicago-------Montreal---New York-------London-----Essen----St.Petersburg       Beijing---Seoul----Tokyo-------->\n    /     |        / |   \\          |      /      \\       /     \\    /   \\        |  \\                    \\    |     /     \\    \n<--+      |       /  |    \\         |     /        \\     /       \\  /     \\       |   \\                    \\   |    /       \\      \n          |      /   |     \\        |    /          \\   /         \\/       \\      |    \\                    \\  |   /         \\     \n      Los Angeles    | Atlanta--Washington          Madrid-------Paris-----Milan  |  Moscow                 Shangai---Taipei--Osaka\n       /       \\     |      |     /                /     \\      /             |   |   / |                         \\     |   \\      \n<-----+         \\    |      |    /                /       \\    /              |   |  /  |                          \\    |    \\\n                 \\   |      |   /                /         \\  /               |   | /   |                           \\   |     \\      \n                 Mexico-----Miami               /         Alger--------------Istanbul   Tehran---Dehli---Calcutta---Hong Kong--Manila---->\n                   | \\        |                /               \\            /   |      /  |     /  |  \\     |    \\     |          |   \\\n                   |  +---+   |               /                 \\       +--+    |    //   |    /   |   \\    |     \\    |          |    +\n                   |       \\  |              /                   \\     /        |   /     |   /    |    \\   |      \\   |          |    |\n                   |        Bogota          /                     Cairo-------Bagdad----Karachi--Mumbai--Chennai----Bangkok-----Saigon |\n                   | /------/ | \\          /                     /     \\     /          /                      \\       |       /  |    |\n                  Lima        |  \\        /                     /       \\   /          /                        \\      |      /   |    +\n                   |          |   \\      /      /-Lagos---Khartoum      Riyad---------+                          \\     |     /    |   /\n                   |          |   Sao Paulo----/   |      /   |                                                   +-Jakarta-+   Sydney--->\n                   |          |   /                |     /    |                                                                      \n                   |          |  /                 |    /     |                        Intensity: n\n                   |          | /                  |   /      |                        Outbreaks: n\n                   |     Buenos Aires           Kinshasa--Johannesburg\n                Santiago                                          \n                      \n     \n"
