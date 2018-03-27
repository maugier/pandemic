{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pandemic.Pretty where

import Control.Lens
import Control.Lens.Fold
import Data.ByteString as BS (putStr)
import Data.String
import Data.Text (Text, pack)
import Pandemic.Rules as P
import Rainbow as R

diseaseColor :: P.Color -> (Chunk a -> Chunk a)
diseaseColor P.Red = bold . fore R.red
diseaseColor P.Blue = bold . fore R.blue
diseaseColor P.Black = bold . fore R.black
diseaseColor P.Yellow = bold . fore R.yellow

roleColor :: Role -> (Chunk a -> Chunk a)
roleColor Scientist = bold . fore (R.white)
roleColor Researcher = bold . fore (R.black)
roleColor Operations = bold . fore (R.green)
roleColor Dispatcher = bold . fore (R.magenta)
roleColor EmergencyPlanner = fore (R.cyan)
roleColor QuarantinePlanner = fore (R.green)
roleColor Medic = bold . fore (R.yellow)

instance IsString t => IsString (Chunk t) where
    fromString = chunk . fromString

class Pretty t where
    pretty :: t -> [Chunk Text]

instance Pretty Text where
    pretty t = [chunk t]

cityColor :: City -> (Chunk a -> Chunk a)
cityColor = diseaseColor . (^. nativeColor)

instance Pretty P.Color where
    pretty color = [diseaseColor color $ chunk (pack (show color))]

instance Pretty City where
    pretty city = [cityColor city . chunk . (^. cityName) $ city]

instance Pretty Role where
    pretty role = [roleColor role . chunk . pack . show $ role]

instance Pretty Player where
    pretty player  = [roleColor (player ^. role) . chunk . pack . (++ ("[" ++ show (player ^. cards . to length) ++ " cards]")) . (^. name) $ player]

instance Pretty InfectionCard where
    pretty (InfectionCard city) = pretty city

instance Pretty PlayerCard where
    pretty EpidemicCard = [fore R.green $ chunk "[EPIDEMIC]"]
    pretty (HandCard hand) = pretty hand

instance Pretty HandCard where
    pretty (EventCard event) = [chunk "[", fore R.yellow $ chunk (pack (show event)), chunk "]"]
    pretty (CityCard city) = [chunk "["] ++ pretty city ++ [chunk "]"]

instance Pretty (P.Color, Disease) where
    pretty (color, disease) = pretty color ++ [" disease:"]
                                           ++ (disease ^. infection . 
                                               ifolded . withIndex . 
                                               to (\(city,amount) -> pretty city ++ [": ", fromString (show amount), " "]))
                                           ++ ["\n"]


instance Pretty Game where
    pretty game = [
        chunk "----------------\n",
        fore green $ chunk (pack ("Outbreaks:" ++ show (game ^. outbreaks))),
        fore green $ chunk (pack (", Intensity: " ++ show (game ^. intensity))),
        chunk "\n"]
        ++ (game ^. diseases . ifolded . withIndex . to pretty)
        ++ (game ^. players . traverse . to pretty)
    
pPrint :: Pretty t => t -> IO ()
pPrint = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors8 . pretty

