module Pandemic.World where

import Pandemic.Rules
import Data.Set (fromList)

city :: String -> Color -> [City] -> City
city name color neigh = City name color (fromList neigh)

sanFrancisco = city "San Francisco" Blue [tokyo, manila, chicago, losAngeles]
chicago = city "Chicago" Blue [sanFrancisco, losAngeles, mexico, atlanta, montreal]
atlanta = city "Atlanta" Blue [chicago, miami, washington]
montreal = city "Montreal" Blue [chicago, washington, newYork]
washington = city "Washington" Blue [montreal, atlanta, miami, newYork]
newYork = city "New York" Blue [montreal, washington, london, madrid]
london = city "London" Blue [newYork, madrid, paris, essen]
madrid = city "Madrid" Blue [newYork, london, saoPaulo, alger, paris]
paris = city "Paris" Blue [london, madrid, alger, essen, milan]
essen = city "Essen" Blue [london, paris, milan, stPetersburg]
milan = city "Milan" Blue [essen, paris, istanbul]
stPetersburg = city "St Petersburg" Blue [essen, istanbul, moscow]

alger = city "Alger" Black [madrid, paris, istanbul, cairo]
cairo = city "Cairo" Black [alger, istanbul, bagdad, riyad]
istanbul = city "Istanbul" Black [alger, milan, stPetersburg, moscow, bagdad, riyad]
moscow = city "Moscow" Black [stPetersburg, istanbul, tehran]
bagdad = city "Bagdad" Black [istanbul, cairo, riyad, karachi, tehran]
riyad = city "Riyad" Black [cairo, bagdad, karachi]
tehran = city "Tehran" Black [moscow, bagdad, karachi, delhi]
karachi = city "Karachi" Black [bagdad, riyad, tehran, delhi, mumbai]
mumbai = city "Mumbai" Black [karachi, delhi, chennai]
delhi = city "New Delhi" Black [tehran, karachi, mumbai, chennai, calcutta]
chennai = city "Chennai" Black [mumbai, delhi, calcutta, bangkok, jakarta]
calcutta = city "Calcutta" Black [delhi, chennai, bangkok, hongkong]

bangkok = city "Bangkok" Red [calcutta, chennai, hongkong, jakarta, saigon]
jakarta = city "Jakarta" Red [chennai, sydney, saigon, bangkok]
sydney = city "Sydney" Red [jakarta, manila, losAngeles]
manila = city "Manila" Red [sydney, saigon, hongkong, taipei, sanFrancisco]
saigon = city "Saigon" Red [jakarta, bangkok, manila, hongkong]
hongkong = city "Hong Kong" Red [calcutta, bangkok, saigon, manila, taipei, shangai]
taipei = city "Taipei" Red [manila, hongkong, shangai, osaka]
osaka = city "Osaka" Red [tokyo, taipei]
shangai = city "Shangai" Red [beijing, hongkong, taipei, seoul, tokyo]
beijing = city "Beijing" Red [shangai, seoul]
seoul = city "Seoul" Red [beijing, shangai, tokyo]
tokyo = city "Tokyo" Red [seoul, shangai, osaka, sanFrancisco]

losAngeles = city "Los Angeles" Yellow [sanFrancisco, sydney, mexico, chicago]
mexico = city "Mexico City" Yellow [losAngeles, chicago, miami, bogota, lima]
miami = city "Miami" Yellow [washington, atlanta, mexico, bogota]
bogota = city "Bogota" Yellow [miami, mexico, lima, buenosAires, saoPaulo]
lima = city "Lima" Yellow [mexico, bogota, santiago]
buenosAires = city "Buenos Aires" Yellow [bogota, saoPaulo]
santiago = city "Santiago" Yellow [lima]
saoPaulo = city "São Paulo" Yellow [bogota, buenosAires, lagos, madrid]
lagos = city "Lagos" Yellow [saoPaulo, kinshasa, khartoum]
kinshasa = city "Kinshasa" Yellow [lagos, johannesburg, khartoum]
johannesburg = city "Johannesburg" Yellow [kinshasa, khartoum]
khartoum = city "Khartoum" Yellow [lagos, kinshasa, johannesburg, cairo]



