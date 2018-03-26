module Pandemic.World where

import Pandemic.Rules
import Data.Set (fromList)
import Data.Text (pack)

city :: String -> Color -> (Int,Int) -> [City] -> City
city name color coord neigh = City (pack name) color coord (fromList neigh)

sanFrancisco = city "San Francisco" Blue (3,10) [tokyo, manila, chicago, losAngeles]
chicago = city "Chicago" Blue (3,23) [sanFrancisco, losAngeles, mexico, atlanta, montreal]
atlanta = city "Atlanta" Blue (7,27) [chicago, miami, washington]
montreal = city "Montreal" Blue (3,37) [chicago, washington, newYork]
washington = city "Washington" Blue (7,37) [montreal, atlanta, miami, newYork]
newYork = city "New York" Blue (3,48) [montreal, washington, london, madrid]
london = city "London" Blue (3,62) [newYork, madrid, paris, essen]
madrid = city "Madrid" Blue (7,55) [newYork, london, saoPaulo, alger, paris]
paris = city "Paris" Blue (7,68) [london, madrid, alger, essen, milan]
essen = city "Essen" Blue (3,73) [london, paris, milan, stPetersburg]
milan = city "Milan" Blue (7,78) [essen, paris, istanbul]
stPetersburg = city "St.Petersburg" Blue (3,86) [essen, istanbul, moscow]

alger = city "Alger" Black (11,61) [madrid, paris, istanbul, cairo]
cairo = city "Cairo" Black (15,69) [alger, istanbul, bagdad, riyad]
istanbul = city "Istanbul" Black (11,81) [alger, milan, stPetersburg, moscow, bagdad, riyad]
moscow = city "Moscow" Black (7,88) [stPetersburg, istanbul, tehran]
bagdad = city "Bagdad" Black (15,81) [istanbul, cairo, riyad, karachi, tehran]
riyad = city "Riyad" Black (18,75) [cairo, bagdad, karachi]
tehran = city "Tehran" Black (11,91) [moscow, bagdad, karachi, delhi]
karachi = city "Karachi" Black (15,92) [bagdad, riyad, tehran, delhi, mumbai]
mumbai = city "Mumbai" Black (15,100) [karachi, delhi, chennai]
delhi = city "New Delhi" Black (11,100) [tehran, karachi, mumbai, chennai, calcutta]
chennai = city "Chennai" Black (15,109) [mumbai, delhi, calcutta, bangkok, jakarta]
calcutta = city "Calcutta" Black (11,109) [delhi, chennai, bangkok, hongkong]

bangkok = city "Bangkok" Red (15,120) [calcutta, chennai, hongkong, jakarta, saigon]
jakarta = city "Jakarta" Red (19,120) [chennai, sydney, saigon, bangkok]
sydney = city "Sydney" Red (19,132) [jakarta, manila, losAngeles]
manila = city "Manila" Red (11,132)[sydney, saigon, hongkong, taipei, sanFrancisco]
saigon = city "Saigon" Red (15,131) [jakarta, bangkok, manila, hongkong]
hongkong = city "Hong Kong" Red (11,121) [calcutta, bangkok, saigon, manila, taipei, shangai]
taipei = city "Taipei" Red (7,121) [manila, hongkong, shangai, osaka]
osaka = city "Osaka" Red (7,129) [tokyo, taipei]
shangai = city "Shangai" Red (7,112) [beijing, hongkong, taipei, seoul, tokyo]
beijing = city "Beijing" Red (3,103) [shangai, seoul]
seoul = city "Seoul" Red (3,112) [beijing, shangai, tokyo]
tokyo = city "Tokyo" Red (3,121) [seoul, shangai, osaka, sanFrancisco]

losAngeles = city "Los Angeles" Yellow (7,12) [sanFrancisco, sydney, mexico, chicago]
mexico = city "Mexico City" Yellow (11,20) [losAngeles, chicago, miami, bogota, lima]
miami = city "Miami" Yellow (11,31) [washington, atlanta, mexico, bogota]
bogota = city "Bogota" Yellow (15,31) [miami, mexico, lima, buenosAires, saoPaulo]
lima = city "Lima" Yellow (17,20) [mexico, bogota, santiago]
buenosAires = city "Buenos Aires" Yellow (23,31) [bogota, saoPaulo]
santiago = city "Santiago" Yellow (24,20) [lima]
saoPaulo = city "Sao Paulo" Yellow (19,39) [bogota, buenosAires, lagos, madrid]
lagos = city "Lagos" Yellow (18,53) [saoPaulo, kinshasa, khartoum]
kinshasa = city "Kinshasa" Yellow (23,52) [lagos, johannesburg, khartoum]
johannesburg = city "Johannesburg" Yellow (23,64) [kinshasa, khartoum]
khartoum = city "Khartoum" Yellow (18,62) [lagos, kinshasa, johannesburg, cairo]



