{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines)

import Data.Char
import Data.List (sort, sortBy, maximumBy)
import Control.Applicative
import qualified Data.Map.Strict as Map

data Event = ShiftStarts Integer | FallsAsleep | WakesUp deriving (Show)
data Log = Log Integer Integer Integer Integer Integer Event deriving (Show)

parseShiftStarts :: Parser Event
parseShiftStarts = do
  string "Guard"
  space
  char '#'
  guard <- decimal
  space
  string "begins"
  space
  string "shift"
  return $ ShiftStarts guard

parseWakesUp :: Parser Event
parseWakesUp = do
  string "wakes"
  space
  string "up"
  return $ WakesUp

parseFallsAsleep :: Parser Event
parseFallsAsleep = do
  string "falls"
  space
  string "asleep"
  return $ FallsAsleep

parseEvent :: Parser Event
parseEvent = parseFallsAsleep <|> parseWakesUp <|> parseShiftStarts

parseLog :: Parser Log
parseLog = do
  char '['
  year <- decimal
  char '-'
  month <- decimal
  char '-'
  day <- decimal
  space
  hour <- decimal
  char ':'
  minute <- decimal
  char ']'
  space
  event <- parseEvent
  return $ Log year month day hour minute event

parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseLog input

--
pack :: [Log] -> Map.Map Integer [Integer]
pack ((Log _ _ _ _ _ (ShiftStarts g)):cs) = pack' cs g $ Map.empty
  where
    pack' :: [Log] -> Integer -> Map.Map Integer [Integer] -> Map.Map Integer [Integer]
    pack' []                                                                 g m = m
    pack' ((Log _ _ _ _ _    (ShiftStarts g'))                          :cs) g m = pack' cs g' m
    pack' ((Log _ _ _ _ start FallsAsleep    ):(Log _ _ _ _ end WakesUp):cs) g m = pack' cs g $ Map.insertWith (++) g [start..end-1] m

--
calculate :: Map.Map Integer [Integer] -> [(Integer, Integer, Integer)]
calculate m = map (calculate') $ Map.toList m
  where
    calculate' :: (Integer, [Integer]) -> (Integer, Integer, Integer)
    calculate' (guard, minutes) = (guard, snd popular, fst popular)
      where
        popular = maximumBy (\a b -> compare (snd a) (snd b)) $ Map.toList $ foldl (\acc a -> Map.insertWith (+) a 1 acc) (Map.empty) minutes

solution :: [Log] -> Integer
solution cs = guard * minute
  where
    (guard,_,minute) = maximumBy (\(a,b,c) (a',b',c') -> compare b b') $ calculate $ pack cs

main :: IO ()
main = do
   content <- getContents
   let
     toTuple (Log y month d h minute _) = (y,month,d,h,minute)
   putStrLn $ show $ solution $ sortBy (\a b -> compare (toTuple a) (toTuple b)) $ map (parser) $ lines content
