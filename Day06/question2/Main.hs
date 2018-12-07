{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (sort, zip, zipWith, maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines)

import Data.Char
import qualified Data.Set as Set
import Data.List (sort, sortBy, maximumBy)
import Data.List.Split
import Control.Applicative
import qualified Data.Map.Strict as Map


data Point = Point Int Int Int deriving (Show)
data Pixel = Pixel Int Int Int deriving (Show)

instance Eq Pixel where
  (Pixel _ x y) == (Pixel _ x' y') = (y,x) == (y',x')

instance Ord Pixel where
  (Pixel _ x y) `compare` (Pixel _ x' y') = (y,x) `compare` (y',x')

instance Eq Point where
  (Point _ x y) == (Point _ x' y') = (x,y) == (x',y')

instance Ord Point where
  (Point _ x y) `compare` (Point _ x' y') = (x,y) `compare` (x',y')


parsePoint :: Int -> Parser Point
parsePoint index = do
  x <- decimal
  char ','
  space
  y <- decimal
  return $ Point index x y

parser :: Int -> ByteString -> Point
parser index input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly (parsePoint index) input


maximumY :: [Point] -> Int
maximumY ps = maximum $ map (\(Point _ _ y) -> y) ps

maximumX :: [Point] -> Int
maximumX ps = maximum $ map (\(Point _ x _) -> x) ps


distance' :: [Point] -> Pixel -> Pixel
distance' []     pixel         = pixel
distance' (point:points) pixel = distance' points $ Pixel (diff' + diff) x' y'
  where
    Point _ x y = point
    Pixel diff x' y' = pixel
    diff' = abs(x - x') + abs(y - y')

distance :: [Point] -> Int -> Int -> Pixel
distance (point:points) x y = distance' points $ Pixel diff x y
  where
    Point _ x' y' = point
    diff = abs(x - x') + abs(y - y')


expand :: [Point] -> Int -> Int -> [Pixel]
expand points width height = map (\(x,y) -> distance points x y) coordinates
  where
    coordinates = [(x,y) | x <- [0..width], y <- [0..height]]


solution :: [Pixel] -> Int
solution pixels = solution' pixels 0
  where
    solution' :: [Pixel] -> Int -> Int
    solution' [] acc = acc
    solution' ((Pixel d _ _):pixels) acc
      | d < 10000 = solution' pixels (acc+1)
      | otherwise = solution' pixels acc


--
main :: IO ()
main = do
   content <- getContents

   let
    points = map (\(index, line) -> parser index line) $ zip [0..] $ lines content
    width = 1 + (maximumX points)
    height = 1 + (maximumX points)
    pixels = expand points width height

   putStrLn . show $ solution pixels
