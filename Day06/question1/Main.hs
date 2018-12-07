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
data Pixel = Pixel Int (Maybe Point) Int Int deriving (Show)

instance Eq Pixel where
  (Pixel _ _ x y) == (Pixel _ _ x' y') = (y,x) == (y',x')

instance Ord Pixel where
  (Pixel _ _ x y) `compare` (Pixel _ _ x' y') = (y,x) `compare` (y',x')

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
distance' (point:points) pixel
    | diff' <  diff = distance' points $ Pixel diff' (Just point) x' y'
    | diff' == diff = distance' points $ Pixel diff' Nothing x' y'
    | diff' >  diff = distance' points pixel
  where
    Point _ x y = point
    Pixel diff point' x' y' = pixel
    diff' = abs(x - x') + abs(y - y')

distance :: [Point] -> Int -> Int -> Pixel
distance (point:points) x y = distance' points $ Pixel diff (Just point) x y
  where
    Point _ x' y' = point
    diff = abs(x - x') + abs(y - y')


expand :: [Point] -> Int -> Int -> [Pixel]
expand points width height = map (\(x,y) -> distance points x y) coordinates
  where
    coordinates = [(x,y) | x <- [0..width], y <- [0..height]]


pack :: [Pixel] -> String
pack [] = ""
pack ((Pixel _ Nothing _ _):pixels) = '.':(pack pixels)
pack ((Pixel _ (Just (Point i _ _)) _ _):pixels) = (show i) ++ (pack pixels)

row :: [Pixel] -> Int -> Set.Set Point
row pixels n = row' pixels n Set.empty
  where
    row' :: [Pixel] -> Int -> Set.Set Point -> Set.Set Point
    row' [] n s = s
    row' ((Pixel _ Nothing _ _):pixels) n s = row' pixels n s
    row' ((Pixel _ (Just point)  _ y):pixels) n s
        | y == n    = row' pixels n $ Set.insert point s
        | otherwise = row' pixels n s

column :: [Pixel] -> Int -> Set.Set Point
column pixels n = column' pixels n Set.empty
  where
    column' :: [Pixel] -> Int -> Set.Set Point -> Set.Set Point
    column' [] n s = s
    column' ((Pixel _ Nothing _ _):pixels) n s = column' pixels n s
    column' ((Pixel _ (Just point)  x _):pixels) n s
        | x == n    = column' pixels n $ Set.insert point s
        | otherwise = column' pixels n s


solution :: [Pixel] -> Set.Set Point -> Map.Map Point Int
solution pixels s = solution' pixels s Map.empty
  where
    solution' :: [Pixel] -> Set.Set Point -> Map.Map Point Int -> Map.Map Point Int
    solution' [] _ acc = acc
    solution' ((Pixel _ Nothing _ _):pixels) s acc = solution' pixels s acc
    solution' ((Pixel _ (Just point) _ _):pixels) s acc
      | (Set.member point s) = solution' pixels s acc
      | otherwise = solution' pixels s $ Map.insertWith (+) point 1 acc


--
main :: IO ()
main = do
   content <- getContents

   let
    points = map (\(index, line) -> parser index line) $ zip [0..] $ lines content
    width = 1 + (maximumX points)
    height = 1 + (maximumX points)
    pixels = expand points width height
    outter' = Set.union (row pixels height) (column pixels width) :: Set.Set Point
    outter'' = Set.union (row pixels 0) (column pixels 0) :: Set.Set Point
    outter = Set.union outter' outter'' :: Set.Set Point

   putStrLn $ show $ maximum $ map (snd) $ Map.toList $ solution pixels outter
