{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (putStr, putStrLn, minimum, filter, unwords, init, sort, zip, zipWith, maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines, init)
import Data.Maybe

import Data.Char
import qualified Data.SortedList as SortedList
import qualified Data.Set as Set
import Data.List (sort, sortBy, maximumBy)
import Data.List.Split
import Control.Applicative
import qualified Data.Map.Strict as Map


data MovingVector = MovingVector Int Int Int Int deriving (Show)


parseSpaceHelper :: Parser Int
parseSpaceHelper = do
  space
  decimal

parseDecimalHelper :: Parser Int
parseDecimalHelper = do
  decimal

parseNegativeHelper :: Parser Int
parseNegativeHelper = do
  char '-'
  n <- decimal
  return $ -n

parseHelper :: Parser Int
parseHelper = parseDecimalHelper <|> parseSpaceHelper <|> parseNegativeHelper

parseMovingVector :: Parser MovingVector
parseMovingVector = do
  string "position=<"
  position_x <- parseHelper
  char ','
  space
  position_y <- parseHelper
  string "> velocity=<"
  velocity_x <- parseHelper
  char ','
  space
  velocity_y <- parseHelper
  char '>'
  return $ MovingVector position_x position_y velocity_x velocity_y

parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseMovingVector input


--
countConnected :: [MovingVector] -> Int
countConnected cs = countConnected' cs $ Map.insertWith (+) 0 0 Map.empty
  where
    countConnected' :: [MovingVector] -> Map.Map Int Int -> Int
    countConnected' [] m = maximum $ map (snd) $ Map.toList m
    countConnected' ((MovingVector position_x position_y _ _):cs) m = countConnected' cs $ Map.insertWith (+) position_y 1 m

toX :: MovingVector -> Int
toX (MovingVector position_x _ _ _) = position_x

toY :: MovingVector -> Int
toY (MovingVector _ position_y _ _) = position_y

toCircle :: MovingVector -> String
toCircle (MovingVector position_x position_y _ _) = "<circle fill=\"#FF0\" stroke=\"#000\" stroke-width=\"1\" cx=\"" ++ (show $ position_x * 10) ++ "\" cy=\"" ++ (show $ position_y * 10) ++ "\" r=\"1\" />"

density :: [MovingVector] -> Float
density cs = len / area
  where
    minimumX = minimum $ map (toX) cs
    maximumX = maximum $ map (toX) cs

    minimumY = minimum $ map (toY) cs
    maximumY = maximum $ map (toY) cs

    area = fromIntegral $ (maximumX - minimumX) * (maximumY - minimumY) :: Float
    len = fromIntegral $ length cs :: Float

advance :: [MovingVector] -> [MovingVector]
advance [] = []
advance ((MovingVector position_x position_y velocity_x velocity_y):cs) = (MovingVector (position_x+velocity_x) (position_y+velocity_y) velocity_x velocity_y):(advance cs)

retreat :: [MovingVector] -> [MovingVector]
retreat [] = []
retreat ((MovingVector position_x position_y velocity_x velocity_y):cs) = (MovingVector (position_x-velocity_x) (position_y-velocity_y) velocity_x velocity_y):(retreat cs)

-- search :: Int -> Float -> [MovingVector] -> [MovingVector] -- [MovingVector]
search 0 acc cs = (0, cs)
search n acc cs
    | acc' < acc = (n, cs)
    | otherwise  = search (n-1) acc' cs'
  where
    acc' = density cs
    cs' = advance cs


--
main :: IO ()
main = do
   content <- getContents
   let
    vectors = map (parser) $ lines content
    (n, vectors'') = search 100000 0 vectors -- advance $ advance $ advance vectors

    vectors' = retreat $ vectors''

    minimumX = minimum $ map (toX) vectors'
    minimumY = minimum $ map (toY) vectors'

    maximumX = maximum $ map (toX) vectors'
    maximumY = maximum $ map (toY) vectors'

   -- putStrLn $ show $ search 10 0 $ map (parser) $ lines content
   --
   putStrLn "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   putStr "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
   putStr $ show (maximumX*10)
   putStr "\" height=\""
   putStr $ show (maximumY*10)
   putStrLn "\">"
   mapM (putStrLn . toCircle) $ vectors'
   putStrLn "</svg>"
