{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines)
import Data.Maybe

import Data.Char
import qualified Data.SortedList as SortedList
import qualified Data.Set as Set
import Data.List (sort, sortBy, maximumBy)
import Data.List.Split
import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as Map


data Node = Node [Node] [Int] deriving (Show)


parseNode :: Parser Node
parseNode = do
  n <- decimal
  space
  m <- decimal

  nodes <- replicateM n $ do
    space
    parseNode

  meta <- replicateM m $ do
    space
    decimal
  return $ Node nodes meta

parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseNode input


solution :: Node -> Int
solution (Node nodes meta) = sum(meta) + meta'
  where
    meta' = foldl (\acc node -> acc + solution node) 0 nodes


--
main :: IO ()
main = do
   content <- getContents
   -- let
   --   numbers = map (read) $ words $ content :: [Int]
   putStrLn $ show $ solution $ parser content
