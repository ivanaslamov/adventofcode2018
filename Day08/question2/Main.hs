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

look :: [Node] -> Int -> Int
look [] _ = 0
look _ 0  = 0
look (c:cs) 1 = solution c
look (c:cs) n = look cs (n-1)


--
solution :: Node -> Int
solution (Node [] meta) = sum(meta)
solution (Node nodes meta) = foldl (\acc m -> acc + look nodes m) 0 meta


--
main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution $ parser content
