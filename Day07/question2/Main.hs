{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (tail, filter, unwords, init, sort, zip, zipWith, maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines, init)
import Data.Maybe

import Data.Char
import qualified Data.SortedList as SortedList
import qualified Data.Set as Set
import Data.List (sort, sortBy, maximumBy)
import Data.List.Split
import Control.Applicative
import qualified Data.Map.Strict as Map


data Worker = Worker Char Int deriving (Show)


parseLine :: Parser (Char, Char)
parseLine = do
  string "Step"
  space
  child <- anyChar
  space
  string "must be finished before step"
  space
  parent <- anyChar
  space
  string "can begin."
  return $ (parent, child)

parser :: ByteString -> (Char, Char)
parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseLine input


--
init :: Map.Map Char [Char] -> Set.Set Char
init m = roots
  where
    parents = Set.fromList $ Map.keys m :: Set.Set Char
    children = Set.fromList $ unwords $ Map.elems m :: Set.Set Char
    roots = Set.difference parents children :: Set.Set Char

available :: Map.Map Char [Char] -> Set.Set Char -> Char -> Bool
available m s c
    | Map.member c m = available' required s
    | otherwise      = True
  where
    Just required = Map.lookup c m :: Maybe [Char]

    available' [] _     = True
    available' (c:cs) s
      | Set.member c s = available' cs s
      | otherwise      = False

charToTime :: Char -> Int
charToTime c = fromJust $ Map.lookup c m
  where
    m = Map.fromList $ zip ['A'..'Z'] [61..]


--
solution :: Int -> [Worker] -> [Worker] -> Map.Map Char [Char] -> Map.Map Char [Char] -> Set.Set Char -> Set.Set Char -> Int
solution n available_workers [] parents children available_nodes completed_nodes
  | Set.null available_nodes = n
solution n available_workers running_workers parents children available_nodes completed_nodes = solution' n available_workers' running_workers' available_nodes' completed_nodes'
  where
    completed :: Worker -> Bool
    completed (Worker _ t)
      | t == 1    = True
      | otherwise = False

    working :: Worker -> Bool
    working (Worker _ t)
      | t == 1    = False
      | otherwise = True

    available_workers' = available_workers ++ (filter (completed) running_workers) :: [Worker]
    running_workers'   = map (\(Worker c n) -> Worker c (n-1)) $ filter (working) running_workers :: [Worker]
    finished_nodes'    = map (\(Worker c n) -> c) $ filter (completed) running_workers :: [Char]
    completed_nodes'   = foldl (\s c -> Set.insert c s) completed_nodes $ finished_nodes' :: Set.Set Char
    available_nodes'   = Set.union available_nodes $ Set.fromList $ filter (available parents completed_nodes') $ foldl (++) [] $ map (\c -> fromMaybe [] $ Map.lookup c children) finished_nodes' :: Set.Set Char

    solution' :: Int -> [Worker] -> [Worker] -> Set.Set Char -> Set.Set Char -> Int
    solution' n _ [] available_nodes _
      | Set.null available_nodes = n
    solution' n [] running_workers available_nodes completed_nodes = solution (n+1) [] running_workers parents children available_nodes completed_nodes
    solution' n available_workers running_workers available_nodes completed_nodes
      | Set.null available_nodes = solution (n+1) available_workers running_workers parents children available_nodes completed_nodes
    solution' n available_workers running_workers available_nodes completed_nodes = solution' n available_workers' running_workers' available_nodes' completed_nodes
      where
        c = Set.elemAt 0 available_nodes :: Char
        available_nodes' = Set.deleteAt 0 available_nodes :: Set.Set Char

        running_workers'   = (Worker c $ charToTime c):running_workers
        available_workers' = tail available_workers


--
main :: IO ()
main = do
   content <- getContents
   let
     tuples = map (parser) $ lines content
     parents = foldl (\m (p,c) -> Map.insertWith (++) p [c] m) Map.empty tuples :: Map.Map Char [Char]
     children = foldl (\m (p,c) -> Map.insertWith (++) c [p] m) Map.empty tuples :: Map.Map Char [Char]
     roots = init children
     available_workers = [Worker 'a' 0, Worker 'a' 0, Worker 'a' 0, Worker 'a' 0, Worker 'a' 0]
     running_workers = []

   putStrLn $ show $ solution 0 available_workers running_workers parents children roots Set.empty
