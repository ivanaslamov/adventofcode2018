{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (filter, unwords, init, sort, zip, zipWith, maximum, length, pack, head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines, init)
import Data.Maybe

import Data.Char
import qualified Data.SortedList as SortedList
import qualified Data.Set as Set
import Data.List (sort, sortBy, maximumBy)
import Data.List.Split
import Control.Applicative
import qualified Data.Map.Strict as Map


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


--
solution :: Map.Map Char [Char] -> Map.Map Char [Char] -> Set.Set Char -> Set.Set Char -> [Char]
solution parents children s _
  | Set.null s = []
solution parents children s os
    | Set.member c os =    solution parents children s' os
    | isNothing new   = c:(solution parents children s' os')
    | otherwise       = c:(solution parents children s'' os')
  where
    c = Set.elemAt 0 s :: Char
    s' = Set.deleteAt 0 s :: Set.Set Char
    os' = Set.insert c os :: Set.Set Char

    new = Map.lookup c children :: Maybe [Char]

    (Just new') = new

    s'' = Set.union s' $ Set.fromList $ filter (available parents os') new' :: Set.Set Char


--
main :: IO ()
main = do
   content <- getContents
   let
     tuples = map (parser) $ lines content
     parents = foldl (\m (p,c) -> Map.insertWith (++) p [c] m) Map.empty tuples :: Map.Map Char [Char]
     children = foldl (\m (p,c) -> Map.insertWith (++) c [p] m) Map.empty tuples :: Map.Map Char [Char]
     roots = init children

   putStrLn $ show $ parents
   putStrLn $ show $ children
   putStrLn $ show $ solution parents children roots Set.empty
