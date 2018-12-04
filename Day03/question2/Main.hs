import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines)

import Data.Char
import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Dimention = Dimention Integer Integer deriving (Show)
data Location = Location Integer Integer deriving (Show)
data Carpet = Carpet Integer Location Dimention deriving (Show)

parseCarpet :: Parser Carpet
parseCarpet = do
  char '#'
  index <- decimal
  space
  char '@'
  space
  x <- decimal
  char ','
  y <- decimal
  char ':'
  space
  width <- decimal
  char 'x'
  height <- decimal
  return $ Carpet index (Location x y) (Dimention width height)

parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseCarpet input

--
expand :: Carpet -> [(Integer, Integer, Integer)]
expand (Carpet index (Location x y) (Dimention width height)) = [(index, x',y') | x'<- [x..x+width-1], y' <- [y..y+height-1]]

--
reduce :: [(Integer, Integer, Integer)] -> Map.Map (Integer, Integer) [Integer]
reduce ts = reduce' ts Map.empty
  where
    reduce' []               m = m
    reduce' ((index,x,y):ts) m = reduce' ts $ Map.insertWith (++) (x,y) [index] m

--
count :: Map.Map a [Integer] -> Set.Set Integer
count m = foldl (aggregator') (Set.empty) $ Map.elems m
  where
    aggregator' :: Set.Set Integer -> [Integer] -> Set.Set Integer
    aggregator' s []  = s
    aggregator' s [c] = s
    aggregator' s cs  = foldl (\acc c -> Set.insert c acc) s cs

--
solution :: [Carpet] -> Integer
solution cs = head $ Set.toList $ Set.difference indexes overlapping
  where
    overlapping = count $ reduce $ foldl (++) [] $ map (expand) cs

    indexes = indexes' cs Set.empty

    indexes' []                      s = s
    indexes' ((Carpet index _ _):as) s = indexes' as $ Set.insert index s

--
main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution $ map (parser) $ lines content
