import Data.Attoparsec.ByteString.Char8 hiding (take, count)
import Data.ByteString.Char8 hiding (head, count, take, foldl, putStrLn, map)
import Prelude hiding (getContents, count, lines)

import Data.Char
import Data.List (sort)
import qualified Data.Map.Strict as Map

data Dimention = Dimention Integer Integer deriving (Show)
data Location = Location Integer Integer deriving (Show)
data Carpet = Carpet Location Dimention deriving (Show)

parseCarpet :: Parser Carpet
parseCarpet = do
  char '#'
  decimal
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
  return $ Carpet (Location x y) (Dimention width height)

parser input = parser' result'
  where
    parser' (Right a) = a
    result' = parseOnly parseCarpet input

--
expand :: Carpet -> [(Integer, Integer)]
expand (Carpet (Location x y) (Dimention width height)) = [(x',y') | x'<- [x..x+width-1], y' <- [y..y+height-1]]

--
reduce :: [(Integer, Integer)] -> Map.Map (Integer, Integer) Integer
reduce ts = reduce' ts Map.empty
  where
    reduce' []     m = m
    reduce' (t:ts) m = reduce' ts $ Map.insertWith (+) t 1 m

count :: Map.Map a Integer -> Integer
count m = foldl (aggregator') 0 $ Map.elems m
  where
    aggregator' acc a
      | a > 1     = acc + 1
      | otherwise = acc

--
solution :: [Carpet] -> Integer
solution cs = count $ reduce $ foldl (++) [] $ map (expand) cs

main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution $ map (parser) $ lines content
