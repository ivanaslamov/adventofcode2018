import Data.Char
import Data.List (sort)
import qualified Data.Map.Strict as Map

parse :: [Char] -> Map.Map Char Integer
parse cs = parse' cs Map.empty
  where
    parse' []     m = m
    parse' (a:as) m = parse' as $ Map.insertWith (+) a 1 m

doubles :: Map.Map Char Integer -> Integer
doubles m = doubles' $ Map.elems m
  where
    doubles' :: [Integer] -> Integer
    doubles' []         = 0
    doubles' (b:rs)
      | b == 2    = 1
      | otherwise = doubles' rs

triples :: Map.Map Char Integer -> Integer
triples m = triples' $ Map.elems m
  where
    triples' :: [Integer] -> Integer
    triples' []         = 0
    triples' (b:rs)
      | b == 3    = 1
      | otherwise = triples' rs

solution :: String -> Integer
solution input = doubles' * triples'
  where
    dict = map (parse) $ lines input
    doubles' = foldl (+) 0 $ map (doubles) dict
    triples' = foldl (+) 0 $ map (triples) dict

main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution content
