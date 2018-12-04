import Data.Char
import Data.List (sort, sortBy)
import qualified Data.Set as Set

store :: [String] -> Maybe String
store cs = store' cs (Set.empty)
  where
    store' []     s = Nothing
    store' (c:cs) s
      | Set.member c s = Just c
      | otherwise      = store' cs $ Set.insert c s

-- using underscore as a special character to keep track of positions
permutate :: String -> [String]
permutate []     = []
permutate (c:rs) = rs':permutate'
  where
    rs' = '_':rs
    permutate' = map (\str -> (c:str)) $ permutate rs

solution :: String -> Maybe String
solution input = store $ foldl (++) [] $ map (permutate) $ lines input

main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution content
