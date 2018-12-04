import Data.Char
import Data.List (sort)
import qualified Data.Set as Set

parse :: [Char] -> Integer
parse [] = 0
parse (c:cs)
    | c == '-'  = 0 - value
    | c == '+'  = value
    | otherwise = 0
  where
    value = read cs

numbers :: String -> [Integer]
numbers = map (parse) . lines

second :: (t, a) -> a
second (_, value) = value

duplicate :: [Integer] -> Maybe Integer
duplicate ns = duplicate' ns 0 (Set.insert 0 Set.empty)
  where
    duplicate' []     _ _ = Nothing
    duplicate' (n:ns) f m
        | Set.member new_f m = Just new_f
        | otherwise          = duplicate' ns new_f (Set.insert new_f m)
      where
        new_f = f + n

solution :: [Integer] -> Maybe Integer
solution input = duplicate $ cycle $ input

main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ solution $ numbers content
