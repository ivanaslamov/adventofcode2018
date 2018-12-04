import Data.Char

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

main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ foldl (+) 0 $ numbers content
