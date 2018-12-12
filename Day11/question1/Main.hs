{-# LANGUAGE OverloadedStrings #-}


import Data.Maybe
import qualified Data.Map.Strict as Map


cell :: Int -> Int -> Int -> Int
cell x y puzzleInput = powerLevel - 5
  where
    rackID = x + 10
    powerLevel = read $ take 1 $ tail $ tail $ (reverse $ show $ (rackID * y + puzzleInput) * rackID) ++ (cycle ['0']) :: Int

region :: Int -> Int -> Int -> Int -> Int
region x y n puzzleInput = sum $ map (\(x,y) -> cell x y puzzleInput) [ (x', y') | x' <- [x..x+n], y' <- [y..y+n] ]


solution :: [(Int, Int)] -> Int -> Int -> Int -> (Int,Int) -> (Int,(Int,Int))
solution [] puzzleInput n acc acc_coord = (acc,acc_coord)
solution ((x,y):cs) puzzleInput n acc acc_coord
    | acc' > acc = solution cs puzzleInput n acc' (x,y)
    | otherwise  = solution cs puzzleInput n acc acc_coord
  where
    acc' = region x y n puzzleInput


solution' :: Int -> Int -> Int -> (Int,Int) -> Int -> (Int, (Int,Int), Int)
solution' puzzleInput 301 acc acc_coord acc_n = (acc, acc_coord, acc_n)
solution' puzzleInput n acc acc_coord acc_n
    | acc' > acc = solution' puzzleInput (n+1) acc' acc_coord' n
    | otherwise  = solution' puzzleInput (n+1) acc acc_coord acc_n
  where
    coords = tail $ [ (x,y) | x <- [1..300-n], y <- [0..300-n] ]
    initCellPower = region 1 1 n puzzleInput
    (acc', acc_coord') = solution coords puzzleInput n initCellPower (1,1)


main :: IO ()
main = do
  let
    puzzleInput = 9435 :: Int
    initCellPower = region 1 1 0 puzzleInput

  putStrLn $ show $ solution' puzzleInput 288 initCellPower (1,1) 0
