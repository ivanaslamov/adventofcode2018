{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List (sort, sortBy, maximumBy)
import Data.Maybe
import Control.Applicative
import qualified Data.Map.Strict as Map

data Tree = Node Char Char Tree Tree | Leaf Char | Empty deriving (Show)

firstCharacter :: Tree -> Char
firstCharacter (Node c _ _ _) = c
firstCharacter (Leaf c)       = c

lastCharacter :: Tree -> Char
lastCharacter (Node _ c _ _) = c
lastCharacter (Leaf c)       = c

trimFirstCharacter :: Tree -> Maybe Tree
trimFirstCharacter (Leaf _)                                     = Nothing
trimFirstCharacter (Node _ last_character (Leaf _)   left_tree) = Just left_tree
trimFirstCharacter (Node _ last_character right_tree left_tree) = Just $ Node first_character' last_character right_tree' left_tree
  where
    (Just right_tree') = trimFirstCharacter right_tree
    first_character'   = firstCharacter right_tree'

trimLastCharacter :: Tree -> Maybe Tree
trimLastCharacter (Leaf _)                                      = Nothing
trimLastCharacter (Node first_character _ right_tree (Leaf _))  = Just right_tree
trimLastCharacter (Node first_character _ right_tree left_tree) = Just $ Node first_character last_character' right_tree left_tree'
  where
    (Just left_tree') = trimLastCharacter left_tree
    last_character'   = lastCharacter left_tree'


--
expand :: [Char] -> Tree
expand [c] = Leaf c
expand cs  = Node first_character last_character left_tree right_tree
  where
    middle_position = (length cs) `div` 2
    (left_string, right_string) = splitAt middle_position cs

    left_tree  = expand left_string
    right_tree = expand right_string

    first_character = firstCharacter left_tree
    last_character  = lastCharacter right_tree

--
solution :: Tree -> Maybe Tree
solution (Leaf c) = Just (Leaf c)
solution (Node start_character end_character left_tree right_tree)
    | converges && (isNothing maybe_right_tree) && (isNothing maybe_left_tree) = Nothing
    | converges && (isNothing maybe_right_tree)                                = maybe_right_tree
    | converges                                 && (isNothing maybe_left_tree) = maybe_left_tree
    | converges                                                                = solution $ Node start_character end_character left_tree' right_tree'
    | otherwise                                                                                                               = Just (Node start_character end_character left_tree right_tree)
  where
    lastLeftCharacter   = (lastCharacter left_tree)
    firstRightCharacter = (firstCharacter right_tree)

    converges = lastLeftCharacter == 'a' && firstRightCharacter == 'A' || lastLeftCharacter == 'b' && firstRightCharacter == 'B' || lastLeftCharacter == 'c' && firstRightCharacter == 'C'

    maybe_right_tree = trimFirstCharacter right_tree
    maybe_left_tree  = trimLastCharacter left_tree

    right_tree' = fromJust maybe_right_tree
    left_tree'  = fromJust maybe_left_tree


--
pack :: Maybe Tree -> [Char]
pack Nothing                      = ""
pack (Just (Leaf c))              = [c]
pack (Just (Node _ _ right left)) = (pack $ Just right) ++ (pack $ Just left)


--
main :: IO ()
main = do
   content <- getContents
   putStrLn $ pack $ solution $ expand $ content
