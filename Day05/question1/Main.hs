{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List (sort, sortBy, maximumBy)
import Data.Maybe
import Control.Applicative
import qualified Data.Map.Strict as Map

data Tree = Node Bool Char Char Int Tree Tree | Leaf Char | Empty deriving (Show)

instance Eq Tree where
    (Leaf c)           == (Leaf c')             = c == c'
    (Node _ a b c _ _) == (Node _ a' b' c' _ _) = (a,b,c) == (a',b',c')
    _                  == _                     = False

firstCharacter :: Tree -> Char
firstCharacter (Node _ c _ _ _ _) = c
firstCharacter (Leaf c)           = c

lastCharacter :: Tree -> Char
lastCharacter (Node _ _ c _ _ _) = c
lastCharacter (Leaf c)           = c

treeLength :: Tree -> Int
treeLength (Leaf _)             = 1
treeLength (Node _ _ _ len _ _) = len

trimFirstCharacter :: Tree -> Maybe Tree
trimFirstCharacter (Leaf _)                                                = Nothing
trimFirstCharacter (Node _      _ _              _   (Leaf _)  right_tree) = Just right_tree
trimFirstCharacter (Node solved _ last_character len left_tree right_tree) = Just $ Node solved first_character' last_character len' left_tree' right_tree
  where
    (Just left_tree') = trimFirstCharacter left_tree
    first_character'  = firstCharacter left_tree'
    len'              = len - 1

trimLastCharacter :: Tree -> Maybe Tree
trimLastCharacter (Leaf _)                                                 = Nothing
trimLastCharacter (Node _      _               _ _   left_tree (Leaf _))   = Just left_tree
trimLastCharacter (Node solved first_character _ len left_tree right_tree) = Just $ Node solved first_character last_character' len' left_tree right_tree'
  where
    (Just right_tree') = trimLastCharacter right_tree
    last_character'    = lastCharacter right_tree'
    len'               = len - 1

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

--
expand :: [Char] -> Tree
expand [c] = Leaf c
expand cs  = Node False first_character last_character len left_tree right_tree
  where
    len = length cs
    middle_position = len `div` 2
    (left_string, right_string) = splitAt middle_position cs

    left_tree  = expand left_string
    right_tree = expand right_string

    first_character = firstCharacter left_tree
    last_character  = lastCharacter right_tree


--
solution :: Tree -> Maybe Tree
solution (Leaf c) = Just (Leaf c)
solution (Node True start_character end_character len left_tree right_tree) = Just $ Node True start_character end_character len left_tree right_tree
solution (Node _ start_character end_character len left_tree right_tree)
    | converges && (isNothing maybe_left_tree) && (isNothing maybe_right_tree) = Nothing
    | converges && (isNothing maybe_left_tree)                                 = solution right_tree'
    | converges                                && (isNothing maybe_right_tree) = solution left_tree'
    | converges                                                                = solution $ Node False start_character' end_character' len' left_tree' right_tree'
  where
    lastLeftCharacter   = lastCharacter left_tree
    firstRightCharacter = firstCharacter right_tree

    converges = (toLower lastLeftCharacter) == (toLower firstRightCharacter) && lastLeftCharacter /= firstRightCharacter

    maybe_left_tree  = trimLastCharacter left_tree
    maybe_right_tree = trimFirstCharacter right_tree

    left_tree'  = fromJust maybe_left_tree
    right_tree' = fromJust maybe_right_tree

    start_character' = (firstCharacter left_tree')
    end_character'   = (lastCharacter right_tree')

    len' = (treeLength left_tree') + (treeLength right_tree')
solution (Node _ start_character end_character len left_tree right_tree)
    | (isNothing maybe_left_tree) && (isNothing maybe_right_tree) = Nothing
    | (isNothing maybe_left_tree)                                 = maybe_right_tree
    |                                (isNothing maybe_right_tree) = maybe_left_tree
    | left_tree' == left_tree     && right_tree' == right_tree    = Just $ Node True start_character end_character len left_tree right_tree
    | otherwise                                                   = solution $ Node False start_character' end_character' len' left_tree' right_tree'
  where
    maybe_left_tree  = solution left_tree
    maybe_right_tree = solution right_tree

    left_tree' = fromJust maybe_left_tree
    right_tree' = fromJust maybe_right_tree

    start_character' = (firstCharacter left_tree')
    end_character'   = (lastCharacter right_tree')

    len' = (treeLength left_tree') + (treeLength right_tree')


--
pack :: Maybe Tree -> [Char]
pack Nothing                          = ""
pack (Just (Leaf c))                  = [c]
pack (Just (Node _ _ _ _ right left)) = (pack $ Just right) ++ (pack $ Just left)


--
main :: IO ()
main = do
   content <- getContents
   putStrLn $ show $ length $ pack $ solution $ expand $ trim $ content
