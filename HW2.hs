{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, mod, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))


-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix suffix = take (length prefix) suffix == prefix

isInfixOf :: String -> String -> Bool
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)

--Helper
tails :: [a] -> [[a]]
tails "" = [""]
tails xs@(_:xs') = xs : tails xs'

isSuffixOf :: String -> String -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf "" _ = True
isSubsequenceOf _ "" = False
isSubsequenceOf xs@(x:xs') (y:ys)
  | x == y    = isSubsequenceOf xs' ys
  | otherwise = isSubsequenceOf xs ys

-- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase deriving Show
type Document = String
--Helper
matches :: Query -> Document -> Bool
matches (Literal phrase) document = phrase `isInfixOf` document
matches (All queries) document = all (`matches` document) queries
matches (Any queries) document = any (`matches` document) queries
matches (None queries) document = not (any (`matches` document) queries)

findDocuments :: Query -> [Document] -> ([Document], [Document])
findDocuments query documents = foldr partitioner ([], []) documents
  where
    partitioner doc (matches, nonmatches)
      | matches query doc = (doc:matches, nonmatches)
      | otherwise         = (matches, doc:nonmatches)

-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

icycle :: [a] -> InfiniteList a
icycle [] = error "Cannot cycle an empty list"
icycle xs = go xs
  where
    go []     = go xs
    go (y:ys) = y :> go ys

naturals :: InfiniteList Int
naturals = iiterate (+1) 0
--Helper
interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x :> xs) ys = x :> interleave ys xs

integers :: InfiniteList Int
integers = interleave naturals (iiterate negate 0)

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a

iinits :: InfiniteList a -> InfiniteList [a]
itails :: InfiniteList a -> InfiniteList (InfiniteList a)

-- Bonus: if you don't wish to implement this, simply write ifind = undefined
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool


-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
postOrder :: Tree a -> [a]
inOrder :: Tree a -> [a]
levelOrder :: Tree a -> [a]
fromListLevelOrder :: [a] -> Tree a
