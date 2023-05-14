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
tails [] = [[]]
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

interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x :> xs) ys = x :> interleave ys xs

integers :: InfiniteList Int
integers = interleave naturals (iiterate negate 0)

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b
iscan f acc (x :> xs) = acc :> iscan f (f x acc) xs

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
izip (x :> xs) (y :> ys) = (x, y) :> izip xs ys

iinits :: InfiniteList a -> InfiniteList [a]
iinits xs = [] :> imap (\(ys, y) -> ys ++ [y]) (izip (iinits xs) xs)

itails :: InfiniteList a -> InfiniteList (InfiniteList a)
itails xs@(x :> xs') = xs :> itails xs'

-- Bonus: if you don't wish to implement this, simply write ifind = undefined
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool
ifind = undefined

-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
preOrder EmptyTree = []
preOrder (Tree left x right) = [x] ++ preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder EmptyTree = []
postOrder (Tree left x right) = postOrder left ++ postOrder right ++ [x]

inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Tree left x right) = inOrder left ++ [x] ++ inOrder right

levelOrder :: Tree a -> [a]
levelOrder t = loAux [t]

--Helpers
loAux :: [Tree a] -> [a]
loAux [] = []
loAux xs = map nodeValue xs ++ loAux (concatMap children xs)

nodeValue :: Tree a -> a
nodeValue (Tree _ x _) = x

children :: Tree a -> [Tree a]
children EmptyTree = []
children (Tree left _ right) = filter (/= EmptyTree) [left, right]
--Helpers End

fromListLevelOrder :: [a] -> Tree a
fromListLevelOrder [] = EmptyTree
fromListLevelOrder xs = let (t, _) = buildTree xs in t
  where
    buildTree :: [a] -> (Tree a, [a])
    buildTree [] = (EmptyTree, [])
    buildTree (x:xs) = 
      let (left, remaining1) = buildTree xs
          (right, remaining2) = buildTree remaining1
      in (Tree left x right, remaining2)
