{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Antimirov
-- Copyright   : (c) 2018 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Antimirov regular expression partial derivative.
--
-----------------------------------------------------------------------------

module Antimirov where

import qualified Data.Set as Set
import Data.Set (Set)

import Regex

--(∩) = Set.intersection
(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = Set.union

(∅) :: Set a
(∅) = Set.empty
--(⊆) = Set.isSubsetOf
--(⊊) = Set.isProperSubsetOf
--(∈) = Set.member

-- | Given a regular expression r, and a symbol c from the alphabet, returns the
-- set of partial derivatives of r.
class Antimirov a where
  antimirov :: RE -> a -> Set RE

-- 1 character partial derivative
instance Antimirov Char where
  antimirov r c = 𝛿 r
    where
      𝛿 Nil       = (∅)
      𝛿 Bot       = (∅)
      𝛿 (Kle t)   = Set.map (\s -> Con s (Kle t)) (𝛿 t)
      𝛿 (Alt t s) = (𝛿 t) ∪ (𝛿 s)
      𝛿 (Con t s)
          | acceptsEmptyStr t = Set.map (\x -> Con x s) (𝛿 t) ∪ (𝛿 s)
          | otherwise         = Set.map (\x -> Con x s) (𝛿 t)
      𝛿 (C a)
          | c == a    = Set.singleton Nil
          | otherwise = (∅)

toRE :: Set RE -> RE
toRE s
    | Set.null s = Bot
    | otherwise  = foldr1 Alt (Set.toList s)

-- word partial derivative
instance Antimirov [Char] where
  antimirov r ys = Set.singleton $ foldl (\x xs -> toRE (antimirov x xs)) r ys

match :: RE -> String -> Bool
match r = acceptsEmptyStr . toRE . antimirov r
