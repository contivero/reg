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

import Regexp

--(∩) = Set.intersection
(∪) = Set.union
(∅) = Set.empty
--(⊆) = Set.isSubsetOf
--(⊊) = Set.isProperSubsetOf
--(∈) = Set.member

class Antimirov a where
  antimirov :: a -> RE -> Set RE

-- 1 character partial derivative
instance Antimirov Char where
  antimirov c = 𝛿
    where
      𝛿 Bot       = (∅)
      𝛿 (Kle r)   = Set.map (\s -> Con s (Kle r)) (𝛿 r)
      𝛿 (Alt r s) = (𝛿 r) ∪ (𝛿 s)
      𝛿 (Con r s)
          | acceptsEmptyStr r = Set.map (\r -> Con r s) (𝛿 r) ∪ (𝛿 s)
          | otherwise         = Set.map (\r -> Con r s) (𝛿 r)
      𝛿 (C a)
          | c == a    = Set.singleton Nil
          | otherwise = (∅)

toRE :: Set RE -> RE
toRE s
    | Set.null s = Bot
    | otherwise  = foldr1 Alt (Set.toList s)

-- word partial derivative
instance Antimirov [Char] where
  antimirov []     = Set.singleton
  antimirov (x:xs) = antimirov xs . toRE . antimirov x
