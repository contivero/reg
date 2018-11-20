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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

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
      𝛿 (Kle t)   = Set.map (<> kle t) (𝛿 t)
      𝛿 (Alt t s) = 𝛿 t ∪ 𝛿 s
      𝛿 (Con t s)
          | acceptsEmptyStr t = Set.map (<> s) (𝛿 t ∪ 𝛿 s)
          | otherwise         = Set.map (<> s) (𝛿 t)
      𝛿 (C a)
          | c == a    = Set.singleton Nil
          | otherwise = (∅)

toRE :: Set RE -> RE
toRE s
    | Set.null s = Bot
    | otherwise  = foldr1 alt (Set.toList s)

-- word partial derivative
instance Antimirov [Char] where
  antimirov r ys = Set.singleton $ foldl (\x xs -> toRE (antimirov x xs)) r ys

match :: RE -> String -> Bool
match r = acceptsEmptyStr . toRE . antimirov r

alphabet :: Regex a -> Set a
alphabet Bot       = (∅)
alphabet Nil       = Set.singleton Nil 
alphabet (C a)     = Set.singleton a
alphabet (Kle r)   = alphabet r
alphabet (Con r s) = alphabet r ∪ alphabet s
alphabet (Alt r s) = alphabet r ∪ alphabet s

regs :: Set RE
m :: Map (Set RE) Int

dfa :: RE -> DFA
dfa r = loop (0, M.empty) Set.empty (Set.singleton r)
  where find rs (n, m) = 
            case M.lookup rs m of
              Just v -> (n+1, M.insert rs
              Nothing -> undefined
        loop s v t f rs =

M.lookup rs m

let dfa r =
  let rec loop s v t f rs =
    let (x, s) = find rs s in
    if I.mem x v then (s, v, t, f)
    else charfold (fun c (s, v, t, f) ->
                     let rs' = deriv c rs in
                     let (y, s) = find rs' s in
                     loop s v ((x,c,y) :: t) f rs')
           (s, I.add x v, t, if R.exists null rs then x :: f else f) in
  let (s, v, t, f) = loop (0, M.empty) I.empty [] [] (R.singleton r) in
  let (fail, (n, m)) = find R.empty s in 
  { size = n; fail = fail; trans = t; final = f }
