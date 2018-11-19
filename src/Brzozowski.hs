{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Brzozowski
-- Copyright   : (c) 2018 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Brzozowski regular expression derivative, with its correspoding concise and
-- elegant matching algorithm.
--
-----------------------------------------------------------------------------
module Brzozowski where

import RegEx

-- We overload brzozowski to denote both the one character derivative, and its
-- extension to words.
class Brzozowski a where
  brzozowski :: a -> RE -> RE

-- Single character derivative
instance Brzozowski Char where
  brzozowski c = 𝛿
    where
      𝛿 Nil       = Bot
      𝛿 Bot       = Bot
      𝛿 (Kle r)   = con (𝛿 r) (kle r)
      𝛿 (Alt r s) = alt (𝛿 r) (𝛿 s)
      𝛿 (Con r s)
          | acceptsEmptyStr r = alt (con (𝛿 r) s) (𝛿 s)
          | otherwise         = alt (𝛿 r) s
      𝛿 (C a)
          | c == a    = Nil
          | otherwise = Bot

-- Word derivative
instance Brzozowski [Char] where
  brzozowski xs r = foldl (flip brzozowski) r xs

-- A sequence s is contained in a regular expression R iff epsilon is contained
-- in DsR (see Theorem 4.2 from Brzozowski's paper).
-- Brzozowski's simple algorithm computes the DFA "on-the-fly", making it less
-- efficient in time, but very efficient in space.
match :: RE -> String -> Bool
match r xs = acceptsEmptyStr $ brzozowski xs r
