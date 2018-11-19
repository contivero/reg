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
  brzozowski :: RE -> a -> RE

-- Single character derivative
instance Brzozowski Char where
  brzozowski r c = 𝛿 r
    where
      𝛿 Nil       = Bot
      𝛿 Bot       = Bot
      𝛿 (Kle s)   = con (𝛿 s) (kle s)
      𝛿 (Alt s t) = alt (𝛿 s) (𝛿 t)
      𝛿 (Con s t)
          | acceptsEmptyStr s = alt (con (𝛿 s) t) (𝛿 t)
          | otherwise         = alt (𝛿 s) t
      𝛿 (C a)
          | c == a    = Nil
          | otherwise = Bot

-- Word derivative
instance Brzozowski [Char] where
  brzozowski = foldl brzozowski

-- A sequence s is contained in a regular expression R iff epsilon is contained
-- in DsR (see Theorem 4.2 from Brzozowski's paper).
-- Brzozowski's simple algorithm computes the DFA "on-the-fly", making it less
-- efficient in time, but very efficient in space.
match :: RE -> String -> Bool
match r = acceptsEmptyStr . brzozowski r
