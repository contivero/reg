module Regexp (
      RE(..)
    , con
    , alt
    , kle
    , acceptsEmptyStr
    ) where

import Data.Semigroup
import Data.Monoid (Monoid)
import Data.List (sort)

data RE = Nil       -- ^ Empty string
        | Bot       -- ^ Bottom, or the empty set, i.e. it matches nothing
        | C Char    -- ^ A character literal
        | Con RE RE -- ^ Concatenation: rs
        | Alt RE RE -- ^ Alternation: r | s
        | Kle RE    -- ^ Kleene closure: r*
        -- | Fun2 (RE -> RE -> Bool) RE RE
        -- | Fun1 (RE -> Bool) RE
  deriving (Show, Eq, Ord)

-- We maintain the invariant that all REs are in ≈-canonical form by using smart
-- constructors, and use structural equality to identify equivalent REs.

con :: RE -> RE -> RE
-- We use associativity to 'normalize' based on order.
con (Con r s) t = Con r1 (Con r2 r3)
  where [r1, r2, r3] = sort [r, s, t]
con r (Con s t) = Con r1 (Con r2 r3)
  where [r1, r2, r3] = sort [r, s, t]
-- Concatenation unit
con Nil r = r
con r Nil = r
--
con Bot r = Bot
con r Bot = Bot

-- r*r* = r*
con u@(Kle r) v@(Kle s)
    | r == s    = Kle r
    | otherwise = Con u v
con r s = Con r s

alt :: RE -> RE -> RE
alt (Alt r s) t = Alt r1 (Alt r2 r3)
  where [r1, r2, r3] = sort [r, s, t]
alt r (Alt s t) = Alt r1 (Alt r2 r3)
  where [r1, r2, r3] = sort [r, s, t]
-- Distributive law
alt s@(Con r1 r2) t@(Con r3 r4)
    | r1 == r3  = con r1 (alt r2 r4)
    | r2 == r4  = con (alt r1 r3) r2
    | otherwise = Alt s t
-- Alternation unit
alt Bot r = r
alt r Bot = r
-- r⁺|ε = ε|r⁺ = r*
alt t@(Con r (Kle s)) Nil
    | r == s = Kle s
alt Nil t@(Con r (Kle s))
    | r == s = Kle s
-- r|rs* = rs*
alt r x@(Con t (Kle s))
    | r == t = x
-- r|s*r = s*r
alt r x@(Con (Kle s) t)
    | r == t = x
-- ε|r* = r*
-- r|r* = r*
-- TODO generalize to rr|r* = r*, rrr|r*=r*,...
alt Nil (Kle r) = Kle r
alt r (Kle s)   = Kle s
alt r s
    -- Idempotent law: r | r = r
    | r == s    = r
    | otherwise = Alt r1 r2
  where [r1, r2] = sort [r, s]

kle :: RE -> RE
kle Bot     = Nil
kle Nil     = Nil
-- (ε|r)* = (r|ε)* = r*
kle (Alt Nil r) = kle r
kle (Alt r Nil) = kle r
-- (r*|s*)* = (r*s*)* = (r|s)*
kle (Kle (Con (Kle r) (Kle s))) = kle (alt r s)
kle (Kle (Alt (Kle r) (Kle s))) = kle (alt r s)
-- (r*)* = r*
kle (Kle r) = r
kle r = Kle r

instance Semigroup RE where
  (<>) = con

instance Monoid RE where
  mempty  = Nil

acceptsEmptyStr :: RE -> Bool
acceptsEmptyStr (C _)     = False
acceptsEmptyStr Bot       = False
acceptsEmptyStr Nil       = True
acceptsEmptyStr (Kle _)   = True
acceptsEmptyStr (Alt r s) = acceptsEmptyStr r || acceptsEmptyStr s
acceptsEmptyStr (Con r s) = acceptsEmptyStr r && acceptsEmptyStr s
