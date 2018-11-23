{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Regex (
      Regex(..)
    , RE
    , con
    , alt
    , kle
    , acceptsEmptyStr
    , alphabet
    , re
    ) where

import Data.Data
import Data.Monoid (Monoid)
import Data.List (sort)
import Data.Void
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote

data Regex a                  -- 'a' amounts to the alphabet we are using
    = Nil                     -- ^ Empty string
    | Bot                     -- ^ Bottom, or the empty set, i.e. it matches nothing
    | C a                     -- ^ A symbol literal
    | Con (Regex a) (Regex a) -- ^ Concatenation: rs
    | Alt (Regex a) (Regex a) -- ^ Alternation: r|s
    | Kle (Regex a)           -- ^ Kleene closure: r*
  deriving (Show, Eq, Ord, Data, Functor)

type RE = Regex Char

-- We maintain the invariant that all REs are in ≈-canonical form by using smart
-- constructors, and use structural equality to identify equivalent REs.

con :: Ord a => Regex a -> Regex a -> Regex a
-- We use associativity to 'normalize' based on order.
con (Con r s) t = foldr1 Con (sort [r, s, t])
con r (Con s t) = foldr1 Con (sort [r, s, t])
-- Concatenation unit
con Nil r = r
con r Nil = r
--
con Bot _ = Bot
con _ Bot = Bot

-- r*r* = r*
con u@(Kle r) v@(Kle s)
    | r == s    = Kle r
    | otherwise = Con u v
con r s = Con r s

alt :: Ord a => Regex a -> Regex a -> Regex a
alt (Alt r s) t = foldr1 Alt (sort [r, s, t])
alt r (Alt s t) = foldr1 Alt (sort [r, s, t])
-- Distributive law
alt s@(Con r1 r2) t@(Con r3 r4)
    | r1 == r3  = con r1 (alt r2 r4)
    | r2 == r4  = con (alt r1 r3) r2
    | otherwise = Alt s t
-- Alternation unit
alt Bot r = r
alt r Bot = r
-- r⁺ = rr* = r*r
-- rr*|ε = ε|rr* = r*
alt (Con r (Kle s)) Nil
    | r == s = Kle s
alt Nil (Con r (Kle s))
    | r == s = Kle s
-- r|rs* = rs*
alt r x@(Con t (Kle _))
    | r == t = x
-- r|s*r = s*r
alt r x@(Con (Kle _) t)
    | r == t = x
-- ε|r* = r*
-- r|r* = r*
-- TODO generalize to rr|r* = r*, rrr|r*=r*,...
alt Nil (Kle r) = Kle r
alt r k@(Kle s)
    | r == s = k
alt r s
    -- Idempotent law: r | r = r
    | r == s    = r
    | otherwise = foldr1 Alt (sort [r, s])

kle :: Ord a => Regex a -> Regex a
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

instance (Ord a) => Semigroup (Regex a) where
  (<>) = con

instance (Ord a) => Monoid (Regex a) where
  mempty  = Nil

acceptsEmptyStr :: Regex a -> Bool
acceptsEmptyStr (C _)     = False
acceptsEmptyStr Bot       = False
acceptsEmptyStr Nil       = True
acceptsEmptyStr (Kle _)   = True
acceptsEmptyStr (Alt r s) = acceptsEmptyStr r || acceptsEmptyStr s
acceptsEmptyStr (Con r s) = acceptsEmptyStr r && acceptsEmptyStr s

-- | Given a regular expression, returns the subset of symbols it uses.
alphabet :: (Ord a) => Regex a -> Set a
alphabet Bot       = Set.empty
alphabet Nil       = Set.empty
alphabet (C r)     = Set.singleton r
alphabet (Kle r)   = alphabet r
alphabet (Con r s) = alphabet r `Set.union` alphabet s
alphabet (Alt r s) = alphabet r `Set.union` alphabet s


-- Regular expression quasiquoter
re :: QuasiQuoter
re = QuasiQuoter {
      quoteExp  = compile
    , quotePat  = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec  = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not handled by the regex quasiquoter."

    compile :: String -> Q Exp
    compile s =
      case runParser regex "" s of
        Left  err    -> fail (show err)
        Right regexp -> dataToExpQ (const Nothing) regexp

type Parser = Parsec Void String

-- Recursive descent parser, based on:
-- http://matt.might.net/articles/parsing-regex-with-recursive-descent/
regex :: Parser RE
regex = do
    t <- term
    (char '|' *> (alt t <$> regex)) <|> pure t
  where
    term = foldl con Nil <$> many factor
    factor = do
      b <- base
      (some (char '*') $> kle b) <|> pure b
    base = (char '(' *> regex <* char ')') <|> (char '\\' *> ch) <|> nil <|> ch
    ch = C <$> noneOf specials
    nil = char 'ε' $> Nil
    specials = "|[]()"
