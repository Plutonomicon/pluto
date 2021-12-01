{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Prelude
  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.ByteString
  , module Data.Either.Extra
  , module Data.List
  , module Data.Map
  , module Data.Text
  , module Prelude
  , (<$$>)
  , letReverse
  ) where


import           Control.Applicative (Applicative (pure), liftA2, (<*>), (<|>))
import           Control.Arrow       (first, second, (***))
import           Control.Monad       (forM_, guard, mzero, void)
import           Data.ByteString     (ByteString)
import           Data.Either.Extra   (eitherToMaybe)
import           Data.List           (concat, length, unzip)
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Prelude             (Bool (False, True), Bounded, Char,
                                      Either (Left, Right), Enum, Eq,
                                      Foldable (foldMap), Functor (fmap), IO,
                                      Integer, Integral, Maybe (Just, Nothing),
                                      Monad (return, (>>=)), Monoid (mempty),
                                      Num ((*), (+), (-)), Ord, Real,
                                      Show (show), String, all, const, foldl,
                                      fst, mconcat, negate, reverse, snd, ($),
                                      (&&), (.), (/=), (<$), (<$>), (<=), (<>),
                                      (=<<), (==), (>=), (>>), (||))


(<$$>) :: ( Functor f, Functor g ) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

-- this gets a special name so it is clear which parts of the code are effected by the let desugaring reversal
letReverse :: [a] -> [a]
letReverse = reverse
