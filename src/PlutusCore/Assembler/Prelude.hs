{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Prelude
  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.ByteString
  , module Data.Either.Extra
  , module Data.List
  , module Data.Text
  , module Prelude
  , (<$$>)
  ) where


import           Control.Applicative (Applicative (pure), liftA2, (<*>), (<|>))
import Control.Arrow ((***), first, second)
import           Control.Monad       (guard, mzero, void)
import           Data.ByteString     (ByteString)
import           Data.Either.Extra   (eitherToMaybe)
import Data.List (unzip, concat)
import           Data.Text           (Text)
import           Prelude             (Bool (False, True), Bounded, Char,
                                      Either (Left, Right), Enum, Eq,
                                      Functor (fmap), IO, Integer,
                                      Maybe (Just, Nothing),
                                      Monad (return, (>>=)), Num ((*), (+), (-)), Real, Integral,
                                      Show (show), String, foldl, fst, negate,
                                      snd, ($), (&&), (.), (/=), (<$), (<$>),
                                      (<=), (<>), (==), (>>), (||), Ord)


(<$$>) :: ( Functor f, Functor g ) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x
