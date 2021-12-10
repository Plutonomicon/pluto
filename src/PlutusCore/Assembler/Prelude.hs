{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Prelude
  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Control.Monad.Except
  , module Data.Bifunctor
  , module Data.ByteString
  , module Data.Either
  , module Data.Either.Extra
  , module Data.Foldable
  , module Data.List
  , module Data.Map
  , module Data.Maybe
  , module Data.String
  , module Data.Text
  , module GHC.Base
  , module GHC.Stack
  , module Prelude
  , (<$$>)
  ) where


import           Control.Applicative  (Applicative (pure), liftA2, (<*>), (<|>))
import           Control.Arrow        (first, second, (***))
import           Control.Monad        (forM_, guard, mzero, void, when, (<=<))
import           Control.Monad.Except (MonadError (throwError),
                                       MonadIO (liftIO), runExceptT)
import           Data.Bifunctor       (bimap)
import           Data.ByteString      (ByteString)
import           Data.Either          (either)
import           Data.Either.Extra    (eitherToMaybe)
import           Data.Foldable        (foldl')
import           Data.List            (concat, find, length, unzip)
import           Data.Map             (Map)
import           Data.Maybe           (mapMaybe, maybe)
import           Data.String          (IsString (fromString))
import           Data.Text            (Text)
import           GHC.Base             (error)
import           GHC.Stack            (HasCallStack)
import           Prelude              (Bool (False, True), Bounded, Char,
                                       Either (Left, Right), Enum, Eq,
                                       Foldable (foldMap, null), Functor (fmap),
                                       IO, Integer, Integral,
                                       Maybe (Just, Nothing),
                                       Monad (return, (>>=)), MonadFail (fail),
                                       Monoid (mempty), Num ((*), (+), (-)),
                                       Ord, Real, Show (show), String, abs, all,
                                       const, curry, drop, flip, foldl,
                                       fromIntegral, fst, head, id, mconcat,
                                       min, negate, not, otherwise, readFile,
                                       reverse, snd, take, ($), (&&), (++), (.),
                                       (/=), (<$), (<$>), (<), (<=), (<>),
                                       (=<<), (==), (>), (>=), (>>), (||))

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x
