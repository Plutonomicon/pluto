{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Data.ByteString
  , module Data.Text
  , module Prelude
  ) where


import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Prelude             (Bool (False, True), Char,
                                      Either (Left, Right), Eq, IO, Integer,
                                      Monad (return, (>>=)), Num ((*), (+)),
                                      Show, String, ($), (.), (<$), (<$>), (<>),
                                      (>>))
