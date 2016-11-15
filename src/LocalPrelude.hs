-------------------------------------------------------------------
-- |
-- Module       : LocalPrelude
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The bits of the Prelude used in this project.
--
-------------------------------------------------------------------
module LocalPrelude (
  -- * Type Classes
    Functor(..)
  , A.Applicative(..)
  , Monad(..)
  , Num(..)
  , Show(..)
  , Eq(..)
  -- * Types
  , Char
  , Either(..)
  , Int
  , IO
  , Maybe(..)
  , String
  , Word8
  -- * Operators
  , (.)
  , ($)
  , ($!)
  , (<$>)
  , (++)
  -- * Functions
  , id
  , const
  , either
  , flip
  , fromIntegral
  , fromMaybe
  , maybe
  , sequence
  ) where

import Prelude
import Control.Applicative as A
import Data.Maybe (fromMaybe)
import Data.Word
