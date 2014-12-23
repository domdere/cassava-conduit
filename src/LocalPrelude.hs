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
    ,   Applicative(..)
    ,   Monad(..)
    ,   Num(..)
    ,   Show(..)
    ,   Eq(..)
    -- * Types
    ,   Char
    ,   Either(..)
    ,   Int
    ,   IO
    ,   Maybe(..)
    ,   String
    ,   Word8
    -- * Operators
    ,   (.)
    ,   ($)
    ,   ($!)
    ,   (<$>)
    ,   (++)
    -- * Functions
    ,   id
    ,   const
    ,   either
    ,   flip
    ,   fromIntegral
    ,   maybe
    ,   sequence
    ) where

import Prelude
import Control.Applicative
import Data.Word
