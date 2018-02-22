{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Data.Csv.Conduit
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Data.Csv.Conduit where

import Data.Csv.Conduit

import qualified Data.ByteString as BS
import Data.Csv (HasHeader(..), defaultDecodeOptions)
import Data.Conduit ((.|), runConduit)
import Data.Conduit.List (sourceList, consume)
import Data.Functor ((<$>))
import Data.List (length)

import Control.Applicative (pure)
import Control.Monad (return)

import Test.Arbitrary (
    intCsvNoTrailingNewline
  , intCsvTrailingNewline
  , diceIt
  )

import Test.QuickCheck (Property, (===), forAll, quickCheckAll)
import Test.QuickCheck.Gen (Gen)

import Prelude (
    IO
  , Bool(..)
  , Either
  , Int
  , ($)
  )

parsesAllRows :: Gen (Int, BS.ByteString) -> Property
parsesAllRows g =
  forAll (diceIt g) $ \(expectedRows, chunks) ->
    let
      result :: Either CsvParseError [[Int]]
      result = runConduit
         $ sourceList chunks
        .| fromCsv defaultDecodeOptions NoHeader
        .| consume
    in (length <$> result) === pure expectedRows

prop_parsesAllRowsWithNewline :: Property
prop_parsesAllRowsWithNewline =
  parsesAllRows intCsvTrailingNewline

prop_parsesAllRowsWithNoNewline :: Property
prop_parsesAllRowsWithNoNewline =
  parsesAllRows intCsvNoTrailingNewline

return []
tests :: IO Bool
tests = $quickCheckAll
