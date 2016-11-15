{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Arbitrary
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Arbitrary (
  -- * Generators
    intCsvNoTrailingNewline
  , intCsvTrailingNewline
  , diceIt
  ) where

import Control.Monad (replicateM)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, choose)

import Prelude

intColumns :: Int -> Gen [Int]
intColumns s = replicateM s arbitrary

intCsvNoTrailingNewlineText :: Gen (Int, T.Text)
intCsvNoTrailingNewlineText = do
  colSize <- choose (1, 30)
  numRows <- choose (0, 150)
  rows <- replicateM numRows (intColumns colSize)
  let t = T.intercalate "\n" . fmap (T.intercalate "," . fmap (T.pack . show)) $ rows
  pure (numRows, t)

intCsvNoTrailingNewline :: Gen (Int, BS.ByteString)
intCsvNoTrailingNewline = fmap T.encodeUtf8 <$> intCsvNoTrailingNewlineText

intCsvTrailingNewline :: Gen (Int, BS.ByteString)
intCsvTrailingNewline =
  fmap (T.encodeUtf8 . flip T.snoc '\n') <$> intCsvNoTrailingNewlineText

diceIt :: Gen (a, BS.ByteString) -> Gen (a, [BS.ByteString])
diceIt g =
  let
    diceIt' :: BS.ByteString -> Gen [BS.ByteString]
    diceIt' "" = pure []
    diceIt' bs = do
      chunkSize <- choose (3, 50)
      let (chunk, remainder) = (BS.take chunkSize bs, BS.drop chunkSize bs)
      (chunk :) <$> diceIt' remainder
  in do
    (rows, bs) <- g
    ((,) rows) <$> diceIt' bs


