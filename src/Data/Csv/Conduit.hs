{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Csv.Conduit
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Conduit interface for cassava
--
-------------------------------------------------------------------
module Data.Csv.Conduit (
    -- * Types
        CsvParseError(..)
    -- * Conduits
    ,   fromCsv
    ,   fromCsvStreamError
    ,   toCsv
    ) where

import LocalPrelude

import Control.Monad.Error.Class ( MonadError(..) )
import Data.Bifunctor ( first )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit ( Conduit, await, yield )
import Data.Conduit.List ( map, mapM )
import Data.Csv ( FromRecord(..), ToRecord(..), DecodeOptions, EncodeOptions, HasHeader, encodeWith )
import Data.Csv.Incremental ( Parser(..), decodeWith )
import Data.Foldable ( mapM_ )

data CsvParseError =
        CsvParseError BS.ByteString String
    |   IncrementalError String

fromCsv :: (Show a, Monad m, FromRecord a, MonadError CsvParseError m) => DecodeOptions -> HasHeader -> Conduit BS.ByteString m a
fromCsv opts h = {-# SCC fromCsv_p #-} terminatingStreamParser $ decodeWith opts h

fromCsvStreamError :: (Monad m, FromRecord a) => DecodeOptions -> HasHeader -> Conduit BS.ByteString m (Either CsvParseError a)
fromCsvStreamError opts h = {-# SCC fromCsvStreamError_p #-} streamParser $ decodeWith opts h


toCsv :: (Monad m, ToRecord a) => EncodeOptions -> Conduit a m BS.ByteString
toCsv opts = {-# SCC toCsv_p #-} map $ BSL.toStrict . encodeWith opts . pure

-- helpers

streamParser :: (Monad m) => Parser a -> Conduit BS.ByteString m (Either CsvParseError a)
streamParser (Fail rest errMsg) = yield $ Left $ CsvParseError rest errMsg
streamParser (Many rs p) = do
    -- send the results down the stream..
    mapM_ (yield . first IncrementalError) rs
    -- wait for more..
    more <- await
    maybe (return ()) (streamParser . p) more
streamParser (Done rs) = mapM_ (yield . first IncrementalError) rs

terminatingStreamParser
    :: (Show a, Monad m, MonadError CsvParseError m)
    => Parser a
    -> Conduit BS.ByteString m a
terminatingStreamParser (Fail rest errMsg) = {-# SCC terminatingStreamParser_Fail_p #-} mapM $ const $ throwError $ CsvParseError rest errMsg
terminatingStreamParser (Many ers p) = {-# SCC terminatingStreamParser_Many_p #-}
    let
        errorHandler :: (Monad m, MonadError CsvParseError m) => String -> Conduit BS.ByteString m a
        errorHandler = mapM . const . throwError . IncrementalError

        safeResultHandler
            :: (Show a, Monad m, MonadError CsvParseError m)
            => (BS.ByteString -> Parser a)
            -> (Parser a -> Conduit BS.ByteString m a)
            -> [a]
            -> Conduit BS.ByteString m a
        safeResultHandler p' f rs = do
            mapM_ yield rs
            -- wait for more..
            await >>= maybe (return ()) (f . p')
    in
        -- send the results down the stream..
        either errorHandler (safeResultHandler p terminatingStreamParser) (sequence ers)
terminatingStreamParser (Done rs) = {-# SCC terminatingStreamParser_Done_p #-} either (mapM . const . throwError . IncrementalError) (mapM_ yield) (sequence rs)

