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
    ,   fromNamedCsv
    ,   fromCsvStreamError
    ,   fromNamedCsvStreamError
    ,   toCsv
    ) where

import LocalPrelude

import Control.Monad.Error.Class ( MonadError(..) )
import Data.Bifunctor ( first )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit ( Conduit, await, yield )
import Data.Conduit.List ( map, mapM )
import Data.Csv ( FromNamedRecord, FromRecord, ToRecord, DecodeOptions, EncodeOptions, HasHeader, encodeWith )
import Data.Csv.Incremental ( HeaderParser(..), Parser(..), decodeByNameWith, decodeWith )
import Data.Foldable ( mapM_ )

data CsvParseError =
        CsvParseError BS.ByteString String
    |   IncrementalError String

-- |
-- Streams parsed records, Errors are not received in the stream but instead after the pipeline is executed,
-- If you want to handle errors as they come and resume, see `fromCsvStreamError`
--
fromCsv :: (Show a, Monad m, FromRecord a, MonadError CsvParseError m) => DecodeOptions -> HasHeader -> Conduit BS.ByteString m a
fromCsv opts h = {-# SCC fromCsv_p #-} terminatingStreamParser $ decodeWith opts h

-- |
-- Parses an instance of `FromNamedRecord`, this conduit drops the Header
--
-- Errors are not seen in the pipeline but rather at the end after executing the pipeline, if you want to handle the errors
-- as they occur, try `fromNamedCsvStreamError` instead.
--
fromNamedCsv :: (Show a, Monad m, FromNamedRecord a, MonadError CsvParseError m) => DecodeOptions -> Conduit BS.ByteString m a
fromNamedCsv opts = {-# SCC fromNamedCsv_p #-} terminatingStreamHeaderParser $ decodeByNameWith opts

-- |
-- Same as `fromCsv` but allows for errors to be handled in the pipeline instead
--
fromCsvStreamError :: (Monad m, FromRecord a) => DecodeOptions -> HasHeader -> Conduit BS.ByteString m (Either CsvParseError a)
fromCsvStreamError opts h = {-# SCC fromCsvStreamError_p #-} streamParser $ decodeWith opts h

-- |
-- Like `fromNamedCsvStream` but allows for errors to be handled in the pipeline itself.
--
fromNamedCsvStreamError :: (Monad m, FromNamedRecord a) => DecodeOptions -> Conduit BS.ByteString m (Either CsvParseError a)
fromNamedCsvStreamError opts = {-# SCC fromCsvStreamError_p #-} streamHeaderParser $ decodeByNameWith opts

-- |
-- Streams from csv to text, does not create headers...
--
toCsv :: (Monad m, ToRecord a) => EncodeOptions -> Conduit a m BS.ByteString
toCsv opts = {-# SCC toCsv_p #-} map $ BSL.toStrict . encodeWith opts . pure

-- helpers

streamHeaderParser :: (Monad m) => HeaderParser (Parser a) -> Conduit BS.ByteString m (Either CsvParseError a)
streamHeaderParser (FailH rest errMsg)  = {-# SCC streamHeaderParser_FailH_p #-} yield $ Left $ CsvParseError rest errMsg
streamHeaderParser (PartialH p)         = {-# SCC streamHeaderParser_PartialH_p #-} await >>= maybe (return ()) (streamHeaderParser . p)
streamHeaderParser (DoneH _ p)          = {-# SCC streamHeaderParser_DoneH_p #-} streamParser p

streamParser :: (Monad m) => Parser a -> Conduit BS.ByteString m (Either CsvParseError a)
streamParser (Fail rest errMsg) = yield $ Left $ CsvParseError rest errMsg
streamParser (Many rs p) = do
    -- send the results down the stream..
    mapM_ (yield . first IncrementalError) rs
    -- wait for more..
    more <- await
    maybe (return ()) (streamParser . p) more
streamParser (Done rs) = mapM_ (yield . first IncrementalError) rs

terminatingStreamHeaderParser
    :: (Show a, Monad m, MonadError CsvParseError m)
    => HeaderParser (Parser a)
    -> Conduit BS.ByteString m a
terminatingStreamHeaderParser (FailH rest errMsg)   = {-# SCC terminatingStreamHeaderParser_FailH_p #-} mapM $ const $ throwError $ CsvParseError rest errMsg
terminatingStreamHeaderParser (PartialH p)          = {-# SCC terminatingStreamHeaderParser_PartialH_p #-} await >>= maybe (return ()) (terminatingStreamHeaderParser . p)
terminatingStreamHeaderParser (DoneH _ p)           = {-# SCC terminatingStreamHeaderParser_DoneH_p #-} terminatingStreamParser p

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

