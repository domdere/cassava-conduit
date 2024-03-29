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
  , CsvStreamHaltParseError(..)
  , CsvStreamRecordParseError(..)
  -- * Conduits
  , fromCsv
  , fromCsvLiftError
  , fromNamedCsv
  , fromNamedCsvLiftError
  , fromCsvStreamError
  , fromCsvStreamErrorNoThrow
  , fromNamedCsvStreamError
  , fromNamedCsvStreamErrorNoThrow
  , toCsv
  ) where

import LocalPrelude

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Error.Class (MonadError(..))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit (ConduitT, await, yield)
import Data.Conduit.List (map, mapM)
import Data.Csv (FromNamedRecord, FromRecord, ToRecord, DecodeOptions, EncodeOptions, HasHeader, encodeWith)
import Data.Csv.Incremental (HeaderParser(..), Parser(..), decodeByNameWith, decodeWith)
import Data.Foldable (mapM_)
import qualified Data.Text as T

data CsvParseError =
    CsvParseError BS.ByteString T.Text
  | IncrementalError T.Text
    deriving (Show, Eq)

-- | When you want to include errors in the stream, this error type represents errors that halt the stream.
-- They do not appear inside the conduit and will instead get returned from running the conduit.
--
data CsvStreamHaltParseError =
  HaltingCsvParseError BS.ByteString T.Text -- ^ the remaining bytestring that was read in but not parsed yet, and the stringy error msg describing the fail.
  deriving (Show, Eq)

-- | When you want to include errors in the stream, these are the errors that can be included in the stream,
-- they are usually problems restricted to individual records, and streaming can resume from the next record
-- you just have to decide on something sensible to do with the per record errors.
--
data CsvStreamRecordParseError =
  CsvStreamRecordParseError T.Text -- ^ The stringy error describing why this record could not be parsed.
  deriving (Show, Eq)

-- |
-- Streams parsed records, Errors are not received in the stream but instead after the pipeline is executed,
-- If you want to handle errors as they come and resume, see `fromCsvStreamError`
--
fromCsv :: (FromRecord a, MonadError CsvParseError m) => DecodeOptions -> HasHeader -> ConduitT BS.ByteString a m ()
fromCsv = fromCsvLiftError id

-- |
-- Sometimes your pipeline will involve an error type other than `CsvParseError`, in which case if you provide
-- a function to project it into your custom error type, you can use this instead of `fromCsv`
--
fromCsvLiftError :: (FromRecord a, MonadError e m) => (CsvParseError -> e) -> DecodeOptions -> HasHeader -> ConduitT BS.ByteString a m ()
fromCsvLiftError f opts h = {-# SCC fromCsvLiftError_p #-} terminatingStreamParser f $ decodeWith opts h


-- |
-- Parses an instance of `FromNamedRecord`, this conduit drops the Header
--
-- Errors are not seen in the pipeline but rather at the end after executing the pipeline, if you want to handle the errors
-- as they occur, try `fromNamedCsvStreamError` instead.
--
fromNamedCsv :: (FromNamedRecord a, MonadError CsvParseError m) => DecodeOptions -> ConduitT BS.ByteString a m ()
fromNamedCsv = fromNamedCsvLiftError id

-- |
-- Sometimes your pipeline will involve an error type other than `CsvParseError`, in which case if you provide
-- a function to project it into your custom error type, you can use this instead of `fromCsv`
--
fromNamedCsvLiftError :: (FromNamedRecord a, MonadError e m) => (CsvParseError -> e) -> DecodeOptions -> ConduitT BS.ByteString a m ()
fromNamedCsvLiftError f opts = {-# SCC fromNamedCsv_p #-} terminatingStreamHeaderParser f $ decodeByNameWith opts

-- |
-- Same as 'fromCsv' but allows for errors to be handled in the pipeline instead
--
fromCsvStreamError :: (FromRecord a, MonadError e m) => DecodeOptions -> HasHeader -> (CsvStreamHaltParseError -> e) -> ConduitT BS.ByteString (Either CsvStreamRecordParseError a) m ()
fromCsvStreamError opts h f = {-# SCC fromCsvStreamError_p #-} streamParser f $ decodeWith opts h

-- |
-- Same as 'fromCsvStreamError' but allows for halting errors to be handled by the pipeline as well.
--
fromCsvStreamErrorNoThrow :: (Monad m, FromRecord a) => DecodeOptions -> HasHeader -> ConduitT BS.ByteString (Either (Either CsvStreamHaltParseError CsvStreamRecordParseError) a) m ()
fromCsvStreamErrorNoThrow opts h = {-# SCC fromCsvStreamErrorNoThrow_p #-} streamParserNoThrow $ decodeWith opts h

-- |
-- Like `fromNamedCsvStream` but allows for errors to be handled in the pipeline itself.
--
fromNamedCsvStreamError :: (FromNamedRecord a, MonadError e m) => DecodeOptions -> (CsvStreamHaltParseError -> e) -> ConduitT BS.ByteString (Either CsvStreamRecordParseError a) m ()
fromNamedCsvStreamError opts f = {-# SCC fromCsvStreamError_p #-} streamHeaderParser f $ decodeByNameWith opts

-- |
-- Like `fromNamedCsvStreamErrorNoThrow` but allows for errors to be handled in the pipeline itself.
--
fromNamedCsvStreamErrorNoThrow :: (Monad m, FromNamedRecord a) => DecodeOptions -> ConduitT BS.ByteString (Either (Either CsvStreamHaltParseError CsvStreamRecordParseError) a) m ()
fromNamedCsvStreamErrorNoThrow opts = {-# SCC fromCsvStreamErrorNoThrow_p #-} streamHeaderParserNoThrow $ decodeByNameWith opts

-- |
-- Streams from csv to text, does not create headers...
--
toCsv :: (Monad m, ToRecord a) => EncodeOptions -> ConduitT a BS.ByteString m ()
toCsv opts = {-# SCC toCsv_p #-} map $ BSL.toStrict . encodeWith opts . pure

-- helpers

streamHeaderParser :: (MonadError e m) => (CsvStreamHaltParseError -> e) -> HeaderParser (Parser a) -> ConduitT BS.ByteString (Either CsvStreamRecordParseError a) m ()
streamHeaderParser f (FailH rest errMsg)  = {-# SCC streamHeaderParser_FailH_p #-} lift . throwError . f $ HaltingCsvParseError rest (T.pack errMsg)
streamHeaderParser f (PartialH p)         = {-# SCC streamHeaderParser_PartialH_p #-} await >>= maybe (return ()) (streamHeaderParser f . p)
streamHeaderParser f (DoneH _ p)          = {-# SCC streamHeaderParser_DoneH_p #-} streamParser f p

streamHeaderParserNoThrow :: (Monad m) => HeaderParser (Parser a) -> ConduitT BS.ByteString (Either (Either CsvStreamHaltParseError CsvStreamRecordParseError) a) m ()
streamHeaderParserNoThrow (FailH rest errMsg)  = {-# SCC streamHeaderParserNoThrow_FailH_p #-} yield $ Left $ Left $ HaltingCsvParseError rest (T.pack errMsg)
streamHeaderParserNoThrow (PartialH p)         = {-# SCC streamHeaderParserNoThrow_PartialH_p #-} await >>= maybe (return ()) (streamHeaderParserNoThrow . p)
streamHeaderParserNoThrow (DoneH _ p)          = {-# SCC streamHeaderParserNoThrow_DoneH_p #-} streamParserNoThrow p

streamParser :: (MonadError e m) => (CsvStreamHaltParseError -> e) -> Parser a -> ConduitT BS.ByteString (Either CsvStreamRecordParseError a) m ()
streamParser f (Fail rest errMsg)   = {-# SCC streamParser_Fail_p #-} lift . throwError . f $ HaltingCsvParseError rest (T.pack errMsg)
streamParser f (Many rs p)          = {-# SCC streamParser_Many_p #-} do
  -- send the results down the stream..
  mapM_ (yield . first (CsvStreamRecordParseError . T.pack)) rs
  -- wait for more..
  more <- await
  streamParser f . p $ fromMaybe BS.empty more
streamParser _ (Done rs)            = {-# SCC streamParser_Done_p #-} mapM_ (yield . first (CsvStreamRecordParseError . T.pack)) rs

streamParserNoThrow :: (Monad m) => Parser a -> ConduitT BS.ByteString (Either (Either CsvStreamHaltParseError CsvStreamRecordParseError) a) m ()
streamParserNoThrow (Fail rest errMsg)   = {-# SCC streamParserNoThrow_Fail_p #-} yield $ Left $ Left $ HaltingCsvParseError rest (T.pack errMsg)
streamParserNoThrow (Many rs p)          = {-# SCC streamParserNoThrow_Many_p #-} do
  -- send the results down the stream..
  mapM_ (yield . first (Right . CsvStreamRecordParseError . T.pack)) rs
  -- wait for more..
  more <- await
  streamParserNoThrow . p $ fromMaybe BS.empty more
streamParserNoThrow (Done rs)            = {-# SCC streamParserNoThrow_Done_p #-} mapM_ (yield . first (Right . CsvStreamRecordParseError . T.pack)) rs

terminatingStreamHeaderParser
  :: (Monad m, MonadError e m)
  => (CsvParseError -> e)
  -> HeaderParser (Parser a)
  -> ConduitT BS.ByteString a m ()
terminatingStreamHeaderParser f (FailH rest errMsg)   = {-# SCC terminatingStreamHeaderParser_FailH_p #-} lift . throwError . f . CsvParseError rest . T.pack $ errMsg
terminatingStreamHeaderParser f (PartialH p)          = {-# SCC terminatingStreamHeaderParser_PartialH_p #-} await >>= maybe (return ()) (terminatingStreamHeaderParser f . p)
terminatingStreamHeaderParser f (DoneH _ p)           = {-# SCC terminatingStreamHeaderParser_DoneH_p #-} terminatingStreamParser f p

terminatingStreamParser
  :: (Monad m, MonadError e m)
  => (CsvParseError -> e)
  -> Parser a
  -> ConduitT BS.ByteString a m ()
terminatingStreamParser f (Fail rest errMsg)    = {-# SCC terminatingStreamParser_Fail_p #-} lift . throwError . f . CsvParseError rest . T.pack $ errMsg
terminatingStreamParser f (Many ers p)          = {-# SCC terminatingStreamParser_Many_p #-}
  let
    errorHandler :: (Monad m, MonadError e m) => (CsvParseError -> e) -> String -> ConduitT BS.ByteString a m ()
    errorHandler f' = mapM . const . throwError . f' . IncrementalError . T.pack

    safeResultHandler
      :: (Monad m)
      => (BS.ByteString -> Parser a)
      -> (Parser a -> ConduitT BS.ByteString a m ())
      -> [a]
      -> ConduitT BS.ByteString a m ()
    safeResultHandler p' f' rs = do
      mapM_ yield rs
      -- wait for more..
      await >>= f' . p' . fromMaybe BS.empty
  in
    -- send the results down the stream..
    either (errorHandler f) (safeResultHandler p (terminatingStreamParser f)) (sequence ers)
terminatingStreamParser f (Done rs) = {-# SCC terminatingStreamParser_Done_p #-} either (lift . throwError . f . IncrementalError . T.pack) (mapM_ yield) (sequence rs)

