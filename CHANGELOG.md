# 0.4.x

## 0.4.0.1 -> 0.4.0.2

- Bumped `conduit` to `1.3.*` (from `1.2.*`)
- The `conduit-extra` dep was unecessary

## 0.3.5.1 -> 0.4.0.0

`cassava` was bumped from `0.4.*` to `0.5.*`.

There were some semantic changes going from cassava 0.4.* -> 0.5.*, see here

But I don't think they muddle with any of the explicit cassava-conduit semantics:

- `QuoteMinimal` semantics are not defined by `cassava-conduit`, so it will change behaviour, but 
- `cassava-conduit` won't compensate for it (hence the bump from `0.3` to `0.4`).
- It doesn't use `foldl'`
- cassava-conduit doesn't use `encodeByNamedWith` so the `encIncludeHeader` flag shouldn't have any effect.

# 0.3.x

## 0.2.2 -> 0.3.0

Some new error types, and error contain `T.Text` and not `String` now

``` Haskell
data CsvParseError =
        CsvParseError BS.ByteString T.Text
    |   IncrementalError T.Text
        deriving (Show, Eq)

-- | When you want to include errors in the stream, this error type represents errors that halt the stream.
-- They do not appear inside the conduit and will instead get returned from running the conduit.
--
data CsvStreamHaltParseError = HaltingCsvParseError BS.ByteString T.Text -- ^ the remaining bytestring that was read in but not parsed yet, and the stringy error msg describing the fail.
    deriving (Show, Eq)

-- | When you want to include errors in the stream, these are the errors that can be included in the stream,
-- they are usually problems restricted to individual records, and streaming can resume from the next record
-- you just have to decide on something sensible to do with the per record errors.
--
data CsvStreamRecordParseError = CsvStreamRecordParseError T.Text deriving (Show, Eq) -- ^ The stringy error describing why this record could not be parsed.
```

New error types are to separate out errors that stop streaming (and hence imply there are valid records that might be omitted) and errors that can be skipped, allowing valid records after to be processed...

``` Haskell
-- |
-- Same as `fromCsv` but allows for errors to be handled in the pipeline instead
--
fromCsvStreamError :: (FromRecord a, MonadError e m) => DecodeOptions -> HasHeader -> (CsvStreamHaltParseError -> e) -> Conduit BS.ByteString m (Either CsvStreamRecordParseError a)
fromCsvStreamError opts h f = {-# SCC fromCsvStreamError_p #-} streamParser f $ decodeWith opts h

-- |
-- Like `fromNamedCsvStream` but allows for errors to be handled in the pipeline itself.
--
fromNamedCsvStreamError :: (FromNamedRecord a, MonadError e m) => DecodeOptions -> (CsvStreamHaltParseError -> e) -> Conduit BS.ByteString m (Either CsvStreamRecordParseError a)
fromNamedCsvStreamError opts f = {-# SCC fromCsvStreamError_p #-} streamHeaderParser f $ decodeByNameWith opts
```

# 0.2.x

## 0.1.0 -> 0.2.0

``` Haskell
fromCsvLiftError :: (FromRecord a, MonadError e m) => (CsvParseError -> e) -> DecodeOptions -> HasHeader -> Conduit BS.ByteString m a
fromNamedCsvLiftError :: (FromNamedRecord a, MonadError e m) => (CsvParseError -> e) -> DecodeOptions -> Conduit BS.ByteString m a
```

# 0.1.x

## 0.0.1 -> 0.1.0

``` Haskell
fromNamedCsv :: (Show a, Monad m, FromNamedRecord a, MonadError CsvParseError m) => DecodeOptions -> Conduit BS.ByteString m a
fromNamedCsvStreamError :: (Monad m, FromNamedRecord a) => DecodeOptions -> Conduit BS.ByteString m (Either CsvParseError a)
```
