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
