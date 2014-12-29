# 0.1.x

## 0.0.1 -> 0.1.0

``` Haskell
fromNamedCsv :: (Show a, Monad m, FromNamedRecord a, MonadError CsvParseError m) => DecodeOptions -> Conduit BS.ByteString m a
fromNamedCsvStreamError :: (Monad m, FromNamedRecord a) => DecodeOptions -> Conduit BS.ByteString m (Either CsvParseError a)
```
