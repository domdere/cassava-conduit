# cassava-conduit [![Build Status](https://img.shields.io/travis/domdere/cassava-conduit.svg?style=flat)](https://travis-ci.org/domdere/cassava-conduit) [![Hackage](https://img.shields.io/hackage/v/cassava-conduit.svg?style=flat)](https://hackage.haskell.org/package/cassava-conduit)

Conduit interface for cassava package

Streaming to CSV is not 100% complete at this stage, and doesn't support encoding to CSV with a header yet

## Example Usage

### The examples project

There is a project containing some examples of the usage, but the gist is here:

``` Haskell

import Data.Csv
import Data.Conduit
import Data.Csv.Conduit

data InputRecord = ...

instance FromRecord InputRecord where
    ...

data OutputRecord = ...

instance ToRecord OutputRecord where
    ...

decodeOpts :: Word8 -> DecodeOptions

encodeOpts :: Word8 -> EncodeOptions

processInput :: InputRecord -> OutputRecord

-- |
--  A Conduit pipeline that streams from '../exampledata/sampleinput.psv', decodes it from a pipe seperated format,
--  processes it with 'processInput' and the encodes it to pipe seperated format and streams it out to '../exampledata/sampleoutput.psv'
--  The first time it encounters a parse error, it will stop streaming and return the error, dropping any decoded records that came through in that batch also...
--
conduitPipeline :: (MonadError CsvParseError m, MonadResource m) => m ()
conduitPipeline = sourceFile "../exampledata/sampleinput.psv" $$ fromCsv (decodeOpts $ fromIntegral $ ord '|') HasHeader =$= map processInput =$= toCsv (encodeOpts $ fromIntegral $ ord '|') =$= sinkFile "../exampledata/sampleoutput.psv"

main :: IO ()
main = do
    res <- runEitherT $ bimapEitherT showError id $ runResourceT conduitPipeline
    either putStrLn return res
```

#### Building the examples project

```
$ cd examples
$ cabal sandbox init
$ cabal sandbox add-source ../
$ cabal install --only-dependencies
$ cabal build
```

## Building the project

``` Shell
./mafia build
```

## Running Unit Tests

``` Shell
./mafia test
```
