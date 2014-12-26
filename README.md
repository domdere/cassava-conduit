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

Install the dependencies first with either:

    cabal install --only-dependencies

If you do not wish to build tests or benchmarks, or:

    cabal install --only-dependencies --enable-tests

If you want to be able to build the tests, or:

    cabal install --only-dependencies --enable-benchmarks

If you wish to build the benchmarks.

The project must be "configured" at least once everytime `cassava-conduit.cabal` changes, this can be done with:

    cabal configure

If you wish to run the unit tests you will have to run:

    cabal configure --enable-tests

If you wish to run benchmarks you will have to run:

    cabal configure --enable-benchmarks

At the moment there are issues with using both flags at the same time.  Its recommended that you use one flag at a time, use `cabal-dev` or `cabal sandbox` 
(see below), and clear your sandbox when switching configurations from one to the other

Then finally build it with:

    cabal build

See `cabal build --help` for more build options.

## Running Unit Tests

**After** running `cabal build`, you can run the unit tests with the command:

    cabal test

## Adding Unit tests

Unit tests are written with [**doctest**] [doctest-github], for instructions on how to add unit tests
see the **doctest** [**User Guide**] [doctest-userguide].

Currently only files in the `src/` directory are searched for tests, it is assumed that the code in `main/`
is a thin layer of code that uses modules from `src/`.

## Running Benchmarks

**After** running `cabal configure --enable-benchmarks` and `cabal build`, the following command will run the benchmarks:

    cabal bench

For newer versions of `cabal`, `cabal bench` will run a `cabal build` automatically if necessary..

## Development: Cabal Dependency Hell?

Cabal's great, but its got its own warts, and when you are developing a few different projects with their own dependency chains, sometimes installing all your libraries to the same place causes problems,

### Cabal version < 1.18

Consider trying [`cabal-dev`] [cabal-dev].  Install it with `cabal install cabal-dev`

In terms of using it, all thats required is replacing `cabal` with `cabal-dev` in all the above command lines.

It will download and install all the dependencies for your project and install them in a `cabal-dev/` directory in your project directory, and they will only be used for this project.

### Cabal version >= 1.18

Cabal version `1.18` and onwards supports sandboxes, which is basically the same idea as `cabal-dev`.

In terms of using it all the commands remain the same, just run `cabal sandbox init` in the root directory of the project before running any of them.

------

The related `cabal-dev` and `sandbox` artifacts are already contained in the `.gitignore` file.

[cabal-dev]: https://github.com/creswick/cabal-dev "creswick/cabal-dev on GitHub.com"
[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest-haskell on GitHub.com"
[doctest-userguide]: https://github.com/sol/doctest-haskell/blob/master/README.markdown#usage "doctest Usage Guide"
