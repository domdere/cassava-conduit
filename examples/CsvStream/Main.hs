{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Data.Csv.Conduit

import Prelude ( Show(..), Eq(..), Num(..) , IO, String, (++), fromIntegral, maybe )
import Control.Applicative ( Applicative(..) )
import Control.Monad ( Monad(..) )
import Control.Monad.Error.Class ( MonadError, throwError )
import Control.Monad.Trans.Except ( withExceptT , runExceptT )
import Control.Monad.Trans.Resource ( MonadResource, runResourceT )
import qualified Data.ByteString.Char8 as BS
import Data.Int ( Int )
import Data.Char ( ord )
import Data.Csv ( DecodeOptions(..), EncodeOptions(..), HasHeader(..), FromRecord(..), ToRecord(..), (.!), defaultDecodeOptions, defaultEncodeOptions, record, toField, FromNamedRecord(..) , (.:) )
import Data.Conduit ( ($$), (=$=), ConduitT, await, yield )
import Data.Conduit.Binary ( sourceFile, sinkFile )
import Data.Conduit.List ( map )
import Data.Either ( either, Either )
import Data.Function ( ($), (.), id )
import Data.Functor ( (<$>) )
import qualified Data.Text as T
import Data.Word ( Word8 )
import System.IO ( putStrLn )

-- Types

newtype InputId = InputId { runInputId :: Int } deriving (Show, Eq)
newtype Operand1 = Operand1 { runOperand1 :: Int } deriving (Show, Eq)
newtype Operand2 = Operand2 { runOperand2 :: Int } deriving (Show, Eq)

data InputRecord = InputRecord
    {   _inputId    :: InputId
    ,   _operand1   :: Operand1
    ,   _operand2   :: Operand2
    }   deriving (Show, Eq)

-- Instances

instance FromRecord InputRecord where
--  parseRecord :: Record -> Parser a
    parseRecord r =
            InputRecord
        <$> (InputId <$> r .! 0)
        <*> (Operand1 <$> r .! 1)
        <*> (Operand2 <$> r .! 2)

instance FromNamedRecord InputRecord where
--  parseRecord :: Record -> Parser a
    parseNamedRecord r =
            InputRecord
        <$> (InputId <$> r .: "index")
        <*> (Operand1 <$> r .: "operand1")
        <*> (Operand2 <$> r .: "operand2")

newtype OutputId    = OutputId { _runOutputId :: Int } deriving (Show, Eq)
newtype SumResult   = SumResult { _runSumResult :: Int } deriving (Show, Eq)

data OutputRecord = OutputRecord
    {   _outputId :: OutputId
    ,   _result   :: SumResult
    }   deriving (Show, Eq)

-- Instances

instance ToRecord OutputRecord where
--  toRecord :: a -> Record
    toRecord (OutputRecord ident res) = record
        [   toField (_runOutputId ident)
        ,   toField (_runSumResult res)
        ]

decodeOpts :: Word8 -> DecodeOptions
decodeOpts delim = defaultDecodeOptions { decDelimiter = delim }

encodeOpts :: Word8 -> EncodeOptions
encodeOpts delim = defaultEncodeOptions
    {   encDelimiter = delim
    }

-- Functions

-- This is the function will be process each record with as we stream them through.
processInput :: InputRecord -> OutputRecord
processInput = OutputRecord
    <$> getOutputId . _inputId
    <*> (makeResult <$> _operand1 <*> _operand2)

-- Conduit Pipeline
conduitPipeline :: (MonadError CsvParseError m, MonadResource m) => String -> String -> m ()
conduitPipeline fpath opath = sourceFile fpath $$ fromCsv (decodeOpts $ fromIntegral $ ord '|') HasHeader =$= map processInput =$= toCsv (encodeOpts $ fromIntegral $ ord '|') =$= sinkFile opath 

stopOnFirstError :: (MonadError CsvParseError m, Monad m) => ConduitT (Either CsvStreamRecordParseError InputRecord) InputRecord m ()
stopOnFirstError = await >>= maybe (return ()) (either (\(CsvStreamRecordParseError errMsg) -> throwError (IncrementalError errMsg)) (\value -> yield value >> stopOnFirstError))

-- Conduit Pipeline
conduitPipeline2 :: (MonadError CsvParseError m, MonadResource m) => String -> String -> m ()
conduitPipeline2 fpath opath = sourceFile fpath $$ fromNamedCsvStreamError (decodeOpts $ fromIntegral $ ord '|') (\(HaltingCsvParseError rest err) -> CsvParseError rest err) =$= stopOnFirstError =$= map processInput =$= toCsv (encodeOpts $ fromIntegral $ ord '|') =$= sinkFile opath 

showError :: CsvParseError -> String
showError (CsvParseError rem err)   = "Csv Parse Error: '" ++ T.unpack err ++ "', remaining input: '" ++ BS.unpack rem ++ "'"
showError (IncrementalError err)    = "Csv Parse Error: '" ++ T.unpack err ++ "'"

main :: IO ()
main = do
    res <- runResourceT . runExceptT . withExceptT showError $ conduitPipeline "../exampledata/sampleinput.psv" "../exampledata/sampleoutput.psv"
    either putStrLn return res

    res <- runResourceT . runExceptT . withExceptT showError $ conduitPipeline2 "../exampledata/sampleinput2.psv" "../exampledata/sampleoutput2.psv"
    either putStrLn return res

-- helpers

getOutputId :: InputId -> OutputId
getOutputId = OutputId . runInputId

makeResult :: Operand1 -> Operand2 -> SumResult
makeResult op1 op2 = SumResult $ runOperand1 op1 + runOperand2 op2

