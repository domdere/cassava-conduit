module Main where

import qualified Test.Data.Csv.Conduit

import Control.Monad

import System.Exit
import System.IO

main :: IO ()
main = hSetBuffering stdout LineBuffering >> mapM id
  [ Test.Data.Csv.Conduit.tests
  ] >>= \rs -> when (not . all id $ rs) exitFailure
