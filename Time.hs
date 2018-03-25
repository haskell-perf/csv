module Main (main) where

import Control.Monad
import Criterion.Main
import Criterion.Types
import System.Directory


main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    []
