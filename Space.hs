{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Example uses of comparing map-like data structures.

module Main where

import Weigh

-- | Weigh maps.
main :: IO ()
main = mainWith (do setColumns [Case, Allocated, Max, Live, GCs])
