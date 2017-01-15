{-# LANGUAGE DataKinds #-}
module Main where

import Protolude
import DataForecast

main :: IO ()
main = print (raw 4 :: TimeSeries '[ 'Day ])
