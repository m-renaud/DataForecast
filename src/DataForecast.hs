{-|
Module      : DataForecast
Description : DataForecast's main module

Top level module for DataForcest. Simply re-exports functionality from sub-modules.
-}
module DataForecast
    ( module DataForecast.TimeSeries
    , module DataForecast.TimeSeries.Aggregate
    ) where

import DataForecast.TimeSeries
import DataForecast.TimeSeries.Aggregate

