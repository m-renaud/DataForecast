{-|
Module      : DataForecast
Description : DataForecast's main module

Top level module for DataForcest. Simply re-exports functionality from sub-modules.
-}
module DataForecast
    ( -- * Time series analysis.
      -- ** Data definitions and utility functions.
      module DataForecast.TimeSeries
      -- ** Aggregation across time series.
    , module DataForecast.TimeSeries.Aggregate
    ) where

import DataForecast.TimeSeries
import DataForecast.TimeSeries.Aggregate

