{-# LANGUAGE GADTs #-}
{-|
Module      : DataForecast.TimeSeries.Aggregate
Description : Aggregation functions over 'TimeSeries'.
-}
module DataForecast.TimeSeries.Aggregate
    ( computeTotal
    ) where

import DataForecast.Prelude
import DataForecast.TimeSeries


-- | Compute the total for each 'SummaryData' in the 'TimeSeries'.
--
-- This propogates totals bottom up from the raw leaf data to the
-- root and updating all 'SummaryData' nodes along the way.
computeTotal :: TimeSeries parts -> TimeSeries parts
computeTotal tts@TimeSeries{} =
    case sdtotal . getSD $ tts of
        Just _total ->
            tts
        Nothing ->
            let
                subparts =
                    subs . getSub $ tts
                newSubparts =
                    fmap (map computeTotal) subparts
                subpartTotal =
                    fromMaybe 0 . fmap (sum . map getTotalOrZero) $ newSubparts
                newSD =
                    setSdTotal subpartTotal (getSD tts)
            in
                setSD newSD (setSub (Subparts newSubparts) tts)

-- | Get the total for the given 'TimeSeries' or `0` if it's not computed.
--
-- TODO: Consider changing this function to compute the total.
getTotalOrZero :: TimeSeries parts -> Double
getTotalOrZero = fromMaybe 0 . sdtotal . getSD
