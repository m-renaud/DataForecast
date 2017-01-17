{-# LANGUAGE GADTs #-}
{-|
Module      : DataForecast.TimeSeries.Aggregate
Description : Aggregation functions over 'TimeSeries'.
-}
module DataForecast.TimeSeries.Aggregate
    ( computeTotal
    , computeMean
    ) where

import DataForecast.Prelude
import DataForecast.TimeSeries
import DataForecast.TimeSeries.Internal


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
                    map computeTotal subparts
                subpartTotal =
                    sum . map getTotalOrZero $ newSubparts
                newSD =
                    setSdTotal subpartTotal (getSD tts)
            in
                setSD newSD (setSub (Subparts newSubparts) tts)


-- | Compute the mean of the time series.
--
-- If there is any missing data in the timeseries then the mean is left as
-- 'Nothing'.
computeMean :: TimeSeries parts -> TimeSeries parts
computeMean ts@TimeSeries{} =
    case sdmean . getSD $ ts of
        Just _mean ->
            ts

        Nothing ->
            let
                tsWithTotals = computeTotal ts
                newSubparts = map computeMean . subs . getSub $ tsWithTotals

                mTotal = sdtotal . getSD $ tsWithTotals
                n = length newSubparts

                mMean =
                    if n == 0 then
                        mTotal
                    else
                        (/ fromIntegral n) <$> mTotal
                newSD = setSdMeanInternal mMean (getSD tsWithTotals)

            in
                setSD newSD (setSub (Subparts newSubparts) tsWithTotals)


-- | Get the total for the given 'TimeSeries' or `0` if it's not computed.
--
-- TODO: Consider changing this function to compute the total.
getTotalOrZero :: TimeSeries parts -> Double
getTotalOrZero = fromMaybe 0 . sdtotal . getSD
