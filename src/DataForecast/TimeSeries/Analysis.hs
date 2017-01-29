{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module DataForecast.TimeSeries.Analysis
    ( mean
    , std
    ) where

import DataForecast.Prelude
import DataForecast.TimeSeries
import DataForecast.TimeSeries.Aggregate


-- | Return the mean of the timeseries.
--
-- It is possible for 'mean' to be undefined and return 'Nothing' if given an
-- empty 'TimeSeries'. If that's the case the caller can determine what a
-- reasonable fallback is.
mean :: TimeSeries (p ': rest) -> Maybe Double
mean = sdmean . getSD . computeMean


-- | Compute the standard deviation for the time series across the top
-- dimension.
--
-- Note:
std :: TimeSeries (p ': rest) -> Maybe Double
std ts =
    let
        subpartTotals = catMaybes . map (sdtotal . getSD) . subs . getSub $ ts
        mMeans = repeat <$> mean ts
        mDistances = zipWith (-) subpartTotals <$> mMeans
        n = length . subs . getSub $ ts
    in
        case mDistances of
            Just distances ->
                let x = (sum . map (^(2 :: Integer)) $ distances)
                        / fromIntegral n
                in
                    Just $ sqrt x
            Nothing ->
                Nothing
