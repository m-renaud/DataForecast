{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module DataForecast.TimeSeries.Analysis
    ( mean
    , err
    , errSquared
    , sse
    , mse
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


-- | Compute the error for the 'TimeSeries'.
err :: TimeSeries (p ': rest) -> Maybe [Double]
err ts =
    let
        subparts = subs . getSub $ ts
        totals = map (sdtotal . getSD) subparts
        tsMean = mean ts
        errors = zipWith (\t m -> (-) <$> t <*> m) totals (repeat tsMean)
    in
        sequence errors


-- | Compute the error squared for the 'TimeSeries'.
errSquared :: TimeSeries (p ': rest) -> Maybe [Double]
errSquared =
    fmap (map (\x -> x * x)) . err


-- | Sum of squared errors.
sse :: TimeSeries (p ': rest) -> Maybe Double
sse =
    fmap sum . errSquared


-- | Mean of the squared errors.
mse :: TimeSeries (p ': rest) -> Maybe Double
mse =
    fmap numericallyStableMean . errSquared


-- From https://hackage.haskell.org/package/hstats-0.3/docs/src/Math-Statistics.html#mean
-- Not currently in stackage.
numericallyStableMean :: Floating a => [a] -> a
numericallyStableMean =
    fst . foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0)

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
