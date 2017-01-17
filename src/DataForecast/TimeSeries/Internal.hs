module DataForecast.TimeSeries.Internal
    ( setSdMeanInternal
    ) where

import DataForecast.Prelude
import DataForecast.TimeSeries

-- | Set the internal mean representation for the 'SummaryData'.
--
-- Note: For internal library use only.
setSdMeanInternal :: Maybe Double -> SummaryData -> SummaryData
setSdMeanInternal mMean sd = sd { sdmean = mMean }


