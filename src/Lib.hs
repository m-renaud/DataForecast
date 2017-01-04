{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DataKinds #-}
{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib where


import Lib.Prelude


-- Use DataKinds to represent the different timeseries that we have.
data DecadeTS
deriving instance Show DecadeTS
data YearTS
deriving instance Show YearTS
data QuarterTS 
deriving instance Show QuarterTS
data MonthTS 
deriving instance Show MonthTS
data DayTS
deriving instance Show DayTS


-- Use a GADT to track the type of the time series so we never make invalid
-- comparisons.
--
-- For example, it is only valid to find the difference between times eries of
-- the same type.
--
-- We use a common type because all time series share many operations.
data TimeSeries (p :: *) (c :: *) where
    DecadeByYearTS :: TimeSeriesData YearTS -> TimeSeries DecadeTS YearTS
    YearByQuarterTS :: TimeSeriesData QuarterTS -> TimeSeries YearTS QuarterTS
    YearByMonthTS :: TimeSeriesData MonthTS -> TimeSeries YearTS MonthTS
    QuarterByMonthTS :: TimeSeriesData MonthTS -> TimeSeries QuarterTS MonthTS
    MonthByDayTS :: TimeSeriesData DayTS -> TimeSeries MonthTS DayTS
    DayValue :: TimeSeriesData () -> TimeSeries DayTS ()

deriving instance (Show p, Show c) => Show (TimeSeries p c)

class GTimeSeries p c where
    -- | Construct a TimeSeries from the constituent 'TimeSeriesData'.
    construct :: TimeSeriesData c -> TimeSeries p c

    getData :: TimeSeries p c -> TimeSeriesData c

    getTotal :: TimeSeries p c -> Double
    getTotal = const 0

fromConstituents :: (Show d, GTimeSeries p c) => [TimeSeries c d] -> TimeSeries p c
fromConstituents constituents =
    construct (TimeSeriesData Nothing Nothing (Just constituents))    

    
instance GTimeSeries YearTS MonthTS where
    construct = YearByMonthTS
    getData (YearByMonthTS tsData) = tsData

instance GTimeSeries MonthTS DayTS where
    construct = MonthByDayTS
    getData (MonthByDayTS tsData) = tsData

instance GTimeSeries DayTS () where
    construct = DayValue
    getData (DayValue tsData) = tsData


-- | Numerical data about a time series.
data TimeSeriesData c = forall d. Show d => TimeSeriesData
    { seriesTotal :: Maybe Double  -- ^ The total across the time series.
    , seriesMean :: Maybe Double  -- ^ The average per constituent time series.
    , seriesConstituents :: Maybe [TimeSeries c d]
    }

deriving instance Show c => Show (TimeSeriesData c)


emptyConstTest :: TimeSeriesData MonthTS
emptyConstTest = TimeSeriesData (Just 100) Nothing (Nothing :: Maybe [TimeSeries MonthTS DayTS])

tTotal :: TimeSeries p c -> Double
tTotal (DecadeByYearTS tsData) = fromMaybe 0 . seriesTotal $ tsData
tTotal (YearByQuarterTS tsData) = fromMaybe 0 . seriesTotal $ tsData
tTotal (YearByMonthTS tsData) = fromMaybe 0 . seriesTotal $ tsData
tTotal (QuarterByMonthTS tsData) = fromMaybe 0 . seriesTotal $ tsData
tTotal (MonthByDayTS tsData) = fromMaybe 0 . seriesTotal $ tsData
tTotal (DayValue tsData) = fromMaybe 0 . seriesTotal $ tsData
    

computeTsTotal :: TimeSeries p c -> TimeSeries p c
computeTsTotal (DecadeByYearTS tsData) = DecadeByYearTS (computeTotal tsData)
computeTsTotal (YearByQuarterTS tsData) = YearByQuarterTS (computeTotal tsData)
computeTsTotal (YearByMonthTS tsData) = YearByMonthTS (computeTotal tsData)
computeTsTotal (QuarterByMonthTS tsData) = QuarterByMonthTS (computeTotal tsData)
computeTsTotal (MonthByDayTS tsData) = MonthByDayTS (computeTotal tsData)
computeTsTotal (DayValue tsData) = DayValue (computeTotal tsData)

computeTotal :: TimeSeriesData c -> TimeSeriesData c
computeTotal tseriesData@TimeSeriesData{seriesTotal, seriesMean, seriesConstituents} =
    case seriesTotal of
        Just _total ->
            tseriesData
        Nothing ->
            case seriesConstituents of
                Just constituents ->
                    let
                        updatedConstituents = map computeTsTotal constituents
                        total = sum . map tTotal $ updatedConstituents
                    in
                        TimeSeriesData{ seriesConstituents = Just updatedConstituents
                                   , seriesTotal = Just total
                                   , seriesMean
                                   }
                Nothing ->
                    tseriesData

numMean :: (Fractional a) => [a] -> a
numMean nums =
    let
        numsLength = length nums
        numsSum = sum nums
    in
        numsSum / (fromIntegral numsLength)

