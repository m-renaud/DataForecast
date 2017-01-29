{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : DataForecast.TimeSeries
Description : Time series analysis.
-}
module DataForecast.TimeSeries
    (
    -- * TimeSeries
      TimeSeries(..)
    , getSD
    , setSD
    , getSub
    , setSub
    -- ** Construction.
    , BuildTS(..)
    , raw
    , fromRawData
    , fromParts

    -- * TsPeriod
    , TsPeriod(..)
    , SPeriod(..)

    -- * SummaryData
    , SummaryData(..)
    , defaultSummary
    , summaryWithTotal
    , setSdTotal
    , setSdMean

    -- * Subparts
    , Subparts(..)
    ) where

import DataForecast.Prelude


-- | Time series date representation with the resolution breakdown encoded in the
-- type.
--
-- 'parts' has kind '[TsPeriod]' which represents the hierarchical breakdown of
-- your data. You are responsible for providing the correct type hierarchy for
-- your data initially, but once you have done that the library ensures that all
-- analysis performed is valid.
--
-- In general you should not use the 'TimeSeries' constructor directly, instead
-- use the 'raw' and 'fromParts' construction functions defined below.
data TimeSeries (parts :: [TsPeriod]) where
    TimeSeries ::
        SPeriod p
        -> SummaryData
        -> Subparts subparts
        -> TimeSeries (p ': subparts)
deriving instance Eq (TimeSeries parts)
deriving instance Show (TimeSeries parts)


-- | Get the 'SummaryData' out of a TimeSeries.
getSD :: TimeSeries parts -> SummaryData
getSD (TimeSeries _ sd _) = sd


-- | Set the 'SummaryData' field of a 'TimeSeries'. We can't use record syntax
-- because https://ghc.haskell.org/trac/ghc/ticket/2595 is not implemented.
setSD :: SummaryData -> TimeSeries parts -> TimeSeries parts
setSD sd (TimeSeries t _sd sub) = TimeSeries t sd sub


-- | Get the 'Subparts' out of a 'TimeSeries'.
getSub :: TimeSeries (p ': subparts) -> Subparts subparts
getSub (TimeSeries _ _ sub) = sub


-- | Set the 'Subparts' of the 'TimeSeries'.
setSub ::
    Subparts subparts
    -> TimeSeries (p ': subparts)
    -> TimeSeries (p ': subparts)
setSub sub (TimeSeries t sd _sub) = TimeSeries t sd sub


-- | Construct a leaf 'TimeSeries' with the given 'rawData'.
-- Usually you should not need to use this directly and can instead use
-- 'fromRawData'.
--
-- __Example__:
--
--   > raw 42 :: TimeSeries '[ 'Year ]
raw :: BuildTS p => Double -> TimeSeries '[p]
raw rawData = build (SummaryData (Just rawData) (Just rawData)) def


-- | Construct a 'TimeSeries' given the constituent raw data.
--
-- __Example__:
--
--   > yearByQuarter :: TimeSeries '[ 'Year, 'Quarter ]
--   > yearByQuarter = fromRawData [ 10, 20, 30, 15 ]
fromRawData :: (BuildTS p, BuildTS parts) => [Double] -> TimeSeries '[p, parts]
fromRawData = fromParts . fmap raw

-- | Construct a 'TimeSeries' given the constituent 'TimeSeries' parts.
fromParts :: BuildTS part
    => [TimeSeries subparts]
    -> TimeSeries (part ': subparts)
fromParts subseries = build def (Subparts subseries)



-- | Typeclass for constructing 'TimeSeries' with default data.
--
-- This is used by several of the 'TimeSeries' construction functions such as
-- 'raw'. It provides a convenient way for constructing time series of different
-- types with the same functions and allowing the concrete type to be pinned down
-- by a type signature.
class BuildTS p where
    build :: SummaryData -> Subparts rest -> TimeSeries (p ': rest)

instance BuildTS 'Year where
    build = TimeSeries SYear
instance BuildTS 'Quarter where
    build = TimeSeries SQuarter
instance BuildTS 'Month where
    build = TimeSeries SMonth
instance BuildTS 'Week where
    build = TimeSeries SWeek
instance BuildTS 'Day where
    build = TimeSeries SDay



-- | Use promoted constructors to represent the different timeseries that we have.
-- When using these as kinds you must prefix them with a single quote (').
--
-- __Example__:
--
--   > raw 4 :: TimeSeries '[ 'Day ]
data TsPeriod
    = Decade
    | Year
    | Quarter
    | Month
    | Week
    | Day
    deriving (Eq, Show)


-- | The singleton for the promoted 'TsPeriod' type.
--
-- See
-- https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell#singleton-patterns
-- for details.
data SPeriod (x :: TsPeriod) where
    SDecade :: SPeriod 'Decade
    SYear :: SPeriod 'Year
    SQuarter :: SPeriod 'Quarter
    SMonth :: SPeriod 'Month
    SWeek :: SPeriod 'Week
    SDay :: SPeriod 'Day
deriving instance Eq (SPeriod x)
deriving instance Show (SPeriod x)



-- | The summarization of the entire time series rooted at the current
-- 'TimeSeries.
data SummaryData = SummaryData
    { sdtotal :: Maybe Double
    , sdmean :: Maybe Double
    }
deriving instance Eq SummaryData
deriving instance Show SummaryData

instance Default SummaryData where
    def = SummaryData Nothing Nothing


-- | The default 'SummaryData' with 'Nothing' for the 'sdtotal' and 'sdmean'.
defaultSummary :: SummaryData
defaultSummary = def


-- | Construct a 'SummaryData' with the 'sdtotal' set.
summaryWithTotal :: Double -> SummaryData
summaryWithTotal total = setSdTotal total defaultSummary


-- | Set the total for the 'SummaryData'.
setSdTotal :: Double -> SummaryData -> SummaryData
setSdTotal total sd = sd { sdtotal = Just total }


-- | Set the mean for the 'SummaryData'.
setSdMean :: Double -> SummaryData -> SummaryData
setSdMean mean sd = sd { sdmean = Just mean }



-- | The constituent components of a 'TimeSeries'. If the 'TimeSeries' is just a
-- leaf with raw data then 'subs' will be 'Nothing'.
data Subparts (parts :: [TsPeriod]) = Subparts
    { subs :: [TimeSeries parts]
    }
deriving instance Eq (Subparts parts)
deriving instance Show (Subparts parts)

instance Default (Subparts '[]) where
    def = Subparts []
