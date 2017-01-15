{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
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
Module      : DataForecast
Description : DataForecast's main module

TODO
-}
module DataForecast where

import DataForecast.Prelude

-- | Use promoted constructors to represent the different timeseries that we have.
-- When using these as kinds you must prefix them with a single quote (').
--
-- __Example__:
--   raw 4 :: TimeSeries '[ 'Day ]
data TsPeriod
    = Decade
    | Year
    | Quarter
    | Month
    | Day


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
    SDay :: SPeriod 'Day
deriving instance Show (SPeriod x)


--
-- TimeSeries
--

-- | Time series date representation with the resolution breakdown encoded in the
-- type.
data TimeSeries (parts :: [TsPeriod]) where
    TimeSeries ::
        SPeriod p
        -> SummaryData
        -> Subparts subparts
        -> TimeSeries (p ': subparts)
deriving instance Show (TimeSeries parts)

-- | Get the 'SummaryData' out of a TimeSeries.
getSD :: TimeSeries parts -> SummaryData
getSD (TimeSeries _ sd _) = sd

-- | Set the 'SummaryData' field of a 'TimeSeries'. We can't use record syntax
-- because https://ghc.haskell.org/trac/ghc/ticket/2595 is not implemented.
setSD :: SummaryData -> TimeSeries parts -> TimeSeries parts
setSD sd (TimeSeries t _sd sub) = TimeSeries t sd sub

-- | Get the 'Subparts' out of a TimeSeries.
getSub :: TimeSeries (p ': subparts) -> Subparts subparts
getSub (TimeSeries _ _ sub) = sub

setSub ::
    Subparts subparts
    -> TimeSeries (p ': subparts)
    -> TimeSeries (p ': subparts)
setSub sub (TimeSeries t sd _sub) = TimeSeries t sd sub


-- Typeclass for constructing TimeSeries with default data.
class BuildTS p where
    build :: SummaryData -> Subparts rest -> TimeSeries (p ': rest)

instance BuildTS 'Year where
    build = TimeSeries SYear
instance BuildTS 'Quarter where
    build = TimeSeries SQuarter
instance BuildTS 'Month where
    build = TimeSeries SMonth
instance BuildTS 'Day where
    build = TimeSeries SDay


--
-- SummaryData
--

data SummaryData = SummaryData
    { sdtotal :: Maybe Double
    , sdmean :: Maybe Double
    }
deriving instance Show SummaryData

instance Default SummaryData where
    def = SummaryData Nothing Nothing

defaultSummary :: SummaryData
defaultSummary = def

summaryWithTotal :: Double -> SummaryData
summaryWithTotal total = setSdTotal total defaultSummary

setSdTotal :: Double -> SummaryData -> SummaryData
setSdTotal total sd = sd { sdtotal = Just total }

setSdMean :: Double -> SummaryData -> SummaryData
setSdMean mean sd = sd { sdmean = Just mean }


--
-- SubParts
--

data Subparts (parts :: [TsPeriod]) = Subparts
    { subs :: Maybe [TimeSeries parts]
    }
deriving instance Show (Subparts parts)

instance Default (Subparts '[]) where
    def = Subparts Nothing


fromParts :: (BuildTS part)
    => [TimeSeries subparts]
    -> TimeSeries (part ': subparts)
fromParts subseries = build def (Subparts (Just subseries))

raw :: BuildTS p => Double -> TimeSeries '[p]
raw t = build (SummaryData (Just t) Nothing) def


-- COMPUTE TOTAL

getTotalOrZero :: TimeSeries parts -> Double
getTotalOrZero = fromMaybe 0 . sdtotal . getSD

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
