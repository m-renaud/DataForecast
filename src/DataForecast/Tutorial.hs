{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module DataForecast.Tutorial where
{-|
Module      : DataForecast.Tutorial
Description : Some simple uses of DataForecast.
-}

import DataForecast
import DataForecast.Prelude

x1 :: TimeSeries '[ 'Year, 'Day ]
x1 = computeTotal yearByDay
x2 :: TimeSeries '[ 'Year, 'Month, 'Day ]
x2 = computeTotal ymd

-- EXAMPLES

yearByDay, yearByDay' :: TimeSeries '[ 'Year, 'Day ]
yearByDay = TimeSeries SYear def (Subparts . Just $ [raw 10, raw 20])
yearByDay' = fromParts [raw 10, raw 20]

yearByMonth :: TimeSeries '[ 'Year, 'Month ]
yearByMonth = TimeSeries SYear def (Subparts . Just $ [raw 12])

monthByDay :: TimeSeries '[ 'Month, 'Day ]
monthByDay = TimeSeries SMonth def (Subparts . Just $ [raw 100])

ymd :: TimeSeries '[ 'Year, 'Month, 'Day ]
ymd =
    fromParts
        [ fromParts [raw 10, raw 20]
        , fromParts [raw 100, raw 200]
        ]

analyze :: TimeSeries parts -> TimeSeries parts -> Bool
analyze _l _r =  True

analyzeFirst :: TimeSeries (a ': restl) -> TimeSeries (a ': restr) -> Bool
analyzeFirst _l _r = True

analyzeSeries ::
    TimeSeries (l ': a ': restl)
    -> TimeSeries (r ': a ': restr)
    -> Bool
analyzeSeries _l _r = True

-- Fails to compile: EXPECTED
--g :: Bool
--g = analyze yearByDay yearByMonth

gFirst :: Bool
gFirst = analyzeFirst yearByDay yearByMonth

gSecond :: Bool
gSecond = analyzeSeries yearByDay monthByDay

ymdTotals :: TimeSeries '[ 'Year, 'Month, 'Day ]
ymdTotals = computeTotal ymd
