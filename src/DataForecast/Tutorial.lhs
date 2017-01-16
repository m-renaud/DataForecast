DataForecast.Tutorial
---------------------

This file walks through the different components of the DataForecast library.

Preliminaries
-------------

This library uses type-level lists and promoted data constructors so we need the
TypeOperators and DataKinds language extensions.

\begin{code}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-|
Module      : DataForecast.Tutorial
Description : Some simple uses of DataForecast.
-}
module DataForecast.Tutorial where

import DataForecast
import DataForecast.Prelude
\end{code}


Constructing a `TimeSeries`
---------------------------

We can construct a `TimeSeries` using the constructor directly and specifying
the resolution (need a better word for this) for each of the components. This is
not how you should construct them when using the library (we'll get to that
soon) but it's important to understand the structure of the `TimeSeries` type.

A `TimeSeries` has 3 main components:

  1. The period of time that the data is for. This is expressed using a
  `TsPeriod` ("time series period") which is something like `Year`, `Quarter`,
  `Month`, or `Day`. This becomes part of the type of the time series so you
  can track the _shape_ of your data.

  2. The summary data for the time series. This includes the total of the entire
  time series and the mean of the constituent pieces.

  3. The subparts of the time series. If you have a year's worth of data at the
  resolution of a quarter then your top level time series will be for the year
  and the subparts will be timeseries for the quarter.

Note: The `TsPeriod` type is used a bit differently in different contexts. In
the constructor below you'll see it used with a prefixed capitol `S` and in
type signatures you'll see it prefixed with a single quote `'`.

So, without further ado let's build a time series for a year's worth of profits.

\begin{code}
yearByQuarterConstructor :: TimeSeries '[ 'Year, 'Quarter ]
yearByQuarterConstructor =
    TimeSeries SYear defaultSummary
        (Subparts [ TimeSeries SQuarter (summaryWithTotal 10) (Subparts [])
                  , TimeSeries SQuarter (summaryWithTotal 20) (Subparts [])
                  , TimeSeries SQuarter (summaryWithTotal 30) (Subparts [])
                  , TimeSeries SQuarter (summaryWithTotal 15) (Subparts [])
                  ])
\end{code}

As you can imagine this would get very tedious, especially when you just want to
provide raw totals for the leaves. To simplify this we provide the `raw`
function so the above can be replaced with:

\begin{code}
yearByQuarterRaw :: TimeSeries '[ 'Year, 'Quarter ]
yearByQuarterRaw =
    TimeSeries SYear defaultSummary
        (Subparts [ raw 10
                  , raw 20
                  , raw 30
                  , raw 15
                  ])
\end{code}


This is much better than the original `yearByQuarterConstructor` above, but we
still have quite a bit of boilerplate when we just want to provide raw leaf
data. We can eliminate the boilerplate completely by using the `fromParts`
function:

\begin{code}
yearByQuarter :: TimeSeries '[ 'Year, 'Quarter ]
yearByQuarter = fromParts [ raw 10, raw 20, raw 30, raw 15 ]
\end{code}

Much better! The `fromParts` and `raw` utilities allow you to construct a
`TimeSeries` of any type, you just need to add the type signature to pin down
the concrete type of your data.


*More Complex Time Series*

We can construct time series with more than two components, for example a years
worth of data broken down by quarter and again by month. We'll be using the
`raw` and `fromParts` functions we introduced above.

\begin{code}
yearByQuarterByMonth :: TimeSeries '[ 'Year, 'Quarter, 'Month ]
yearByQuarterByMonth =
    fromParts
        [ fromParts [ raw 10, raw 15, raw 20 ]
        , fromParts [ raw 20, raw 40, raw 40 ]
        , fromParts [ raw 42, raw 50, raw 45 ]
        , fromParts [ raw 42, raw 42, raw 42 ]
        ]
\end{code}


Aggregating Time Series
---------------------

Note: Not much here yet.

Lets aggregate the totals across our first time series:

\begin{code}
yearByQuarterTotal :: TimeSeries '[ 'Year, 'Quarter ]
yearByQuarterTotal = computeTotal yearByQuarter
\end{code}

This will update the top level total in the `SummaryData` of our time series by
summing the totals of the constituent parts.


    λ> computeTotal yearByQuarter
    TimeSeries SYear (SummaryData {sdtotal = Just 75.0, ... }) ...


We can do the same thing for our `yearByQuarterByMonth` timeseries. This
function works for an arbitrarily nested time series.


Analyzing Time Series
---------------------

One of the benefits of representing time series this way is we can ensure that
only semantically valid analysis can be performed. For example, it makes no
sense to compare two time series together if their data are different
shapes. Luckily we can encode these requirements in the type.

Lets look at a function that requires that the shape of both time series is the
same:

\begin{code}
analyze :: TimeSeries parts -> TimeSeries parts -> Bool
analyze _l _r =  True
\end{code}

Now, this is a trivial function but the important part to note is the type
signature. Both arguments to the function must have the type `TimeSeries parts`
which means that the `parts` must be the same. So, the following would be valid:

\begin{code}
analyzeYearByQuarter :: Bool
analyzeYearByQuarter = analyze yearByQuarterConstructor yearByQuarter
\end{code}

But the following will fail to compile:

\begin{code}
-- Note: `yearByQuarter` defined above.
-- yearByQuarter :: TimeSeries '[ 'Year, 'Quarter ]
-- yearByQuarter = fromParts [ raw 10, raw 20, raw 30, raw 15 ]

quarterByMonth :: TimeSeries '[ 'Quarter, 'Month ]
quarterByMonth = fromParts [ raw 100, raw 200, raw 201, raw 404 ]

-- brokenAnalyzeTwoDifferentTimeSeries = analyze yearByQuarter quarterByMonth
\end{code}

This will fail to compile with the following warning:


    Couldn't match type ‘'Quarter’ with ‘'Year’
    Expected type: TimeSeries '['Year, 'Quarter]
      Actual type: TimeSeries '['Quarter, 'Month]
    In the second argument of ‘analyze’, namely ‘quarterByMonth’


It very clearly says that it was expecting the second argument to analyze,
namely `quarterByMonth` to have the type `TimeSeries '[ 'Year, 'Quarter ]` but
you gave it a timeseries of a different type.


*Partial Commonality*

Now, if we were restricted to only analyzing time series of the same type we
wouldn't be able to do much. Some analysis only requires that the total time
period be the same. For example, if we wanted to see which year had higher
profits we could write the following analysis function:

\begin{code}
hasHigherProfitsThan :: TimeSeries (a ': restl) -> TimeSeries (a ': restr) -> Bool
hasHigherProfitsThan l r =
    let
        lProfits = fromMaybe 0 . sdtotal . getSD . computeTotal $ l
        rProfits = fromMaybe 0 . sdtotal . getSD . computeTotal $ r
    in
        lProfits > rProfits
\end{code}

Once again the key is the type signature, we say the two arguments to this
function must both be `TimeSeries` with the condition that their first
resolution is the same. So we can pass two `Year` time series, or two `Quarter`
time series, but not a `Year` time series and a `Quarter` time series. This
means that the following is a valid analysis:

\begin{code}
yearA :: TimeSeries '[ 'Year ]
yearA = raw 100

yearB :: TimeSeries '[ 'Year, 'Quarter ]
yearB = fromParts [ raw 20, raw 20, raw 10, raw 30 ]

aHigherThanB :: Bool
aHigherThanB = yearA `hasHigherProfitsThan` yearB
\end{code}

But this one is not:

\begin{code}
yearC :: TimeSeries '[ 'Year ]
yearC = raw 1000

quarterA :: TimeSeries '[ 'Quarter ]
quarterA = raw 500

-- yearAndQuarterComp = yearC `hasHigherProfitsThan` quarterA
\end{code}


We can do the same to ensure the time series match up to any level of resolution.

\begin{code}
analyzeSeries ::
    TimeSeries (l ': a ': restl)
    -> TimeSeries (r ': a ': restr)
    -> Bool
analyzeSeries _l _r = True
\end{code}

