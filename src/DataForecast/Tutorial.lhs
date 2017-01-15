DataForecast.Tutorial
---------------------

This file walks through the different components of the DataForecast library.

Preliminaries
-------------

This library uses type-level lists and promoted data constructors so we need the
TypeOperators and DataKinds language extensions.

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DataKinds #-}
> module DataForecast.Tutorial where
> {-|
> Module      : DataForecast.Tutorial
> Description : Some simple uses of DataForecast.
> -}
> 
> import DataForecast
> import DataForecast.Prelude


Constructing a `TimeSeries`
---------------------------

We can construct a `TimeSeries` using the constructor directly and specifying the
resolution (need a better word for this) for each of the components. In the code below

> yearByQuarterConstructor :: TimeSeries '[ 'Year, 'Quarter ]
> yearByQuarterConstructor =
>     TimeSeries SYear defaultSummary
>         (Subparts . Just $ [ TimeSeries SQuarter (summaryWithTotal 10) (Subparts Nothing)
>                            , TimeSeries SQuarter (summaryWithTotal 20) (Subparts Nothing)
>                            , TimeSeries SQuarter (summaryWithTotal 30) (Subparts Nothing)
>                            , TimeSeries SQuarter (summaryWithTotal 15) (Subparts Nothing)
>                            ])


As you can imagine this would get very tedious, especially when you just want to
provide raw totals for the leaves. To simplify this we provide the `raw`
function so the above can be replaced with:

> yearByQuarterRaw :: TimeSeries '[ 'Year, 'Quarter ]
> yearByQuarterRaw =
>     TimeSeries SYear defaultSummary
>         (Subparts . Just $ [ raw 10
>                            , raw 20
>                            , raw 30
>                            , raw 15
>                            ])


This is much better than the original `yearByQuarterConstructor` above, but we
still have quite a bit of boilerplate when we just want to provide raw leaf
data. We can eliminate the boilerplate completely by using the `fromParts`
function:

> yearByQuarter :: TimeSeries '[ 'Year, 'Quarter ]
> yearByQuarter = fromParts [ raw 10, raw 20, raw 30, raw 15 ]

Much better! The `fromParts` and `raw` utilities allow you to construct a
`TimeSeries` of any type, you just need to add the type signature.


*More Complex Time Series*

We can construct time series with more than two components, for example a years
worth of data broken down by quarter and again by month. We'll be using the
`raw` and `fromParts` functions we introduced above.

> yearByQuarterByMonth :: TimeSeries '[ 'Year, 'Quarter, 'Month ]
> yearByQuarterByMonth =
>     fromParts
>         [ fromParts [ raw 10, raw 15, raw 20 ]
>         , fromParts [ raw 20, raw 40, raw 40 ]
>         , fromParts [ raw 42, raw 50, raw 45 ]
>         , fromParts [ raw 42, raw 42, raw 42 ]
>         ]


Aggregating Time Series
---------------------

Note: Not much here yet.

Lets aggregate the totals across our first time series:

> yearByQuarterTotal :: TimeSeries '[ 'Year, 'Quarter ]
> yearByQuarterTotal = computeTotal yearByQuarter

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

> analyze :: TimeSeries parts -> TimeSeries parts -> Bool
> analyze _l _r =  True

Now, this is a trivial function but the important part to note is the type
signature. Both arguments to the function must have the type `TimeSeries parts`
which means that the `parts` must be the same. So, the following would be valid:

> analyzeYearByQuarter :: Bool
> analyzeYearByQuarter = analyze yearByQuarterConstructor yearByQuarter

But the following will fail to compile:

> -- Note: `yearByQuarter` defined above.
> -- yearByQuarter :: TimeSeries '[ 'Year, 'Quarter ]
> -- yearByQuarter = fromParts [ raw 10, raw 20, raw 30, raw 15 ]
>
> quarterByMonth :: TimeSeries '[ 'Quarter, 'Month ]
> quarterByMonth = fromParts [ raw 100, raw 200, raw 201, raw 404 ]
> 
> -- brokenAnalyzeTwoDifferentTimeSeries = analyze yearByQuarter quarterByMonth

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

> hasHigherProfitsThan :: TimeSeries (a ': restl) -> TimeSeries (a ': restr) -> Bool
> hasHigherProfitsThan l r =
>     let
>         lProfits = fromMaybe 0 . sdtotal . getSD . computeTotal $ l
>         rProfits = fromMaybe 0 . sdtotal . getSD . computeTotal $ r
>     in
>         lProfits > rProfits

Once again the key is the type signature, we say the two arguments to this
function must both be `TimeSeries` with the condition that their first
resolution is the same. So we can pass two `Year` time series, or two `Quarter`
time series, but not a `Year` time series and a `Quarter` time series. This
means that the following is a valid analysis:

> yearA :: TimeSeries '[ 'Year ]
> yearA = raw 100
>
> yearB :: TimeSeries '[ 'Year, 'Quarter ]
> yearB = fromParts [ raw 20, raw 20, raw 10, raw 30 ]
>
> aHigherThanB :: Bool
> aHigherThanB = yearA `hasHigherProfitsThan` yearB

But this one is not:

> yearC :: TimeSeries '[ 'Year ]
> yearC = raw 1000
>
> quarterA :: TimeSeries '[ 'Quarter ]
> quarterA = raw 500
> 
> -- yearAndQuarterComp = yearC `hasHigherProfitsThan` quarterA


We can do the same to ensure the time series match up to any level of resolution.

> analyzeSeries ::
>     TimeSeries (l ': a ': restl)
>     -> TimeSeries (r ': a ': restr)
>     -> Bool
> analyzeSeries _l _r = True

