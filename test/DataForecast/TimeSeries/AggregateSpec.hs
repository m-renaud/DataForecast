{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module DataForecast.TimeSeries.AggregateSpec (spec) where

import Prelude ((!!))
import Protolude
import Test.Hspec

import DataForecast.TimeSeries
import DataForecast.TimeSeries.Aggregate


spec :: Spec
spec = do
    computeTotalSpec
    computeMeanSpec


computeTotalSpec :: Spec
computeTotalSpec = do
    describe "computeTotal" $ do

        context "on raw timeseries" $ do
            it "does not change summary data total" $
                (sdtotal . getSD . computeTotal $
                 (raw 10 :: TimeSeries '[ 'Year ]))
                `shouldBe` Just 10
            it "does not change summary data mean" $
                (sdmean . getSD . computeTotal $
                 (raw 10 :: TimeSeries '[ 'Year ]))
                `shouldBe` Just 10

        context "on two level timeseries" $ do
            it "sets summary data total" $
                (sdtotal . getSD . computeTotal $
                 (fromParts [raw 10, raw 20]
                  :: TimeSeries '[ 'Year, 'Quarter ]))
                 `shouldBe` Just 30
            it "does not set summary data mean" $
                (sdmean . getSD . computeTotal $
                 (fromParts [raw 10, raw 20] :: TimeSeries '[ 'Year, 'Quarter ]))
                `shouldBe` Nothing
        computeTotalMultiLevelSpec


computeTotalMultiLevelSpec :: Spec
computeTotalMultiLevelSpec =
    context "on multi-level timeseries" $ do
        it "sets summary data total on all sub-components" $ do
            let timeSeries :: TimeSeries '[ 'Year, 'Quarter, 'Month ]
                timeSeries =
                    fromParts [ fromParts [ raw 1, raw 2 ]
                              , fromParts [ raw 10, raw 20 ]
                              ]
                totaledTimeSeries = computeTotal timeSeries
            (sdtotal . getSD $ totaledTimeSeries) `shouldBe` Just 33
            (sdtotal . getSD . nthSubpart 0 $ totaledTimeSeries)
             `shouldBe` Just 3
            (sdtotal . getSD . nthSubpart 1 $ totaledTimeSeries)
             `shouldBe` Just 30


computeMeanSpec :: Spec
computeMeanSpec = do
    describe "computeMean" $ do

        context "on raw timeseries" $ do
            it "does not change summary data mean" $
                (sdmean . getSD . computeMean $
                (raw 10 :: TimeSeries '[ 'Year ]))
                `shouldBe` Just 10
            it "does not change summary data total" $
                (sdtotal . getSD . computeMean $
                (raw 10 :: TimeSeries '[ 'Year ]))
                `shouldBe` Just 10

        context "on two level timeseries" $ do
            it "sets summary data mean" $
                (sdmean . getSD . computeMean $
                 (fromParts [raw 10, raw 20]
                  :: TimeSeries '[ 'Year, 'Quarter ]))
                `shouldBe` Just 15.0
            it "sets summary data total" $
                (sdtotal . getSD . computeMean $
                 (fromParts [raw 10, raw 20]
                  :: TimeSeries '[ 'Year, 'Quarter ]))
                 `shouldBe` Just 30

        computeMeanMultiLevelSpec


computeMeanMultiLevelSpec :: Spec
computeMeanMultiLevelSpec =
    context "on multi-level timeseries" $ do
        it "sets summary data mean on all sub-components" $ do
            let timeSeries :: TimeSeries '[ 'Year, 'Quarter, 'Month ]
                timeSeries =
                    fromParts [ fromParts [ raw 1, raw 2 ]
                              , fromParts [ raw 10, raw 20 ]
                              ]
                timeSeriesWithMeans = computeMean timeSeries
            (sdmean . getSD $ timeSeriesWithMeans) `shouldBe` Just 16.5
            (sdmean . getSD . nthSubpart 0 $ timeSeriesWithMeans)
             `shouldBe` Just 1.5
            (sdmean . getSD . nthSubpart 1 $ timeSeriesWithMeans)
             `shouldBe` Just 15.0


nthSubpart :: Int -> TimeSeries (p ': rest) -> (TimeSeries rest)
nthSubpart n = (!! n) . subs . getSub
