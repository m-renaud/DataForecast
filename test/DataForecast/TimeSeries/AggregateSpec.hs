{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module DataForecast.TimeSeries.AggregateSpec (spec) where

import Prelude ((!!))
import Protolude
import Test.Hspec
import Test.QuickCheck

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
                property $ \x ->
                    (sdtotal . getSD . computeTotal $
                        (raw x :: TimeSeries '[ 'Year ]))
                    `shouldBe` Just x
            it "does not change summary data mean" $
                property $ \x ->
                    (sdmean . getSD . computeTotal $
                        (raw x :: TimeSeries '[ 'Year ]))
                    `shouldBe` Just x

        context "on two level timeseries" $ do
            it "sets summary data total" $
                property $ \xs ->
                    (sdtotal . getSD . computeTotal $
                        (fromParts (map raw xs)
                         :: TimeSeries '[ 'Year, 'Quarter ]))
                    `shouldBe` Just (sum xs)
            it "does not set summary data mean" $
                property $ \xs ->
                    (sdmean . getSD . computeTotal $
                        (fromParts (map raw xs)
                         :: TimeSeries '[ 'Year, 'Quarter ]))
                    `shouldBe`  Nothing
        computeTotalMultiLevelSpec


computeTotalMultiLevelSpec :: Spec
computeTotalMultiLevelSpec =
    context "on multi-level timeseries" $ do
        it "sets summary data total on all sub-components" $ do
            property $ \(xs, ys) ->
                let timeSeries :: TimeSeries '[ 'Year, 'Quarter, 'Month ]
                    timeSeries =
                        fromParts [ fromParts $ map raw xs
                                  , fromParts $ map raw ys
                                  ]
                    totaledTimeSeries = computeTotal timeSeries
                in
                    (sdtotal . getSD $ totaledTimeSeries)
                    `shouldBe` Just (sum xs + sum ys)



computeMeanSpec :: Spec
computeMeanSpec = do
    describe "computeMean" $ do

        context "on raw timeseries" $ do
            it "does not change summary data mean" $
                property $ \x ->
                    (sdmean . getSD . computeMean $
                    (raw x :: TimeSeries '[ 'Year ]))
                    `shouldBe` Just x
            it "does not change summary data total" $
                property $ \x ->
                    (sdtotal . getSD . computeMean $
                    (raw x :: TimeSeries '[ 'Year ]))
                    `shouldBe` Just x

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
