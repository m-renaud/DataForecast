{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module DataForecast.TimeSeries.AnalysisSpec (spec) where


import Protolude
import Test.Hspec
import Test.QuickCheck

import DataForecast.TimeSeries
import DataForecast.TimeSeries.Analysis

spec :: Spec
spec = do
    meanSpec
    errSpec
    errSquaredSpec


emptyTs :: TimeSeries '[ 'Year, 'Quarter ]
emptyTs = fromRawData []


meanSpec :: Spec
meanSpec =
    describe "mean" $ do
    context "on empty timeseries" $ do
        it "is Nothing" $
            mean emptyTs `shouldBe` Just 0.0


errSpec :: Spec
errSpec =
    describe "err" $ do
    context "on empty timeseries" $ do
        it "is empty list" $
            err emptyTs `shouldBe` Just []
    it "sum is zero" $
        property $
        \rawData ->
            (sum <$> err (fromRawData rawData :: TimeSeries '[ 'Year, 'Quarter ]))
            `closeTo` Just 0.0


closeTo :: Maybe Double -> Maybe Double -> Bool
closeTo (Just l) (Just r) = l - r < 0.00001
closeTo _ _ = False


errSquaredSpec :: Spec
errSquaredSpec =
    describe "errSquared" $ do
    context "on empty timeseries" $ do
        it "is empty list" $
            errSquared emptyTs `shouldBe` Just []
