module DataForecast.TutorialSpec (spec) where

import Protolude
import Test.Hspec

import DataForecast.TimeSeries
import DataForecast.Tutorial

spec :: Spec
spec = do
    constructorsEquivalentSpec
    yearByQuarterTotalSpec
    higherProfitsSpec


constructorsEquivalentSpec :: Spec
constructorsEquivalentSpec = do
    describe "year-by-quarter constructors" $ do
        it "yearByQuarterConstructor == yearByQuarterRaw" $
            yearByQuarterConstructor `shouldBe` yearByQuarterRaw
        it "yearByQuarterConstructor == yearByQuarter" $
            yearByQuarterConstructor `shouldBe` yearByQuarter
        it "yearByQuarterRaw == yearByQuarter" $
            yearByQuarterRaw `shouldBe` yearByQuarter


yearByQuarterTotalSpec :: Spec
yearByQuarterTotalSpec =
    describe "computeTotal yearByQuarter" $ do
    it "has 'Just 75.0' summary data" $
        (sdtotal . getSD $ yearByQuarterTotal) `shouldBe` Just 75.0


higherProfitsSpec :: Spec
higherProfitsSpec = do
    describe "yearA `hasHigherProfitsThan` yearA" $ do
        it "is 'False'" $
            (yearA `hasHigherProfitsThan` yearA) `shouldBe` False
    describe "yearA `hasHigherProfitsThan` yearB" $ do
        it "is 'True'" $
            (yearA `hasHigherProfitsThan` yearB) `shouldBe` True
