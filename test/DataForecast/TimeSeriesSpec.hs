{-# LANGUAGE DataKinds #-}
module DataForecast.TimeSeriesSpec (spec) where

import Protolude
import Test.Hspec

import DataForecast.TimeSeries

spec :: Spec
spec = do
    rawSpec
    fromPartsSpec
    defaultSummarySpec
    summaryWithTotalSpec
    setSdTotalSpec
    setSdMeanSpec


-- | Pin down the 'TimeSeries' type to avoid ambiguous instance errors.
rawDay :: Double -> TimeSeries '[ 'Day ]
rawDay = raw


rawSpec :: Spec
rawSpec =
    describe "when 'raw' constructor used" $ do
    it "summary data total is equal to 'raw' argument" $
        (sdtotal . getSD $ rawDay 10) `shouldBe` Just 10
    it "summary data mean is 'Nothing'" $
        (sdmean . getSD $ rawDay 10) `shouldBe` Nothing


fromPartsSpec :: Spec
fromPartsSpec =
    describe "when 'fromParts' constructor used" $ do
    context "with empty subparts list" $ do
        it "has 'defaultSummary' data" $
            (getSD $ (fromParts [] :: TimeSeries '[ 'Year, 'Quarter ]))
             `shouldBe` defaultSummary
        it "has empty subpartrs" $
            (getSub $ (fromParts [] :: TimeSeries '[ 'Year, 'Quarter ]))
            `shouldBe` Subparts []
    context "with non-empty subparts list" $ do
        it "has 'defaultSummary' data" $
            (getSD $
             (fromParts [raw 10, raw 20] :: TimeSeries '[ 'Year, 'Quarter ]))
            `shouldBe` defaultSummary
        it "has same subparts as list argument" $
            (getSub $
             (fromParts [raw 10, raw 20] :: TimeSeries '[ 'Year, 'Quarter ]))
            `shouldBe` Subparts [raw 10, raw 20]


defaultSummarySpec :: Spec
defaultSummarySpec =
    describe "when 'defaultSummary' called" $ do
    it "summary data total is 'Nothing'" $
        sdtotal defaultSummary `shouldBe` Nothing
    it "summary data mean is 'Nothing'" $
        sdmean defaultSummary `shouldBe` Nothing


summaryWithTotalSpec :: Spec
summaryWithTotalSpec =
    describe "when 'summaryWithTotal' constructor used" $ do
    it "summary data total is set" $
        sdtotal (summaryWithTotal 10) `shouldBe` Just 10
    it "summary data mean is not set" $
        sdmean (summaryWithTotal 10) `shouldBe` Nothing


setSdTotalSpec :: Spec
setSdTotalSpec =
    describe "when setSdTotal called" $ do
    context "on default 'SummaryData'" $ do
        it "sdtotal is Just" $
            sdtotal (setSdTotal 10 defaultSummary) `shouldBe` Just 10
        it  "sdmean is still Nothing" $
            sdmean (setSdTotal 10 defaultSummary) `shouldBe` Nothing
    context "on 'SummaryData' with total set" $ do
        it "sdtotal is overwritten" $
            (sdtotal . setSdTotal 20 . summaryWithTotal $ 10)
            `shouldBe` Just 20
        it "sdmean is still Nothing" $
            (sdmean . setSdTotal 20 . summaryWithTotal $ 10)
            `shouldBe` Nothing


setSdMeanSpec :: Spec
setSdMeanSpec =
    describe "when setSdMean called" $ do
    context "on default 'SummaryData'" $ do
        it "sdmean is Just" $
            sdmean (setSdMean 10 defaultSummary) `shouldBe` Just 10
        it  "sdtotal is still Nothing" $
            sdtotal (setSdMean 10 defaultSummary) `shouldBe` Nothing
