module Phoityne.Example.CreditCardSpec where

import SpecHelper

spec :: Spec
spec =
    describe "Credit Card" $ do
        context "with 0" $
            it "should be []" $
                toDigits 0 `shouldBe` []
        context "with -17" $
            it "should be []" $
                toDigits (-17) `shouldBe` []
        context "with 1234" $
            it "should be [1,2,3,4]" $
                toDigits 1234 `shouldBe` [1,2,3,4]

        context "with 0" $
            it "should be []" $
                toDigitsRev 0 `shouldBe` []
        context "with -25" $
            it "should be []" $
                toDigitsRev (-25) `shouldBe` []
        context "with 1234" $
            it "should be [4,3,2,1]" $
                toDigitsRev 1234 `shouldBe` [4,3,2,1]

        context "with []" $
            it "should be []" $
                doubleEveryOther [] `shouldBe` []
        context "with [9]" $
            it "should be [9]" $
                doubleEveryOther [9] `shouldBe` [9]
        context "with [2,4,6]" $
            it "should be [2,8,6]" $
                doubleEveryOther [2,4,6] `shouldBe` [2,8,6]
        context "with [8,7,6,5]" $
            it "should be [16,7,12,5]" $
                doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

        context "with []" $
            it "should be []" $
                sumDigits [] `shouldBe` 0
        context "with [9]" $
            it "should be 9" $
                sumDigits [9] `shouldBe` 9
        context "with [8,7,6,5]" $
            it "should be 26" $
                sumDigits [8,7,6,5] `shouldBe` 26
        
        context "with 180" $
            it "should be 0" $
                sumRemainder 180 `shouldBe` 0
        
        context "with 93" $
            it "should be 3" $
                sumRemainder 93 `shouldBe` 3

        -- context "validate with 4012888888881881" $
        --     it "should be True" $
        --         validate 4012888888881881 `shouldBe` True
        
        -- context "validate with 4012888888881882" $
        --     it "should be False" $
        --         validate 4012888888881882 `shouldBe` False

        context "rev double with 5678" $
            it "should be [16,7,12,5]" $
                doubleEveryOther(toDigitsRev 5678)  `shouldBe` [16,7,12,5]

        context "sum rev double with 5678" $
            it "should be 40" $
                sumDigits(doubleEveryOther(toDigitsRev 5678))  `shouldBe` 40

        context "validate sum rev double with 6678" $
            it "should be FALSE" $
                validate(sumDigits(doubleEveryOther(toDigitsRev 5678)))  `shouldBe` False