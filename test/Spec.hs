{-# LANGUAGE OverloadedStrings #-}
module TestMain (main) where

import Test.Hspec
import Test.Hspec.Hedgehog

-- import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

-- import Core.Types
-- import Has
-- import Core.Hsprjup

-- | Following is only for example
-- | you must adapt it accordingly
main :: IO ()
main = hspec $
  describe "test hsprjup properties" $ do
    it "test sample for type one - test A" $ do
      (+) 1 1 `shouldBe` (2 :: Int)
    it "test property with hedgehog - property 001" $ hedgehog $ do
      success
