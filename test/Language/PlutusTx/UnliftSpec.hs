{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PlutusTx.UnliftSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Ledger
import Language.PlutusTx as PlutusTx
import Language.PlutusTx.Unlift
import Language.PlutusTx.Lift.Class (Lift)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import GHC.Generics
import Control.Applicative ((<|>))

data CustomData
  = CustomData1
  { cd1Foo    :: ByteString
  , cd1Bar    :: Int
  , cd1Baz    :: PubKey
  }
  | CustomData2
  { cd2Foo    :: Int
  , cd2Bar    :: PubKey
  }
  deriving (Show, Eq, Ord, Generic)
PlutusTx.makeLift ''CustomData

instance Unlift CustomData where
  unlift (DataScript ds) =
    (
      CustomData1
      <$> unlift (DataScript ($$(Ledger.compileScript [|| \(CustomData1 a _ _) -> a ||]) `applyScript` ds))
      <*> unlift (DataScript ($$(Ledger.compileScript [|| \(CustomData1 _ b _) -> b ||]) `applyScript` ds))
      <*> unlift (DataScript ($$(Ledger.compileScript [|| \(CustomData1 _ _ c) -> c ||]) `applyScript` ds))
    ) <|> (
      CustomData2
      <$> unlift (DataScript ($$(Ledger.compileScript [|| \(CustomData2 a _) -> a ||]) `applyScript` ds))
      <*> unlift (DataScript ($$(Ledger.compileScript [|| \(CustomData2 _ b) -> b ||]) `applyScript` ds))
    )

instance Arbitrary B8.ByteString where
  arbitrary = B8.pack <$> arbitrary

instance Arbitrary CustomData where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PubKey where
  arbitrary = PubKey <$> arbitrary

unliftedReversesLifted :: (Show a, Eq a, Lift a, Unlift a) => a -> Expectation
unliftedReversesLifted result = unlift (DataScript (lifted result)) `shouldBe` Just result

spec :: Spec
spec = do
  describe "unlift core datatypes" $ do
    prop "Int" $ unliftedReversesLifted @Int
    prop "PubKey" $ unliftedReversesLifted . PubKey
    prop "ByteString" $ unliftedReversesLifted . B.pack
  describe "unlift custom datatypes" $ do
    prop "CustomData" $ unliftedReversesLifted @CustomData
