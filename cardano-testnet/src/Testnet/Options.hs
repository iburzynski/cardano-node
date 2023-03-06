{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Options
  ( BabbageTestnetOptions(..)
  , defaultTestnetOptions
  ) where

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Scientific

import           Testnet.Util.Runtime (NodeLoggingFormat (..))

{- HLINT ignore "Redundant flip" -}

data BabbageTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes :: Int
  , babbageSlotDuration :: Int
  , babbageSecurityParam :: Int
  , babbageTotalBalance :: Int
  , babbageNodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: BabbageTestnetOptions
defaultTestnetOptions = BabbageTestnetOptions
  { babbageNumSpoNodes = 3
  , babbageSlotDuration = 200
  , babbageSecurityParam = 10
  , babbageTotalBalance = 10020000000
  , babbageNodeLoggingFormat = NodeLoggingFormatAsJson
  }


defaultYamlConfig :: Aeson.Value
defaultYamlConfig = Aeson.Object $
  mconcat $ map (uncurry Aeson.singleton)
    [ ("Protocol", "Cardano")
    , ("PBftSignatureThreshold", Aeson.Number (fromFloatDigits (0.6 :: Double)))
    , ("minSeverity", "Debug")
    , ("ByronGenesisFile", undefined)
    , ("ShelleyGenesisFile", undefined)
    , ("AlonzoGenesisFile", undefined)
    , ("RequiresNetworkMagic", undefined)
    , ("LastKnownBlockVersion", Aeson.Number 6)
    , ("LastKnownBlockVersion", Aeson.Number 0)
    , ("TestShelleyHardForkAtEpoch", Aeson.Number 0)
    , ("TestAllegraHardForkAtEpoch", Aeson.Number 0)
    , ("TestMaryHardForkAtEpoch", Aeson.Number 0)
    , ("TestAlonzoHardForkAtEpoch", Aeson.Number 0)
    , ("TestBabbageHardForkAtEpoch", Aeson.Number 0)
    , ("TestEnableDevelopmentHardForkEras", Aeson.Bool True)
    ]
