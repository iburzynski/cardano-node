{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Key.ConvertCardanoAddressKey
  ( golden_convertCardanoAddressByronSigningKey
  , golden_convertCardanoAddressIcarusSigningKey
  , golden_convertCardanoAddressShelleyPaymentSigningKey
  , golden_convertCardanoAddressShelleyStakeSigningKey
  , golden_legacyAndBip32ExtendedKeysIndirectEquivalence
  ) where

import           Cardano.Api

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)

import           Hedgehog (Property, (===))
import           Hedgehog.Internal.Property (evalEither)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | An example signing key generated by @cardano-address@ using the
-- deprecated Byron style.
exampleByronSigningKey :: Text
exampleByronSigningKey =
  "xprv1pp72a64en2vf568jywe9azlgrqe3p2jjf9gxxeejn2fex8g889x54w6emg2egkaz2rxyc"
    <> "560fp0hrv8y0hzpuzu27zhhhgwc8t5tvrczz2jnhhjwdnd6cdjx4dxehrsr2pr406rchw"
    <> "ctfwrgpc9r7nmakvaegyz9"

-- | An example signing key generated by @cardano-address@ using the Icarus
-- style.
exampleIcarusSigningKey :: Text
exampleIcarusSigningKey =
  "xprv1yq7c6nlmxncg7txy0z6lqf3fww4vm20m60lrxttx5lr4qmkvh395m3p59v8fn4ku9mzyc"
    <> "g2rkxatgwm86uc3pvrt06e43afya6rm0s2azlpnc9yrhygl2heckeyhhtgad08c0zljpn"
    <> "c6fse2ldzyx9c86yvddxjw"

-- | An example signing key generated by @cardano-address@ using the Shelley
-- style.
exampleShelleySigningKey :: Text
exampleShelleySigningKey =
  "xprv1yq7c6nlmxncg7txy0z6lqf3fww4vm20m60lrxttx5lr4qmkvh395m3p59v8fn4ku9mzyc"
    <> "g2rkxatgwm86uc3pvrt06e43afya6rm0s2azlpnc9yrhygl2heckeyhhtgad08c0zljpn"
    <> "c6fse2ldzyx9c86yvddxjw"

-- | Test that converting a @cardano-address@ Byron signing key yields the
-- expected result.
golden_convertCardanoAddressByronSigningKey :: Property
golden_convertCardanoAddressByronSigningKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

    -- `cardano-address` signing key filepath
    signingKeyFp <- noteTempFile tempDir "cardano-address-byron.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-byron.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleByronSigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key","convert-cardano-address-key"
      , "--byron-payment-key"
      , "--signing-key-file", signingKeyFp
      , "--out-file", convertedSigningKeyFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    actualConvertedSigningKey <- H.readFile convertedSigningKeyFp
    expectedConvertedSigningKey <-
      H.readFile $
        "test/data/golden/shelley/keys/converted_cardano-address_keys/"
          <> "byron_signing_key"
    expectedConvertedSigningKey === actualConvertedSigningKey

-- | Test that converting a @cardano-address@ Icarus signing key yields the
-- expected result.
golden_convertCardanoAddressIcarusSigningKey :: Property
golden_convertCardanoAddressIcarusSigningKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

    -- `cardano-address` signing key filepath
    signingKeyFp <- H.noteTempFile tempDir "cardano-address-icarus.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-icarus.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleIcarusSigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key","convert-cardano-address-key"
      , "--icarus-payment-key"
      , "--signing-key-file", signingKeyFp
      , "--out-file", convertedSigningKeyFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    actualConvertedSigningKey <- H.readFile convertedSigningKeyFp
    expectedConvertedSigningKey <-
      H.readFile $
        "test/data/golden/shelley/keys/converted_cardano-address_keys/"
          <> "icarus_signing_key"
    expectedConvertedSigningKey === actualConvertedSigningKey

-- | Test that converting a @cardano-address@ Shelley payment signing key
-- yields the expected result.
golden_convertCardanoAddressShelleyPaymentSigningKey :: Property
golden_convertCardanoAddressShelleyPaymentSigningKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

    -- `cardano-address` signing key filepath
    signingKeyFp <-
      noteTempFile tempDir "cardano-address-shelley-payment.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      noteTempFile tempDir "converted-cardano-address-shelley-payment.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleShelleySigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key","convert-cardano-address-key"
      , "--shelley-payment-key"
      , "--signing-key-file", signingKeyFp
      , "--out-file", convertedSigningKeyFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    actualConvertedSigningKey <- H.readFile convertedSigningKeyFp
    expectedConvertedSigningKey <-
      H.readFile $
        "test/data/golden/shelley/keys/converted_cardano-address_keys/"
          <> "shelley_payment_signing_key_bip32"
    expectedConvertedSigningKey === actualConvertedSigningKey

-- | Test that converting a @cardano-address@ Shelley stake signing key yields
-- the expected result.
golden_convertCardanoAddressShelleyStakeSigningKey :: Property
golden_convertCardanoAddressShelleyStakeSigningKey =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do

    -- `cardano-address` signing key filepath
    signingKeyFp <-
      noteTempFile tempDir "cardano-address-shelley-stake.skey"

    -- Converted signing key filepath
    convertedSigningKeyFp <-
      H.noteTempFile tempDir "converted-cardano-address-shelley-stake.skey"

    -- Write `cardano-address` signing key to disk
    H.textWriteFile signingKeyFp exampleShelleySigningKey
    H.assertFilesExist [signingKeyFp]

    -- Convert the `cardano-address` signing key
    void $ execCardanoCLI
      [ "key","convert-cardano-address-key"
      , "--shelley-stake-key"
      , "--signing-key-file", signingKeyFp
      , "--out-file", convertedSigningKeyFp
      ]

    -- Check for existence of the converted signing key file
    H.assertFilesExist [convertedSigningKeyFp]

    -- Check that the contents of the converted signing key file match that of
    -- the golden file.
    actualConvertedSigningKey <- H.readFile convertedSigningKeyFp
    expectedConvertedSigningKey <-
      H.readFile $
        "test/data/golden/shelley/keys/converted_cardano-address_keys/"
          <> "shelley_stake_signing_key_bip32"
    expectedConvertedSigningKey === actualConvertedSigningKey

-- | Test that confirms whether we deserialize a legacy cardano-crypto/BIP32
-- extended key we get the same verification key.
golden_legacyAndBip32ExtendedKeysIndirectEquivalence :: Property
golden_legacyAndBip32ExtendedKeysIndirectEquivalence =
  propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
    ebip32 <-
       liftIO . readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
         $ "test/data/golden/shelley/keys/converted_cardano-address_keys/" <>
           "shelley_payment_signing_key_bip32"
    elegacy <-
       liftIO . readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
         $ "test/data/golden/shelley/keys/converted_cardano-address_keys/" <>
           "shelley_payment_signing_key_legacy"

    bip32 <- evalEither ebip32
    legacy <- evalEither elegacy

    getVerificationKey bip32 === getVerificationKey legacy

