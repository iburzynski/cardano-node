{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hedgehog.Roundtrip.CBOR
  ( roundtrip_CDDL_Tx
  , trippingCbor
  ) where

import           Cardano.Api

import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | Assert that CBOR serialisation and deserialisation roundtrips.
trippingCbor :: ()
  => HasCallStack
  => H.MonadTest m
  => Show a
  => Eq a
  => SerialiseAsCBOR a
  => AsType a
  -> a
  -> m ()
trippingCbor typeProxy v = GHC.withFrozenCallStack $
  H.tripping v serialiseToCBOR (deserialiseFromCBOR typeProxy)

roundtrip_CDDL_Tx
  :: (IsCardanoEra era, HasCallStack)
  => CardanoEra era
  -> Gen (Tx era)
  -> Property
roundtrip_CDDL_Tx era gen =
  H.property $ do
    GHC.withFrozenCallStack $ H.noteShow_ era
    val <- H.forAll gen
    H.tripping val serialiseTxLedgerCddl (deserialiseTxLedgerCddl era)
