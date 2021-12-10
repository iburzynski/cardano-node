{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gen.Cardano.Api.Typed
  ( genAddressByron
  , genAddressInEra
  , genAddressShelley
  , genCertificate
  , genCostModel
  , genMaybePraosNonce
  , genPraosNonce
  , genProtocolParameters
  , genValueNestedRep
  , genValueNestedBundle
  , genByronKeyWitness
  , genShelleyKeyWitness

  , genTxId
  , genTxIn
  , genTxOutTxContext
  , genTxOutUTxOContext
  , genUTxO

    -- * Scripts
  , genHashableScriptData
  , genReferenceScript
  , genScript
  , genSimpleScript
  , genPlutusScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , genScriptData
  , genScriptDataSchema
  , genScriptValidity

  , genAssetName
  , genAssetId
  , genCardanoKeyWitness
  , genEpochNo
  , genExecutionUnitPrices
  , genExecutionUnits
  , genHashScriptData
  , genKESPeriod
  , genNat
  , genNetworkId
  , genNetworkMagic
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genOperationalCertificateWithCounter
  , genPaymentCredential
  , genPolicyId
  , genQuantity
  , genRationalInt64
  , genSeed
  , genShelleyBootstrapWitness
  , genShelleyHash
  , genShelleyWitness
  , genShelleyWitnessSigningKey
  , genSignedQuantity
  , genSigningKey
  , genSlotNo
  , genStakeAddress
  , genStakeAddressReference
  , genStakeCredential
  , genTtl
  , genTx
  , genTxAuxScripts
  , genTxBody
  , genTxBodyContent
  , genTxCertificates
  , genTxFee
  , genTxIndex
  , genTxInsCollateral
  , genTxInsReference
  , genTxMetadataInEra
  , genTxMintValue
  , genLovelace
  , genValue
  , genValueDefault
  , genVerificationKey
  , genVerificationKeyHash
  , genUpdateProposal
  , genProtocolParametersUpdate
  , genScriptDataSupportedInAlonzoEra
  , genTxOutDatumHashTxContext
  , genTxOutDatumHashUTxOContext
  , genTxOutValue
  , genTxReturnCollateral
  , genTxScriptValidity
  , genTxTotalCollateral
  , genTxUpdateProposal
  , genTxValidityLowerBound
  , genTxValidityRange
  , genTxValidityUpperBound
  , genTxWithdrawals
  , genUnsignedQuantity
  , genValueForMinting
  , genValueForTxOut
  , genWitnesses
  , genWitnessNetworkIdOrByronAddress

  , genRational
  ) where

import           Cardano.Api hiding (txIns)
import qualified Cardano.Api as Api
import           Cardano.Api.Byron (KeyWitness (ByronKeyWitness),
                   WitnessNetworkIdOrByronAddress (..))
import           Cardano.Api.Shelley (Hash (..), KESPeriod (KESPeriod),
                   OperationalCertificateIssueCounter (OperationalCertificateIssueCounter),
                   PlutusScript (PlutusScriptSerialised), ProtocolParameters (..),
                   ReferenceScript (..), ReferenceTxInsScriptsInlineDatumsSupportedInEra (..),
                   StakeCredential (StakeCredentialByKey), StakePoolKey,
                   refInsScriptsAndInlineDatsSupportedInEra)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio (Ratio, (%))
import           Data.String
import           Data.Word (Word64)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Shelley.TxBody as Ledger (EraIndependentTxBody)
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Plutus

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Numeric.Natural (Natural)

import qualified Cardano.Crypto.Hash.Class as CRYPTO
import           Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.SafeHash (unsafeMakeSafeHash)

import           Data.Functor (($>))
import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)
import           Test.Gen.Cardano.Api.Metadata (genTxMetadata)

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use let" -}

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: CardanoEra era -> Gen (AddressInEra era)
genAddressInEra era =
  case cardanoEraStyle era of
    LegacyByronEra ->
      byronAddressInEra <$> genAddressByron

    ShelleyBasedEra _ ->
      Gen.choice
        [ byronAddressInEra   <$> genAddressByron
        , shelleyAddressInEra <$> genAddressShelley
        ]

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen Lovelace
genLovelace = Lovelace <$> Gen.integral (Range.linear 0 5000)


----------------------------------------------------------------------------
-- SimpleScript generators
--

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript SimpleScriptLanguage =
    SimpleScript <$> genSimpleScript
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

genSimpleScript :: Gen SimpleScript
genSimpleScript =
    genTerm
  where
    genTerm = Gen.recursive Gen.choice nonRecursive recursive

    -- Non-recursive generators
    nonRecursive =
      [ RequireSignature . verificationKeyHash <$> genVerificationKey AsPaymentKey
      , RequireTimeBefore <$> genSlotNo
      , RequireTimeAfter <$> genSlotNo
      ]

    -- Recursive generators
    recursive =
      [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm

      , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (List.length ts))
           return (RequireMOf m ts)
      ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> Gen.bytes (Range.linear 0 32)

genScriptDataSchema :: Gen ScriptDataJsonSchema
genScriptDataSchema = Gen.element [ScriptDataJsonNoSchema, ScriptDataJsonDetailedSchema]

genHashableScriptData :: Gen HashableScriptData
genHashableScriptData = do
  sd <- genScriptData
  case deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR sd of
    Left e -> error $ "genHashableScriptData: " <> show e
    Right r -> return r


{-# DEPRECATED genScriptData "Use genHashableScriptData" #-}
genScriptData :: Gen ScriptData
genScriptData =
    Gen.recursive
      Gen.choice
        [ ScriptDataNumber <$> genInteger
        , ScriptDataBytes  <$> genByteString
        ]
        -- The Gen.recursive combinator calls these with the size halved
        [ ScriptDataConstructor <$> genConstructorInteger
                                <*> genScriptDataList
        , ScriptDataList <$> genScriptDataList
        , ScriptDataMap  <$> genScriptDataMap
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    (-fromIntegral (maxBound :: Word64) :: Integer)
                    (2 * fromIntegral (maxBound :: Word64) :: Integer))


    genConstructorInteger :: Gen Integer
    genConstructorInteger = Gen.integral
                  (Range.linear
                    0 -- TODO: Alonzo should be -> (-fromIntegral (maxBound :: Word64) :: Integer)
                      -- Wrapping bug needs to be fixed in Plutus library
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

    genScriptDataList :: Gen [ScriptData]
    genScriptDataList =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) genScriptData

    genScriptDataMap  :: Gen [(ScriptData, ScriptData)]
    genScriptDataMap =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) $
          (,) <$> genScriptData <*> genScriptData


-- ----------------------------------------------------------------------------
-- Script generators for any language, or any language valid in a specific era
--

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
    Gen.choice
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    Gen.choice
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)


----------------------------------------------------------------------------
-- Multi-asset generators
--

genAssetName :: Gen AssetName
genAssetName =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element ["", "a", "b", "c"])
    , (1, AssetName <$> Gen.bytes (Range.singleton  32))
    , (1, AssetName <$> Gen.bytes (Range.constant 1 31))
    ]

genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId = Gen.choice [ AssetId <$> genPolicyId <*> genAssetName
                        , return AdaAssetId
                        ]

genQuantity :: Range Integer -> Gen Quantity
genQuantity range = fromInteger <$> Gen.integral range

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity (Range.constantFrom 0 (-2) 2)

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    Gen.list (Range.constant 0 10)
             ((,) <$> genAId <*> genQuant)

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: Gen Value
genValueDefault = genValue genAssetId genSignedQuantity

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity


-- Note that we expect to sometimes generate duplicate policy id keys since we
-- pick 90% of policy ids from a set of just three.
genValueNestedRep :: Gen ValueNestedRep
genValueNestedRep =
  ValueNestedRep <$> Gen.list (Range.constant 0 5) genValueNestedBundle

genValueNestedBundle :: Gen ValueNestedBundle
genValueNestedBundle =
  Gen.choice
    [ ValueNestedBundleAda <$> genSignedQuantity
    , ValueNestedBundle <$> genPolicyId
                        <*> Gen.map (Range.constant 0 5)
                                    ((,) <$> genAssetName <*> genSignedQuantity)
    ]

genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

genOperationalCertificate :: Gen OperationalCertificate
genOperationalCertificate = fst <$> genOperationalCertificateWithCounter

genOperationalCertificateIssueCounter :: Gen OperationalCertificateIssueCounter
genOperationalCertificateIssueCounter = snd <$> genOperationalCertificateWithCounter

genOperationalCertificateWithCounter :: Gen (OperationalCertificate, OperationalCertificateIssueCounter)
genOperationalCertificateWithCounter = do
    kesVKey <- genVerificationKey AsKesKey
    stkPoolOrGenDelExtSign <- Gen.either (genSigningKey AsStakePoolKey) (genSigningKey AsGenesisDelegateExtendedKey)
    kesP <- genKESPeriod
    c <- Gen.integral $ Range.linear 0 1000
    let stakePoolVer = either getVerificationKey (convert . getVerificationKey) stkPoolOrGenDelExtSign
        iCounter = OperationalCertificateIssueCounter c stakePoolVer

    case issueOperationalCertificate kesVKey stkPoolOrGenDelExtSign kesP iCounter of
      -- This case should be impossible as we clearly derive the verification
      -- key from the generated signing key.
      Left err -> fail $ displayError err
      Right pair -> return pair
  where
    convert :: VerificationKey GenesisDelegateExtendedKey
            -> VerificationKey StakePoolKey
    convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                   -> VerificationKey StakePoolKey)
            . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                   -> VerificationKey GenesisDelegateKey)


-- TODO: Generate payment credential via script
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddress :: Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

-- TODO: Generate StakeAddressReference via pointer
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential
    , return NoStakeAddress
    ]

-- TODO: Generate StakeCredential via script
genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> TxOutAdaOnly adaOnlyInEra <$> genLovelace
    Right multiAssetInEra -> TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOutTxContext :: CardanoEra era -> Gen (TxOut CtxTx era)
genTxOutTxContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashTxContext era
        <*> genReferenceScript era

genTxOutUTxOContext :: CardanoEra era -> Gen (TxOut CtxUTxO era)
genTxOutUTxOContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashUTxOContext era
        <*> genReferenceScript era

genReferenceScript :: CardanoEra era -> Gen (ReferenceScript era)
genReferenceScript era =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> return ReferenceScriptNone
    Just supp -> ReferenceScript supp <$> genScriptInAnyLang

genUTxO :: CardanoEra era -> Gen (UTxO era)
genUTxO era =
  UTxO <$> Gen.map (Range.constant 0 5) ((,) <$> genTxIn <*> (toCtxUTxOTxOut <$> genTxOutTxContext era))

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      error "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra supported <$> genTxMetadata
        ]

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        Gen.list (Range.linear 0 3)
                 (genScriptInEra era)

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing -> pure TxWithdrawalsNone
    Just supported ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals supported mempty)
          -- TODO: Generate withdrawals
        ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
  case certificatesSupportedInEra era of
    Nothing -> pure TxCertificatesNone
    Just supported -> do
      certs <- Gen.list (Range.constant 0 3) genCertificate
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates supported certs $ BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

-- TODO: Add remaining certificates
genCertificate :: Gen Certificate
genCertificate =
  Gen.choice
    [ StakeAddressRegistrationCertificate <$> genStakeCredential
    , StakeAddressDeregistrationCertificate <$> genStakeCredential
    ]

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case updateProposalSupportedInEra era of
    Nothing -> pure TxUpdateProposalNone
    Just supported ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , TxUpdateProposal supported <$> genUpdateProposal era
        ]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
  txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txInsCollateral <- genTxInsCollateral era
  txInsReference <- genTxInsReference era
  txOuts <-
    Gen.list (Range.constant 1 10) (genTxOutTxContext era)
    <&> fixDatumHashCollisions
      -- Without this fix, generated script data may have the same hashes for
      -- both present (value + hash) and non-present (hash only) values.
  txTotalCollateral <- genTxTotalCollateral era
  txReturnCollateral <- genTxReturnCollateral era
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- BuildTxWith <$> Gen.maybe (genProtocolParameters era)
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txInsReference
    , Api.txOuts
    , Api.txTotalCollateral
    , Api.txReturnCollateral
    , Api.txFee
    , Api.txValidityRange
    , Api.txMetadata
    , Api.txAuxScripts
    , Api.txExtraKeyWits
    , Api.txProtocolParams
    , Api.txWithdrawals
    , Api.txCertificates
    , Api.txUpdateProposal
    , Api.txMintValue
    , Api.txScriptValidity
    }


-- | Ensure that all script data with the same hash are
-- either all presented as values or all presented as hashes.
--
-- It's possible to have a hash without its datum, and also with its datum,
-- and these two possibilities are semantically equivalent.
fixDatumHashCollisions :: forall era. [TxOut CtxTx era] -> [TxOut CtxTx era]
fixDatumHashCollisions outs = map replaceOutHashWithItsDatum outs
  where
    replaceOutHashWithItsDatum :: TxOut CtxTx era -> TxOut CtxTx era
    replaceOutHashWithItsDatum (TxOut address value datum script) =
      TxOut address value (replaceHashWithItsDatum datum) script

    replaceHashWithItsDatum :: TxOutDatum CtxTx era -> TxOutDatum CtxTx era
    replaceHashWithItsDatum datum =
      case datum of
        TxOutDatumHash _ hash -> fromMaybe datum $ Map.lookup hash hashedData
        _ -> datum

    hashedData :: Map (Hash ScriptData) (TxOutDatum CtxTx era)
    hashedData =
      Map.fromList
        [ (hashScriptDataBytes scriptData, datum)
        | TxOut _ _ datum@(TxOutDatumInTx _ scriptData) _ <- outs
        ]


genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> Gen.choice
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> Gen.list (Range.linear 0 10) genTxIn
                          ]

genTxInsReference :: CardanoEra era -> Gen (TxInsReference BuildTx era)
genTxInsReference era =
    case refInsScriptsAndInlineDatsSupportedInEra era of
      Nothing        -> pure TxInsReferenceNone
      Just supported -> TxInsReference supported <$> Gen.list (Range.linear 0 10) genTxIn

genTxReturnCollateral :: CardanoEra era -> Gen (TxReturnCollateral CtxTx era)
genTxReturnCollateral era =
  case totalAndReturnCollateralSupportedInEra  era of
    Nothing -> return TxReturnCollateralNone
    Just supp ->
      TxReturnCollateral supp <$>  genTxOutTxContext era

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral era =
  case totalAndReturnCollateralSupportedInEra  era of
    Nothing -> return TxTotalCollateralNone
    Just supp ->
      TxTotalCollateral supp <$> genLovelace

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> genLovelace

genTxBody :: IsCardanoEra era => CardanoEra era -> Gen (TxBody era)
genTxBody era = do
  res <- Api.createAndValidateTransactionBody <$> genTxBodyContent era
  case res of
    Left err -> fail (displayError err)
    Right txBody -> pure txBody

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTx :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> genTxBody era

genWitnesses :: CardanoEra era -> Gen [KeyWitness era]
genWitnesses era =
  case cardanoEraStyle era of
    LegacyByronEra    -> Gen.list (Range.constant 1 10) genByronKeyWitness
    ShelleyBasedEra _ -> do
      bsWits  <- Gen.list (Range.constant 0 10)
                          (genShelleyBootstrapWitness era)
      keyWits <- Gen.list (Range.constant 0 10)
                          (genShelleyKeyWitness era)
      return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genByronKeyWitness :: Gen (KeyWitness ByronEra)
genByronKeyWitness = do
  pmId <- genProtocolMagicId
  txinWitness <- genVKWitness pmId
  return $ ByronKeyWitness txinWitness

genWitnessNetworkIdOrByronAddress :: Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  Gen.choice
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

genShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyBootstrapWitness era =
 makeShelleyBootstrapWitness
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBody era
   <*> genSigningKey AsByronKey

genShelleyKeyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyKeyWitness era =
  makeShelleyKeyWitness
    <$> genTxBody era
    <*> genShelleyWitnessSigningKey

genShelleyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyWitness era =
  Gen.choice
   [ genShelleyKeyWitness era
   , genShelleyBootstrapWitness era
   ]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  Gen.choice [ WitnessPaymentKey <$>  genSigningKey AsPaymentKey
             , WitnessPaymentExtendedKey <$>  genSigningKey AsPaymentExtendedKey
             , WitnessStakeKey <$>  genSigningKey AsStakeKey
             , WitnessStakePoolKey <$>  genSigningKey AsStakePoolKey
             , WitnessGenesisDelegateKey <$>  genSigningKey AsGenesisDelegateKey
             , WitnessGenesisUTxOKey <$>  genSigningKey AsGenesisUTxOKey
             ]

genCardanoKeyWitness
  :: CardanoEra era
  -> Gen (KeyWitness era)
genCardanoKeyWitness era =
  case cardanoEraStyle era of
    LegacyByronEra    -> genByronKeyWitness
    ShelleyBasedEra _ -> genShelleyWitness era

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genRational :: Gen Rational
genRational =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Word64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Word64 -> Rational
    ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genPraosNonce :: Gen PraosNonce
genPraosNonce = makePraosNonce <$> Gen.bytes (Range.linear 0 32)

genMaybePraosNonce :: Gen (Maybe PraosNonce)
genMaybePraosNonce = Gen.maybe genPraosNonce

genProtocolParameters :: CardanoEra era -> Gen ProtocolParameters
genProtocolParameters era = do
  protocolParamProtocolVersion <- (,) <$> genNat <*> genNat
  protocolParamDecentralization <- Gen.maybe genRational
  protocolParamExtraPraosEntropy <- genMaybePraosNonce
  protocolParamMaxBlockHeaderSize <- genNat
  protocolParamMaxBlockBodySize <- genNat
  protocolParamMaxTxSize <- genNat
  protocolParamTxFeeFixed <- genNat
  protocolParamTxFeePerByte <- genNat
  protocolParamMinUTxOValue <- Gen.maybe genLovelace
  protocolParamStakeAddressDeposit <- genLovelace
  protocolParamStakePoolDeposit <- genLovelace
  protocolParamMinPoolCost <- genLovelace
  protocolParamPoolRetireMaxEpoch <- genEpochNo
  protocolParamStakePoolTargetNum <- genNat
  protocolParamPoolPledgeInfluence <- genRationalInt64
  protocolParamMonetaryExpansion <- genRational
  protocolParamTreasuryCut <- genRational
  protocolParamUTxOCostPerWord <- sequence $ protocolUTxOCostPerWordSupportedInEra era $> genLovelace
  protocolParamCostModels <- pure mempty
  --TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolParamPrices <- Gen.maybe genExecutionUnitPrices
  protocolParamMaxTxExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxBlockExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxValueSize <- Gen.maybe genNat
  protocolParamCollateralPercent <- Gen.maybe genNat
  protocolParamMaxCollateralInputs <- Gen.maybe genNat
  protocolParamUTxOCostPerByte <- sequence $ protocolUTxOCostPerByteSupportedInEra era $> genLovelace

  pure ProtocolParameters {..}

genProtocolParametersUpdate :: CardanoEra era -> Gen ProtocolParametersUpdate
genProtocolParametersUpdate era = do
  protocolUpdateProtocolVersion     <- Gen.maybe ((,) <$> genNat <*> genNat)
  protocolUpdateDecentralization    <- Gen.maybe genRational
  protocolUpdateExtraPraosEntropy   <- Gen.maybe genMaybePraosNonce
  protocolUpdateMaxBlockHeaderSize  <- Gen.maybe genNat
  protocolUpdateMaxBlockBodySize    <- Gen.maybe genNat
  protocolUpdateMaxTxSize           <- Gen.maybe genNat
  protocolUpdateTxFeeFixed          <- Gen.maybe genNat
  protocolUpdateTxFeePerByte        <- Gen.maybe genNat
  protocolUpdateMinUTxOValue        <- preAlonzoParam era genLovelace
  protocolUpdateStakeAddressDeposit <- Gen.maybe genLovelace
  protocolUpdateStakePoolDeposit    <- Gen.maybe genLovelace
  protocolUpdateMinPoolCost         <- Gen.maybe genLovelace
  protocolUpdatePoolRetireMaxEpoch  <- Gen.maybe genEpochNo
  protocolUpdateStakePoolTargetNum  <- Gen.maybe genNat
  protocolUpdatePoolPledgeInfluence <- Gen.maybe genRationalInt64
  protocolUpdateMonetaryExpansion   <- Gen.maybe genRational
  protocolUpdateTreasuryCut         <- Gen.maybe genRational
  protocolUpdateUTxOCostPerWord     <- sequence $ protocolUTxOCostPerWordSupportedInEra era $> genLovelace
  protocolUpdateCostModels          <- genCostModels era
  protocolUpdatePrices              <- alonzoParam era genExecutionUnitPrices
  protocolUpdateMaxTxExUnits        <- alonzoParam era genExecutionUnits
  protocolUpdateMaxBlockExUnits     <- alonzoParam era genExecutionUnits
  protocolUpdateMaxValueSize        <- alonzoParam era genNat
  protocolUpdateCollateralPercent   <- alonzoParam era genNat
  protocolUpdateMaxCollateralInputs <- alonzoParam era genNat
  protocolUpdateUTxOCostPerByte     <- sequence $ protocolUTxOCostPerByteSupportedInEra era $> genLovelace

  pure ProtocolParametersUpdate{..}

genUpdateProposal :: CardanoEra era -> Gen UpdateProposal
genUpdateProposal era =
  UpdateProposal
    <$> Gen.map (Range.constant 1 3)
        ( (,)
          <$> genVerificationKeyHash AsGenesisKey
          <*> genProtocolParametersUpdate era
        )
    <*> genEpochNo

genCostModel :: Gen CostModel
genCostModel = do
  let costModelParams = Alonzo.getCostModelParams Plutus.testingCostModelV1
  eCostModel <- Alonzo.mkCostModel <$> genPlutusLanguage
                                   <*> mapM (const $ Gen.integral (Range.linear 0 5000)) costModelParams
  case eCostModel of
    Left err -> error $ "genCostModel: " <> show err
    Right cModel -> return . CostModel $ Alonzo.getCostModelParams cModel

genPlutusLanguage :: Gen Language
genPlutusLanguage = Gen.element [PlutusV1, PlutusV2]

genCostModels :: CardanoEra era -> Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels era
  | anyCardanoEra era >= anyCardanoEra AlonzoEra =
      Gen.map
        (Range.linear 0 (length plutusScriptVersions))
        ((,) <$> Gen.element plutusScriptVersions <*> genCostModel)
  | otherwise = pure Map.empty
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

-- | Gen for Alonzo-specific parameters
alonzoParam :: CardanoEra era -> Gen a -> Gen (Maybe a)
alonzoParam era gen
  | anyCardanoEra era >= anyCardanoEra AlonzoEra = Just <$> gen
  | otherwise = pure Nothing

-- | 'Gen.maybe' but with condition if era is not Alonzo-based
preAlonzoParam :: CardanoEra era -> Gen a -> Gen (Maybe a)
preAlonzoParam era gen
  | anyCardanoEra era >= anyCardanoEra AlonzoEra = pure Nothing
  | otherwise = Gen.maybe gen

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHashTxContext :: CardanoEra era -> Gen (TxOutDatum CtxTx era)
genTxOutDatumHashTxContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInAlonzoEra <$> genHashableScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInBabbageEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInBabbageEra <$> genHashableScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInConwayEra <$> genHashScriptData
                    , TxOutDatumInTx ScriptDataInConwayEra <$> genHashableScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra <$> genHashableScriptData
                    ]

genTxOutDatumHashUTxOContext :: CardanoEra era -> Gen (TxOutDatum CtxUTxO era)
genTxOutDatumHashUTxOContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInBabbageEra <$> genHashScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash ScriptDataInConwayEra <$> genHashScriptData
                    , TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInConwayEra <$> genHashableScriptData
                    ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genScriptDataSupportedInAlonzoEra :: Gen (ScriptDataSupportedInEra AlonzoEra)
genScriptDataSupportedInAlonzoEra = pure ScriptDataInAlonzoEra
