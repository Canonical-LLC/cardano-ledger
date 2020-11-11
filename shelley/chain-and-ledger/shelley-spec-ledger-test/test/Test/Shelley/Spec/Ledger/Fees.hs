{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Fees
  ( sizeTests,
  )
where

import qualified Cardano.Ledger.Crypto as Cr
import Cardano.Binary (serialize)
import Cardano.Crypto.VRF (VRFAlgorithm)
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.Val as Val
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map (empty, singleton)
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API
  ( Addr,
    Credential (..),
    DCert (..),
    DelegCert (..),
    Delegation (..),
    MultiSig (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    Tx (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    textToDns,
    textToUrl,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    Hash,
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState (txsize)
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    hashScript,
  )
import Shelley.Spec.Ledger.TxBody
  ( PoolMetaData (..),
    StakePoolRelay (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.Hashing(EraIndependentTxBody,HashAnnotated(hashAnnotated))
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock, C)
import Test.Shelley.Spec.Ledger.Generator.Core (genesisId)
import Test.Shelley.Spec.Ledger.Utils (mkAddr, mkKeyPair, mkVRFKeyPair, unsafeMkUnitInterval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

sizeTest ::
  Cr.Crypto c =>
  proxy (ShelleyEra c) ->
  BSL.ByteString ->
  Tx (ShelleyEra c) ->
  Integer ->
  Assertion
sizeTest _ b16 tx s = do
  (Base16.encode (serialize tx) @?= b16) >> (txsize tx @?= s)

alicePay :: forall crypto. Cr.Crypto crypto => KeyPair 'Payment crypto
alicePay = KeyPair @ 'Payment @crypto vk sk
  where
    (sk, vk) = mkKeyPair @crypto (0, 0, 0, 0, 0)

aliceStake :: forall crypto. Cr.Crypto crypto => KeyPair 'Staking crypto
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @crypto (0, 0, 0, 0, 1)

aliceSHK :: forall c. Era (ShelleyEra c) => Credential 'Staking (ShelleyEra c)
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

alicePool :: forall crypto. Cr.Crypto crypto => KeyPair 'StakePool crypto
alicePool = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @crypto (0, 0, 0, 0, 2)

alicePoolKH :: forall crypto. Cr.Crypto crypto => KeyHash 'StakePool crypto
alicePoolKH = (hashKey . vKey) alicePool

aliceVRF :: forall v. VRFAlgorithm v => (VRF.SignKeyVRF v, VRF.VerKeyVRF v)
aliceVRF = mkVRFKeyPair (0, 0, 0, 0, 3)

alicePoolParams :: forall c. Era (ShelleyEra c) => PoolParams (ShelleyEra c)
alicePoolParams =
  PoolParams
    { _poolId = alicePoolKH,
      _poolVrf = hashVerKeyVRF . snd $ aliceVRF @(Cr.VRF (Crypto (ShelleyEra c))),
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays =
        StrictSeq.singleton $
          SingleHostName SNothing $
            fromJust $ textToDns "relay.io",
      _poolMD =
        SJust $
          PoolMetaData
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }

aliceAddr :: forall c. Era (ShelleyEra c) => Addr (ShelleyEra c)
aliceAddr = mkAddr (alicePay, aliceStake)

bobPay :: forall crypto. Cr.Crypto crypto => KeyPair 'Payment crypto
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @crypto (1, 0, 0, 0, 0)

bobStake :: forall crypto. Cr.Crypto crypto => KeyPair 'Staking crypto
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @crypto (1, 0, 0, 0, 1)

bobSHK :: forall c. Era (ShelleyEra c) => Credential 'Staking (ShelleyEra c)
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

bobAddr :: forall c. Era (ShelleyEra c) => Addr (ShelleyEra c)
bobAddr = mkAddr (bobPay, bobStake)

carlPay :: forall crypto. Cr.Crypto crypto => KeyPair 'Payment crypto
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (2, 0, 0, 0, 0)

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness
txbSimpleUTxO :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbSimpleUTxO =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

-- | to use makeWitnessVKey, we need to know we can sign the TxBody for that (ShelleyEra c)
type BodySignable era = DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)

txSimpleUTxO :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txSimpleUTxO =
  Tx
    { _body = txbSimpleUTxO,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated $ txbSimpleUTxO @c) [alicePay]
          },
      _metadata = SNothing
    }

txSimpleUTxOBytes16 :: BSL.ByteString
txSimpleUTxOBytes16 = "83a40081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030aa10081824873ed39075e40d2a650ecca0b99fca3d8d173ed39075e40d2a6f6"

-- | Transaction which consumes two UTxO and creates five UTxO
-- | and has two witness
txbMutiUTxO :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbMutiUTxO =
  TxBody
    { _inputs =
        Set.fromList
          [ TxIn genesisId 0,
            TxIn genesisId 1
          ],
      _outputs =
        StrictSeq.fromList
          [ TxOut aliceAddr (Val.inject $ Coin 10),
            TxOut aliceAddr (Val.inject $ Coin 20),
            TxOut aliceAddr (Val.inject $ Coin 30),
            TxOut bobAddr (Val.inject $ Coin 40),
            TxOut bobAddr (Val.inject $ Coin 50)
          ],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 199,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txMutiUTxO :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txMutiUTxO =
  Tx
    { _body = txbMutiUTxO,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated $ txbMutiUTxO @c)
                [ alicePay,
                  bobPay
                ]
          },
      _metadata = SNothing
    }

txMutiUTxOBytes16 :: BSL.ByteString
txMutiUTxOBytes16 = "83a40082824a9db8a41713ad20245f4e00824a9db8a41713ad20245f4e01018582510075c40f44e1c155bedab80d3ec7c2190b0a82510075c40f44e1c155bedab80d3ec7c2190b1482510075c40f44e1c155bedab80d3ec7c2190b181e8251009ed1f6c32150add8a084ba8f6c83c1a518288251009ed1f6c32150add8a084ba8f6c83c1a518320218c7030aa10082824873ed39075e40d2a650f44dc9848e2c0aea73ed39075e40d2a682483e046f8a4a4eeda150f44dc9848e2c0aea3e046f8a4a4eeda1f6"

-- | Transaction which registers a stake key
txbRegisterStake :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbRegisterStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.fromList [DCertDeleg (RegKey aliceSHK)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRegisterStake :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txRegisterStake =
  Tx
    { _body = txbRegisterStake,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated $ txbRegisterStake @c) [alicePay]
          },
      _metadata = SNothing
    }

txRegisterStakeBytes16 :: BSL.ByteString
txRegisterStakeBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a04818200820048dab80d3ec7c2190ba10081824873ed39075e40d2a650e4d2720634c8e8ab73ed39075e40d2a6f6"

-- | Transaction which delegates a stake key
txbDelegateStake :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbDelegateStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs =
        StrictSeq.fromList
          [ DCertDeleg
              (Delegate $ Delegation bobSHK alicePoolKH)
          ],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txDelegateStake :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txDelegateStake =
  Tx
    { _body = txbDelegateStake,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated $ txbDelegateStake @c)
                [asWitness (alicePay), asWitness bobStake]
          },
      _metadata = SNothing
    }

txDelegateStakeBytes16 :: BSL.ByteString
txDelegateStakeBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a04818302820048a084ba8f6c83c1a548bc5edd0d46d5e843a10082824873ed39075e40d2a650a0f42ce9b916eb9d73ed39075e40d2a68248244ad6b5eb5665c750a0f42ce9b916eb9d244ad6b5eb5665c7f6"

-- | Transaction which de-registers a stake key
txbDeregisterStake :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbDeregisterStake =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.fromList [DCertDeleg (DeRegKey aliceSHK)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txDeregisterStake :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txDeregisterStake =
  Tx
    { _body = txbDeregisterStake,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated $ txbDeregisterStake @c)
                [alicePay @(Crypto (ShelleyEra c))]
          },
      _metadata = SNothing
    }

txDeregisterStakeBytes16 :: BSL.ByteString
txDeregisterStakeBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a04818201820048dab80d3ec7c2190ba10081824873ed39075e40d2a650be9cfdc830b8cb5173ed39075e40d2a6f6"

-- | Transaction which registers a stake pool
txbRegisterPool :: Cr.Crypto c => TxBody (ShelleyEra c)
txbRegisterPool =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.fromList [DCertPool (RegPool alicePoolParams)],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRegisterPool :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txRegisterPool =
  Tx
    { _body = txbRegisterPool,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated $ txbRegisterPool @c) [alicePay]
          },
      _metadata = SNothing
    }

txRegisterPoolBytes16 :: BSL.ByteString
txRegisterPoolBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a04818a0348bc5edd0d46d5e8434a3d64a89de764031618600105d81e82010a49e0dab80d3ec7c2190b8148dab80d3ec7c2190b818301f66872656c61792e696f826a616c6963652e706f6f6c427b7da10081824873ed39075e40d2a650d744f1f7d47c27e473ed39075e40d2a6f6"

-- | Transaction which retires a stake pool
txbRetirePool :: forall c. Cr.Crypto c => TxBody (ShelleyEra c)
txbRetirePool =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.fromList [DCertPool (RetirePool alicePoolKH (EpochNo 5))],
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txRetirePool :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txRetirePool =
  Tx
    { _body = txbRetirePool,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated $ txbRetirePool @c) [alicePay]
          },
      _metadata = SNothing
    }

txRetirePoolBytes16 :: BSL.ByteString
txRetirePoolBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a0481830448bc5edd0d46d5e84305a10081824873ed39075e40d2a65024842bb48f600bb073ed39075e40d2a6f6"

-- | Simple Transaction which consumes one UTxO and creates one UTxO
-- | and has one witness
md :: MD.MetaData
md = MD.MetaData $ Map.singleton 0 (MD.List [MD.I 5, MD.S "hello"])

txbWithMD :: Cr.Crypto c => TxBody (ShelleyEra c)
txbWithMD =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SJust $ MD.hashMetaData md
    }

txWithMD :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txWithMD =
  Tx
    { _body = txbWithMD,
      _witnessSet =
        mempty
          { addrWits = makeWitnessesVKey (hashAnnotated $ txbWithMD @c) [alicePay]
          },
      _metadata = SJust md
    }

txWithMDBytes16 :: BSL.ByteString
txWithMDBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a074a4eece6527f366cfa5e71a10081824873ed39075e40d2a650203cdabf7e292c0673ed39075e40d2a6a10082056568656c6c6f"

-- | Spending from a multi-sig address
msig :: forall c. Era (ShelleyEra c) => MultiSig (ShelleyEra c)
msig =
  RequireMOf
    2
    [ (RequireSignature . asWitness . hashKey . vKey) (alicePay),
      (RequireSignature . asWitness . hashKey . vKey) bobPay,
      (RequireSignature . asWitness . hashKey . vKey) carlPay
    ]

txbWithMultiSig :: Cr.Crypto c => TxBody (ShelleyEra c)
txbWithMultiSig =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0], -- acting as if this is multi-sig
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl Map.empty,
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txWithMultiSig :: forall c. Mock c => Tx (ShelleyEra c)
txWithMultiSig =
  Tx
    { _body = txbWithMultiSig,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated $ txbWithMultiSig @c)
                [alicePay, bobPay],
            scriptWits = Map.singleton (hashScript @(ShelleyEra c) msig) msig
          },
      _metadata = SNothing
    }

txWithMultiSigBytes16 :: BSL.ByteString
txWithMultiSigBytes16 = "83a40081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030aa20082824873ed39075e40d2a650ecca0b99fca3d8d173ed39075e40d2a682483e046f8a4a4eeda150ecca0b99fca3d8d13e046f8a4a4eeda101818303028382004875c40f44e1c155be8200489ed1f6c32150add8820048b59ebd7e616fad7ef6"

-- | Transaction with a Reward Withdrawal
txbWithWithdrawal :: Cr.Crypto c => TxBody (ShelleyEra c)
txbWithWithdrawal =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut aliceAddr (Val.inject $ Coin 10)],
      _certs = StrictSeq.empty,
      _wdrls = Wdrl $ Map.singleton (RewardAcnt Testnet aliceSHK) (Val.inject $ Coin 100),
      _txfee = Coin 94,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }

txWithWithdrawal :: forall c. (Cr.Crypto c, BodySignable (ShelleyEra c)) => Tx (ShelleyEra c)
txWithWithdrawal =
  Tx
    { _body = txbWithWithdrawal,
      _witnessSet =
        mempty
          { addrWits =
              makeWitnessesVKey
                (hashAnnotated $ txbWithWithdrawal @c)
                [asWitness (alicePay), asWitness aliceStake]
          },
      _metadata = SNothing
    }

txWithWithdrawalBytes16 :: BSL.ByteString
txWithWithdrawalBytes16 = "83a50081824a9db8a41713ad20245f4e00018182510075c40f44e1c155bedab80d3ec7c2190b0a02185e030a05a149e0dab80d3ec7c2190b1864a10082824873ed39075e40d2a6507b208423b1e15f5173ed39075e40d2a682489ac25c873e2248cc507b208423b1e15f519ac25c873e2248ccf6"

-- NOTE the txsize function takes into account which actual crypto parameter is use.
-- These tests are using MD5Prefix and MockDSIGN so that:
--       the regular hash length is ----> 10
--       the address hash length is ---->  8
--       the verification key size is -->  8
--       the signature size is ---------> 13

sizeTests :: TestTree
sizeTests =
  testGroup
    "Fee Tests"
    [ testCase "simple utxo" $ sizeTest p txSimpleUTxOBytes16 txSimpleUTxO 75,
      testCase "multiple utxo" $ sizeTest p txMutiUTxOBytes16 txMutiUTxO 198,
      testCase "register stake key" $ sizeTest p txRegisterStakeBytes16 txRegisterStake 90,
      testCase "delegate stake key" $ sizeTest p txDelegateStakeBytes16 txDelegateStake 126,
      testCase "deregister stake key" $ sizeTest p txDeregisterStakeBytes16 txDeregisterStake 90,
      testCase "register stake pool" $ sizeTest p txRegisterPoolBytes16 txRegisterPool 154,
      testCase "retire stake pool" $ sizeTest p txRetirePoolBytes16 txRetirePool 89,
      testCase "metadata" $ sizeTest p txWithMDBytes16 txWithMD 96,
      testCase "multisig" $ sizeTest p txWithMultiSigBytes16 txWithMultiSig 141,
      testCase "reward withdrawal" $ sizeTest p txWithWithdrawalBytes16 txWithWithdrawal 116
    ]
  where
    p :: Proxy C
    p = Proxy
