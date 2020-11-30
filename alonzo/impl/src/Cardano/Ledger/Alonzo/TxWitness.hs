{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    TxWitness (TxWitness, witsVKey, witsBoot, witsScript, witsData, witsRdmr),
    EraIndependentScriptData,
    ScriptDataHash,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Data (Data)
import Cardano.Ledger.Alonzo.Scripts (Tag)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Era (Crypto))
import Control.DeepSeq (NFData)
import Data.Coders
import Data.Map.Strict (Map)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics
import GHC.Records
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.TxBody (WitVKey)

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _witsVKey :: Set (WitVKey 'Witness era),
    _witsBoot :: Set (BootstrapWitness era),
    _witsScriptData :: ScriptData era
  }
  deriving (Generic, Typeable)

newtype TxWitness era = TxWitnessConstr (MemoBytes (TxWitnessRaw era))

pattern TxWitness ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness era) ->
  Set (BootstrapWitness era) ->
  Map (ScriptHash era) (Core.Script era) ->
  Set (Data era) ->
  Map RdmrPtr (Data era) ->
  TxWitness era
pattern TxWitness
  { witsVKey,
    witsBoot,
    witsScript,
    witsData,
    witsRdmr
  } <-
  TxWitnessConstr
    ( Memo
        ( TxWitnessRaw
            witsVKey
            witsBoot
            ( ScriptDataConstr
                ( Memo
                    ( ScriptDataRaw
                        witsScript
                        witsData
                        witsRdmr
                      )
                    _
                  )
              )
          )
        _
      )
  where
    TxWitness witsVKey witsBoot witsScript witsDat witsRdmr =
      TxWitnessConstr
        . memoBytes
        $ encodeWitnessRaw witsVKey witsBoot witsScript witsDat witsRdmr

-- | Right-biased semigroup - if there are (somehow) multiple entries either for
-- a given 'ScriptHash' or a given 'Data', this will bias to the entry on the
-- right. Note that this should not happen in practise.
instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (TxWitness era)
  where
  TxWitness a b c d e <> TxWitness a' b' c' d' e' =
    TxWitness
      (a `Set.union` a')
      (b `Set.union` b')
      (c <> c')
      (d `Set.union` d')
      (e <> e')

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Monoid (TxWitness era)
  where
  mempty = TxWitness mempty mempty mempty mempty mempty

--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------
instance
  (Core.Script era ~ script) =>
  HasField "witsScript" (TxWitness era) (Map (ScriptHash era) script)
  where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataScripts
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

instance HasField "witsData" (TxWitness era) (Set (Data era)) where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataData
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

instance HasField "witsRdmrs" (TxWitness era) (Map RdmrPtr (Data era)) where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataRdmrs
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

--------------------------------------------------------------------------------
-- ScriptData
--------------------------------------------------------------------------------

data ScriptDataRaw era = ScriptDataRaw
  { _scriptDataScripts :: Map (ScriptHash era) (Core.Script era),
    _scriptDataData :: Set (Data era),
    _scriptDataRdmrs :: Map RdmrPtr (Data era)
  }

-- | 'ScriptData' is a projection of the parts of 'TxWitness' which may be
-- hashed to include in the transaction body. Note that this cannot be the hash
-- of the entire witness set, since the VKey witnesses themselves contain a hash
-- of the transaction body, creating a circular dependency.
--
-- The 'ScriptData' type itself is internal to this module; it is never directly
-- serialised or deserialised, but will automatically be populated upon
-- deserialisation or creation of 'TxWitness'
newtype ScriptData era = ScriptDataConstr
  { unScriptDataConstr ::
      MemoBytes (ScriptDataRaw era)
  }
  deriving newtype (ToCBOR)

pattern ScriptData ::
  (Era era, ToCBOR (Core.Script era)) =>
  Map (ScriptHash era) (Core.Script era) ->
  Set (Data era) ->
  Map RdmrPtr (Data era) ->
  ScriptData era
pattern ScriptData s d r <-
  ScriptDataConstr (Memo (ScriptDataRaw s d r) _)
  where
    ScriptData s d r =
      ScriptDataConstr $
        memoBytes $
          encodeScriptDataRaw (ScriptDataRaw s d r)

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (ScriptData era)
  where
  (ScriptData s d r)
    <> (ScriptData s' d' r') =
      ScriptData (s <> s') (d `Set.union` d') (r <> r')

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Monoid (ScriptData era)
  where
  mempty = ScriptData mempty mempty mempty

data EraIndependentScriptData

newtype ScriptDataHash era
  = ScriptDataHash
      (Hash.Hash (HASH (Crypto era)) EraIndependentScriptData)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
instance ToCBOR RdmrPtr where
  toCBOR (RdmrPtr t w) = encode $ Rec RdmrPtr !> To t !> To w

instance FromCBOR RdmrPtr where
  fromCBOR = decode $ RecD RdmrPtr <! From <! From

encodeScriptDataRaw ::
  (Era era, ToCBOR (Core.Script era)) =>
  ScriptDataRaw era ->
  Encode ('Closed 'Dense) (ScriptDataRaw era)
encodeScriptDataRaw sdr =
  Rec ScriptDataRaw
    !> E mapToCBOR (_scriptDataScripts sdr)
    !> E encodeFoldable (_scriptDataData sdr)
    !> E mapToCBOR (_scriptDataRdmrs sdr)

instance
  ( FromCBOR (Data era),
    Typeable (Core.Script era),
    FromCBOR (Annotator (Core.Script era)),
    Era era
  ) =>
  FromCBOR (Annotator (ScriptDataRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD ScriptDataRaw)
        <*! D (sequence <$> mapFromCBOR)
        <*! Ann (D (decodeSet fromCBOR))
        <*! Ann (D mapFromCBOR)

deriving via
  (Mem (ScriptDataRaw era))
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      FromCBOR (Data era),
      Typeable (Core.Script era)
    ) =>
    FromCBOR (Annotator (ScriptData era))

-- | Encode witness information.
encodeWitnessRaw ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness era) ->
  Set (BootstrapWitness era) ->
  Map (ScriptHash era) (Core.Script era) ->
  Set (Data era) ->
  Map RdmrPtr (Data era) ->
  Encode ('Closed Dense) (TxWitnessRaw era)
encodeWitnessRaw a b s d r =
  Rec TxWitnessRaw
    !> E encodeFoldable a
    !> E encodeFoldable b
    !> To (ScriptData s d r)

instance
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (Data era),
    ToCBOR (Core.Script era),
    Typeable (Core.Script era)
  ) =>
  FromCBOR (Annotator (TxWitnessRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxWitnessRaw)
        <*! D (fmap Set.fromList . sequence <$> decodeList fromCBOR)
        <*! D (fmap Set.fromList . sequence <$> decodeList fromCBOR)
        <*! From
