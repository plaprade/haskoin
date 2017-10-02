{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Types where

import           Control.DeepSeq                        (NFData (..))
import           Control.Exception                      (Exception)
import           Control.Monad
import           Control.Monad.Trans                    (MonadIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString                        as BS
import           Data.Char                              (toLower)
import           Data.Int                               (Int64)
import           Data.List.Split                        (chunksOf)
import qualified Data.Serialize                         as S
import           Data.String                            (fromString)
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)
import           Data.Typeable                          (Typeable)
import           Data.Word                              (Word32, Word64, Word8)
import qualified Database.Esqueleto                     as E
import           Database.Esqueleto.Internal.Sql        (SqlSelect)
import qualified Database.Persist                       as P
import           Database.Persist.Class
import           Database.Persist.Sql
import           GHC.Generics
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Database

type AccountName = Text

-- TODO: Add NFData instances for all those types

data TxType
    = TxIncoming
    | TxOutgoing
    | TxSelf
    deriving (Eq, Show, Read)

instance NFData TxType where
    rnf x = x `seq` ()

$(deriveJSON (dropSumLabels 2 0 "") ''TxType)

data TxConfidence
    = TxOffline
    | TxDead
    | TxPending
    | TxBuilding
    deriving (Eq, Show, Read)

instance NFData TxConfidence where
    rnf x = x `seq` ()

$(deriveJSON (dropSumLabels 2 0 "") ''TxConfidence)

data AccountType
    = AccountRegular
    | AccountMultisig
        { accountTypeRequiredSigs :: !Int
        , accountTypeTotalKeys    :: !Int
        }
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 7 11 "type") ''AccountType)

instance NFData AccountType where
    rnf t = case t of
        AccountRegular -> ()
        AccountMultisig m n -> rnf m `seq` rnf n

data AddressType
    = AddressInternal
    | AddressExternal
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 7 0 "") ''AddressType)

instance NFData AddressType where
    rnf x = x `seq` ()

addrTypeIndex :: AddressType -> KeyIndex
addrTypeIndex AddressExternal = 0
addrTypeIndex AddressInternal = 1

data CoinSignData = CoinSignData
    { coinSignOutPoint     :: !OutPoint
    , coinSignScriptOutput :: !ScriptOutput
    , coinSignDeriv        :: !SoftPath
    }
    deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 8) ''CoinSignData)

instance S.Serialize CoinSignData where
    get = do
        p <- S.get
        s <- readBS >>= \bs -> case decodeOutputBS bs of
            Right s -> return s
            _ -> error "Invalid ScriptOutput in CoinSignData"
        S.get >>= \dM -> case toSoft (dM :: DerivPath) of
            Just d -> return $ CoinSignData p s d
            _ -> error "Invalid derivation in CoinSignData"
      where
        readBS = S.get >>= \(VarInt l) -> S.getByteString $ fromIntegral l

    put (CoinSignData p s d) = do
        S.put p
        writeBS $ encodeOutputBS s
        S.put $ toGeneric d
      where
        writeBS bs = do
            S.put $ VarInt $ fromIntegral $ BS.length bs
            S.putByteString bs

data OfflineTxData = OfflineTxData
    { offlineTxDataTx       :: !Tx
    , offlineTxDataCoinData :: ![CoinSignData]
    }
    deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 13) ''OfflineTxData)

instance S.Serialize OfflineTxData where
    get = OfflineTxData <$> S.get <*> (replicateList =<< S.get)
      where
        replicateList (VarInt c) = replicateM (fromIntegral c) S.get

    put (OfflineTxData t ds) = do
        S.put t
        S.put $ VarInt $ fromIntegral $ length ds
        forM_ ds S.put

data AddressInfo = AddressInfo
    { addressInfoAddress :: !Address
    , addressInfoValue   :: !(Maybe Word64)
    , addressInfoIsLocal :: !Bool
    }
    deriving (Eq, Show, Read, Generic)

instance S.Serialize AddressInfo

$(deriveJSON (dropFieldLabel 11) ''AddressInfo)

instance NFData AddressInfo where
    rnf AddressInfo{..} =
        rnf addressInfoAddress `seq`
        rnf addressInfoValue `seq`
        rnf addressInfoIsLocal

{- Helper Types -}

data WalletException = WalletException !String
    deriving (Eq, Read, Show, Typeable)

instance Exception WalletException

data BTCNode = BTCNode { btcNodeHost :: !String, btcNodePort :: !Int }
    deriving (Eq, Read, Show)

requestJson :: Int -> Int -> Options
requestJson c f = (dropSumLabels c f "method")
    { tagSingleConstructors = True
    , allNullaryToStringTag = False
    }

$(deriveJSON (dropFieldLabel 7) ''BTCNode)

{- Persistent Instances -}

instance PersistField XPrvKey where
    toPersistValue = PersistText . cs . xPrvExport
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent XPrvKey" $ xPrvImport $ cs txt
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent XPrvKey" $ xPrvImport bs
    fromPersistValue _ = Left "Invalid Persistent XPrvKey"

instance PersistFieldSql XPrvKey where
    sqlType _ = SqlString

instance PersistField [XPubKey] where
    toPersistValue = PersistText . cs . encode
    fromPersistValue (PersistText txt) =
        maybeToEither "Invalid Persistent XPubKey" $ decodeStrict' $ cs txt
    fromPersistValue (PersistByteString bs) =
        maybeToEither "Invalid Persistent XPubKey" $ decodeStrict' bs
    fromPersistValue _ = Left "Invalid Persistent XPubKey"

instance PersistFieldSql [XPubKey] where
    sqlType _ = SqlString

instance PersistField DerivPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent DerivPath" . fmap getParsedPath .
        parsePath . cs $ txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent DerivPath" . fmap getParsedPath .
        parsePath . cs $ bs
    fromPersistValue _ = Left "Invalid Persistent DerivPath"

instance PersistFieldSql DerivPath where
    sqlType _ = SqlString

instance PersistField HardPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent HardPath" $ parseHard $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent HardPath" $ parseHard $ cs bs
    fromPersistValue _ = Left "Invalid Persistent HardPath"

instance PersistFieldSql HardPath where
    sqlType _ = SqlString

instance PersistField SoftPath where
    toPersistValue = PersistText . cs . pathToStr
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent SoftPath" $ parseSoft $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent SoftPath" $ parseSoft $ cs bs
    fromPersistValue _ = Left "Invalid Persistent SoftPath"

instance PersistFieldSql SoftPath where
    sqlType _ = SqlString

instance PersistField AccountType where
    toPersistValue = PersistText . cs . encode
    fromPersistValue (PersistText txt) = maybeToEither
        "Invalid Persistent AccountType" $ decodeStrict' $ cs txt
    fromPersistValue (PersistByteString bs) = maybeToEither
        "Invalid Persistent AccountType" $ decodeStrict' bs
    fromPersistValue _ = Left "Invalid Persistent AccountType"

instance PersistFieldSql AccountType where
    sqlType _ = SqlString

instance PersistField AddressType where
    toPersistValue ts = PersistBool $ case ts of
        AddressExternal -> True
        AddressInternal -> False

    fromPersistValue (PersistBool b) = return $
        if b then AddressExternal else AddressInternal

    fromPersistValue (PersistInt64 i) = return $ case i of
        0 -> AddressInternal
        _ -> AddressExternal

    fromPersistValue _ = Left "Invalid Persistent AddressType"

instance PersistFieldSql AddressType where
    sqlType _ = SqlBool

instance PersistField TxType where
    toPersistValue ts = PersistText $ case ts of
        TxIncoming -> "incoming"
        TxOutgoing -> "outgoing"
        TxSelf     -> "self"

    fromPersistValue (PersistText txt) = case txt of
        "incoming" -> return TxIncoming
        "outgoing" -> return TxOutgoing
        "self"     -> return TxSelf
        _ -> Left "Invalid Persistent TxType"

    fromPersistValue (PersistByteString bs) = case bs of
        "incoming" -> return TxIncoming
        "outgoing" -> return TxOutgoing
        "self"     -> return TxSelf
        _ -> Left "Invalid Persistent TxType"

    fromPersistValue _ = Left "Invalid Persistent TxType"

instance PersistFieldSql TxType where
    sqlType _ = SqlString

instance PersistField Address where
    toPersistValue = PersistText . cs . addrToBase58
    fromPersistValue (PersistText a) =
        maybeToEither "Invalid Persistent Address" $ base58ToAddr $ cs a
    fromPersistValue (PersistByteString a) =
        maybeToEither "Invalid Persistent Address" $ base58ToAddr a
    fromPersistValue _ = Left "Invalid Persistent Address"

instance PersistFieldSql Address where
    sqlType _ = SqlString

instance PersistField BloomFilter where
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left  (fromString e)
    fromPersistValue _ = Left "Invalid Persistent BloomFilter"

instance PersistFieldSql BloomFilter where
    sqlType _ = SqlBlob

instance PersistField BlockHash where
    toPersistValue = PersistText . cs . blockHashToHex
    fromPersistValue (PersistText h) =
        maybeToEither "Could not decode BlockHash" $ hexToBlockHash $ cs h
    fromPersistValue (PersistByteString h) =
        maybeToEither "Could not decode BlockHash" $ hexToBlockHash h
    fromPersistValue _ = Left "Invalid Persistent BlockHash"

instance PersistFieldSql BlockHash where
    sqlType _ = SqlString

instance PersistField TxHash where
    toPersistValue = PersistText . cs . txHashToHex
    fromPersistValue (PersistText h) =
        maybeToEither "Invalid Persistent TxHash" $ hexToTxHash $ cs h
    fromPersistValue (PersistByteString h) =
        maybeToEither "Invalid Persistent TxHash" $ hexToTxHash h
    fromPersistValue _ = Left "Invalid Persistent TxHash"

instance PersistFieldSql TxHash where
    sqlType _ = SqlString

instance PersistField TxConfidence where
    toPersistValue tc = PersistText $ case tc of
        TxOffline  -> "offline"
        TxDead     -> "dead"
        TxPending  -> "pending"
        TxBuilding -> "building"

    fromPersistValue (PersistText txt) = case txt of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        _ -> Left "Invalid Persistent TxConfidence"

    fromPersistValue (PersistByteString bs) = case bs of
        "offline"  -> return TxOffline
        "dead"     -> return TxDead
        "pending"  -> return TxPending
        "building" -> return TxBuilding
        _ -> Left "Invalid Persistent TxConfidence"

    fromPersistValue _ = Left "Invalid Persistent TxConfidence"

instance PersistFieldSql TxConfidence where
    sqlType _ = SqlString

instance PersistField Tx where
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent Tx"

instance PersistFieldSql Tx where
    sqlType _ = SqlOther "MEDIUMBLOB"

instance PersistField PubKeyC where
    toPersistValue = PersistText . cs . encodeHex . S.encode
    fromPersistValue (PersistText txt) =
        case hex >>= S.decode of
            Right x -> Right x
            Left  e -> Left (fromString e)
      where
        hex = maybeToEither "Could not decode hex" (decodeHex (cs txt))
    fromPersistValue (PersistByteString bs) =
        case hex >>= S.decode of
            Right x -> Right x
            Left  e -> Left (fromString e)
      where
        hex = maybeToEither "Could not decode hex" (decodeHex bs)
    fromPersistValue _ = Left "Invalid Persistent PubKeyC"

instance PersistFieldSql PubKeyC where
    sqlType _ = SqlString

instance PersistField ScriptOutput where
    toPersistValue = PersistByteString . encodeOutputBS
    fromPersistValue (PersistByteString bs) =
        case decodeOutputBS bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent ScriptOutput"

instance PersistFieldSql ScriptOutput where
    sqlType _ = SqlBlob

instance PersistField [AddressInfo] where
    toPersistValue = PersistByteString . S.encode
    fromPersistValue (PersistByteString bs) =
        case S.decode bs of
            Right x -> Right x
            Left  e -> Left (fromString e)
    fromPersistValue _ = Left "Invalid Persistent AddressInfo"

instance PersistFieldSql [AddressInfo] where
    sqlType _ = SqlOther "MEDIUMBLOB"

{- Helpers -}

-- Join AND expressions with OR conditions in a binary way
join2 :: [E.SqlExpr (E.Value Bool)] -> E.SqlExpr (E.Value Bool)
join2 xs = case xs of
    [] -> E.val False
    [x] -> x
    _ -> let (ls,rs) = splitAt (length xs `div` 2) xs
         in  join2 ls E.||. join2 rs

splitSelect :: (SqlSelect a r, MonadIO m)
            => [t]
            -> ([t] -> E.SqlQuery a)
            -> E.SqlPersistT m [r]
splitSelect ts queryF =
    fmap concat $ forM vals $ E.select . queryF
  where
    vals = chunksOf paramLimit ts

splitUpdate :: ( MonadIO m
               , P.PersistEntity val
               , P.PersistEntityBackend val ~ E.SqlBackend
               )
            => [t]
            -> ([t] -> E.SqlExpr (E.Entity val) -> E.SqlQuery ())
            -> E.SqlPersistT m ()
splitUpdate ts updateF =
    forM_ vals $ E.update . updateF
  where
    vals = chunksOf paramLimit ts

splitDelete :: MonadIO m => [t] -> ([t] -> E.SqlQuery ()) -> E.SqlPersistT m ()
splitDelete ts deleteF =
    forM_ vals $ E.delete . deleteF
  where
    vals = chunksOf paramLimit ts

splitInsertMany_ :: ( MonadIO m
                    , P.PersistEntity val
                    , P.PersistEntityBackend val ~ E.SqlBackend
                    )
                 => [val] -> E.SqlPersistT m ()
splitInsertMany_ = mapM_ P.insertMany_ . chunksOf paramLimit

limitOffset :: Word32 -> Word32 -> E.SqlQuery ()
limitOffset l o = do
    when (l > 0) $ E.limit  $ fromIntegral l
    when (o > 0) $ E.offset $ fromIntegral o

