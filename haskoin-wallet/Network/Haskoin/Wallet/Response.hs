module Network.Haskoin.Wallet.Response where

import           Control.DeepSeq                 (NFData (..))
import           Data.Aeson.TH
import           Data.Int                        (Int64)
import           Data.Text
import           Data.Time                       (UTCTime)
import           Data.Word
import qualified Network.Haskoin.Block           as B
import qualified Network.Haskoin.Crypto          as C
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Script
import qualified Network.Haskoin.Transaction     as T
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Types

data WalletResponse a
    = ResponseData { responseResult :: !a }
    | ResponseError { responseError :: !Text }
    | ResponseOK
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "status") ''WalletResponse)

data Account = Account
    { accountName     :: !Text
    , accountType     :: !AccountType
    , accountMaster   :: !(Maybe C.XPrvKey)
    , accountMnemonic :: !(Maybe Text)
    , accountKeys     :: ![C.XPubKey]
    , accountGap      :: !Word32
    , accountCreated  :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 7) ''Account)

data Balance = Balance
    { balanceInBalance  :: !Word64
    , balanceOutBalance :: !Word64
    , balanceCoins      :: !Int
    , balanceSpentCoins :: !Int
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 7) ''Balance)

instance NFData Balance where
    rnf Balance{..} =
        rnf balanceInBalance `seq`
        rnf balanceOutBalance `seq`
        rnf balanceCoins `seq`
        rnf balanceSpentCoins

data Address = Address
    { addressAddress :: !C.Address
    , addressIndex   :: !C.KeyIndex
    , addressType    :: !AddressType
    , addressLabel   :: !Text
    , addressRedeem  :: !(Maybe ScriptOutput)
    , addressKey     :: !(Maybe C.PubKeyC)
    , addressBalance :: !(Maybe Balance)
    , addressCreated :: !UTCTime
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 7) ''Address)

data Tx = Tx
    { txTxHash          :: !T.TxHash
    , txNosigHash       :: !T.TxHash
    , txType            :: !TxType
    , txInValue         :: !Word64
    , txOutValue        :: !Word64
    , txValue           :: !Int64
    , txInputs          :: ![AddressInfo]
    , txOutputs         :: ![AddressInfo]
    , txChange          :: ![AddressInfo]
    , txTx              :: !T.Tx
    , txIsCoinbase      :: !Bool
    , txConfidence      :: !TxConfidence
    , txConfirmedBy     :: !(Maybe B.BlockHash)
    , txConfirmedHeight :: !(Maybe Word32)
    , txConfirmedDate   :: !(Maybe Word32)
    , txCreated         :: !UTCTime
    , txAccount         :: !AccountName
    , txConfirmations   :: !(Maybe Word32)
    , txBestBlock       :: !(Maybe B.BlockHash)
    , txBestBlockHeight :: !(Maybe BlockHeight)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 2) ''Tx)

data Coin = Coin
    { coinHash       :: !T.TxHash
    , coinPos        :: !Word32
    , coinValue      :: !Word64
    , coinScript     :: !ScriptOutput
    , coinCreated    :: !UTCTime
    , coinTx         :: !(Maybe Tx)
    , coinAddress    :: !(Maybe Address)
    , coinSpendingTx :: !(Maybe Tx)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 4) ''Coin)

data Block = Block
    { blockHash   :: !B.BlockHash
    , blockHeight :: !BlockHeight
    , blockPrev   :: !B.BlockHash
    , blockTxs    :: ![Tx]
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 5) ''Block)

data BlockInfo = BlockInfo
   { blockInfoHash          :: !B.BlockHash
   , blockInfoHeight        :: !BlockHeight
   , blockInfoVersion       :: !Word32
   , blockInfoTimestamp     :: !UTCTime
   , blockInfoPrevBlock     :: !B.BlockHash
   , blockInfoMerkleRoot    :: !C.Hash256
   , blockInfoBits          :: !Word32
   , blockInfoNonce         :: !Word32
   , blockInfoChain         :: !Word32
   , blockInfoChainWork     :: !Double
   } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 9) ''BlockInfo)

data List a = List
    { listItems :: ![a]
    , listTotal :: !Word32
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 4) ''List)

data TxComplete = TxComplete
    { txCompleteTx       :: !T.Tx
    , txCompleteComplete :: !Bool
    } deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 10) ''TxComplete)

data Notif
    = NotifBlock !Block
    | NotifTx    !Tx
    deriving (Eq, Show, Read)

$(deriveJSON (dropSumLabels 5 5 "type") ''Notif)

