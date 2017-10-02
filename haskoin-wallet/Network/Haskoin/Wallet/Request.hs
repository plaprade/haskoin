module Network.Haskoin.Wallet.Request where

import           Control.DeepSeq                 (NFData (..))
import           Data.Aeson.TH
import           Data.Int                        (Int64)
import           Data.Text
import           Data.Time                       (UTCTime)
import           Data.Word
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import qualified Network.Haskoin.Wallet.Response as R
import           Network.Haskoin.Wallet.Types

data List = List
    { listOffset  :: !Word32
    , listLimit   :: !Word32
    , listReverse :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (dropFieldLabel 4) ''List)

data GetAccount = GetAccount
    { getAccountAccName :: !AccountName }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''GetAccount)

data Accounts = Accounts
    { accountsLocator :: !List }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 8) ''Accounts)

data NewAccount = NewAccount
    { newAccountName     :: !AccountName
    , newAccountType     :: !AccountType
    , newAccountMnemonic :: !(Maybe Text)
    , newAccountPassword :: !(Maybe Text)
    , newAccountEntropy  :: !(Maybe Word8)
    , newAccountMaster   :: !(Maybe XPrvKey)
    , newAccountDeriv    :: !(Maybe KeyIndex)
    , newAccountKeys     :: ![XPubKey]
    , newAccountReadOnly :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''NewAccount)

data RenameAccount = RenameAccount
    { renameAccountOldName :: !AccountName
    , renameAccountNewName :: !AccountName
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 13) ''RenameAccount)

data AddPubKeys = AddPubKeys
    { addPubKeysAccName :: !AccountName
    , addPubKeysKeys    :: ![XPubKey]
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''AddPubKeys)

data SetAccountGap = SetAccountGap
    { setAccountGapAccName :: !AccountName
    , setAccountGapGap     :: !Word32
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 13) ''SetAccountGap)

data Addresses = Addresses
    { addressesAccName  :: !AccountName
    , addressesAddrType :: !AddressType
    , addressesMinConf  :: !Word32
    , addressesOffline  :: !Bool
    , addressesLocator  :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 9) ''Addresses)

data UnusedAddresses = UnusedAddresses
    { unusedAddressesAccName  :: !AccountName
    , unusedAddressesAddrType :: !AddressType
    , unusedAddressesLocator  :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 15) ''UnusedAddresses)

data GetAddress = GetAddress
    { getAddressAccName   :: !AccountName
    , getAddressAddrIndex :: !KeyIndex
    , getAddressAddrType  :: !AddressType
    , getAddressMinConf   :: !Word32
    , getAddressOffline   :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''GetAddress)

data PubKeyIndex = PubKeyIndex
    { pubKeyIndexAccName  :: !AccountName
    , pubKeyIndexKey      :: !PubKeyC
    , pubKeyIndexAddrType :: !AddressType
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 11) ''PubKeyIndex)

data SetAddressLabel = SetAddressLabel
    { setAddressLabelAccName  :: !AccountName
    , setAddressLabelAddrIndex:: !KeyIndex
    , setAddressLabelAddrType :: !AddressType
    , setAddressLabelLabel    :: !Text
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 15) ''SetAddressLabel)

data GenerateAddresses = GenerateAddresses
    { generateAddressesAccName  :: !AccountName
    , generateAddressesAddrIndex:: !KeyIndex
    , generateAddressesAddrType :: !AddressType
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 17) ''GenerateAddresses)

data Txs = Txs
    { txsAccName :: !AccountName
    , txsLocator :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 3) ''Txs)

data PendingTxs = PendingTxs
    { pendingTxsAccName :: !AccountName
    , pendingTxsLocator :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''PendingTxs)

data DeadTxs = DeadTxs
    { deadTxsAccName :: !AccountName
    , deadTxsLocator :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 7) ''DeadTxs)

data AddressTxs = AddressTxs
    { addressTxsAccName   :: !AccountName
    , addressTxsAddrIndex :: !KeyIndex
    , addressTxsAddrType  :: !AddressType
    , addressTxsLocator   :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''AddressTxs)

data CreateTx = CreateTx
    { createTxAccName    :: !AccountName
    , createTxRecipients :: ![(Address, Word64)]
    , createTxFee        :: !Word64
    , createTxMinConf    :: !Word32
    , createTxRcptFee    :: !Bool
    , createTxSign       :: !Bool
    , createTxSignKey    :: !(Maybe XPrvKey)
    } deriving (Eq, Show)

$(deriveJSON (requestJson 0 8) ''CreateTx)

data ImportTx = ImportTx
    { importTxAccName :: !AccountName
    , importTxTx      :: !Tx
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 8) ''ImportTx)

data SignTx = SignTx
    { signTxAccName :: !AccountName
    , signTxTxHash  :: !TxHash
    , signTxSignKey :: !(Maybe XPrvKey)
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 6) ''SignTx)

data GetTx = GetTx
    { getTxAccName :: !AccountName
    , getTxTxHash  :: !TxHash
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 5) ''GetTx)

data DeleteTx = DeleteTx
    { deleteTxTxHash :: !TxHash }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 8) ''DeleteTx)

data OfflineTx = OfflineTx
    { offlineTxAccName :: !AccountName
    , offlineTxTxHash  :: !TxHash
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 9) ''OfflineTx)

data SignOfflineTx = SignOfflineTx
    { signOfflineTxAccName  :: !AccountName
    , signOfflineTxSignKey  :: !(Maybe XPrvKey)
    , signOfflineTxTxVal    :: !Tx
    , signOfflineTxCoinData :: ![CoinSignData]
    }
    deriving (Eq, Show)

$(deriveJSON (requestJson 0 13) ''SignOfflineTx)

data Balance = Balance
    { balanceAccName :: !AccountName
    , balanceMinConf :: !Word32
    , balanceOffline :: !Bool
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 7) ''Balance)

data SyncBlock = SyncBlock
    { syncBlockAccName   :: !AccountName
    , syncBlockBlockHash :: !BlockHash
    , syncBlockLocator   :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 9) ''SyncBlock)

data SyncHeight = SyncHeight
    { syncHeightAccName :: !AccountName
    , syncHeightHeight  :: !BlockHeight
    , syncHeightLocator :: !List
    }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''SyncHeight)

data BlockInfo = BlockInfo
    { blockInfoBlocks :: ![BlockHash] }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 9) ''BlockInfo)

data NodeRescan = NodeRescan
    { nodeRescanTimestamp :: !(Maybe Word32) }
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 10) ''NodeRescan)

data NodeStatus = NodeStatus
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 0) ''NodeStatus)

data StopServer = StopServer
    deriving (Eq, Show, Read)

$(deriveJSON (requestJson 0 0) ''StopServer)

