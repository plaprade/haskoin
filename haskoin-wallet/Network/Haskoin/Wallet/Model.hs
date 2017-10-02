module Network.Haskoin.Wallet.Model
( -- Database types
  Account(..)
, AccountId
, WalletAddr(..)
, WalletAddrId
, WalletState(..)
, WalletStateId
, WalletCoin(..)
, WalletCoinId
, SpentCoin(..)
, SpentCoinId
, WalletTx(..)
, WalletTxId
, EntityField(..)
, Unique(..)
, migrateWallet
) where

import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import           Data.Time                       (UTCTime)
import           Data.Word                       (Word32, Word64)
import           Database.Persist                (EntityField, Unique)
import           Database.Persist.Quasi          (lowerCaseSettings)
import           Database.Persist.TH             (mkMigrate, mkPersist,
                                                  persistFileWith, share,
                                                  sqlSettings)
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Wallet.Types

share [ mkPersist sqlSettings
      , mkMigrate "migrateWallet"
      ]
    $(persistFileWith lowerCaseSettings "config/models")

