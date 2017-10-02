module Network.Haskoin.Wallet.Tests (tests) where

import           Data.Aeson                           (FromJSON, ToJSON, decode,
                                                       encode)
import           Data.HashMap.Strict                  (singleton)
import           Network.Haskoin.Wallet.Arbitrary     ()
import qualified Network.Haskoin.Wallet.Request       as Q
import qualified Network.Haskoin.Wallet.Response      as R
import           Network.Haskoin.Wallet.Types
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
    [ testGroup "Serialize & de-serialize types to JSON"
        [ testProperty "TxType" (metaID :: TxType -> Bool)
        , testProperty "TxConfidence" (metaID :: TxConfidence -> Bool)
        , testProperty "AccountType" (metaID :: AccountType -> Bool)
        , testProperty "List" (metaID :: Q.List -> Bool)
        , testProperty "AddressType" (metaID :: AddressType -> Bool)

        -- Request Types
        , testProperty "GetAccount" (metaID :: Q.GetAccount -> Bool)
        , testProperty "Accounts" (metaID :: Q.Accounts -> Bool)
        , testProperty "NewAccount" (metaID :: Q.NewAccount -> Bool)
        , testProperty "RenameAccount" (metaID :: Q.RenameAccount -> Bool)
        , testProperty "AddPubKeys" (metaID :: Q.AddPubKeys -> Bool)
        , testProperty "SetAccountGap" (metaID :: Q.SetAccountGap -> Bool)
        , testProperty "Addresses" (metaID :: Q.Addresses -> Bool)
        , testProperty "UnusedAddresses" (metaID :: Q.UnusedAddresses -> Bool)
        , testProperty "GetAddress" (metaID :: Q.GetAddress -> Bool)
        , testProperty "PubKeyIndex" (metaID :: Q.PubKeyIndex -> Bool)
        , testProperty "SetAddressLabel" (metaID :: Q.SetAddressLabel -> Bool)
        , testProperty "GenerateAddresses"
            (metaID :: Q.GenerateAddresses -> Bool)
        , testProperty "Txs" (metaID :: Q.Txs -> Bool)
        , testProperty "PendingTxs" (metaID :: Q.PendingTxs -> Bool)
        , testProperty "DeadTxs" (metaID :: Q.DeadTxs -> Bool)
        , testProperty "AddressTxs" (metaID :: Q.AddressTxs -> Bool)
        , testProperty "CreateTx" (metaID :: Q.CreateTx -> Bool)
        , testProperty "ImportTx" (metaID :: Q.ImportTx -> Bool)
        , testProperty "SignTx" (metaID :: Q.SignTx -> Bool)
        , testProperty "GetTx" (metaID :: Q.GetTx -> Bool)
        , testProperty "DeleteTx" (metaID :: Q.DeleteTx -> Bool)
        , testProperty "OfflineTx" (metaID :: Q.OfflineTx -> Bool)
        , testProperty "SignOfflineTx" (metaID :: Q.SignOfflineTx -> Bool)
        , testProperty "Balance" (metaID :: Q.Balance -> Bool)
        , testProperty "SyncBlock" (metaID :: Q.SyncBlock -> Bool)
        , testProperty "SyncHeight" (metaID :: Q.SyncHeight -> Bool)
        , testProperty "BlockInfo" (metaID :: Q.BlockInfo -> Bool)
        , testProperty "NodeRescan" (metaID :: Q.NodeRescan -> Bool)
        , testProperty "NodeStatus" (metaID :: Q.NodeStatus -> Bool)
        , testProperty "StopServer" (metaID :: Q.StopServer -> Bool)

        -- Result types
        , testProperty "Account" (metaID :: R.Account -> Bool)
        , testProperty "Address" (metaID :: R.Address -> Bool)
        , testProperty "Tx" (metaID :: R.Tx -> Bool)
        , testProperty "Coin" (metaID :: R.Coin -> Bool)
        , testProperty "Block" (metaID :: R.Block -> Bool)
        , testProperty "CoinSignData" (metaID :: CoinSignData -> Bool)
        , testProperty "OfflineTxData" (metaID :: OfflineTxData -> Bool)
        , testProperty "AddressInfo" (metaID :: AddressInfo -> Bool)
        , testProperty "Balance" (metaID :: R.Balance -> Bool)
        , testProperty "TxComplete" (metaID :: R.TxComplete -> Bool)
        , testProperty "List" (metaID :: R.List R.Tx -> Bool)
        , testProperty "Notif" (metaID :: R.Notif -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)

