module Network.Haskoin.Wallet.Tests (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.HashMap.Strict (singleton)

import Network.Haskoin.Wallet.Arbitrary ()
import Network.Haskoin.Wallet

tests :: [Test]
tests =
    [ testGroup "Serialize & de-serialize types to JSON"
        [ testProperty "TxType" (metaID :: TxType -> Bool)
        , testProperty "TxConfidence" (metaID :: TxConfidence -> Bool)
        , testProperty "AccountType" (metaID :: AccountType -> Bool)
        , testProperty "NewAccount" (metaID :: NewAccount -> Bool)
        , testProperty "ListRequest" (metaID :: ListRequest -> Bool)
        , testProperty "CreateTx" (metaID :: CreateTx -> Bool)
        , testProperty "AddressType" (metaID :: AddressType -> Bool)
        , testProperty "WalletRequest" (metaID :: WalletRequest -> Bool)
        -- JSON types
        , testProperty "JsonAccount" (metaID :: JsonAccount -> Bool)
        , testProperty "JsonAddr" (metaID :: JsonAddr -> Bool)
        , testProperty "JsonTx" (metaID :: JsonTx -> Bool)
        , testProperty "JsonCoin" (metaID :: JsonCoin -> Bool)
        , testProperty "JsonBlock" (metaID :: JsonBlock -> Bool)
        -- Result types
        , testProperty "CoinSignData" (metaID :: CoinSignData -> Bool)
        , testProperty "OfflineTxData" (metaID :: OfflineTxData -> Bool)
        , testProperty "AddressInfo" (metaID :: AddressInfo -> Bool)
        , testProperty "BalanceInfo" (metaID :: BalanceInfo -> Bool)
        , testProperty "TxCompleteRes" (metaID :: TxCompleteRes -> Bool)
        , testProperty "ListResult" (metaID :: ListResult JsonTx -> Bool)
        , testProperty "RescanRes" (metaID :: ListResult JsonTx -> Bool)
        , testProperty "WalletResponse"
            (metaID :: WalletResponse (ListResult JsonTx) -> Bool)
        , testProperty "Notif" (metaID :: Notif -> Bool)
        ]
    ]

metaID :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
metaID x = (decode . encode) (singleton ("object" :: String) x) ==
    Just (singleton ("object" :: String) x)

