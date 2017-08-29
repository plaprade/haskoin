{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import           Network.Haskoin.Test
import           Network.Haskoin.Wallet
import           Test.QuickCheck

instance Arbitrary TxType where
    arbitrary = elements
        [ TxIncoming
        , TxOutgoing
        , TxSelf
        ]

instance Arbitrary TxConfidence where
    arbitrary = elements
        [ TxOffline
        , TxDead
        , TxPending
        , TxBuilding
        ]

instance Arbitrary AddressInfo where
    arbitrary = AddressInfo <$> arbitraryAddress
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary BalanceInfo where
    arbitrary = BalanceInfo <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary AccountType where
    arbitrary = oneof
        [ return AccountRegular
        , do
            (m, n) <- arbitraryMSParam
            return $ AccountMultisig m n
        ]

instance Arbitrary NewAccount where
    arbitrary = NewAccount <$> arbitraryText
                           <*> arbitrary
                           <*> genMaybe arbitraryText
                           <*> genMaybe arbitraryText
                           <*> arbitrary
                           <*> genMaybe arbitraryXPrvKey
                           <*> arbitrary
                           <*> listOf (snd <$> arbitraryXPubKey)
                           <*> arbitrary

instance Arbitrary ListRequest where
    arbitrary = ListRequest <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CoinSignData where
    arbitrary = CoinSignData <$> arbitraryOutPoint
                             <*> arbitraryScriptOutput
                             <*> arbitrarySoftPath

instance Arbitrary OfflineTxData where
    arbitrary = OfflineTxData <$> arbitraryTx <*> listOf arbitrary

instance Arbitrary CreateTx where
    arbitrary = CreateTx <$> (listOf $ (,) <$> arbitraryAddress <*> arbitrary)
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> genMaybe arbitraryXPrvKey

instance Arbitrary AddressType where
    arbitrary = elements [ AddressInternal, AddressExternal ]


instance Arbitrary WalletRequest where
    arbitrary = oneof
        [ AccountReq <$> arbitraryText
        , AccountsReq <$> arbitrary
        , NewAccountReq <$> arbitrary
        , RenameAccountReq <$> arbitraryText <*> arbitraryText
        , AddPubKeysReq <$> arbitraryText
                        <*> (listOf $ snd <$> arbitraryXPubKey)
        , SetAccountGapReq <$> arbitraryText <*> arbitrary
        , AddrsReq <$> arbitraryText
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
        , UnusedAddrsReq <$> arbitraryText <*> arbitrary <*> arbitrary
        , AddressReq <$> arbitraryText
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
        , PubKeyIndexReq <$> arbitraryText
                         <*> (snd <$> arbitraryPubKeyC)
                         <*> arbitrary
        , SetAddrLabelReq <$> arbitraryText
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitraryText
        , GenerateAddrsReq <$> arbitraryText
                           <*> arbitrary
                           <*> arbitrary
        , TxsReq <$> arbitraryText <*> arbitrary
        , PendingTxsReq <$> arbitraryText <*> arbitrary
        , DeadTxsReq <$> arbitraryText <*> arbitrary
        , AddrTxsReq <$> arbitraryText
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
        , CreateTxReq <$> arbitraryText <*> arbitrary
        , ImportTxReq <$> arbitraryText
                      <*> arbitraryTx
        , SignTxReq <$> arbitraryText
                    <*> arbitraryTxHash
                    <*> genMaybe arbitraryXPrvKey
        , TxReq <$> arbitraryText <*> arbitraryTxHash
        , DeleteTxReq <$> arbitraryTxHash
        , OfflineTxReq <$> arbitraryText <*> arbitraryTxHash
        , SignOfflineTxReq <$> arbitraryText
                           <*> genMaybe arbitraryXPrvKey
                           <*> arbitraryTx
                           <*> listOf arbitrary
        , BalanceReq <$> arbitraryText <*> arbitrary <*> arbitrary
        , SyncBlockReq <$> arbitraryText <*> arbitraryBlockHash <*> arbitrary
        , SyncHeightReq <$> arbitraryText <*> arbitrary <*> arbitrary
        , BlockInfoReq <$> listOf arbitraryBlockHash
        , NodeRescanReq <$> arbitrary
        , return NodeStatusReq
        , return StopServerReq
        ]

instance Arbitrary JsonAccount where
    arbitrary = JsonAccount <$> arbitraryText
                            <*> arbitrary
                            <*> genMaybe arbitraryXPrvKey
                            <*> genMaybe arbitraryText
                            <*> listOf (snd <$> arbitraryXPubKey)
                            <*> arbitrary
                            <*> arbitraryUTCTime

instance Arbitrary JsonAddr where
    arbitrary = JsonAddr <$> arbitraryAddress
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitraryText
                         <*> genMaybe arbitraryScriptOutput
                         <*> genMaybe (snd <$> arbitraryPubKeyC)
                         <*> arbitraryUTCTime
                         <*> genMaybe arbitrary

instance Arbitrary JsonTx where
    arbitrary = JsonTx <$> arbitraryTxHash
                       <*> arbitraryTxHash
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> (choose (0,10) >>= (`vectorOf` arbitrary))
                       <*> (choose (0,10) >>= (`vectorOf` arbitrary))
                       <*> (choose (0,10) >>= (`vectorOf` arbitrary))
                       <*> arbitraryTx
                       <*> arbitrary
                       <*> arbitrary
                       <*> genMaybe arbitraryBlockHash
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitraryUTCTime
                       <*> arbitraryText
                       <*> arbitrary
                       <*> genMaybe arbitraryBlockHash
                       <*> arbitrary

instance Arbitrary JsonCoin where
    arbitrary = JsonCoin <$> arbitraryTxHash
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitraryScriptOutput
                         <*> arbitraryUTCTime
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

instance Arbitrary TxCompleteRes where
    arbitrary = TxCompleteRes <$> arbitraryTx <*> arbitrary

instance Arbitrary a => Arbitrary (ListResult a) where
    arbitrary = ListResult <$> (choose (0,10) >>= (`vectorOf` arbitrary))
                           <*> arbitrary

instance Arbitrary RescanRes where
    arbitrary = RescanRes <$> arbitrary

instance Arbitrary a => Arbitrary (WalletResponse a) where
    arbitrary = oneof [ ResponseError <$> arbitraryText
                      , ResponseValid <$> arbitrary
                      ]

instance Arbitrary JsonBlock where
    arbitrary =
        JsonBlock <$> arbitraryBlockHash
                  <*> arbitrary
                  <*> arbitraryBlockHash
                  <*> (choose (0,10) >>= (`vectorOf` arbitrary))

instance Arbitrary Notif where
    arbitrary = oneof [ NotifBlock <$> arbitrary
                      , NotifTx <$> arbitrary
                      ]

