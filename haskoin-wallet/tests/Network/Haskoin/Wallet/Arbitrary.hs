{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.Arbitrary where

import           Network.Haskoin.Test
import qualified Network.Haskoin.Wallet.Request  as Q
import qualified Network.Haskoin.Wallet.Response as R
import           Network.Haskoin.Wallet.Types
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

instance Arbitrary R.Balance where
    arbitrary = R.Balance <$> arbitrary
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

instance Arbitrary Q.List where
    arbitrary = Q.List <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CoinSignData where
    arbitrary = CoinSignData <$> arbitraryOutPoint
                             <*> arbitraryScriptOutput
                             <*> arbitrarySoftPath

instance Arbitrary OfflineTxData where
    arbitrary = OfflineTxData <$> arbitraryTx <*> listOf arbitrary

instance Arbitrary AddressType where
    arbitrary = elements [ AddressInternal, AddressExternal ]


instance Arbitrary Q.GetAccount where
    arbitrary = Q.GetAccount <$> arbitraryText

instance Arbitrary Q.Accounts where
    arbitrary = Q.Accounts <$> arbitrary

instance Arbitrary Q.NewAccount where
    arbitrary = Q.NewAccount <$> arbitraryText
                             <*> arbitrary
                             <*> genMaybe arbitraryText
                             <*> genMaybe arbitraryText
                             <*> arbitrary
                             <*> genMaybe arbitraryXPrvKey
                             <*> arbitrary
                             <*> listOf (snd <$> arbitraryXPubKey)
                             <*> arbitrary

instance Arbitrary Q.RenameAccount where
    arbitrary = Q.RenameAccount <$> arbitraryText <*> arbitraryText

instance Arbitrary Q.AddPubKeys where
    arbitrary = Q.AddPubKeys <$> arbitraryText
                             <*> (listOf $ snd <$> arbitraryXPubKey)

instance Arbitrary Q.SetAccountGap where
    arbitrary = Q.SetAccountGap <$> arbitraryText <*> arbitrary

instance Arbitrary Q.Addresses where
    arbitrary = Q.Addresses <$> arbitraryText
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary Q.UnusedAddresses where
    arbitrary = Q.UnusedAddresses <$> arbitraryText <*> arbitrary <*> arbitrary

instance Arbitrary Q.GetAddress where
    arbitrary = Q.GetAddress <$> arbitraryText
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary Q.PubKeyIndex where
    arbitrary = Q.PubKeyIndex <$> arbitraryText
                              <*> (snd <$> arbitraryPubKeyC)
                              <*> arbitrary

instance Arbitrary Q.SetAddressLabel where
    arbitrary = Q.SetAddressLabel <$> arbitraryText
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitraryText

instance Arbitrary Q.GenerateAddresses where
    arbitrary = Q.GenerateAddresses <$> arbitraryText
                                    <*> arbitrary
                                    <*> arbitrary

instance Arbitrary Q.Txs where
    arbitrary = Q.Txs <$> arbitraryText <*> arbitrary

instance Arbitrary Q.PendingTxs where
    arbitrary = Q.PendingTxs <$> arbitraryText <*> arbitrary

instance Arbitrary Q.DeadTxs where
    arbitrary = Q.DeadTxs <$> arbitraryText <*> arbitrary

instance Arbitrary Q.AddressTxs where
    arbitrary = Q.AddressTxs <$> arbitraryText
                             <*> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance Arbitrary Q.CreateTx where
    arbitrary = Q.CreateTx <$> arbitraryText
                           <*> (listOf $ (,) <$> arbitraryAddress <*> arbitrary)
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> genMaybe arbitraryXPrvKey

instance Arbitrary Q.ImportTx where
    arbitrary = Q.ImportTx <$> arbitraryText <*> arbitraryTx

instance Arbitrary Q.SignTx where
    arbitrary = Q.SignTx <$> arbitraryText
                         <*> arbitraryTxHash
                         <*> genMaybe arbitraryXPrvKey

instance Arbitrary Q.GetTx where
    arbitrary = Q.GetTx <$> arbitraryText <*> arbitraryTxHash

instance Arbitrary Q.DeleteTx where
    arbitrary = Q.DeleteTx <$> arbitraryTxHash

instance Arbitrary Q.OfflineTx where
    arbitrary = Q.OfflineTx <$> arbitraryText <*> arbitraryTxHash

instance Arbitrary Q.SignOfflineTx where
    arbitrary = Q.SignOfflineTx <$> arbitraryText
                                <*> genMaybe arbitraryXPrvKey
                                <*> arbitraryTx
                                <*> listOf arbitrary

instance Arbitrary Q.Balance where
    arbitrary = Q.Balance <$> arbitraryText <*> arbitrary <*> arbitrary

instance Arbitrary Q.SyncBlock where
    arbitrary = Q.SyncBlock <$> arbitraryText
                            <*> arbitraryBlockHash
                            <*> arbitrary

instance Arbitrary Q.SyncHeight where
    arbitrary = Q.SyncHeight <$> arbitraryText <*> arbitrary <*> arbitrary

instance Arbitrary Q.BlockInfo where
    arbitrary = Q.BlockInfo <$> listOf arbitraryBlockHash

instance Arbitrary Q.NodeRescan where
    arbitrary = Q.NodeRescan <$> arbitrary

instance Arbitrary Q.NodeStatus where
    arbitrary = return Q.NodeStatus

instance Arbitrary Q.StopServer where
    arbitrary = return Q.StopServer

instance Arbitrary R.Account where
    arbitrary = R.Account <$> arbitraryText
                          <*> arbitrary
                          <*> genMaybe arbitraryXPrvKey
                          <*> genMaybe arbitraryText
                          <*> listOf (snd <$> arbitraryXPubKey)
                          <*> arbitrary
                          <*> arbitraryUTCTime

instance Arbitrary R.Address where
    arbitrary = R.Address <$> arbitraryAddress
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitraryText
                          <*> genMaybe arbitraryScriptOutput
                          <*> genMaybe (snd <$> arbitraryPubKeyC)
                          <*> genMaybe arbitrary
                          <*> arbitraryUTCTime

instance Arbitrary R.Tx where
    arbitrary = R.Tx <$> arbitraryTxHash
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

instance Arbitrary R.Coin where
    arbitrary = R.Coin <$> arbitraryTxHash
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitraryScriptOutput
                       <*> arbitraryUTCTime
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance Arbitrary R.Block where
    arbitrary = R.Block <$> arbitraryBlockHash
                        <*> arbitrary
                        <*> arbitraryBlockHash
                        <*> (choose (0,10) >>= (`vectorOf` arbitrary))

instance Arbitrary R.TxComplete where
    arbitrary = R.TxComplete <$> arbitraryTx <*> arbitrary

instance Arbitrary a => Arbitrary (R.List a) where
    arbitrary = R.List <$> (choose (0,10) >>= (`vectorOf` arbitrary))
                       <*> arbitrary

instance Arbitrary a => Arbitrary (R.WalletResponse a) where
    arbitrary = oneof [ R.ResponseData <$> arbitrary
                      , R.ResponseError <$> arbitraryText
                      , return R.ResponseOK
                      ]

instance Arbitrary R.Notif where
    arbitrary = oneof [ R.NotifBlock <$> arbitrary
                      , R.NotifTx <$> arbitrary
                      ]

