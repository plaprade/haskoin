module Network.Haskoin.Wallet.Spec where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Control.Monad.Logger            as L
import           Control.Monad.Trans.Resource    (runResourceT)
import qualified Data.Aeson                      as J
import           Data.Default                    (def)
import           Data.Maybe                      (fromJust)
import qualified Data.HashMap.Strict             as HM
import qualified Data.ByteString                 as BS
import           Data.String.Conversions         (cs)
import qualified Data.Text                       as T
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime)
import           Data.Word                       (Word32, Word64)
import           Data.Serialize                  as S
import qualified Database.Persist.Sqlite         as DB
import           Network.Haskoin.Crypto
import           Network.Haskoin.Transaction
import           Network.Haskoin.Script
import qualified Network.Haskoin.Wallet.Request  as Q
import qualified Network.Haskoin.Wallet.Response as R
import           Network.Haskoin.Wallet.Server
import           Network.Haskoin.Wallet.Settings
import           Network.Haskoin.Wallet.Types
import qualified System.ZMQ4                     as ZMQ
import           Test.Hspec
import           Test.QuickCheck

apiSpec :: Spec
apiSpec = describe "Wallet API" $ do

    it "can create a regular account" $ withTestServer $ \ctx -> do
        api ctx newAcc
            `shouldCompareTo`
            R.ResponseData R.Account
                { accountName     = "Hello World"
                , accountType     = AccountRegular
                , accountMaster   = Just (fst keys1)
                , accountMnemonic = Nothing
                , accountKeys     = [snd keys1]
                , accountGap      = 10
                , accountCreated  = ignoreTime
                }

    it "can query existing accounts" $ withTestServer $ \ctx -> do
        _ <- api ctx newAcc
        _ <- api ctx newMs1
        _ <- api ctx newMs2
        api ctx (Q.GetAccount "Multisig A")
            `shouldCompareTo`
            R.ResponseData R.Account
                { accountName     = "Multisig A"
                , accountType     = AccountMultisig 2 2
                , accountMaster   = Just (fst keys1)
                , accountMnemonic = Nothing
                , accountKeys     = [snd keys1]
                , accountGap      = 0
                , accountCreated  = ignoreTime
                }
        api ctx (Q.Accounts defListRequest)
            `shouldCompareTo`
            R.ResponseData
                ( R.List [ R.Account
                             { accountName     = "Hello World"
                             , accountType     = AccountRegular
                             , accountMaster   = Just (fst keys1)
                             , accountMnemonic = Nothing
                             , accountKeys     = [snd keys1]
                             , accountGap      = 10
                             , accountCreated  = ignoreTime
                             }
                         , R.Account
                             { accountName     = "Multisig A"
                             , accountType     = AccountMultisig 2 2
                             , accountMaster   = Just (fst keys1)
                             , accountMnemonic = Nothing
                             , accountKeys     = [snd keys1]
                             , accountGap      = 0
                             , accountCreated  = ignoreTime
                             }
                         , R.Account
                             { accountName     = "Multisig B"
                             , accountType     = AccountMultisig 2 2
                             , accountMaster   = Just (fst keys2)
                             , accountMnemonic = Nothing
                             , accountKeys     = [snd keys2]
                             , accountGap      = 0
                             , accountCreated  = ignoreTime
                             }
                         ] 3
                )

    it "can rename an account" $ withTestServer $ \ctx -> do
        _ <- api ctx newAcc{ Q.newAccountName = "Account A" }
        let accRes = R.Account
                { accountName     = "Account K"
                , accountType     = AccountRegular
                , accountMaster   = Just (fst keys1)
                , accountMnemonic = Nothing
                , accountKeys     = [snd keys1]
                , accountGap      = 10
                , accountCreated  = ignoreTime
                }
        api ctx (Q.RenameAccount "Account A" "Account K")
            `shouldCompareTo` R.ResponseData accRes
        api ctx (Q.GetAccount "Account K")
            `shouldCompareTo` R.ResponseData accRes
        api ctx (Q.GetAccount "Account A")
            `shouldReturn` R.ResponseError "Account Account A does not exist"

    it "can add pubkeys to an account" $ withTestServer $ \ctx -> do
        _ <- api ctx newMs1
        api ctx (Q.AddPubKeys "Multisig A" [snd keys2])
            `shouldCompareTo`
            R.ResponseData R.Account
                { accountName     = "Multisig A"
                , accountType     = AccountMultisig 2 2
                , accountMaster   = Just (fst keys1)
                , accountMnemonic = Nothing
                , accountKeys     = [snd keys1, snd keys2]
                , accountGap      = 10
                , accountCreated  = ignoreTime
                }

    it "can change the account gap" $ withTestServer $ \ctx -> do
        _ <- api ctx newAcc
        let q = Q.Addresses "Hello World" AddressExternal 0 False defListRequest
        api ctx q
            `shouldCompareTo`
            R.ResponseData (R.List (map (addrsAcc1 !!) [5..9]) 10)
        api ctx (Q.SetAccountGap "Hello World" 20)
            `shouldCompareTo`
            R.ResponseData R.Account
                { accountName     = "Hello World"
                , accountType     = AccountRegular
                , accountMaster   = Just (fst keys1)
                , accountMnemonic = Nothing
                , accountKeys     = [snd keys1]
                , accountGap      = 20
                , accountCreated  = ignoreTime
                }
        api ctx q
            `shouldCompareTo`
            R.ResponseData (R.List (map (addrsAcc1 !!) [15..19]) 20)

    it "can list unused addresses" $ withTestServer $ \ctx -> do
        _ <- api ctx newAcc
        let q = Q.UnusedAddresses "Hello World" AddressExternal defListRequest
        api ctx q `shouldCompareTo`
            R.ResponseData (R.List (map (addrsAcc1 !!) [0..4]) 10)
        api ctx $ Q.ImportTx "Hello World" $ fundAcc [10000000]
        api ctx q `shouldCompareTo`
            R.ResponseData (R.List (map (addrsAcc1 !!) [1..5]) 10)

{- Testing defenitions -}

pwd :: T.Text
pwd = "correct horse battery staple"

mnem1 :: T.Text
mnem1 = "snow senior nerve virus fabric now fringe clip marble interest analyst can"

mnem2 :: T.Text
mnem2 = "bike palace cannon basic lazy head reflect shiver return arrow caught town"

keys1 :: (XPrvKey, XPubKey)
keys1 = ( "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh"
        , "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"
        )

keys2 :: (XPrvKey, XPubKey)
keys2 = ( "xprv9yopS25nKJeGStDY9Ve85Gub4ziF3o4M5cHdiceCtJtoNE8V7Q8umVw56eKkwipLMMYa33v32uWKCoxAiXDmPz8gaKUKXC4pv6bjEnijPkz"
        , "xpub6CoAqXcg9gCZfNJ1FXB8SQrKd2YjTFnCSqDEX13pSeRnF2TdewTAKJFYwwx3DeWHNVJTYrBQgZHRZwHRF3omZKB3ZhyQNJvr2VYViiV7gC3"
        )

addrsAcc1 :: [Address]
addrsAcc1 =
    [ "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
    , "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"
    , "1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr"
    , "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"
    , "1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx"
    , "1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"
    , "1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE"
    , "1Hx9xWAHhcjea5uJnyADktCfcLbuBnRnwA"
    , "1HXJhfiD7JFCGMFZnhKRsZxoPF7xDTqWXP"
    , "1MZpAt1FofY69B6fzooFxZqe6SdrVrC3Yw"
    , "16jj9LFEK2ULQ9dMkDArv6ud4xVQVbWyfX"
    , "1N3JqZGk4XxBWqmr1Xq7X5pLebmw45vVX2"
    , "15iUfuMdV2Ntuo8M4EhzA4Pa8XxyRR5bVK"
    , "1BJWxYEfcxd7gGeScnE2N8pEpXeBhafWTH"
    , "131g23BHkaG3KmsyV4vtcJavXG9fuE4Q6h"
    , "1BiGCFAmCG53MRf1Vy9rhHbtVNt32HjrpU"
    , "116GvxV9tpLnj5ZHJehyKXkgighxVWmn2W"
    , "1756gAxeqZQa6BmyMVkShaLyvw1kmsdK6A"
    , "1wRpGbYSbS8kZtRaTSMMNhFf5PhYW4BY5"
    , "1JDcuqDyRoGghF2btC3ocC95KebYN7kWhD"
    ]

newAcc :: Q.NewAccount
newAcc = Q.NewAccount
    { newAccountName     = "Hello World"
    , newAccountType     = AccountRegular
    , newAccountMnemonic = Just mnem1
    , newAccountPassword = Just pwd
    , newAccountEntropy  = Nothing
    , newAccountMaster   = Nothing
    , newAccountDeriv    = Nothing
    , newAccountKeys     = []
    , newAccountReadOnly = False
    }

newMs1 :: Q.NewAccount
newMs1 = Q.NewAccount
    { newAccountName     = "Multisig A"
    , newAccountType     = AccountMultisig 2 2
    , newAccountMnemonic = Just mnem1
    , newAccountPassword = Just pwd
    , newAccountEntropy  = Nothing
    , newAccountMaster   = Nothing
    , newAccountDeriv    = Nothing
    , newAccountKeys     = []
    , newAccountReadOnly = False
    }

newMs2 :: Q.NewAccount
newMs2 = Q.NewAccount
    { newAccountName     = "Multisig B"
    , newAccountType     = AccountMultisig 2 2
    , newAccountMnemonic = Just mnem2
    , newAccountPassword = Just pwd
    , newAccountEntropy  = Nothing
    , newAccountMaster   = Nothing
    , newAccountDeriv    = Nothing
    , newAccountKeys     = []
    , newAccountReadOnly = False
    }

testTx :: [(TxHash, Word32)] -> [(Address, Word64)] -> Tx
testTx xs ys =
    createTx 1 txi txo 0
  where
    txi = map (\(h,p) -> TxIn (OutPoint h p) (BS.pack [1]) maxBound) xs
    f   = encodeOutputBS . PayPKHash
    txo = map (\(a,v) -> TxOut v $ f a ) ys

dummyTid1 :: TxHash
dummyTid1 = "0000000000000000000000000000000000000000000000000000000000000001"

fundAcc :: [Word64] -> Tx
fundAcc amnts
    | null amnts = error "fundAcc does not accept an empty list"
    | length amnts > 5 = error "fundAcc input too large"
    | otherwise = testTx [ (dummyTid1, 0) ] $ zip addrsAcc1 amnts

defListRequest :: Q.List
defListRequest = Q.List
    { listOffset  = 0
    , listLimit   = 5
    , listReverse = False
    }

ignoreTime :: UTCTime
ignoreTime = posixSecondsToUTCTime 0

{- Spec Utilities -}

shouldCompareTo :: (HasCallStack, Show a, Eq a, TestCompare a b)
              => IO a -> b -> Expectation
shouldCompareTo action expected = action >>= (`testCompare` expected)

class TestCompare a b where
    testCompare :: a -> b -> Expectation

instance TestCompare a b =>
         TestCompare (R.WalletResponse a) (R.WalletResponse b) where
    R.ResponseData a `testCompare` R.ResponseData b = a `testCompare` b
    R.ResponseError a `testCompare` R.ResponseError b = a `shouldBe` b
    R.ResponseOK `testCompare` R.ResponseOK = return ()
    _ `testCompare` _ = expectationFailure "Invalid TestCompare"

instance TestCompare a b => TestCompare (R.List a) (R.List b) where
    (R.List xs i1) `testCompare` (R.List ys i2) = do
        i1 `shouldBe` i2
        forM_ (zip xs ys) $ \(a,b) -> a `testCompare` b

instance TestCompare R.Account R.Account where
    acc1 `testCompare` acc2 =
        acc1{ R.accountCreated = ignoreTime } `shouldBe`
        acc2{ R.accountCreated = ignoreTime }

instance TestCompare R.Address Address where
    rAddr `testCompare` addr = R.addressAddress rAddr `shouldBe` addr

{- Testing Utilities -}

api :: RequestPair a b => ZMQ.Context -> a -> IO (R.WalletResponse b)
api ctx req = do
    ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock socket
        ZMQ.send sock [] (cs $ J.encode req)
        joinWalletResponse . J.eitherDecode . cs <$> ZMQ.receive sock

joinWalletResponse :: Either String (R.WalletResponse b) -> R.WalletResponse b
joinWalletResponse (Right r) = r
joinWalletResponse (Left err) = R.ResponseError $ cs err

withTestServer :: (ZMQ.Context -> IO ()) -> IO ()
withTestServer test =
    ZMQ.withContext $ \ctx -> do
        withAsync (memoryServer ctx) $ \a -> do
            link a
            test ctx
            cancel a

memoryServer :: ZMQ.Context -> IO ()
memoryServer ctx =
    run $ runSPVServerWithContext testConfig ctx
  where
    run           = runResourceT . runLogging
    runLogging    = L.runStderrLoggingT . L.filterLogger logFilter
    logFilter _ _ = False -- No Logging

databaseConf :: DB.SqliteConf
databaseConf = DB.SqliteConf ":memory:" 1

socket :: String
socket = "inproc://haskointest"

socketNotif :: String
socketNotif = "inproc://haskointestnotif"

testConfig :: Config
testConfig =
    def { configBind = socket
        , configBindNotif = socketNotif
        , configMode = SPVOffline
        , configDatabase = HM.fromList [ ( "prodnet", databaseConf ) ]
        , configLogFile = ""
        , configPidFile = ""
        , configLogLevel = L.LevelError
        }

