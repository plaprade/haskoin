module Network.Haskoin.Wallet.Spec where

import           Control.Concurrent.Async
import qualified Control.Monad.Logger            as L
import           Control.Monad.Trans.Resource    (runResourceT)
import qualified Data.Aeson                      as J
import           Data.Default                    (def)
import qualified Data.HashMap.Strict             as HM
import           Data.String.Conversions         (cs)
import qualified Data.Text                       as T
import qualified Database.Persist.Sqlite         as DB
import           Network.Haskoin.Crypto
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
    it "replies with the right message types" $ withTestServer $ \ctx -> do
        api ctx newMs1
            `shouldNotReturn` R.ResponseOK
        api ctx (Q.AddPubKeys "Multisig A" [snd keys2])
            `shouldNotReturn` R.ResponseOK
        return ()

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
    , newAccountDeriv    = Just 1
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
    , newAccountDeriv    = Just 2
    , newAccountKeys     = []
    , newAccountReadOnly = False
    }

defListRequest :: Q.List
defListRequest = Q.List
    { listOffset  = 0
    , listLimit   = 10
    , listReverse = False
    }

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
    logFilter _ l = l >= configLogLevel testConfig

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

