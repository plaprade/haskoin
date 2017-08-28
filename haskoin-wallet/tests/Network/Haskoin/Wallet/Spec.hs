module Network.Haskoin.Wallet.Spec where

import           Control.Concurrent.Async
import qualified Control.Monad.Logger            as L
import           Control.Monad.Reader
import qualified Control.Monad.Trans.Resource    as R
import qualified Data.Aeson                      as J
import           Data.Default                    (def)
import qualified Data.HashMap.Strict             as HM
import           Data.String.Conversions         (cs)
import qualified Database.Persist.Sqlite         as DB
import           Network.Haskoin.Wallet
import           Network.Haskoin.Wallet.Server
import           Network.Haskoin.Wallet.Settings
import qualified System.ZMQ4                     as ZMQ
import           Test.Hspec
import           Test.QuickCheck

apiSpec :: Spec
apiSpec = describe "Wallet API" $ do
    it "Can create a new account" $ withTestServer $ do
        let newAcc = NewAccount
                { newAccountName     = "Hello World"
                , newAccountType     = AccountRegular
                , newAccountMnemonic = Nothing
                , newAccountPassword = Nothing
                , newAccountEntropy  = Nothing
                , newAccountMaster   = Nothing
                , newAccountDeriv    = Nothing
                , newAccountKeys     = []
                , newAccountReadOnly = False
                }
        (Right (ResponseValid (Just res))) <- api $ NewAccountReq newAcc
        liftIO $ jsonAccountName res `shouldBe` "Hello World"

{- Testing Utilities -}

type APITest = ReaderT ZMQ.Context IO

api :: J.FromJSON a
    => WalletRequest
    -> APITest (Either String (WalletResponse a))
api req = do
    ctx <- ask
    liftIO $
        ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
            ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
            ZMQ.connect sock socket
            ZMQ.send sock [] (cs $ J.encode req)
            J.eitherDecode . cs <$> ZMQ.receive sock

withTestServer :: APITest () -> IO ()
withTestServer test =
    ZMQ.withContext $ \ctx -> do
        withAsync (memoryServer ctx) $ \a -> do
            link a
            runReaderT test ctx
            cancel a

memoryServer :: ZMQ.Context -> IO ()
memoryServer ctx =
    run $ runSPVServerWithContext testConfig ctx
  where
    run           = R.runResourceT . runLogging
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

