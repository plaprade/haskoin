module Network.Haskoin.Wallet.Spec where

import           Control.Concurrent.Async
import qualified Control.Monad.Logger            as L
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
    it "replies with the right message types" $ withTestServer $ \ctx -> do
        apiValid ctx (NewAccountReq defNewAccount)
            `shouldNotReturn` (Nothing :: Maybe JsonAccount)
        return ()

{- Testing functions -}


{- Testing Types -}

defNewAccount :: NewAccount
defNewAccount = NewAccount
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

{- Testing Utilities -}

apiValid :: J.FromJSON a
         => ZMQ.Context -> WalletRequest -> IO (Maybe a)
apiValid ctx req = do
    resE <- api ctx req
    case resE of
        Right (ResponseValid resM)-> return resM
        Right (ResponseError err) -> do
            expectationFailure $ cs err
            return Nothing
        Left err -> do
            expectationFailure $ cs err
            return Nothing

api :: J.FromJSON a => ZMQ.Context -> WalletRequest
    -> IO (Either String (WalletResponse a))
api ctx req = do
    ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock socket
        ZMQ.send sock [] (cs $ J.encode req)
        J.eitherDecode . cs <$> ZMQ.receive sock

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

