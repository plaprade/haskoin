module Main where

import qualified Control.Concurrent              as Con
import qualified Control.Exception               as Except
import qualified Control.Monad                   as M
import qualified Control.Monad.Logger            as Log
import qualified Control.Monad.Trans.Resource    as Resource
import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Encode.Pretty        as PrettyJSON
import qualified Data.HashMap.Strict             as HM
import           Data.String.Conversions         (cs)
import qualified Database.Persist.Sqlite         as DB
import qualified Network.Haskoin.Node.STM        as Node
import qualified Network.Haskoin.Wallet.Request  as Q
import qualified Network.Haskoin.Wallet.Response as R
import           Network.Haskoin.Wallet.Server
import           Network.Haskoin.Wallet.Settings
import           Network.Haskoin.Wallet.Types
import qualified System.ZMQ4                     as ZMQ

databaseConf :: DB.SqliteConf
databaseConf = DB.SqliteConf "/tmp/tmpdb" 1

cmdSocket :: String
cmdSocket = "inproc://cmd"

notifSocket :: String
notifSocket = "inproc://notif"


-- |Simple example app that embeds a haskoin-wallet server.
--  Start wallet server + notification thread, and execute Status command when pressing ENTER
main :: IO ()
main = ZMQ.withContext $ \ctx -> do
    -- Server
    putStrLn "Starting server..."
    _ <- Con.forkIO $ runWallet walletServerConf ctx
    -- Notify thread
    putStrLn "Starting notification thread..."
    _ <- Con.forkIO $ notifyThread ctx notifyHandler
    -- Status loop
    M.forever $ do
        putStrLn "Press ENTER to get server status..."
        _ <- getLine
        cmdGetStatus ctx >>= printStatusJSON
  where
    printStatusJSON     = putStrLn . cs . PrettyJSON.encodePretty
    notifyHandler notif =
        putStrLn $ "NOTIFY: New block: " ++ cs (PrettyJSON.encodePretty notif)

-- |Run haskoin-wallet using the specified ZeroMQ Context,
--  and log to stderr.
runWallet :: Config -> ZMQ.Context -> IO ()
runWallet cfg ctx = run $ runSPVServerWithContext cfg ctx
    where run           = Resource.runResourceT . runLogging
          runLogging    = Log.runStderrLoggingT . Log.filterLogger logFilter
          logFilter _ l = l >= configLogLevel cfg

cmdGetStatus :: ZMQ.Context -> IO Node.NodeStatus
cmdGetStatus ctx =
    sendCmdOrFail Q.NodeStatus ctx >>=
    \res -> case res of
        Nothing     -> error "ERROR: Status command: no response."
        Just status -> return status

sendCmdOrFail :: (RequestPair a b, JSON.ToJSON a, JSON.FromJSON b)
              => a -> ZMQ.Context -> IO (Maybe b)
sendCmdOrFail cmd ctx =
    sendCmd cmd ctx >>= \res -> case res of
        R.ResponseData r  -> return $ Just r
        R.ResponseOK      -> return Nothing
        R.ResponseError e -> error $ "ERROR: Send cmd, ResponseError: " ++ cs e

sendCmd :: (RequestPair a b, JSON.ToJSON a, JSON.FromJSON b)
        => a -> ZMQ.Context -> IO (R.WalletResponse b)
sendCmd req ctx =
    ZMQ.withSocket ctx ZMQ.Req $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock cmdSocket
        ZMQ.send sock [] (cs $ JSON.encode req)
        joinWalletResponse . JSON.eitherDecode . cs <$> ZMQ.receive sock

joinWalletResponse :: Either String (R.WalletResponse b) -> R.WalletResponse b
joinWalletResponse (Right r) = r
joinWalletResponse (Left err) = R.ResponseError $ cs err

-- |Connect to notify socket, subscribe to new blocks,
--  and execute the supplied handler for each new block as it arrives.
notifyThread :: ZMQ.Context -> (R.Notif -> IO ()) -> IO ()
notifyThread ctx handler = waitAndCatch $
    ZMQ.withSocket ctx ZMQ.Sub $ \sock -> do
        ZMQ.setLinger (ZMQ.restrict (0 :: Int)) sock
        ZMQ.connect sock notifSocket
        ZMQ.subscribe sock "[block]"
        putStrLn "NOTIFY: Connected. Subscribed to new blocks."
        M.forever $ do
            [_,m] <- ZMQ.receiveMulti sock
            notif <- either failOnErr return $ JSON.eitherDecode (cs m)
            handler notif
  where
    failOnErr = fail . ("NOTIFY: ERROR: recv failed: " ++)
    waitAndCatch ioa = Con.threadDelay 10000 >>
                           ioa `Except.finally` waitAndCatch ioa

btcNodes :: [BTCNode]
btcNodes =
    [ BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "dnsseed.bitcoin.dashjr.org"      8333
    , BTCNode "dnsseed.bluematt.me"             8333
    , BTCNode "seed.bitcoinstats.com"           8333
    , BTCNode "seed.bitcoin.jonasschnelli.ch"   8333
    , BTCNode "seed.bitcoin.sipa.be"            8333
    , BTCNode "seed.bitnodes.io"                8333
    , BTCNode "seed.btcc.com"                   8333
    ]

walletServerConf :: Config
walletServerConf = Config
    { configCount          = 100
    -- ^ Output size of commands
    , configMinConf        = 6
    -- ^ Minimum number of confirmations
    , configSignTx         = True
    -- ^ Sign transactions
    , configFee            = 50000
    -- ^ Fee to pay per 1000 bytes when creating new transactions
    , configRcptFee        = False
    -- ^ Recipient pays fee (dangerous, no config file setting)
    , configAddrType       = AddressExternal
    -- ^ Return internal instead of external addresses
    , configDisplayPubKeys = False
    -- ^ Display public keys instead of addresses
    , configOffline        = False
    -- ^ Display the balance including offline transactions
    , configEntropy        = 16
    -- ^ Entropy in bytes to use when generating a mnemonic (between 16 and 32)
    , configReversePaging  = False
    -- ^ Use reverse paging for displaying addresses and transactions
    , configDerivIndex     = 0
    -- ^ Derivation path when creating account
    , configFormat         = OutputNormal
    -- ^ How to format the command-line results
    , configConnect        = cmdSocket
    -- ^ ZeroMQ socket to connect to (location of the server)
    , configConnectNotif   = notifSocket
    -- ^ ZeroMQ socket to connect for notifications
    , configDetach         = False
    -- ^ Detach server when launched from command-line
    , configFile           = ""
    -- ^ Configuration file
    , configTestnet        = False
    -- ^ Use Testnet3 network
    , configDir            = ""
    -- ^ Working directory
    , configBind           = cmdSocket
    -- ^ Bind address for the ZeroMQ socket
    , configBindNotif      = notifSocket
    -- ^ Bind address for ZeroMQ notifications
    , configBTCNodes       = HM.fromList [ ( "prodnet", btcNodes ) ]
    -- ^ Trusted Bitcoin full nodes to connect to
    , configMode           = SPVOnline
    -- ^ Operation mode of the SPV node.
    , configBloomFP        = 0.00001
    -- ^ False positive rate for the bloom filter.
    , configDatabase       = HM.fromList [ ( "prodnet", databaseConf ) ]
    -- ^ Database configuration
    , configLogFile        = ""
    -- ^ Log file
    , configPidFile        = ""
    -- ^ PID File
    , configLogLevel       = Log.LevelInfo
    -- ^ Log level
    , configVerbose        = True
    -- ^ Verbose
    , configServerKey      = Nothing
    -- ^ Server key for authentication and encryption (server config)
    , configServerKeyPub   = Nothing
    -- ^ Server public key for authentication and encryption (client config)
    , configClientKey      = Nothing
    -- ^ Client key for authentication and encryption (client config)
    , configClientKeyPub   = Nothing
    -- ^ Client public key for authentication and encryption
    }
