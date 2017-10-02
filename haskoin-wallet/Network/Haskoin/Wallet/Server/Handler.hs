{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Network.Haskoin.Wallet.Server.Handler where

import           Control.Arrow                      (first)
import           Control.Concurrent.STM.TBMChan     (TBMChan)
import           Control.Exception                  (SomeException (..),
                                                     tryJust)
import           Control.Monad                      (forM, liftM, unless, when)
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.Logger               (MonadLoggerIO, logError,
                                                     logInfo)
import           Control.Monad.Reader               (ReaderT, asks, runReaderT)
import           Control.Monad.Trans                (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Trans.Resource       (MonadResource)
import           Data.Aeson                         (FromJSON, ToJSON,
                                                     Value (..), toJSON)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (catMaybes)
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text, pack, unpack)
import           Data.Word                          (Word32, Word64)
import           Database.Esqueleto                 (Entity (..), SqlPersistT)
import           Database.Persist.Sql               (ConnectionPool,
                                                     SqlPersistM,
                                                     runSqlPersistMPool,
                                                     runSqlPool)
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto
import qualified Network.Haskoin.Node.BlockChain    as N
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Node.Peer
import           Network.Haskoin.Node.STM
import qualified Network.Haskoin.Transaction        as HT
import qualified Network.Haskoin.Wallet.Accounts    as A
import           Network.Haskoin.Wallet.Block
import           Network.Haskoin.Wallet.Model
import qualified Network.Haskoin.Wallet.Request     as Q
import           Network.Haskoin.Wallet.Response    (WalletResponse (..))
import qualified Network.Haskoin.Wallet.Response    as R
import           Network.Haskoin.Wallet.Settings
import qualified Network.Haskoin.Wallet.Transaction as T
import           Network.Haskoin.Wallet.Types

type Handler m = ReaderT HandlerSession m

data HandlerSession = HandlerSession
    { handlerConfig    :: !Config
    , handlerPool      :: !ConnectionPool
    , handlerNodeState :: !(Maybe SharedNodeState)
    , handlerNotifChan :: !(TBMChan R.Notif)
    }

runHandler :: Monad m => Handler m a -> HandlerSession -> m a
runHandler = runReaderT

runDB :: MonadBaseControl IO m => SqlPersistT m a -> Handler m a
runDB action = asks handlerPool >>= lift . runDBPool action

runDBPool :: MonadBaseControl IO m => SqlPersistT m a -> ConnectionPool -> m a
runDBPool = runSqlPool

tryDBPool :: MonadLoggerIO m => ConnectionPool -> SqlPersistM a -> m (Maybe a)
tryDBPool pool action = do
    resE <- liftIO $ tryJust f $ runSqlPersistMPool action pool
    case resE of
        Right res -> return $ Just res
        Left err -> do
            $(logError) $ pack $ unwords [ "A database error occured:", err]
            return Nothing
  where
    f (SomeException e) = Just $ show e

runNode :: MonadIO m => NodeT m a -> Handler m a
runNode action = do
    nodeStateM <- asks handlerNodeState
    case nodeStateM of
        Just nodeState -> lift $ runNodeT action nodeState
        _ -> error "runNode: No node state available"

{- RequestPair Instance -}

class
    (FromJSON a, ToJSON a, FromJSON b, ToJSON b) =>
    RequestPair a b | a -> b where
    dispatch :: ( MonadLoggerIO m
                , MonadThrow m
                , MonadBaseControl IO m
                , MonadResource m
                )
             => a -> Handler m (WalletResponse b)

instance RequestPair Q.GetAccount R.Account where
    dispatch (Q.GetAccount name) = do
        $(logInfo) $ format $ unlines
            [ "Account"
            , "  Account name: " ++ unpack name
            ]
        Entity _ acc <- runDB $ A.getAccount name
        return $ ResponseData $ A.fromAccount Nothing acc

instance RequestPair Q.Accounts (R.List R.Account) where
    dispatch (Q.Accounts lq@Q.List{..}) = do
        $(logInfo) $ format $ unlines
            [ "Accounts"
            , "  Offset      : " ++ show listOffset
            , "  Limit       : " ++ show listLimit
            , "  Reversed    : " ++ show listReverse
            ]
        (accs, cnt) <- runDB $ A.accounts lq
        return $ ResponseData $ R.List (map (A.fromAccount Nothing) accs) cnt

instance RequestPair Q.NewAccount R.Account where
    dispatch newAcc@Q.NewAccount{..} = do
        $(logInfo) $ format $ unlines
            [ "NewAccount"
            , "  Account name: " ++ unpack newAccountName
            , "  Account type: " ++ show newAccountType
            ]
        (Entity _ newAcc', mnemonicM) <- runDB $ A.newAccount newAcc
        -- Update the bloom filter if the account is complete
        whenOnline $ when (A.isCompleteAccount newAcc') updateNodeFilter
        return $ ResponseData $ A.fromAccount mnemonicM newAcc'

instance RequestPair Q.RenameAccount R.Account where
    dispatch (Q.RenameAccount oldName newName) = do
        $(logInfo) $ format $ unlines
            [ "RenameAccount"
            , "  Account name: " ++ unpack oldName
            , "  New name    : " ++ unpack newName
            ]
        newAcc <- runDB $ do
            accE <- A.getAccount oldName
            A.renameAccount accE newName
        return $ ResponseData $ A.fromAccount Nothing newAcc

instance RequestPair Q.AddPubKeys R.Account where
    dispatch (Q.AddPubKeys name keys) = do
        $(logInfo) $ format $ unlines
            [ "AddPubKeys"
            , "  Account name: " ++ unpack name
            , "  Key count   : " ++ show (length keys)
            ]
        newAcc <- runDB $ do
            accE <- A.getAccount name
            A.addAccountKeys accE keys
        -- Update the bloom filter if the account is complete
        whenOnline $ when (A.isCompleteAccount newAcc) updateNodeFilter
        return $ ResponseData $ A.fromAccount Nothing newAcc

instance RequestPair Q.SetAccountGap R.Account where
    dispatch (Q.SetAccountGap name gap) = do
        $(logInfo) $ format $ unlines
            [ "SetAccountGap"
            , "  Account name: " ++ unpack name
            , "  New gap size: " ++ show gap
            ]
        -- Update the gap
        Entity _ newAcc <- runDB $ do
            accE <- A.getAccount name
            A.setAccountGap accE gap
        -- Update the bloom filter
        whenOnline updateNodeFilter
        return $ ResponseData $ A.fromAccount Nothing newAcc

instance RequestPair Q.Addresses (R.List R.Address) where
    dispatch (Q.Addresses name addrType minConf offline lq@Q.List{..}) = do
        $(logInfo) $ format $ unlines
            [ "Addresses"
            , "  Account name: " ++ unpack name
            , "  Address type: " ++ show addrType
            , "  Start index : " ++ show listOffset
            , "  Reversed    : " ++ show listReverse
            , "  MinConf     : " ++ show minConf
            , "  Offline     : " ++ show offline
            ]

        (res, bals, cnt) <- runDB $ do
            accE <- A.getAccount name
            (res, cnt) <- A.addressList accE addrType lq
            case res of
                [] -> return (res, [], cnt)
                _ -> do
                    let is = map walletAddrIndex res
                        (iMin, iMax) = (minimum is, maximum is)
                    bals <-
                        T.addressBalances
                            accE iMin iMax addrType minConf offline
                    return (res, bals, cnt)

        -- Join addresses and balances together
        let g (addr, bal) = T.fromAddress addr (Just bal)
            addrBals = map g $ M.elems $ joinAddrs res bals
        return $ ResponseData $ R.List addrBals cnt
      where
        joinAddrs addrs bals =
            let f addr = (walletAddrIndex addr, addr)
            in  M.intersectionWith (,)
                    (M.fromList $ map f addrs) (M.fromList bals)

instance RequestPair Q.UnusedAddresses (R.List R.Address) where
    dispatch (Q.UnusedAddresses name addrType lq@Q.List{..}) = do
        $(logInfo) $ format $ unlines
            [ "UnusedAddresses"
            , "  Account name: " ++ unpack name
            , "  Address type: " ++ show addrType
            , "  Offset      : " ++ show listOffset
            , "  Limit       : " ++ show listLimit
            , "  Reversed    : " ++ show listReverse
            ]
        (addrs, cnt) <- runDB $ do
            accE <- A.getAccount name
            A.unusedAddresses accE addrType lq
        return $ ResponseData $ R.List (map (`T.fromAddress` Nothing) addrs) cnt

instance RequestPair Q.GetAddress R.Address where
    dispatch (Q.GetAddress name i addrType minConf offline) = do
        $(logInfo) $ format $ unlines
            [ "GetAddress"
            , "  Account name: " ++ unpack name
            , "  Index       : " ++ show i
            , "  Address type: " ++ show addrType
            ]

        (addr, balM) <- runDB $ do
            accE <- A.getAccount name
            addrE <- A.getAddress accE addrType i
            bals <- T.addressBalances accE i i addrType minConf offline
            return $ case bals of
                ((_,bal):_) -> (entityVal addrE, Just bal)
                _           -> (entityVal addrE, Nothing)
        return $ ResponseData $ T.fromAddress addr balM

-- TODO: How can we generalize this? Perhaps as part of wallet searching?
instance RequestPair Q.PubKeyIndex [R.Address] where
    dispatch (Q.PubKeyIndex name key addrType) = do
        $(logInfo) $ format $ unlines
            [ "PubKeyIndex"
            , "  Account name: " ++ unpack name
            , "  Key         : " ++ show key
            , "  Address type: " ++ show addrType
            ]
        addrLst <- runDB $ do
            accE <- A.getAccount name
            A.lookupByPubKey accE key addrType
        -- TODO: We don't return the balance for now. Maybe add it? Or not?
        return $ ResponseData $ map (`T.fromAddress` Nothing) addrLst

instance RequestPair Q.SetAddressLabel R.Address where
    dispatch (Q.SetAddressLabel name i addrType label) = do
        $(logInfo) $ format $ unlines
            [ "SetAddressLabel"
            , "  Account name: " ++ unpack name
            , "  Index       : " ++ show i
            , "  Label       : " ++ unpack label
            ]
        newAddr <- runDB $ do
            accE <- A.getAccount name
            A.setAddrLabel accE i addrType label
        return $ ResponseData $ T.fromAddress newAddr Nothing

instance RequestPair Q.GenerateAddresses Int where
    dispatch (Q.GenerateAddresses name i addrType) = do
        $(logInfo) $ format $ unlines
            [ "GenerateAddresses"
            , "  Account name: " ++ unpack name
            , "  Index       : " ++ show i
            ]
        cnt <- runDB $ do
            accE <- A.getAccount name
            A.generateAddrs accE addrType i
        -- Update the bloom filter
        whenOnline updateNodeFilter
        return $ ResponseData cnt

-- This is a generic function (see specifics below)
txsGen :: (MonadLoggerIO m, MonadBaseControl IO m, MonadThrow m)
       => AccountName
       -> Q.List
       -> String
       -> (AccountId -> Q.List -> SqlPersistT m ([WalletTx], Word32))
       -> Handler m (WalletResponse (R.List R.Tx))
txsGen name lq@Q.List{..} cmd f = do
    $(logInfo) $ format $ unlines
        [ cmd
        , "  Account name: " ++ unpack name
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]

    (res, cnt, bb) <- runDB $ do
        Entity ai _ <- A.getAccount name
        bb <- T.walletBestBlock
        (res, cnt) <- f ai lq
        return (res, cnt, bb)

    return $ ResponseData $ R.List (map (T.fromTx name (Just bb)) res) cnt

instance RequestPair Q.Txs (R.List R.Tx) where
    dispatch (Q.Txs name lq) =
        txsGen name lq "Txs" (T.txs Nothing)

instance RequestPair Q.PendingTxs (R.List R.Tx) where
    dispatch (Q.PendingTxs name lq) =
        txsGen name lq "PendingTxs" (T.txs (Just TxPending))

instance RequestPair Q.DeadTxs (R.List R.Tx) where
    dispatch (Q.DeadTxs name lq) =
        txsGen name lq "DeadTxs" (T.txs (Just TxDead))

instance RequestPair Q.AddressTxs (R.List R.Tx) where
    dispatch (Q.AddressTxs name index addrType lq@Q.List{..}) = do
        $(logInfo) $ format $ unlines
            [ "AddressTxs"
            , "  Account name : " ++ unpack name
            , "  Address index: " ++ show index
            , "  Address type : " ++ show addrType
            , "  Offset       : " ++ show listOffset
            , "  Limit        : " ++ show listLimit
            , "  Reversed     : " ++ show listReverse
            ]

        (res, cnt, bb) <- runDB $ do
            accE <- A.getAccount name
            addrE <- A.getAddress accE addrType index
            bb <- T.walletBestBlock
            (res, cnt) <- T.addrTxs accE addrE lq
            return (res, cnt, bb)
        return $ ResponseData $ R.List (map (T.fromTx name (Just bb)) res) cnt

instance RequestPair Q.CreateTx R.Tx where
    dispatch ctx@(Q.CreateTx name rs fee minconf rcptFee sign _) = do
        $(logInfo) $ format $ unlines
            [ "CreateTx"
            , "  Account name: " ++ unpack name
            , "  Recipients  : " ++ show (map (first addrToBase58) rs)
            , "  Fee         : " ++ show fee
            , "  Minconf     : " ++ show minconf
            , "  Rcpt. Fee   : " ++ show rcptFee
            , "  Sign        : " ++ show sign
            ]

        notif <- asks handlerNotifChan

        (bb, txRes, newAddrs) <- runDB $ do
            accE <- A.getAccount name
            bb   <- T.walletBestBlock
            (txRes, newAddrs) <- T.createWalletTx accE (Just notif) ctx
            return (bb, txRes, newAddrs)

        whenOnline $ do
            -- Update the bloom filter
            unless (null newAddrs) updateNodeFilter
            -- If the transaction is pending, broadcast it to the network
            when (walletTxConfidence txRes == TxPending) $
                runNode $ N.broadcastTxs [walletTxHash txRes]

        return $ ResponseData $ T.fromTx name (Just bb) txRes

instance RequestPair Q.ImportTx R.Tx where
    dispatch (Q.ImportTx name tx) = do
        $(logInfo) $ format $ unlines
            [ "ImportTx"
            , "  Account name: " ++ unpack name
            , "  TxId        : " ++ cs (HT.txHashToHex (HT.txHash tx))
            ]

        notif <- asks handlerNotifChan

        (bb, txRes, newAddrs) <- runDB $ do
            Entity ai _ <- A.getAccount name
            bb <- T.walletBestBlock
            (res, newAddrs) <- T.importTx tx (Just notif) ai
            case filter ((== ai) . walletTxAccount) res of
                (txRes:_) -> return (bb, txRes, newAddrs)
                _ -> throwM $ WalletException "Could not import the transaction"

        whenOnline $ do
            -- Update the bloom filter
            unless (null newAddrs) updateNodeFilter
            -- If the transaction is pending, broadcast it to the network
            when (walletTxConfidence txRes == TxPending) $
                runNode $ N.broadcastTxs [walletTxHash txRes]

        return $ ResponseData $ T.fromTx name (Just bb) txRes

instance RequestPair Q.SignTx R.Tx where
    dispatch (Q.SignTx name txid masterM) = do
        $(logInfo) $ format $ unlines
            [ "SignTx"
            , "  Account name: " ++ unpack name
            , "  TxId        : " ++ cs (HT.txHashToHex txid)
            ]

        notif <- asks handlerNotifChan

        (bb, txRes, newAddrs) <- runDB $ do
            accE@(Entity ai _) <- A.getAccount name
            bb <- T.walletBestBlock
            (res, newAddrs) <- T.signAccountTx accE (Just notif) masterM txid
            case filter ((== ai) . walletTxAccount) res of
                (txRes:_) -> return (bb, txRes, newAddrs)
                _ -> throwM $ WalletException "Could not sign the transaction"

        whenOnline $ do
            -- Update the bloom filter
            unless (null newAddrs) updateNodeFilter
            -- If the transaction is pending, broadcast it to the network
            when (walletTxConfidence txRes == TxPending) $
                runNode $ N.broadcastTxs [walletTxHash txRes]

        return $ ResponseData $ T.fromTx name (Just bb) txRes

instance RequestPair Q.GetTx R.Tx where
    dispatch (Q.GetTx name txid) = do
        $(logInfo) $ format $ unlines
            [ "GetTx"
            , "  Account name: " ++ unpack name
            , "  TxId        : " ++ cs (HT.txHashToHex txid)
            ]

        (res, bb) <- runDB $ do
            Entity ai _ <- A.getAccount name
            bb <- T.walletBestBlock
            res <- T.getAccountTx ai txid
            return (res, bb)

        return $ ResponseData $ T.fromTx name (Just bb) res

-- TODO: This should be limited to a single account
instance RequestPair Q.DeleteTx () where
    dispatch (Q.DeleteTx txid) = do
        $(logInfo) $ format $ unlines
            [ "DeleteTx"
            , "  TxId: " ++ cs (HT.txHashToHex txid)
            ]
        runDB $ T.deleteTx txid
        return ResponseOK

instance RequestPair Q.OfflineTx OfflineTxData where
    dispatch (Q.OfflineTx accountName txid) = do
        $(logInfo) $ format $ unlines
            [ "OfflineTx"
            , "  Account name: " ++ unpack accountName
            , "  TxId        : " ++ cs (HT.txHashToHex txid)
            ]
        (dat, _) <- runDB $ do
            Entity ai _ <- A.getAccount accountName
            T.getOfflineTxData ai txid
        return $ ResponseData dat

instance RequestPair Q.SignOfflineTx R.TxComplete where
    dispatch (Q.SignOfflineTx accountName masterM tx signData) = do
        $(logInfo) $ format $ unlines
            [ "SignOfflineTx"
            , "  Account name: " ++ unpack accountName
            , "  TxId        : " ++ cs (HT.txHashToHex (HT.txHash tx))
            ]
        Entity _ acc <- runDB $ A.getAccount accountName

        let signedTx = T.signOfflineTx acc masterM tx signData
            complete = HT.verifyStdTx signedTx $ map toDat signData
            toDat CoinSignData{..} = (coinSignScriptOutput, coinSignOutPoint)

        return $ ResponseData $ R.TxComplete signedTx complete

instance RequestPair Q.Balance Word64 where
    dispatch (Q.Balance name minconf offline) = do
        $(logInfo) $ format $ unlines
            [ "Balance"
            , "  Account name: " ++ unpack name
            , "  Minconf     : " ++ show minconf
            , "  Offline     : " ++ show offline
            ]
        bal <- runDB $ do
            Entity ai _ <- A.getAccount name
            T.accountBalance ai minconf offline
        return $ ResponseData bal

-- Generic Syncing
syncGen ::
    ( MonadThrow m
    , MonadLoggerIO m
    , MonadBaseControl IO m
    )
    => AccountName
    -> (Either BlockHeight BlockHash)
    -> Q.List
    -> Handler m (WalletResponse (R.List R.Block))
syncGen acc blockE lq@Q.List{..} = runDB $ do
    $(logInfo) $ format $ unlines
        [ "Sync"
        , "  Account name: " ++ cs acc
        , "  Block       : " ++ showBlock
        , "  Offset      : " ++ show listOffset
        , "  Limit       : " ++ show listLimit
        , "  Reversed    : " ++ show listReverse
        ]
    R.List nodes cnt <- mainChain blockE lq
    ResponseData <$> case nodes of
        [] -> return $ R.List [] cnt
        b:_ -> do
            Entity ai _ <- A.getAccount acc
            ts <- T.accTxsFromBlock ai (nodeBlockHeight b)
                (fromIntegral $ length nodes)
            let bts = blockTxs nodes ts
            return $ R.List (map f bts) cnt
  where
    f (block, txs') = R.Block
        { blockHash   = nodeHash block
        , blockHeight = nodeBlockHeight block
        , blockPrev   = nodePrev block
        , blockTxs    = map (T.fromTx acc Nothing) txs'
        }
    showBlock = case blockE of
        Left  e -> show e
        Right b -> cs $ blockHashToHex b

instance RequestPair Q.SyncBlock (R.List R.Block) where
    dispatch (Q.SyncBlock acc bh lq) = syncGen acc (Right bh) lq

instance RequestPair Q.SyncHeight (R.List R.Block) where
    dispatch (Q.SyncHeight acc height lq) = syncGen acc (Left height) lq

instance RequestPair Q.BlockInfo [R.BlockInfo] where
    dispatch (Q.BlockInfo headerLst) = do
        $(logInfo) $ format "Received BlockInfo request"
        lstMaybeBlk <- forM headerLst (runNode . runSqlNodeT . getBlockByHash)
        return $ ResponseData $ map fromNodeBlock $ catMaybes lstMaybeBlk

instance RequestPair Q.NodeRescan Word32 where
    dispatch (Q.NodeRescan tM) = do
        t <- case tM of
            Just t  -> return $ adjustFCTime t
            Nothing -> do
                timeM <- runDB A.firstAddrTime
                maybe err (return . adjustFCTime) timeM
        $(logInfo) $ format $ unlines
            [ "Node Rescan"
            , "  Timestamp: " ++ show t
            ]
        whenOnline $ do
            runDB T.resetRescan
            runNode $ atomicallyNodeT $ N.rescanTs t
        return $ ResponseData t
      where
        err = throwM $ WalletException
            "No keys have been generated in the wallet"

instance RequestPair Q.NodeStatus NodeStatus where
    dispatch _ = do
        $(logInfo) $ format "Node Status"
        status <- runNode $ atomicallyNodeT N.nodeStatus
        return $ ResponseData status

instance RequestPair Q.StopServer () where
    dispatch _ = do
        $(logInfo) $ format "Received StopServer request"
        return ResponseOK

{- Helpers -}

whenOnline :: Monad m => Handler m () -> Handler m ()
whenOnline handler = do
    mode <- configMode `liftM` asks handlerConfig
    when (mode == SPVOnline) handler

updateNodeFilter
    :: (MonadBaseControl IO m, MonadLoggerIO m, MonadThrow m)
    => Handler m ()
updateNodeFilter = do
    $(logInfo) $ format "Sending a new bloom filter"
    (bloom, elems, _) <- runDB A.getBloomFilter
    runNode $ atomicallyNodeT $ sendBloomFilter bloom elems

adjustFCTime :: Timestamp -> Timestamp
adjustFCTime ts = fromInteger $ max 0 $ toInteger ts - 86400 * 7

format :: String -> Text
format str = pack $ "[ZeroMQ] " ++ str

