module Network.Haskoin.Wallet.Block where

import           Control.Exception                  (throw)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.Trans                (MonadIO)
import           Data.Maybe                         (fromMaybe)
import           Data.Time.Clock.POSIX              (posixSecondsToUTCTime)
import           Database.Persist.Sql               (SqlPersistT)
import           Network.Haskoin.Block
import           Network.Haskoin.Node.HeaderTree
import           Network.Haskoin.Wallet.Model
import qualified Network.Haskoin.Wallet.Request     as Q
import qualified Network.Haskoin.Wallet.Response    as R
import           Network.Haskoin.Wallet.Transaction
import           Network.Haskoin.Wallet.Types

mainChain :: (MonadIO m, MonadThrow m)
          => Either BlockHeight BlockHash
          -> Q.List
          -> SqlPersistT m (R.List NodeBlock)
mainChain blockE Q.List{..} = do
    bestHash <- fst <$> walletBestBlock
    bestM <- getBlockByHash bestHash
    best <- maybe (throwM $ WalletException "Could not find wallet best block")
        return bestM
    remoteNode <- case blockE of
        Right h -> do
            remoteNodeM <- getBlockByHash h
            maybe (throwM $ WalletException "Colud not get remote node")
                return remoteNodeM
        Left h -> do
            heightNodeM <- getBlockByHeight best h
            maybe (throwM $ WalletException "Could not find bock height")
                return heightNodeM
    frst <- (+1) . nodeBlockHeight <$> splitBlock best remoteNode
    if nodeBlockHeight best < frst
        then return $ R.List [] 0
        else do
            let cnt = nodeBlockHeight best - frst
                limit = min listLimit (cnt - listOffset)
                offset =
                    if listReverse
                    then cnt - listOffset - limit
                    else listOffset
            nodes <- getBlocksFromHeight best limit (frst + offset)
            return $ R.List nodes cnt

blockTxs :: [NodeBlock] -> [WalletTx] -> [(NodeBlock, [WalletTx])]
blockTxs blocks transactions = reverse $ go [] blocks transactions
  where
    go bs [] _ = bs
    go bs (n:ns) [] = go ((n,[]):bs) ns []
    go [] (n:ns) xs = go [(n,[])] ns xs
    go (b:bs) (n:ns) (x:xs)
       | nodeHash (fst b) == blockHashOf x =
           go ((fst b, x : snd b) : bs) (n:ns) xs
       | nodeHash n == blockHashOf x =
           go ((n, [x]) : b : bs) ns xs
       | otherwise = go ((n, []) : b : bs) ns (x:xs)
    blockHashOf t = fromMaybe
        (throw $ WalletException "Unexpected unconfirmed transaction")
        (walletTxConfirmedBy t)

fromNodeBlock :: NodeBlock -> R.BlockInfo
fromNodeBlock nb =
    R.BlockInfo
       { blockInfoHash       = headerHash header
       , blockInfoVersion    = blockVersion header
       , blockInfoPrevBlock  = prevBlock header
       , blockInfoNonce      = bhNonce header
       , blockInfoBits       = blockBits header
       , blockInfoMerkleRoot = merkleRoot header
       , blockInfoTimestamp  = utcTimestamp
       , blockInfoChainWork  = nodeWork   nb
       , blockInfoHeight     = nodeHeight nb
       , blockInfoChain      = nodeChain  nb
       }
  where
    header = nodeHeader nb
    utcTimestamp = posixSecondsToUTCTime . realToFrac .  blockTimestamp $ header

