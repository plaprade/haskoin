module Network.Haskoin.Wallet.Client.Commands
( cmdStart
, cmdStop
, cmdNewAcc
, cmdAddKey
, cmdSetGap
, cmdAccount
, cmdRenameAcc
, cmdAccounts
, cmdList
, cmdUnused
, cmdLabel
, cmdURI
, cmdTxs
, cmdAddrTxs
, cmdGetIndex
, cmdGenAddrs
, cmdSend
, cmdSendMany
, cmdImport
, cmdSign
, cmdBalance
, cmdGetTx
, cmdGetOffline
, cmdSignOffline
, cmdRescan
, cmdDecodeTx
, cmdVersion
, cmdStatus
, cmdBlockInfo
, cmdMonitor
, cmdSync
, cmdKeyPair
, cmdDeleteTx
, cmdPending
, cmdDead
, cmdDice
, decodeBase6
, diceToEntropy
, diceToMnemonic
, mixEntropy
)
where

import           Control.Concurrent.Async.Lifted          (async, wait)
import           Control.Monad
import qualified Control.Monad.Reader                     as RE
import           Control.Monad.Trans                      (liftIO)
import           Data.Aeson
import           Data.Bits                                (xor)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Base64                   as B64
import qualified Data.ByteString.Char8                    as B8
import           Data.List                                (intercalate,
                                                           intersperse)
import           Data.Maybe
import           Data.Monoid                              ((<>))
import           Data.Restricted                          (rvalue)
import qualified Data.Serialize                           as S
import           Data.String                              (fromString)
import           Data.String.Conversions                  (cs)
import qualified Data.Text                                as T
import qualified Data.Time.Format                         as Time
import           Data.Word                                (Word32, Word64)
import qualified Data.Yaml                                as YAML
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Node.STM
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Accounts          (rootToAccKey)
import qualified Network.Haskoin.Wallet.Client.PrettyJson as JSON
import qualified Network.Haskoin.Wallet.Request           as Q
import qualified Network.Haskoin.Wallet.Response          as R
import           Network.Haskoin.Wallet.Response          (WalletResponse(..))
import           Network.Haskoin.Wallet.Server
import           Network.Haskoin.Wallet.Settings
import           Network.Haskoin.Wallet.Types
import qualified Network.Haskoin.Wallet.Server.Handler    as H
import qualified Network.URI.Encode                       as URI
import           Numeric                                  (readInt)
import qualified System.Console.Haskeline                 as Haskeline
import           System.Entropy                           (getEntropy)
import           System.IO                                (stderr)
import           System.ZMQ4
import           Text.Read                                (readMaybe)

type Handler = RE.ReaderT Config IO

accountExists :: String -> Handler Bool
accountExists name = do
    res <- sendZmq (Q.GetAccount $ T.pack name)
    return $ case res of
        ResponseData _ -> True
        _ -> False

accountKeyExists :: String -> Handler Bool
accountKeyExists name = do
    R.Account{..} <- fromWalletResponse <$> sendZmq (Q.GetAccount $ T.pack name)
    return $ isJust accountMaster

data ParsedKey = ParsedXPrvKey  !XPrvKey
               | ParsedXPubKey  !XPubKey
               | ParsedMnemonic !Mnemonic !Passphrase !XPrvKey
               | ParsedNothing

askMnemonicOrKey :: String -> Handler ParsedKey
askMnemonicOrKey msg =
    go . cs =<< askInput msg
  where
    go "" = return ParsedNothing
    go str = case xPrvImport str of
        Just k -> return $ ParsedXPrvKey k
        _ -> case xPubImport str of
            Just p -> return $ ParsedXPubKey p
            -- This first check is just to verify if the mnemonic parses
            _ -> case mnemonicToSeed "" str of
                Right _ -> do
                    pass <- cs <$> askPassword
                    let seed = fromRight $ mnemonicToSeed (cs pass) str
                    return $ ParsedMnemonic str pass (makeXPrvKey seed)
                _ -> error "Could not parse mnemonic or extended key"

askInput :: String -> Handler String
askInput msg = do
    inputM <- liftIO $
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getPassword (Just '*') msg
    return $ fromMaybe err inputM
  where
    err = error "No action due to EOF"

askPassword :: Handler String
askPassword = do
    pass <- askInput "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInput "Enter your mnemonic password again: "
        when (pass /= pass2) $ error "Passwords do not match"
    return pass

askSigningKeys :: String -> Handler (Maybe XPrvKey)
askSigningKeys name = do
    -- Only ask for signing keys if the account doesn't have one already
    go =<< accountKeyExists name
  where
    go True = return Nothing
    go _ = do
        input <- askMnemonicOrKey "Enter mnemonic or extended private key: "
        case input of
            ParsedXPrvKey k      -> return $ Just k
            ParsedMnemonic _ _ k -> return $ Just k
            _ -> error "Need a private key to sign"

-- hw start [config] [--detach]
cmdStart :: Handler ()
cmdStart = do
    cfg <- RE.ask
    liftIO $ runSPVServer cfg

-- hw stop [config]
cmdStop :: Handler ()
cmdStop = do
    sendZmq Q.StopServer >>= (flip printResponse $ \() -> return ())
    liftIO $ putStrLn "Process stopped"

-- First argument: is account read-only?
cmdNewAcc :: Bool -> String -> [String] -> Handler ()
cmdNewAcc readOnly name ls = do
    e <- RE.asks configEntropy
    d <- RE.asks configDerivIndex
    _ <- return $! typ
    accountExists name >>= (`when` error "Account exists")
    (masterM, keyM, mnemonicM, passM) <- go =<< askMnemonicOrKey
        "Enter mnemonic, extended key or leave empty to generate: "
    let newAcc = Q.NewAccount
            { newAccountName     = T.pack name
            , newAccountType     = typ
            , newAccountMnemonic = cs <$> mnemonicM
            , newAccountPassword = cs <$> passM
            , newAccountEntropy  = Just e
            , newAccountMaster   = masterM
            , newAccountDeriv    = Just d
            , newAccountKeys     = maybeToList keyM
            , newAccountReadOnly = readOnly
            }
    res <- sendZmq newAcc
    printResponse res $ putStr . printAccount
  where
    go (ParsedXPrvKey k)      = return (Just k, Nothing, Nothing, Nothing)
    go (ParsedXPubKey p)      = return (Nothing, Just p, Nothing, Nothing)
    go (ParsedMnemonic m x _) = return (Nothing, Nothing, Just m, Just x)
    go ParsedNothing = do
        pass <- cs <$> askPassword
        return (Nothing, Nothing, Nothing, Just pass)
    typ = case ls of
        [] -> AccountRegular
        [mS, nS] -> fromMaybe (error "Account information incorrect") $ do
            m <- readMaybe mS
            n <- readMaybe nS
            return $ AccountMultisig m n
        _ -> error "Number of parametres incorrect"

cmdAddKey :: String -> Handler ()
cmdAddKey name = do
    d <- RE.asks configDerivIndex
    accountExists name >>= (`unless` error "Account does not exist")
    let msg = "Enter mnemonic or extended private key: "
    key <- askMnemonicOrKey msg >>= \pk -> return $ case pk of
        ParsedXPrvKey k      -> deriveXPubKey $ rootToAccKey k d
        ParsedMnemonic _ _ k -> deriveXPubKey $ rootToAccKey k d
        ParsedXPubKey p      -> p
        ParsedNothing        -> error "Invalid empty input"
    res <- sendZmq (Q.AddPubKeys (T.pack name) [key])
    printResponse res $ putStr . printAccount

cmdSetGap :: String -> String -> Handler ()
cmdSetGap name gap = do
    res <- sendZmq (Q.SetAccountGap (T.pack name) $ read gap)
    printResponse res $ putStr . printAccount

cmdAccount :: String -> Handler ()
cmdAccount name = do
    res <- sendZmq (Q.GetAccount $ T.pack name)
    printResponse res $ putStr . printAccount

cmdAccounts :: [String] -> Handler ()
cmdAccounts ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    pageAction page Q.Accounts $ printIntersperse "-" . map printAccount

cmdRenameAcc :: String -> String -> Handler ()
cmdRenameAcc oldName newName = do
    res <- sendZmq $ Q.RenameAccount (T.pack oldName) (T.pack newName)
    printResponse res $ putStr . printAccount

pageAction :: (H.RequestPair a (R.List b), FromJSON b, ToJSON a)
           => Word32
           -> (Q.List -> a)
           -> ([b] -> IO ())
           -> Handler ()
pageAction page requestBuilder action = do
    c <- RE.asks configCount
    r <- RE.asks configReversePaging
    case c of
        0 -> do
            res <- sendZmq (requestBuilder $ Q.List 0 0 r)
            printResponse res $ \(R.List b _) -> action b
        _ -> do
            when (page < 1) $ error "Page cannot be less than 1"
            res <- sendZmq (requestBuilder $ Q.List ((page - 1) * c) c r)
            printResponse res $ \(R.List b m) -> case m of
                0 -> putStrLn $ "No elements"
                _ -> do
                    putStrLn $  "Page " ++ show page
                             ++ " of " ++ show (pages m c)
                             ++ " (" ++ show m ++ " elements)"
                    action b
  where
    pages m c | m `mod` c == 0 = m `div` c
              | otherwise = m `div` c + 1

cmdList :: String -> [String] -> Handler ()
cmdList name ls = do
    t <- RE.asks configAddrType
    m <- RE.asks configMinConf
    o <- RE.asks configOffline
    p <- RE.asks configDisplayPubKeys
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    pageAction page (Q.Addresses (T.pack name) t m o) $
        mapM_ $ putStrLn . printAddress p

cmdUnused :: String -> [String] -> Handler ()
cmdUnused name ls = do
    t <- RE.asks configAddrType
    p <- RE.asks configDisplayPubKeys
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    pageAction page (Q.UnusedAddresses (T.pack name) t) $
        mapM_ $ putStrLn . printAddress p

cmdLabel :: String -> String -> String -> Handler ()
cmdLabel name iStr label = do
    t <- RE.asks configAddrType
    p <- RE.asks configDisplayPubKeys
    res <- sendZmq (Q.SetAddressLabel (T.pack name) i t addrLabel)
    printResponse res $ putStrLn . printAddress p
  where
    i         = read iStr
    addrLabel = T.pack label

cmdTxs :: String -> [String] -> Handler ()
cmdTxs name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- RE.asks configReversePaging
    pageAction page (Q.Txs (T.pack name)) $ \ts ->
        printIntersperse "-" $
            map (printTx Nothing) $
                if r then ts else reverse ts

cmdPending :: String -> [String] -> Handler ()
cmdPending name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- RE.asks configReversePaging
    pageAction page (Q.PendingTxs (T.pack name)) $ \ts ->
        printIntersperse "-" $
            map (printTx Nothing) $
                if r then ts else reverse ts

cmdDead :: String -> [String] -> Handler ()
cmdDead name ls = do
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    r <- RE.asks configReversePaging
    pageAction page (Q.DeadTxs (T.pack name)) $ \ts ->
        printIntersperse "-" $
            map (printTx Nothing) $
                if r then ts else reverse ts

cmdAddrTxs :: String -> String -> [String] -> Handler ()
cmdAddrTxs name i ls = do
    t <- RE.asks configAddrType
    m <- RE.asks configMinConf
    o <- RE.asks configOffline
    r <- RE.asks configReversePaging
    let page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    res <- sendZmq $ Q.GetAddress (T.pack name) index t m o
    case res of
        ResponseData R.Address{..} ->
            pageAction page (Q.AddressTxs (T.pack name) index t) $ \ts ->
                printIntersperse "-" $
                    map (printTx (Just addressAddress)) $
                        if r then ts else reverse ts
        _ -> error "Could not find address"
  where
    index = fromMaybe (error "Could not read index") $ readMaybe i

cmdGetIndex :: String -> String -> Handler ()
cmdGetIndex name k = do
    t <- RE.asks configAddrType
    res <- sendZmq $ Q.PubKeyIndex (T.pack name) (fromString k) t
    printResponse res go
  where
    go [] = putStrLn $ "No matching pubkey found"
    go as = mapM_ (putStrLn . printAddress True) as

cmdGenAddrs :: String -> String -> Handler ()
cmdGenAddrs name i = do
    t <- RE.asks configAddrType
    let req = Q.GenerateAddresses (T.pack name) (read i) t
    res <- sendZmq req
    printResponse res $ \cnt -> putStrLn $
        unwords [ "Generated", show (cnt :: Int), "addresses" ]

-- Build a bitcoin payment request URI
cmdURI :: String -> String -> [String] -> Handler ()
cmdURI name iStr ls = do
    t <- RE.asks configAddrType
    res <- sendZmq (Q.GetAddress (T.pack name) i t 0 False)
    case res of
        ResponseData a ->
            let uri = buildPaymentRequest (R.addressAddress a) ls
            in  liftIO $ putStrLn $ cs uri
        _ -> error "No address found"
  where
    i = read iStr

buildPaymentRequest :: Address -> [String] -> BS.ByteString
buildPaymentRequest a ls =
    "bitcoin:" <> addrToBase58 a <> cs params
  where
    params = if null paramStr then "" else "?" <> paramStr
    paramStr = concat $ intersperse "&" $ zipWith ($)
        [ ("amount" `buildParam`) . parseAmount
        , ("message" `buildParam`) . URI.encode
        ] ls
    parseAmount str = show (read str :: Double)
    buildParam str val = str <> "=" <> val

cmdSend :: String -> String -> String -> Handler ()
cmdSend name addrStr amntStr = cmdSendMany name [addrStr ++ ":" ++ amntStr]

cmdSendMany :: String -> [String] -> Handler ()
cmdSendMany name xs = case mapM (f . g) xs of
    Just rcps -> do
        fee     <- RE.asks configFee
        rcptFee <- RE.asks configRcptFee
        minconf <- RE.asks configMinConf
        sign    <- RE.asks configSignTx
        masterM <- if sign then askSigningKeys name else return Nothing
        let action =
                Q.CreateTx (T.pack name) rcps fee minconf rcptFee sign masterM
        res <- sendZmq action
        printResponse res $ putStr . printTx Nothing
    _ -> error "Could not parse recipient information"
  where
    g str   = map cs $ T.splitOn ":" (T.pack str)
    f [a,v] = liftM2 (,) (base58ToAddr a) (readMaybe $ cs v)
    f _     = Nothing

getHexTx :: Handler Tx
getHexTx = do
    hexM <- Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine ""
    return $
        fromMaybe (error "Could not parse transaction") $
        decodeToMaybe =<< decodeHex . cs =<< hexM

cmdImport :: String -> Handler ()
cmdImport name = do
    tx <- getHexTx
    res <- sendZmq (Q.ImportTx (T.pack name) tx)
    printResponse res $ liftIO . putStr . printTx Nothing

cmdSign :: String -> String -> Handler ()
cmdSign name txidStr = case hexToTxHash $ cs txidStr of
    Just txid -> do
        masterM <- askSigningKeys name
        res <- sendZmq (Q.SignTx (T.pack name) txid masterM)
        printResponse res $ liftIO . putStr . printTx Nothing
    _ -> error "Could not parse txid"

cmdGetOffline :: String -> String -> Handler ()
cmdGetOffline name tidStr = case hexToTxHash $ cs tidStr of
    Just tid -> do
        res <- sendZmq (Q.OfflineTx (T.pack name) tid)
        printResponse res $ putStrLn . cs . B64.encode . S.encode
    _ -> error "Could not parse txid"

cmdSignOffline :: String -> Handler ()
cmdSignOffline name = do
    inputM <- Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine ""
    case S.decode =<< B64.decode . cs =<< maybeToEither "" inputM of
        Right (OfflineTxData tx dat) -> do
            masterM <- askSigningKeys name
            res <- sendZmq (Q.SignOfflineTx (T.pack name) masterM tx dat)
            printResponse res $ \(R.TxComplete sTx _) ->
                putStrLn $ cs $ encodeHex $ S.encode sTx
        _ -> error "Could not decode input data"

cmdBalance :: String -> Handler ()
cmdBalance name = do
    m <- RE.asks configMinConf
    o <- RE.asks configOffline
    res <- sendZmq (Q.Balance (T.pack name) m o)
    printResponse res $ \bal ->
        putStrLn $ unwords [ "Balance:", show bal ]

cmdGetTx :: String -> String -> Handler ()
cmdGetTx name tidStr = case hexToTxHash $ cs tidStr of
    Just tid -> do
        res <- sendZmq (Q.GetTx (T.pack name) tid)
        printResponse res $ putStr . printTx Nothing
    _ -> error "Could not parse txid"

cmdRescan :: [String] -> Handler ()
cmdRescan timeLs = do
    let timeM = (fromMaybe err . readMaybe) <$> listToMaybe timeLs
        err   = error "Could not decode time"
    res <- sendZmq (Q.NodeRescan timeM)
    printResponse res $ \ts ->
        putStrLn $ unwords [ "Timestamp:", show ts]

cmdDeleteTx :: String -> Handler ()
cmdDeleteTx tidStr = case hexToTxHash $ cs tidStr of
    Just tid -> do
        res <- sendZmq (Q.DeleteTx tid)
        printResponse res $ \() -> return ()
    Nothing -> error "Could not parse txid"

cmdMonitor :: [String] -> Handler ()
cmdMonitor ls = do
    cfg@Config{..} <- RE.ask
    -- TODO: I can do this in the same thread without ^C twice (see sendZmq)
    liftIO $ withContext $ \ctx -> withSocket ctx Sub $ \sock -> do
        setLinger (restrict (0 :: Int)) sock
        setupAuth cfg sock
        connect sock configConnectNotif
        subscribe sock "[block]"
        forM_ ls $ \name -> subscribe sock $ "{" <> cs name <> "}"
        forever $ do
            [_,m] <- receiveMulti sock
            handleNotif configFormat $ eitherDecode $ cs m

cmdSync :: String -> String -> [String] -> Handler ()
cmdSync acc block ls = do
    r <- RE.asks configReversePaging
    case length block of
        64 ->
            let a = Q.SyncBlock (cs acc) $
                        fromMaybe (error "Could not decode block id") $
                        hexToBlockHash $ cs block
            in pageAction page a $ f r
        _  ->
            let a = Q.SyncHeight (cs acc) $
                        fromMaybe (error "Could not decode block height") $
                        readMaybe block
            in pageAction page a $ f r
  where
    page = fromMaybe 1 $ listToMaybe ls >>= readMaybe
    f r blocks =
        printIntersperse "-" $ map printBlock $
            if r then reverse blocks else blocks

cmdDecodeTx :: Handler ()
cmdDecodeTx = do
    tx <- getHexTx
    format <- RE.asks configFormat
    liftIO $ formatStr $ cs $ case format of
        OutputJSON -> cs $ JSON.encodePretty . encodeTxJSON $ tx
        _          -> YAML.encode $ encodeTxJSON tx

cmdVersion :: Handler ()
cmdVersion = liftIO $ do
    putStrLn $ unwords [ "network   :", cs networkName ]
    putStrLn $ unwords [ "user-agent:", cs haskoinUserAgent ]

cmdStatus :: Handler ()
cmdStatus = do
    v   <- RE.asks configVerbose
    res <- sendZmq Q.NodeStatus
    printResponse res $ mapM_ putStrLn . printNodeStatus v

cmdKeyPair :: Handler ()
cmdKeyPair = do
    (pub, sec) <- curveKeyPair
    liftIO $ do
        B8.putStrLn $ B8.unwords [ "public :", rvalue pub ]
        B8.putStrLn $ B8.unwords [ "private:", rvalue sec ]

cmdBlockInfo :: [String] -> Handler ()
cmdBlockInfo headers = do
    -- Show best block if no arguments are provided
    hashL <- if null headers then
            -- Fetch best block hash from status msg, and return as list
            f <$> sendZmq Q.NodeStatus
        else
            return (map fromString headers)
    sendZmq (Q.BlockInfo hashL) >>= \res ->
        printResponse res $ printIntersperse "-" . concat . map printBlockInfo
  where
    f = (:[]) . nodeStatusBestBlock . fromWalletResponse

-- Do not reuse a dice roll to generate mnemonics because:
-- (D xor B) xor (D xor C) = (D xor D) xor (B xor C) = (B xor C)
-- You will leak (B xor C) which is the xor of your computer entropy.
-- In other words, don't reuse any part of a one time pad.
cmdDice :: String -> Handler ()
cmdDice rolls = case diceToEntropy rolls of
    Right ent1 -> do
        -- Get more entropy from /dev/urandom
        ent2 <- liftIO $ getEntropy 32
        -- Mix the entropy using xor and generate a mnemonic
        case toMnemonic $ mixEntropy ent1 ent2 of
            Right ms -> liftIO $ putStrLn $ cs ms
            Left err -> error err
    Left err -> error err

-- Mix entropy of same length by xoring them
mixEntropy :: BS.ByteString -> BS.ByteString -> BS.ByteString
mixEntropy ent1 ent2
    | BS.length ent1 == BS.length ent2 = BS.pack $ BS.zipWith xor ent1 ent2
    | otherwise = error "Entropy is not of the same length"

diceToMnemonic :: String -> Either String BS.ByteString
diceToMnemonic = toMnemonic <=< diceToEntropy

-- Transform 99 dice rolls (255.9 bits of entropy) into zero padded 32 bytes
diceToEntropy :: String -> Either String BS.ByteString
diceToEntropy rolls
    | length rolls /= 99 = Left "99 dice rolls are required"
    | otherwise = do
        ent <- maybeToEither "Could not decode base6" $ decodeBase6 $ cs rolls
        -- This check should probably never trigger
        when (BS.length ent > 32) $ Left "Invalid entropy length"
        let z = BS.replicate (32 - BS.length ent) 0x00
        return $ BS.append z ent

b6Data :: BS.ByteString
b6Data = "612345"

b6' :: Char -> Maybe Int
b6' = flip B8.elemIndex b6Data

decodeBase6 :: BS.ByteString -> Maybe BS.ByteString
decodeBase6 t
    | BS.null t = Just BS.empty
    | otherwise = integerToBS <$> decodeBase6I t

decodeBase6I :: BS.ByteString -> Maybe Integer
decodeBase6I bs = case resM of
    Just (i,[]) -> return i
    _ -> Nothing
  where
    resM = listToMaybe $ readInt 6 (isJust . b6') f $ cs bs
    f    = fromMaybe (error "Could not decode base6") . b6'

{- Helpers -}

printIntersperse :: String -> [String] -> IO ()
printIntersperse i xs = sequence_ $ intersperse (putStrLn i) $ map putStr xs

handleNotif :: OutputFormat -> Either String R.Notif -> IO ()
handleNotif _   (Left e) = error e
handleNotif fmt (Right notif) = case fmt of
    OutputJSON -> formatStr $ cs $
        JSON.encodePretty notif
    OutputYAML -> do
        putStrLn "---"
        formatStr $ cs $ YAML.encode notif
        putStrLn "..."
    OutputNormal ->
        putStrLn $ printNotif notif

printResponse :: ToJSON b => WalletResponse b -> (b -> IO ()) -> Handler ()
printResponse res custom = case res of
    ResponseData d    -> liftIO . formatOutput d =<< RE.asks configFormat
    ResponseOK        -> liftIO $ putStrLn "Command completed successfully."
    ResponseError err -> error $ cs err
  where
    formatOutput a format = case format of
        OutputJSON   -> formatStr $ cs $ JSON.encodePretty a
        OutputYAML   -> formatStr $ cs $ YAML.encode a
        OutputNormal -> custom a

fromWalletResponse :: WalletResponse b -> b
fromWalletResponse res = case res of
    ResponseData d    -> d
    ResponseOK        -> error "fromWalletResponse return ResponseOK"
    ResponseError err -> error $ cs err

sendZmq :: (H.RequestPair a b, ToJSON a, FromJSON b)
        => a -> Handler (WalletResponse b)
sendZmq req = do
    cfg <- RE.ask
    let msg = cs $ encode req
    when (configVerbose cfg) $ liftIO $
        B8.hPutStrLn stderr $ "Outgoing JSON: " `mappend` msg
    -- TODO: If I do this in the same thread I have to ^C twice to exit
    a <- async $ liftIO $ withContext $ \ctx ->
        withSocket ctx Req $ \sock -> do
            setLinger (restrict (0 :: Int)) sock
            setupAuth cfg sock
            connect sock (configConnect cfg)
            send sock [] (cs $ encode req)
            joinWalletResponse . eitherDecode . cs <$> receive sock
    wait a

joinWalletResponse :: Either String (WalletResponse b) -> WalletResponse b
joinWalletResponse (Right r) = r
joinWalletResponse (Left err) = ResponseError $ cs err

setupAuth :: (SocketType t)
          => Config
          -> Socket t
          -> IO ()
setupAuth cfg sock = do
    let clientKeyM    = configClientKey    cfg
        clientKeyPubM = configClientKeyPub cfg
        serverKeyPubM = configServerKeyPub cfg
    forM_ clientKeyM $ \clientKey -> do
        let serverKeyPub = fromMaybe
              (error "Server public key not provided")
              serverKeyPubM
            clientKeyPub = fromMaybe
              (error "Client public key not provided")
              clientKeyPubM
        setCurveServerKey TextFormat serverKeyPub sock
        setCurvePublicKey TextFormat clientKeyPub sock
        setCurveSecretKey TextFormat clientKey sock

formatStr :: String -> IO ()
formatStr str = forM_ (lines str) putStrLn

encodeTxJSON :: Tx -> Value
encodeTxJSON tx = object
    [ "txid"     .= (cs $ txHashToHex (txHash tx) :: T.Text)
    , "version"  .= txVersion tx
    , "inputs"   .= map encodeTxInJSON (txIn tx)
    , "outputs"  .= map encodeTxOutJSON (txOut tx)
    , "locktime" .= txLockTime tx
    ]

encodeTxInJSON :: TxIn -> Value
encodeTxInJSON (TxIn o s i) = object $
    [ "outpoint"   .= encodeOutPointJSON o
    , "sequence"   .= i
    , "raw-script" .= (cs $ encodeHex s :: T.Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const []) f $ decodeInputBS s
    f inp = ["decoded-script" .= encodeScriptInputJSON inp]

encodeTxOutJSON :: TxOut -> Value
encodeTxOutJSON (TxOut v s) = object $
    [ "value"      .= v
    , "raw-script" .= (cs $ encodeHex s :: T.Text)
    , "script"     .= encodeScriptJSON sp
    ] ++ decoded
  where
    sp = fromMaybe (Script []) $ decodeToMaybe s
    decoded = either (const [])
                 (\out -> ["decoded-script" .= encodeScriptOutputJSON out])
                 (decodeOutputBS s)

encodeOutPointJSON :: OutPoint -> Value
encodeOutPointJSON (OutPoint h i) = object
    [ "txid" .= (cs $ txHashToHex h :: T.Text)
    , "pos"  .= i
    ]

encodeScriptJSON :: Script -> Value
encodeScriptJSON (Script ops) =
    toJSON $ map f ops
  where
    f (OP_PUSHDATA bs _) = String $ T.pack $ unwords
        ["OP_PUSHDATA", cs $ encodeHex bs]
    f x = String $ T.pack $ show x

encodeScriptInputJSON :: ScriptInput -> Value
encodeScriptInputJSON si = case si of
    RegularInput (SpendPK s) -> object
        [ "spendpubkey" .= object [ "sig" .= encodeSigJSON s ] ]
    RegularInput (SpendPKHash s p) -> object
        [ "spendpubkeyhash" .= object
            [ "sig"            .= encodeSigJSON s
            , "pubkey"         .= (cs $ encodeHex (S.encode p) :: T.Text)
            , "sender-address" .= (cs $ addrToBase58 (pubKeyAddr p) :: T.Text)
            ]
        ]
    RegularInput (SpendMulSig sigs) -> object
        [ "spendmulsig" .= object [ "sigs" .= map encodeSigJSON sigs ] ]
    ScriptHashInput s r -> object
        [ "spendscripthash" .= object
            [ "scriptinput" .= encodeScriptInputJSON (RegularInput s)
            , "redeem" .= encodeScriptOutputJSON r
            , "raw-redeem" .= (cs $ encodeHex (encodeOutputBS r) :: T.Text)
            , "sender-address" .= (cs $ addrToBase58 (scriptAddr r) :: T.Text)
            ]
        ]

encodeScriptOutputJSON :: ScriptOutput -> Value
encodeScriptOutputJSON so = case so of
    PayPK p -> object
        [ "pay2pubkey" .= object
          [ "pubkey" .= (cs $ encodeHex (S.encode p) :: T.Text) ]
        ]
    PayPKHash a -> object
        [ "pay2pubkeyhash" .= object
            [ "address-hex" .=
              (cs $ encodeHex (S.encode $ getAddrHash a) :: T.Text)
            , "address-base58" .= (cs $ addrToBase58 a :: T.Text)
            ]
        ]
    PayMulSig ks r -> object
        [ "pay2mulsig" .= object
            [ "required-keys" .= r
            , "pubkeys" .= (map (cs . encodeHex . S.encode) ks :: [T.Text])
            ]
        ]
    PayScriptHash a -> object
        [ "pay2scripthash" .= object
            [ "address-hex" .= (cs $ encodeHex $
                S.encode $ getAddrHash a :: T.Text)
            , "address-base58" .= (cs (addrToBase58 a) :: T.Text)
            ]
        ]
    DataCarrier bs -> object
        [ "op_return" .= object
            [ "data" .= (cs $ encodeHex bs :: T.Text)
            ]
        ]

encodeSigJSON :: TxSignature -> Value
encodeSigJSON ts@(TxSignature _ sh) = object
    [ "raw-sig" .= (cs $ encodeHex (encodeSig ts) :: T.Text)
    , "sighash" .= encodeSigHashJSON sh
    ]

encodeSigHashJSON :: SigHash -> Value
encodeSigHashJSON sh = case sh of
    SigAll acp -> object
        [ "type" .= String "SigAll"
        , "acp"  .= acp
        ]
    SigNone acp -> object
        [ "type" .= String "SigNone"
        , "acp"  .= acp
        ]
    SigSingle acp -> object
        [ "type" .= String "SigSingle"
        , "acp"  .= acp
        ]
    SigUnknown acp v -> object
        [ "type"  .= String "SigUnknown"
        , "acp"   .= acp
        , "value" .= v
        ]

{- Print utilities -}

printAccount :: R.Account -> String
printAccount R.Account{..} = unlines $
    [ "Account : " ++ T.unpack accountName
    , "Type    : " ++ showType
    , "Gap     : " ++ show accountGap
    ]
    ++
    [ "Index   : " ++ show i | i <- childLs ]
    ++
    [ "Mnemonic: " ++ cs ms
    | ms <- maybeToList accountMnemonic
    ]
    ++
    concat [ printKeys | not (null accountKeys) ]
  where
    childLs = case accountType of
        AccountRegular -> map xPubChild accountKeys
        _ -> maybeToList $ xPrvChild <$> accountMaster
    printKeys =
        ("Keys    : " ++ cs (xPubExport (head accountKeys))) :
        map (("          " ++) . cs . xPubExport) (tail accountKeys)
    showType = case accountType of
        AccountRegular -> if isNothing accountMaster
                              then "Read-Only" else "Regular"
        AccountMultisig m n -> unwords
            [ if isNothing accountMaster
                 then "Read-Only Multisig" else "Multisig"
            , show m, "of", show n
            ]

printAddress :: Bool -> R.Address -> String
printAddress displayPubKey R.Address{..} = unwords $
    [ show addressIndex, ":", cs dat ]
    ++
    [ "(" ++ T.unpack addressLabel ++ ")"
    | not (null $ T.unpack addressLabel)
    ]
    ++ concat
    [ [ "[Received: "    ++ show (R.balanceInBalance  bal) ++ "]"
      , "[Coins: "       ++ show (R.balanceCoins      bal) ++ "]"
      , "[Spent Coins: " ++ show (R.balanceSpentCoins bal) ++ "]"
      ]
    | isJust addressBalance && R.balanceCoins bal > 0
    ]
  where
    dat | displayPubKey =
            maybe "<no pubkey available>" (encodeHex . S.encode) addressKey
        | otherwise = addrToBase58 addressAddress
    bal = fromMaybe (error "Could not get address balance") addressBalance

printNotif :: R.Notif -> String
printNotif (R.NotifTx   tx) = printTx Nothing tx
printNotif (R.NotifBlock b) = printBlock b

printTx :: Maybe Address -> R.Tx -> String
printTx aM tx@R.Tx{..} = unlines $
    [ "Id         : " ++ cs (txHashToHex txTxHash) ]
    ++
    [ "Value      : " ++ printTxType txType ++ " " ++ show txValue ]
    ++
    [ "Confidence : " ++ printTxConfidence tx ]
    ++ concat
    [ printAddrInfos "Inputs     : " txInputs
    | not (null txInputs)
    ]
    ++ concat
    [ printAddrInfos "Outputs    : " txOutputs
    | not (null txOutputs)
    ]
    ++ concat
    [ printAddrInfos "Change     : " txChange
    | not (null txChange)
    ]
  where
    printAddrInfos header xs =
        (header ++ f (head xs)) :
        map (("             " ++) . f) (tail xs)
    f (AddressInfo addr valM local) = unwords $
        cs (addrToBase58 addr) :
        [ show v | v <- maybeToList valM ]
        ++
        [ "<-" | maybe local (== addr) aM ]

printTxConfidence :: R.Tx -> String
printTxConfidence R.Tx{..} = case txConfidence of
    TxBuilding -> "Building" ++ confirmations
    TxPending  -> "Pending" ++ confirmations
    TxDead     -> "Dead" ++ confirmations
    TxOffline  -> "Offline"
  where
    confirmations = case txConfirmations of
        Just conf -> " (Confirmations: " ++ show conf ++ ")"
        _         -> ""

printTxType :: TxType -> String
printTxType t = case t of
    TxIncoming -> "Incoming"
    TxOutgoing -> "Outgoing"
    TxSelf     -> "Self"

printBlock :: R.Block -> String
printBlock R.Block{..} = unlines
    [ "Block Hash      : " ++ cs (blockHashToHex blockHash)
    , "Block Height    : " ++ show blockHeight
    , "Previous block  : " ++ cs (blockHashToHex blockPrev)
    , "Transactions    : " ++ show (length blockTxs)
    ]

printNodeStatus :: Bool -> NodeStatus -> [String]
printNodeStatus verbose NodeStatus{..} =
    [ "Network Height    : " ++ show nodeStatusNetworkHeight
    , "Best Header       : " ++ cs (blockHashToHex nodeStatusBestHeader)
    , "Best Header Height: " ++ show nodeStatusBestHeaderHeight
    , "Best Block        : " ++ cs (blockHashToHex nodeStatusBestBlock)
    , "Best Block Height : " ++ show nodeStatusBestBlockHeight
    , "Bloom Filter Size : " ++ show nodeStatusBloomSize
    ] ++
    [ "Header Peer       : " ++ show h
    | h <- maybeToList nodeStatusHeaderPeer, verbose
    ] ++
    [ "Merkle Peer       : " ++ show m
    | m <- maybeToList nodeStatusMerklePeer, verbose
    ] ++
    [ "Pending Headers   : " ++ show nodeStatusHaveHeaders | verbose ] ++
    [ "Pending Tickles   : " ++ show nodeStatusHaveTickles | verbose ] ++
    [ "Pending Txs       : " ++ show nodeStatusHaveTxs | verbose ] ++
    [ "Pending GetData   : " ++ show (map txHashToHex nodeStatusGetData)
    | verbose
    ] ++
    [ "Pending Rescan    : " ++ show r
    | r <- maybeToList nodeStatusRescan, verbose
    ] ++
    [ "Synced Mempool    : " ++ show nodeStatusMempool | verbose ] ++
    [ "HeaderSync Lock   : " ++ show nodeStatusSyncLock | verbose ] ++
    [ "Peers: " ] ++
    intercalate ["-"] (map (printPeerStatus verbose) nodeStatusPeers)

printPeerStatus :: Bool -> PeerStatus -> [String]
printPeerStatus verbose PeerStatus{..} =
    [ "  Peer Id  : " ++ show peerStatusPeerId
    , "  Peer Host: " ++ peerHostString peerStatusHost
    , "  Connected: " ++ if peerStatusConnected then "yes" else "no"
    , "  Height   : " ++ show peerStatusHeight
    ] ++
    [ "  Protocol : " ++ show p | p <- maybeToList peerStatusProtocol
    ] ++
    [ "  UserAgent: " ++ ua | ua <- maybeToList peerStatusUserAgent
    ] ++
    [ "  Avg Ping : " ++ p | p <- maybeToList peerStatusPing
    ] ++
    [ "  DoS Score: " ++ show d | d <- maybeToList peerStatusDoSScore
    ] ++
    [ "  Merkles  : " ++ show peerStatusHaveMerkles | verbose ] ++
    [ "  Messages : " ++ show peerStatusHaveMessage | verbose ] ++
    [ "  Nonces   : " ++ show peerStatusPingNonces | verbose ] ++
    [ "  Reconnect: " ++ show t
    | t <- maybeToList peerStatusReconnectTimer, verbose
    ] ++
    [ "  Logs     : " | verbose ] ++
    [ "    - " ++ msg | msg <- fromMaybe [] peerStatusLog, verbose]

printBlockInfo :: R.BlockInfo -> [String]
printBlockInfo R.BlockInfo{..} =
    [ "Block Height     : " ++ show blockInfoHeight
    , "Block Hash       : " ++ cs (blockHashToHex blockInfoHash)
    , "Block Timestamp  : " ++ formatUTCTime blockInfoTimestamp
    , "Previous Block   : " ++ cs (blockHashToHex blockInfoPrevBlock)
    , "Merkle Root      : " ++ show blockInfoMerkleRoot
    , "Block Version    : " ++ "0x" ++ cs (encodeHex versionData)
    , "Block Difficulty : " ++ show (blockDiff blockInfoBits)
    , "Chain Work       : " ++ show blockInfoChainWork
    ]
  where
    blockDiff :: Word32 -> Double
    blockDiff target = getTarget (blockBits genesisHeader) / getTarget target
    getTarget   = fromIntegral . decodeCompact
    versionData = integerToBS (fromIntegral blockInfoVersion)
    formatUTCTime = Time.formatTime Time.defaultTimeLocale
        "%Y-%m-%d %H:%M:%S (UTC)"

