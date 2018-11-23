\subsection{Cardano.BM.Output.Katip}

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Output.Katip
    (
      setup
    , pass
    --, takedown
    ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, modifyMVar_, withMVar)
import           Control.Exception.Safe (catchIO)
import           Control.Monad (forM_)
import           Control.Lens ((^.))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Version (Version (..), showVersion)
import           GHC.Conc (atomically, myThreadId)
import           System.Directory (createDirectoryIfMissing)
import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout, openFile, IOMode (WriteMode))
import           System.Directory (createDirectoryIfMissing)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Katip as K
import           Katip.Core (Item (..), LogItem (..), Scribe (..),
                     ScribeHandle (..), ScribeSettings (..), Severity (..),
                     Verbosity (..), WorkerMessage (NewItem), getThreadIdText,
                     intercalateNs, mkThreadIdText, renderSeverity,
                     tryWriteTBQueue, unLogStr)
import qualified Katip.Core as KC
import           Katip.Scribes.Handle (brackets)

import qualified Cardano.BM.Data as Data
import qualified Cardano.BM.Output.Internal as Internal
\end{code}
%endif

Katip is a singleton.
\begin{code}
-- internal access to katip
{-# NOINLINE katip #-}
katip :: MVar KatipInternal
katip = unsafePerformIO $ do
    newMVar $ error "Katip MVar is not initialized."

-- Our internal state
data KatipInternal = KatipInternal
    { kInstance  :: [Data.NamedLogItem]  -- TODO
    , kLogEnv    :: K.LogEnv  -- TODO
    }

\end{code}

Setup |katip| and its scribes according to the configuration
\begin{code}
setup :: Text -> Data.Configuration -> IO ()
setup cfoKey _ = do
    -- TODO setup katip
    le0 <- K.initLogEnv
                (K.Namespace ["ouroboros-bm"])
                (fromString $ (T.unpack cfoKey) <> ":" <> showVersion mockVersion)
    -- request a new time 'getCurrentTime' at most 100 times a second
    timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
    let le1 = updateEnv le0 timer
    stdoutScribe <- mkStdoutScribeJson K.V0
    le <- register [(Data.StdoutSK, "stdout", stdoutScribe)] le1
    _ <- takeMVar katip
    putMVar katip $ KatipInternal [] le
  where
    updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
    updateEnv le timer =
        le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }
    register :: [(Data.ScribeKind, T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
    register [] le = return le
    register ((kind, name, scribe) : scs) le =
        let name' = T.pack (show kind) <> "::" <> name in
        register scs =<< K.registerScribe name' scribe scribeSettings le
    mockVersion :: Version
    mockVersion = Version [0,1,0,0] []
    scribeSettings :: ScribeSettings
    scribeSettings = ScribeSettings bufferSize
      where
        bufferSize = 5000  -- size of the queue (in log items)

\end{code}

\begin{code}
example :: IO ()
example = do
    setup "CFOKEY" Data.Configuration
    pass (T.pack (show Data.StdoutSK)) $ Data.LogNamed
                                            { Data.lnName = "test"
                                            , Data.lnItem = Data.LP $ Data.LogMessage $ Data.LogItem
                                                { Data.liSelection = Data.Both
                                                , Data.liSeverity  = Data.Info
                                                , Data.liPayload   = "Hello!"
                                                }
                                            }
    pass (T.pack (show Data.StdoutSK)) $ Data.LogNamed
                                            { Data.lnName = "test"
                                            , Data.lnItem = Data.LP $ Data.LogValue "cpu-no" 1
                                            }

-- useful instances for Katip
deriving instance K.ToObject Data.LogObject
deriving instance K.ToObject Data.LogItem
deriving instance K.ToObject (Maybe Data.LogObject)
instance KC.LogItem Data.LogObject where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem Data.LogItem where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem (Maybe Data.LogObject) where
    payloadKeys _ _ = KC.AllKeys

pass :: T.Text -> Data.NamedLogItem -> IO ()
pass backend namedLogItem = withMVar katip $ \k -> do
    -- TODO go through list of registered scribes
    --      and put into queue of scribe if backend kind matches
    --      compare start of name of scribe to (show backend <> "::")
    let env = kLogEnv k
    forM_ (Map.toList $ K._logEnvScribes env) $
          \(scName, (ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
                if backend `T.isPrefixOf` scName
                then do
                    let item = Data.lnItem namedLogItem
                    let (sev, msg, payload) = case item of
                                (Data.LP (Data.LogMessage logItem)) ->
                                    (Data.liSeverity logItem, Data.liPayload logItem, Nothing)
                                _ ->
                                    (Data.Info, "", Just item)
                    threadIdText <- mkThreadIdText <$> myThreadId
                    let ns = Data.lnName namedLogItem
                    itemTime <- env ^. KC.logEnvTimer
                    let itemKatip = Item {
                              _itemApp       = env ^. KC.logEnvApp
                            , _itemEnv       = env ^. KC.logEnvEnv
                            , _itemSeverity  = sev2klog sev
                            , _itemThread    = threadIdText
                            , _itemHost      = env ^. KC.logEnvHost
                            , _itemProcess   = env ^. KC.logEnvPid
                            , _itemPayload   = payload
                            , _itemMessage   = K.logStr msg
                            , _itemTime      = itemTime
                            , _itemNamespace = (env ^. KC.logEnvApp) <> (K.Namespace [ns])
                            , _itemLoc       = Nothing
                            }
                    atomically $ tryWriteTBQueue shChan (NewItem itemKatip)
                else return False
\end{code}

\begin{spec}
    -- TODO go through list of registered scribes
    --      and put into queue of scribe if backend kind matches
    --      compare start of name of scribe to (show backend <> "::")
    forM_ (toList $ K._logEnvScribes (kLogEnv k)) $
          \(scName, (ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
              if scName `T.isPrefixOf` backend
              then return False --TODO atomically $ tryWriteTBQueue shChan (NewItem item)
              else return False

\end{spec}

\subsubsection{Scribes}
\begin{code}
mkStdoutScribe :: Verbosity -> IO Scribe
mkStdoutScribe = mkTextFileScribeH stdout True

mkStdoutScribeJson :: Verbosity -> IO Scribe
mkStdoutScribeJson = mkJsonFileScribeH stdout True

mkStderrScribe :: Verbosity -> IO Scribe
mkStderrScribe = mkTextFileScribeH stderr True

mkJsonFileScribeH :: Handle -> Bool -> Verbosity -> IO Scribe
mkJsonFileScribeH handler color verb = do
    mkFileScribeH handler formatter color verb
  where
    formatter :: (LogItem a) => Handle -> Bool -> Verbosity -> Item a -> IO ()
    formatter h _ verbosity item = do
        let tmsg = case _itemMessage item of
                K.LogStr ""  -> encodeToLazyText $ K.itemJson verbosity item
                K.LogStr msg -> encodeToLazyText $ K.itemJson verbosity $
                                    item { _itemMessage = K.logStr (""::Text)
                                            , _itemPayload = Data.LogItem Data.Both Data.Info $ toStrict $ toLazyText msg
                                            }
                                            -- TODO this need reconsidering !!!
        TIO.hPutStrLn h tmsg

mkTextFileScribeH :: Handle -> Bool -> Verbosity -> IO Scribe
mkTextFileScribeH handler color verb = do
    mkFileScribeH handler formatter color verb
  where
    formatter h colorize verbosity item =
        TIO.hPutStrLn h $! toLazyText $ formatItem colorize verbosity item

mkFileScribeH
    :: Handle
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO ())  -- format and output function
    -> Bool  -- whether the output is colourized
    -> Verbosity
    -> IO Scribe
mkFileScribeH h formatter colorize verbosity = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. LogItem a =>  Item a -> IO ()
        logger item = withMVar locklocal $ \_ ->
                        formatter h colorize verbosity item
    pure $ Scribe logger (hClose h)

-- | create a katip scribe for logging to a file in textual representation
mkTextFileScribe :: Internal.FileDescription -> Bool -> Data.Severity -> Verbosity -> IO Scribe
mkTextFileScribe fdesc colorize s v = do
    mkFileScribe fdesc formatter colorize s v
  where
    formatter :: Handle -> Bool -> Verbosity -> Item a -> IO ()
    formatter hdl colorize' v' item = do
        -- case _item payload
        let tmsg = toLazyText $ formatItem colorize' v' item
        TIO.hPutStrLn hdl tmsg

-- | create a katip scribe for logging to a file
--   and handle file rotation within the katip-invoked logging function
mkFileScribe
    :: Internal.FileDescription
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO ())  -- format and output function, returns written bytes
    -> Bool  -- whether the output is colourized
    -> Data.Severity
    -> Verbosity
    -> IO Scribe
mkFileScribe fdesc formatter colorize _ v = do
    let prefixDir = Internal.prefixPath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (Internal.prtoutException ("cannot log prefix directory: " ++ prefixDir))
    let fpath = Internal.filePath fdesc
    h <- catchIO (openFile fpath WriteMode) $
                        \e -> do
                            Internal.prtoutException ("error while opening log: " ++ fpath) e
                            -- fallback to standard output in case of exception
                            return stdout
    hSetBuffering h LineBuffering
    scribestate <- newMVar h
    let finalizer :: IO ()
        finalizer = withMVar scribestate hClose
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item =
        --   when (checkItem s sevfilter item) $
              withMVar scribestate $ \handler ->
                  formatter handler colorize v item
    return $ Scribe logger finalizer

\end{code}

\begin{code}
formatItem :: Bool -> Verbosity -> Item a -> Builder
formatItem withColor _verb Item{..} =
    fromText header <>
    fromText " " <>
    brackets (fromText timestamp) <>
    fromText " " <>
    unLogStr _itemMessage
  where
    header = colorBySeverity _itemSeverity $
             "[" <> mconcat namedcontext <> ":" <> severity <> ":" <> threadid <> "]"
    namedcontext = intercalateNs _itemNamespace
    severity = renderSeverity _itemSeverity
    threadid = getThreadIdText _itemThread
    timestamp = T.pack $ formatTime defaultTimeLocale tsformat _itemTime
    tsformat :: String
    tsformat = "%F %T%2Q %Z"
    colorBySeverity s m = case s of
      EmergencyS -> red m
      AlertS     -> red m
      CriticalS  -> red m
      ErrorS     -> red m
      NoticeS    -> magenta m
      WarningS   -> yellow m
      InfoS      -> blue m
      _          -> m
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c m
      | withColor = "\ESC["<> c <> "m" <> m <> "\ESC[0m"
      | otherwise = m

-- translate Severity to Katip.Severity
sev2klog :: Data.Severity -> K.Severity
sev2klog = \case
    Data.Debug   -> K.DebugS
    Data.Info    -> K.InfoS
    Data.Notice  -> K.NoticeS
    Data.Warning -> K.WarningS
    Data.Error   -> K.ErrorS

\end{code}
