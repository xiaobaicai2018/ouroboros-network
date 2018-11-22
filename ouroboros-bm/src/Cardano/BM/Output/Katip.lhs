\subsection{Cardano.BM.Output.Katip}

%if False
\begin{code}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Output.Katip
    (
      setup
    , pass
    --, takedown
    ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, modifyMVar_)
import           Control.Exception (bracket_)
import           Control.Exception.Safe (catchIO)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as T.Lazy
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Version (Version (..), showVersion)
import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout)
import           System.Directory (createDirectoryIfMissing)
import           System.IO.Unsafe (unsafePerformIO)

import           Katip.Core (Item (..), Scribe (..), Severity (..),
                     Verbosity (..), getThreadIdText, intercalateNs,
                     renderSeverity, unLogStr, LogItem (..), ScribeSettings (..))
import           Katip.Scribes.Handle (brackets)
import qualified Katip as K

import qualified Cardano.BM.Data as Data
import qualified Cardano.BM.Output.Internal as Internal
import           Cardano.BM.Output.Rotator (RotationParameters (..), cleanupRotator, evalRotator, initializeRotator)

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
                (K.Namespace ["cardano-sl"])
                (fromString $ (T.unpack cfoKey) <> ":" <> showVersion mockVersion)
    timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
    let le1 = updateEnv le0 timer
    stdoutScribe <- mkStdoutScribe K.V0
    le <- register [("stdout", stdoutScribe)] le1
    _ <- takeMVar katip
    -- putMVar (getLSI lh) $ LoggingHandlerInternal cfg (Just le) ctx counter
    putMVar katip $ KatipInternal [] le
  where
    register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
    register [] le = return le
    register ((name, scribe) : scs) le =
        register scs =<< K.registerScribe name scribe scribeSettings le
        -- TODO register scribe with type and name
        -- registerScribe ((show kind) ++ "::" ++ name) scribe sets le
    updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
    -- request a new time 'getCurrentTime' at most 100 times a second
    updateEnv le timer =
        le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }
    mockVersion :: Version
    mockVersion = Version [0,1,0,0] []
    scribeSettings :: ScribeSettings
    scribeSettings = ScribeSettings bufferSize
      where
        bufferSize = 5000   -- size of the queue (in log items)

\end{code}

\begin{code}
pass :: T.Text -> Data.NamedLogItem -> IO ()
pass backend item = pure () --do
    -- k <- takeMVar katip
    -- putMVar katip $ KatipInternal (kInstance k <> [item]) (kSomething k)
\end{code}

\begin{spec}
    -- TODO go through list of registered scribes
    --      and put into queue of scribe if backend kind matches
    --      compare start of name of scribe to (show backend <> "::")
    forM_ ((kLogEnv k) ^. KC.logEnvScribes) $
          \(scName, (KC.ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
              if scName `startsWith` backend
              then atomically (KC.tryWriteTBQueue shChan (KC.NewItem item))
              else ()
    putMVar katip k

\end{spec}

\subsubsection{Scribes}
\begin{code}
mkStdoutScribe :: Verbosity -> IO Scribe
mkStdoutScribe = mkFileScribeH stdout True

mkStderrScribe :: Verbosity -> IO Scribe
mkStderrScribe = mkFileScribeH stderr True

mkFileScribeH :: Handle -> Bool -> Verbosity -> IO Scribe
mkFileScribeH h colorize verbosity = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: Item a -> IO ()
        logger item = -- when (checkItem sev sevMap item) $ --TODO probably checkItem is redundant
            bracket_ (takeMVar locklocal) (putMVar locklocal ()) $ pure ()
                -- case _itemPayload item of
                --     lognamed@(Data.LogNamed _ _) ->
                --         case Data.lnItem lognamed of
                --             Data.LP (Data.LogMessage logItem) ->
                --                 output (Data.lnName lognamed) $ Data.liPayload logItem
                --             obj ->
                --                 output (Data.lnName lognamed) $ T.Lazy.toStrict (encodeToLazyText obj)
                --     _ -> pure ()

    pure $ Scribe logger (hClose h)
  where
    output nm msg = TIO.putStrLn $ nm <> " :: " <> msg
                 -- TIO.hPutStrLn h $! toLazyText $ formatItem colorize verbosity item

-- | create a katip scribe for logging to a file in textual representation
mkTextFileScribe :: RotationParameters -> Internal.FileDescription -> Bool -> Data.Severity -> Verbosity -> IO Scribe
mkTextFileScribe rot fdesc colorize s v = do
    mkFileScribe rot fdesc formatter colorize s v
  where
    formatter :: Handle -> Bool -> Verbosity -> Item a -> IO Int
    formatter hdl colorize' v' item = do
        -- case _item payload
        let tmsg = toLazyText $ formatItem colorize' v' item
        TIO.hPutStrLn hdl tmsg
        return $ fromIntegral $ T.Lazy.length tmsg

-- | create a katip scribe for logging to a file
--   and handle file rotation within the katip-invoked logging function
mkFileScribe
    :: RotationParameters
    -> Internal.FileDescription
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int)  -- format and output function, returns written bytes
    -> Bool  -- whether the output is colourized
    -> Data.Severity
    -> Verbosity
    -> IO Scribe
mkFileScribe rot fdesc formatter colorize s v = do
    let prefixDir = Internal.prefixPath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (Internal.prtoutException ("cannot log prefix directory: " ++ prefixDir))
    trp <- initializeRotator rot fdesc
    scribestate <- newMVar trp    -- triple of (handle), (bytes remaining), (rotate time)
    -- sporadically remove old log files - every 10 seconds
    cleanup <- mkAutoUpdate defaultUpdateSettings { updateAction = cleanupRotator rot fdesc, updateFreq = 10000000 }
    let finalizer :: IO ()
        finalizer = do
            modifyMVar_ scribestate $ \(hdl, b, t) -> do
                hClose hdl
                return (hdl, b, t)
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item =
        --   when (checkItem s sevfilter item) $
              modifyMVar_ scribestate $ \(hdl, bytes, rottime) -> do
                  byteswritten <- formatter hdl colorize v item
                  -- remove old files
                  cleanup
                  -- detect log file rotation
                  let bytes' = bytes - (toInteger $ byteswritten)
                  let tdiff' = round $ diffUTCTime rottime (_itemTime item)
                  if bytes' < 0 || tdiff' < (0 :: Integer)
                     then do   -- log file rotation
                        hClose hdl
                        (hdl2, bytes2, rottime2) <- evalRotator rot fdesc
                        return (hdl2, bytes2, rottime2)
                     else
                        return (hdl, bytes', rottime)

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

\end{code}
