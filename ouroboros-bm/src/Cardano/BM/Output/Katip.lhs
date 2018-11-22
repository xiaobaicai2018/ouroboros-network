\subsection{Cardano.BM.Output.Katip}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.BM.Output.Katip
    (
      setup
    , pass
    --, takedown
    ) where

import           Cardano.BM.Data

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception (bracket_)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Katip.Core (Item (..), Scribe (..), Severity (..),
                     Verbosity (..), getThreadIdText, intercalateNs,
                     renderSeverity, unLogStr)
import           Katip.Scribes.Handle (brackets)

import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout)
import           System.IO.Unsafe (unsafePerformIO)

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
    { kInstance  :: [NamedLogItem]  -- TODO keep K.LogEnv
    , kLogEnv :: K.LogEnv  -- TODO
    }

\end{code}

Setup |katip| and its scribes according to the configuration
\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar katip
    -- TODO setup katip
    putMVar katip $ KatipInternal [] []

    -- TODO register scribe with type and name
    --      registerScribe ((show kind) ++ "::" ++ name) scribe sets le
\end{code}

\begin{code}
pass :: Text -> NamedLogItem -> IO ()
pass backend item = do
    k <- takeMVar katip
    putMVar katip $ KatipInternal (kInstance k <> [item]) (kSomething k)
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
            bracket_ (takeMVar locklocal) (putMVar locklocal ()) $
                TIO.hPutStrLn h $! toLazyText $ formatItem colorize verbosity item
    pure $ Scribe logger (hClose h)

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

