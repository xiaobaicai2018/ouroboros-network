\subsection{Cardano.BM.Output.Switchboard}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Switchboard
    (
      setup
    , pass
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
--import           Control.Concurrent.STM (atomically)
--import qualified Control.Concurrent.STM.TBQueue as TBQ

import           Cardano.BM.Data

import           System.IO.Unsafe (unsafePerformIO)

\end{code}
%endif

The switchboard is a singleton.
\begin{code}
-- | internal access to the switchboard
{-# NOINLINE switchboard #-}
switchboard :: MVar SwitchboardInternal
switchboard = unsafePerformIO $ do
    newMVar $ error "Switchboard MVar is not initialized."

-- | Our internal state
data SwitchboardInternal = SwitchboardInternal
    { sbQueue    :: [NamedLogItem]  -- TODO
    , sbBackends :: [Int]  -- TODO
    }

\end{code}

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar switchboard
    -- TODO prepare new queue
    --q <- atomically $ TBQ.newTBQueue 204dd8
    -- TODO start queue process
    putMVar switchboard $ SwitchboardInternal [] []
  where
    --spawnProc :: IO (Async.Async ())
    --spawnProc = Async.async qProc
    --qProc = do
    --    nli <- atomically $ TBQ.readTBQueue q
    --    forM_ 

\end{code}

\begin{code}
pass :: NamedLogItem -> IO ()
pass item = do
    sb <- takeMVar switchboard
    putMVar switchboard $ SwitchboardInternal (sbQueue sb <> [item]) (sbBackends sb)

\end{code}

