\subsection{Cardano.BM.Output.Switchboard}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Switchboard
    (
      setup
    , pass
    , takedown
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (forM_)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data (Backend (..), NamedLogItem)
import qualified Cardano.BM.Output.Katip as Katip

import           System.IO.Unsafe (unsafePerformIO)

\end{code}
%endif

\subsubsection{State representation}
The switchboard is a singleton.

\begin{code}
-- internal access to the switchboard
{-# NOINLINE switchboard #-}
switchboard :: MVar SwitchboardInternal
switchboard = unsafePerformIO $ do
    newMVar $ error "Switchboard MVar is not initialized."

-- Our internal state
data SwitchboardInternal = SwitchboardInternal
    { sbQueue    :: TBQ.TBQueue (Maybe NamedLogItem)
    , sbDispatch :: Async.Async ()
    , sbBackends :: [Backend]
    }

\end{code}

\subsubsection{Starting the switchboard from configuration}
The queue is initialized and the message dispatcher launched.
TODO: the backends should be connected according to configuration.

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar switchboard
    q <- atomically $ TBQ.newTBQueue 2048
    d <- spawnDispatcher q
    -- TODO connect backends according to configuration
    let be = [ Backend {pass'=Katip.pass "StdoutSK"} ]
    putMVar switchboard $ SwitchboardInternal q d be

spawnDispatcher :: TBQ.TBQueue (Maybe NamedLogItem) -> IO (Async.Async ())
spawnDispatcher queue = Async.async qProc
  where
    qProc = do
        nli' <- atomically $ TBQ.readTBQueue queue
        case nli' of
            Just nli -> do
                putStrLn $ "dispatcher read: " ++ (show nli)
                withMVar switchboard $ \sb ->
                    forM_ (sbBackends sb) (dispatch nli)
                qProc
            Nothing -> return ()   -- end dispatcher
    dispatch :: NamedLogItem -> Backend -> IO ()
    dispatch nli backend = (pass' backend) nli

\end{code}

\subsubsection{Process incoming messages}
Incoming messages are put into the queue, and
then processed by the dispatcher.

\begin{code}
pass :: NamedLogItem -> IO ()
pass item = do
    putStrLn $ "Cardano.BM.Output.Switchboard.pass " ++ (show item)
    withMVar switchboard $ \sb ->
        atomically $ writequeue (sbQueue sb) item
  where
    writequeue :: TBQ.TBQueue (Maybe NamedLogItem) -> NamedLogItem -> STM ()
    writequeue q i = do
        nocapacity <- TBQ.isFullTBQueue q
        if not nocapacity
          then TBQ.writeTBQueue q (Just i)
          else return ()

\end{code}

\subsubsection{Halting the switchboard}
The queue is flushed before the dispatcher terminates.

\begin{code}
takedown :: IO ()
takedown = do
    (q, d) <- withMVar switchboard $ \sb ->
                   return (sbQueue sb, sbDispatch sb)
    -- send terminating item to the queue
    atomically $ TBQ.writeTBQueue q Nothing
    -- wait for the dispatcher to exit
    _ <- Async.waitCatch d
    return ()

\end{code}
