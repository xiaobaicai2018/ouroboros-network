\subsection{Cardano.BM.Output.Switchboard}\label{sec:Switchboard}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Output.Switchboard
    (
      Switchboard
    , setup
    , pass
    , takedown
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar, withMVar)
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (forM_)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data (Backend (..), HasPass (..), NamedLogItem)
import qualified Cardano.BM.Output.Aggregation
import qualified Cardano.BM.Output.EKGView
import qualified Cardano.BM.Output.Log

\end{code}
%endif

\subsubsection{State representation}\label{code:Switchboard}
The switchboard is a singleton.

\begin{code}
type SwitchboardMVar = MVar SwitchboardInternal
newtype Switchboard = Switchboard
    { getSB :: SwitchboardMVar }

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
setup :: Configuration -> IO Switchboard
setup cfg = do
    ekg <- Cardano.BM.Output.EKGView.setup cfg
    agg <- Cardano.BM.Output.Aggregation.setup cfg
    log <- Cardano.BM.Output.Log.setup cfg
    -- TODO connect backends according to configuration
    let bs = [ MkBackend {pass' = Cardano.BM.Output.Log.passN "StdoutSK" log}
             , MkBackend {pass' = Cardano.BM.Output.EKGView.pass ekg}
             , MkBackend {pass' = Cardano.BM.Output.Aggregation.pass agg} ]

    sbref <- newEmptyMVar
    q <- atomically $ TBQ.newTBQueue 2048
    d <- spawnDispatcher sbref q
    putMVar sbref $ SwitchboardInternal q d bs
    return $ Switchboard sbref
  where
    spawnDispatcher :: SwitchboardMVar -> TBQ.TBQueue (Maybe NamedLogItem) -> IO (Async.Async ())
    spawnDispatcher switchboard queue = Async.async qProc
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
instance HasPass Switchboard where
    pass switchboard item = do
        let writequeue :: TBQ.TBQueue (Maybe NamedLogItem) -> NamedLogItem -> STM ()
            writequeue q i = do
                nocapacity <- TBQ.isFullTBQueue q
                if not nocapacity
                then TBQ.writeTBQueue q (Just i)
                else return ()
        putStrLn $ "Cardano.BM.Output.Switchboard.pass " ++ (show item)
        withMVar (getSB switchboard) $ \sb ->
            atomically $ writequeue (sbQueue sb) item

\end{code}

\subsubsection{Halting the switchboard}
The queue is flushed before the dispatcher terminates.

\begin{code}
takedown :: Switchboard -> IO ()
takedown switchboard = do
    (q, d) <- withMVar (getSB switchboard) $ \sb ->
                   return (sbQueue sb, sbDispatch sb)
    -- send terminating item to the queue
    atomically $ TBQ.writeTBQueue q Nothing
    -- wait for the dispatcher to exit
    _ <- Async.waitCatch d
    return ()

\end{code}
