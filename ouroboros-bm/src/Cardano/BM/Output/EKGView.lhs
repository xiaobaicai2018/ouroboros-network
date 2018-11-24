
\subsection{Cardano.BM.Output.EKGView}
\label{module:Cardano.BM.Output.EKGView}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.EKGView
    (
      setup
    , pass
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data (LogNamed (..), LogObject (..), LogPrims (..),
                     LoggerName, NamedLogItem)
\end{code}
%endif

The ekgview is a singleton.
\begin{code}
-- internal access to the ekgview
{-# NOINLINE ekgview #-}
ekgview :: MVar EKGViewInternal
ekgview = unsafePerformIO $ do
    newMVar $ error "EKGView MVar is not initialized."

-- Our internal state
data EKGHandle
data EKGViewInternal = EKGViewInternal
    { evGauges  :: HM.HashMap Text EKGHandle
    , evLabels  :: HM.HashMap Text EKGHandle
    }

\end{code}

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar ekgview
    -- TODO create EKG server
    putMVar ekgview $ EKGViewInternal HM.empty HM.empty

\end{code}

\begin{code}
pass :: NamedLogItem -> IO ()
pass item = do
    withMVar ekgview $ \ekg ->
        pass' (lnItem item) (lnName item) ekg
  where
    pass' :: LogObject -> LoggerName -> EKGViewInternal -> IO ()
    pass' (LP (LogMessage value)) logname (EKGViewInternal _ labels) =
        pure ()
    pass' (LP (LogValue iname value)) logname (EKGViewInternal gauges _) =
        let name = logname <> "." <> iname
        in
        case HM.lookup name gauges of
            Nothing -> do
                -- TODO create gauge in EKG
                -- TODO keep reference to gauge in map
                -- TODO send value to gauge in EKG
                pure ()
            Just ekghdl -> do
                -- TODO send value to gauge in EKG
                pure ()

    pass' _ _ _ = pure ()

\end{code}

