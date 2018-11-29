
\subsection{Cardano.BM.Output.EKGView}
\label{module:Cardano.BM.Output.EKGView}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Output.EKGView
    (
      EKGView
    , setup
    , pass
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar, withMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data (HasPass (..), LogNamed (..), LogObject (..),
                     LogPrims (..), LoggerName, NamedLogItem)
\end{code}
%endif

The ekgview is a singleton.
\begin{code}
type EKGViewMVar = MVar EKGViewInternal
newtype EKGView = EKGView
    { getEV :: EKGViewMVar }

-- Our internal state
data EKGHandle
data EKGViewInternal = EKGViewInternal
    { evGauges  :: HM.HashMap Text EKGHandle
    , evLabels  :: HM.HashMap Text EKGHandle
    }

\end{code}

\begin{code}
setup :: Configuration -> IO EKGView
setup _ = do
    evref <- newEmptyMVar
    -- TODO create EKG server
    putMVar evref $ EKGViewInternal HM.empty HM.empty
    return $ EKGView evref

\end{code}

\begin{code}
instance HasPass EKGView where
    pass ekgview item =
        let pass' :: LogObject -> LoggerName -> EKGViewInternal -> IO ()
            pass' (LP (LogMessage value)) logname (EKGViewInternal _ labels) =
                pure ()
            pass' (LP (LogValue iname value)) logname (EKGViewInternal gauges _) =
                let name = logname <> "." <> iname
                in
                case HM.lookup name gauges of
                    Nothing -> --do
                        -- TODO create gauge in EKG
                        -- TODO keep reference to gauge in map
                        -- TODO send value to gauge in EKG
                        pure ()
                    Just ekghdl -> --do
                        -- TODO send value to gauge in EKG
                        pure ()

            pass' _ _ _ = pure ()
        in
        withMVar (getEV ekgview) $ \ekg ->
            pass' (lnItem item) (lnName item) ekg

\end{code}

