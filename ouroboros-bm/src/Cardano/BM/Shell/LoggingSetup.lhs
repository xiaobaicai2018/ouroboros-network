
\subsection{Cardano.BM.Shell.Logging}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Shell.LoggingSetup
    (
    , LoggerName
    , loggingCardanoFeature
    ) where

import           Cardano.BM.Configuration (Configuration, parse)
import           Cardano.BM.Data
import           Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace (logDebug, logError, logInfo, logNotice,
                     logWarning) as Trace
import           Cardano.Shell.Types (CardanoFeature (..), NoDependency)

\end{code}
%endif

\begin{code}

data LoggingFunctions = LoggingFunctions {
    logDebug, logInfo, logNotice, logWarning, logError :: forall m. (MonadIO m) => Trace m -> Text -> m ()
  , appendName :: MonadIO m => Text -> Trace m -> m (Trace m)
  , getTrace :: Trace m
  }

\end{code}

\begin{code}

loggingCardanoFeature :: CardanoFeature NoDependency Configuration LoggingFunctions
loggingCardanoFeature = CardanoFeature {
    featureType = LoggingMonitoringFeature
  , featureParseConfiguration = parse
  , featureStart = setupLogging
  }
  where
    setupLogging :: CardanoEnvironment -> NoDependency -> CardanoConfiguration -> Configuration -> IO LoggingFunctions
    setupLogging env _ topconf conf = do
        tr <- setupTrace conf
        return $ LoggingFunctions {
            logDebug = 

parseConfiguration :: 
\end{code}
