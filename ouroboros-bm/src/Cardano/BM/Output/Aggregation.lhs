\subsection{Cardano.BM.Output.Aggregation}
\label{module:Cardano.BM.Output.Aggregation}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
    (
      setup
    , pass
    , inspect
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Data (LogObject (..), LogPrims (..), Configuration, NamedLogItem, LoggerName, LogNamed (..))
import           Cardano.BM.Aggregated (Aggregated (..), updateAggregation)

import           System.IO.Unsafe (unsafePerformIO)

\end{code}
%endif

The aggregation is a singleton.
\begin{code}
-- internal access to the aggregation
{-# NOINLINE aggregation #-}
aggregation :: MVar AggregationInternal
aggregation = unsafePerformIO $ do
    newMVar $ error "Aggregation MVar is not initialized."

-- Our internal state
data AggregationInternal = AggregationInternal
    { agMap   :: HM.HashMap Text Aggregated
    , agSome  :: [Int]  -- TODO
    }

\end{code}

\begin{code}
inspect :: Text -> IO (Maybe Aggregated)
inspect name =
    withMVar aggregation $ \ag ->
        return $ HM.lookup name (agMap ag)
\end{code}

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar aggregation
    -- TODO create thread which will periodically output
    --      aggregated values to the switchboard
    putMVar aggregation $ AggregationInternal HM.empty []

\end{code}

\begin{code}
pass :: NamedLogItem -> IO ()
pass item = do
    ag <- takeMVar aggregation
    putMVar aggregation $ AggregationInternal (updated $ agMap ag) (agSome ag)
  where
    updated agmap = pass' (lnItem item) (lnName item) agmap
    pass' :: LogObject -> LoggerName -> HM.HashMap Text Aggregated -> HM.HashMap Text Aggregated
    pass' (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
        in
        HM.alter (\m -> updateAggregation value m) name agmap
    -- TODO for text messages aggregate on delta of timestamps
    pass' _ _ agmap = agmap

\end{code}

