\subsection{Cardano.BM.Configuration.Model}
\label{module:Cardano.BM.Configuration.Model}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Configuration.Model
    (
      setup
    , inspectSeverity
    , setSeverity
    , getBackend
    , registerBackend
    --, inspectOutput
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Data

import           System.IO.Unsafe (unsafePerformIO)

\end{code}
%endif

The configuration is a singleton.
\begin{code}
-- internal access to the configuration
{-# NOINLINE configuration #-}
configuration :: MVar ConfigurationInternal
configuration = unsafePerformIO $ do
    newMVar $ error "Configuration MVar is not initialized."

-- Our internal state
data ConfigurationInternal = ConfigurationInternal
    { cgMapSeverity :: HM.HashMap Text Severity
    , cgMapOutput   :: HM.HashMap Text [Backend]
    , cgOptions     :: HM.HashMap Text Aeson.Object
    }
--    options:  config.logrotation = { maxFiles = 10; maxSize = 5000000 }
--              config.logprefix = { path = "/mnt/disk/spacy" }

getBackends :: Text -> IO (Maybe [Backend])
    withMVar configuration $ \cg ->
        outs <- HM.lookup name (cgMapOutput cg)
        return $ case outs of
          -- default? stdout
          Nothing -> do
              os <- defaultBackends
              Just os
          Just os -> Just os

defaultBackends :: IO [Backend]
defaultBackends = do
    -- read configuration?
    return [ Backend {pass = Cardano.BM.Output.Katip.pass (show StdoutSK)},
           , Backend {pass = Cardano.BM.Output.Katip.pass (show FileTextSK)},
           , Backend {pass = Cardano.BM.Output.Katip.pass (show FileJsonSK)},
           ]

registerBackend :: Text -> Maybe Backend -> IO ()
registerBackend kn f = pure ()
  --  registerBackend "some" (Just Backend { pass = Katip.pass (show StdoutSK) })
  --  registerBackend "sever.error" (Just Backend { pass = Katip.pass "StdoutSK::severe.log") })

\end{code}

\begin{code}
inspectSeverity :: Text -> IO (Maybe Severity)
inspectSeverity name =
    withMVar configuration $ \cg ->
        return $ HM.lookup name (cgMapSeverity cg)

\end{code}

\begin{code}
setSeverity :: Text -> Maybe Severity -> IO ()
setSeverity _name _sev = do
    cg <- takeMVar configuration
    putMVar configuration $ 

\end{code}

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar configuration
    -- TODO create thread which will periodically output
    --      aggregated values to the switchboard
    putMVar configuration $ ConfigurationInternal HM.empty HM.empty HM.empty

\end{code}

