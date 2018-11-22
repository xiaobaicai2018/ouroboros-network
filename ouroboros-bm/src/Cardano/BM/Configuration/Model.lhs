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
    , getBackends
    , registerBackend
    --, inspectOutput
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)

import           Cardano.BM.Data (Configuration, Severity (..), Backend (..), ScribeKind (..))
import qualified Cardano.BM.Output.Katip as Cardano.BM.Output.Katip

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
getBackends name =
    withMVar configuration $ \cg -> do
        let outs = HM.lookup name (cgMapOutput cg)
        case outs of
          -- default? stdout
          Nothing -> do
              os <- defaultBackends
              return $ Just os
          Just os -> return $ Just os

defaultBackends :: IO [Backend]
defaultBackends = do
    -- read configuration?
    return [ Backend {pass' = Cardano.BM.Output.Katip.pass (pack (show StdoutSK  ))}
           , Backend {pass' = Cardano.BM.Output.Katip.pass (pack (show FileTextSK))}
           , Backend {pass' = Cardano.BM.Output.Katip.pass (pack (show FileJsonSK))}
           ]

registerBackend :: Text -> Maybe Backend -> IO ()
registerBackend kn f = pure () -- TODO
  --  registerBackend "some" (Just Backend { pass' = Katip.pass (show StdoutSK) })
  --  registerBackend "sever.error" (Just Backend { pass' = Katip.pass "StdoutSK::severe.log") })

\end{code}

\begin{code}
inspectSeverity :: Text -> IO (Maybe Severity)
inspectSeverity name =
    withMVar configuration $ \cg ->
        return $ HM.lookup name (cgMapSeverity cg)

\end{code}

\begin{code}
-- if Maybe Severity given is Nothing then the entry for this name is deleted.
setSeverity :: Text -> Maybe Severity -> IO ()
setSeverity _name _sev = do
    cg <- takeMVar configuration
    putMVar configuration $ cg { cgMapSeverity = HM.update (const _sev) _name (cgMapSeverity cg) }

\end{code}

\begin{code}
setup :: Configuration -> IO ()
setup _ = do
    _ <- takeMVar configuration
    -- TODO create thread which will periodically output
    --      aggregated values to the switchboard
    putMVar configuration $ ConfigurationInternal HM.empty HM.empty HM.empty

\end{code}
