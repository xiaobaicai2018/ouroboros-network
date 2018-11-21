{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | internal definitions for "Cardano.BM.Output.Log"

module Cardano.BM.Output.Internal
       ( newConfig
       , registerBackends
       , s2kname
       , sev2klog
    --    , updateConfig
    --    , getConfig
       , getLinesLogged
       , getLogEnv
       , getLogContext
       , incrementLinesLogged
       , modifyLinesLogged
       , LoggingHandler (..)
       , loggingHandler
       , logItem'
       , logItem''
       , ToObject (..)
    --    , FileDescription (..)
    --    , mkFileDescription
       ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent (myThreadId)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar,
                     withMVar)
import           Control.Lens ((^.))
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.STM (atomically)
import           Data.Aeson (Object, ToJSON (..), Value (..))
import           Data.Map.Strict (elems)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import qualified Language.Haskell.TH as TH
import           System.IO.Unsafe (unsafePerformIO)
-- import           Control.Exception.Safe (Exception (..))
-- import           Data.Version (showVersion)
-- import           System.FilePath (splitFileName, (</>))

import qualified Katip as K
import qualified Katip.Core as KC

import           Cardano.BM.Data (LogNamed (..), LogObject (..))
import           Cardano.BM.Data (LoggerName, Severity (..))
-- import           Cardano.BM.LoggerConfig (LoggerConfig (..))
-- import           Paths_cardano_sl_util (version)

-- | internal access to logging handler
{-# NOINLINE loggingHandler #-}
loggingHandler :: MVar LoggingHandler
loggingHandler = unsafePerformIO $ do
    newMVar $ error "LoggingHandler MVar is not initialized."

-- | translate Severity to @Katip.Severity@
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

-- | translate Name to @Katip.Namespace@
s2kname :: LoggerName -> K.Namespace
s2kname s = K.Namespace $ T.splitOn "." s

-- -- | log files have a prefix and a name
-- data FileDescription = FileDescription {
--                          prefixpath :: !FilePath,
--                          filename   :: !FilePath }
--                        deriving (Show)

-- mkFileDescription :: FilePath -> FilePath -> FileDescription
-- mkFileDescription bp fp =
--     -- if fp contains a filename in a directory path
--     --    move this path to the prefix and only keep the name
--     let (extbp, fname) = splitFileName fp
--     in
--     FileDescription { prefixpath = bp </> extbp
--                     , filename = fname }

-- | Our internal state
data LoggingHandlerInternal = LoggingHandlerInternal
    { --lsiConfig      :: !(Maybe LoggerConfig)
    {-,-} lsiLogEnv  :: !(Maybe K.LogEnv)
    , lsiLogContext  :: !(Maybe K.LogContexts)
    -- | Counter for the number of lines that are logged
    , lsiLinesLogged :: !Integer
    }

-- | internal data structure to be passed around
type LoggingMVar = MVar LoggingHandlerInternal
newtype LoggingHandler = LoggingHandler
    { getLSI :: LoggingMVar
    }

-- getConfig :: LoggingHandler -> IO (Maybe LoggerConfig)
-- getConfig lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiConfig

getLogEnv :: LoggingHandler -> IO (Maybe K.LogEnv)
getLogEnv lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLogEnv

getLogContext :: LoggingHandler -> IO (Maybe K.LogContexts)
getLogContext lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLogContext

getLinesLogged :: LoggingHandler -> IO Integer
getLinesLogged lh = withMVar (getLSI lh) $ \LoggingHandlerInternal{..} -> return lsiLinesLogged

-- | from tests, we want to count the number of lines logged
incrementLinesLogged :: LoggingHandler -> IO ()
incrementLinesLogged lh = modifyLinesLogged lh (+1)
modifyLinesLogged :: LoggingHandler -> (Integer -> Integer) -> IO ()
modifyLinesLogged lh f = do
    LoggingHandlerInternal {-cfg-} env ctx counter <- takeMVar (getLSI lh)
    putMVar (getLSI lh) $ LoggingHandlerInternal {-cfg-} env ctx $ f counter

-- updateConfig :: LoggingHandler -> LoggerConfig -> IO ()
-- updateConfig lh lc = modifyMVar_ (getLSI lh) $ \LoggingHandlerInternal{..} ->
--     return $ LoggingHandlerInternal (Just lc) lsiLogEnv lsiLogContext lsiLinesLogged

-- | create internal state given a configuration @LoggerConfig@
newConfig :: {- LoggerConfig -> -} IO LoggingHandler
newConfig {-lc-} = do
    mv <- newMVar $ LoggingHandlerInternal {-(Just lc)-} Nothing Nothing 0
    return $ LoggingHandler mv

-- | register scribes in `katip`
registerBackends :: T.Text -> LoggingHandler -> [(T.Text, K.Scribe)] -> IO ()
registerBackends cfoKey lh scribes = do
    LoggingHandlerInternal {-cfg-} _ ctx counter <- takeMVar (getLSI lh)
    le0 <- K.initLogEnv (s2kname "cardano-sl") $ fromString $ (T.unpack cfoKey) <> ":" <> "0.0.0.1" --showVersion version
    -- use 'getCurrentTime' to get a more precise timestamp
    -- as katip uses per default some internal buffered time variable
    timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
    let le1 = updateEnv le0 timer
    le <- register scribes le1
    putMVar (getLSI lh) $ LoggingHandlerInternal {-cfg-} (Just le) ctx counter
      where
        register :: [(T.Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
        register [] le = return le
        register ((n, s):scs) le =
            register scs =<< K.registerScribe n s scribeSettings le
        updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
        -- request a new time 'getCurrentTime' at most 100 times a second
        updateEnv le timer =
            le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }

scribeSettings :: KC.ScribeSettings
scribeSettings = KC.ScribeSettings bufferSize
  where
    bufferSize = 5000   -- size of the queue (in log items)

-- | Equivalent to katip's logItem without the `Katip m` constraint
logItem'
    :: (ToObject a, MonadIO m)
    => a
    -> KC.Namespace
    -> K.LogEnv
    -> Maybe TH.Loc
    -> KC.Severity
    -> KC.LogStr
    -> m ()
logItem' a ns env loc sev msg = do
    liftIO $ do
      item <- K.Item
        <$> pure (env ^. KC.logEnvApp)
        <*> pure (env ^. KC.logEnvEnv)
        <*> pure sev
        <*> (KC.mkThreadIdText <$> myThreadId)
        <*> pure (env ^. KC.logEnvHost)
        <*> pure (env ^. KC.logEnvPid)
        <*> pure a
        <*> pure msg
        <*> (env ^. KC.logEnvTimer)
        <*> pure ((env ^. KC.logEnvApp) <> ns)
        <*> pure loc
      forM_ (elems (env ^. KC.logEnvScribes)) $
          \ (KC.ScribeHandle _ shChan) -> atomically (KC.tryWriteTBQueue shChan (KC.NewItem item))

logItem''
    :: (ToObject a, MonadIO m)
    => K.LogEnv
    -> KC.Item a
    -> m ()
logItem'' env item =
    liftIO $
      forM_ (elems (env ^. KC.logEnvScribes)) $
          \ (KC.ScribeHandle _ shChan) -> atomically (KC.tryWriteTBQueue shChan (KC.NewItem item))

-- | Katip requires JSON objects to be logged as context. This
-- typeclass provides a default instance which uses ToJSON and
-- produces an empty object if 'toJSON' results in any type other than
-- object. If you have a type you want to log that produces an Array
-- or Number for example, you'll want to write an explicit instance
-- here. You can trivially add a ToObject instance for something with
-- a ToJSON instance like:
--
-- > instance ToObject Foo
class ToObject a where
    toObject :: a -> Object
    default toObject :: ToJSON a => a -> Object
    toObject v = case toJSON v of
        Object o -> o
        _        -> mempty

deriving instance ToObject (LogNamed LogObject)

instance ToObject () where
    toObject _ = mempty

instance {-# INCOHERENT #-} ToObject v => KC.ToObject v where
    toObject = toObject

instance {-# INCOHERENT #-} KC.ToObject a => KC.LogItem a where
    payloadKeys _ _ = KC.AllKeys
