{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Logging implemented with library `katip`

module Cardano.BM.Log
       (
        -- * Logging
         LogContext
    --    , LoggingHandler
       -- * Configuration
    --    , LoggerConfig (..)
       -- * Startup
    --    , setupLogging
       -- * other functions
    --    , closeLogScribes
       , logItem'
       -- * class for structured logging
       , ToObject (..)
       ) where

import           Control.Concurrent (myThreadId)
import           Control.Lens ((^.))
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Control.Lens (each)
import           Control.Monad.STM (atomically)
import           Data.Aeson (Object, ToJSON (..), Value (..))
import           Data.Map.Strict (elems)
-- import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import           Cardano.BM.Data (LogNamed (..), LogObject (..))
import           Cardano.BM.Internal (LoggingHandler)

import qualified Katip as K
import qualified Katip.Core as KC

-- | alias - pretend not to depend on katip
type LogContext = K.KatipContext
type LogContextT = K.KatipContextT

-- -- | log a Text with severity
-- logMessage :: (LogContext m) => Severity -> Text -> m ()
-- logMessage sev msg = logMessage' (Internal.sev2klog sev) $ K.logStr msg
-- logMessage' :: (LogContext m) => K.Severity -> K.LogStr -> m ()
-- logMessage' s m = K.logItemM Nothing s m

-- | setup logging according to configuration @LoggerConfig@
--   the backends (scribes) will be registered with katip
-- setupLogging :: MonadIO m => Text -> LoggerConfig -> m LoggingHandler
-- setupLogging cfoKey lc = do
--     lh <- liftIO $ Internal.newConfig lc
--     scribes <- liftIO $ meta lh lc
--     liftIO $ Internal.registerBackends cfoKey lh scribes
--     return lh
--       where
--         -- returns a list of: (name, Scribe, finalizer)
--         meta :: LoggingHandler -> LoggerConfig -> IO [(T.Text, K.Scribe)]
--         meta _lh _lc = do
--             -- setup scribes according to configuration
--             let lhs = _lc ^. lcLoggerTree ^. ltHandlers ^.. each
--                 basepath = _lc ^. lcBasePath
--                 sevfilter = _lc ^. lcLoggerTree ^. ltNamedSeverity
--                 -- default rotation parameters: max. 24 hours, max. 10 files kept, max. size 5 MB
--                 rotation = fromMaybe (RotationParameters { _rpMaxAgeHours=24,
--                                                            _rpKeepFilesNum=10,
--                                                            _rpLogLimitBytes=5*1000*1000 })
--                                      (_lc ^. lcRotation)
--             forM lhs (\lh -> case (lh ^. lhBackend) of
--                     FileJsonBE -> do
--                         let bp = fromMaybe "./" basepath
--                             fp = fromMaybe "node.json" $ lh ^. lhFpath
--                             fdesc = Internal.mkFileDescription bp fp
--                             nm = lh ^. lhName
--                         scribe <- mkJsonFileScribe
--                                       rotation
--                                       sevfilter
--                                       fdesc
--                                       (fromMaybe Debug $ lh ^. lhMinSeverity)
--                                       K.V3
--                         return (nm, scribe)
--                     FileTextBE -> do
--                         let bp = fromMaybe "./" basepath
--                             fp = fromMaybe "node.log" $ lh ^. lhFpath
--                             fdesc = Internal.mkFileDescription bp fp
--                             nm = lh ^. lhName
--                         scribe <- mkTextFileScribe
--                                       rotation
--                                       sevfilter
--                                       fdesc
--                                       True
--                                       (fromMaybe Debug $ lh ^. lhMinSeverity)
--                                       K.V0
--                         return (nm, scribe)
--                     StdoutBE -> do
--                         scribe <- mkStdoutScribe
--                                       sevfilter
--                                       (fromMaybe Debug $ lh ^. lhMinSeverity)
--                                       K.V0
--                         return (lh ^. lhName, scribe)
--                     StderrBE -> do
--                         scribe <- mkStderrScribe
--                                       sevfilter
--                                       (fromMaybe Debug $ lh ^. lhMinSeverity)
--                                       K.V0
--                         return (lh ^. lhName, scribe)
--                     DevNullBE -> do
--                         scribe <- mkDevNullScribe _lh
--                                       sevfilter
--                                       (fromMaybe Debug $ lh ^. lhMinSeverity)
--                                       K.V0
--                         return (lh ^. lhName, scribe)
--                  )

-- closeLogScribes :: MonadIO m => LoggingHandler -> m ()
-- closeLogScribes lh = do
--     mayle <- liftIO $ Internal.getLogEnv lh
--     case mayle of
--             Nothing -> error "logging not yet initialized. Abort."
--             Just le -> void $ liftIO $ K.closeScribes le

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
