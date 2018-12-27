{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Benchmark.STM
    (
      bracketObserve
    ) where

import           Data.Text

import           Ouroboros.Network.MonadClass

import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, logNotice, subTrace, typeofTrace)

bracketObserve :: (MonadSTM n, MonadSay n) => Trace IO -> Text -> Tr n t -> n t
bracketObserve logTrace0 name action = do
    logTrace <- liftIOaction $ subTrace name logTrace0
    let subtrace = typeofTrace logTrace
    bracketObserve' subtrace logTrace action
  where
    bracketObserve' :: (MonadSTM n, MonadSay n) => SubTrace -> Trace IO -> Tr n t -> n t
    bracketObserve' NoTrace _ act =
        atomically act
    bracketObserve' subtrace logTrace act = do
        mCountersid <- liftIOaction $ observeOpen subtrace logTrace
        -- run action; if an exception is caught will be logged and rethrown.
        t <- atomically act

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                 liftIOaction $ logNotice logTrace ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <-  liftIOaction $ observeClose subtrace logTrace countersid []
                    case res of
                        Left ex -> liftIOaction $ logNotice logTrace ("ObserveClose: " <> pack (show ex))
                        _ -> pure ()
        pure t
