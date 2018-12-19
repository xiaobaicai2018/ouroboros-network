module Ouroboros.Util.STM
    (
      bracketObserve
    ) where

import           Data.Text

import           Ouroboros.Network.MonadClass

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, subTrace, typeofTrace)

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
        countersid <- liftIOaction $ observeOpen subtrace logTrace
        -- run action, returns result only
        t <- atomically act
        liftIOaction $ observeClose subtrace logTrace countersid []
        pure t
