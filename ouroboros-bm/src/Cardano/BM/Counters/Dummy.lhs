
\subsection{Cardano.BM.Counters.Dummy}

This is a dummy definition of |readCounters| on platforms that do not support the
'proc' filesystem from which we would read the counters.

\todo[inline]{we could well imagine that some day we support all platforms}

\begin{code}
module Cardano.BM.Counters.Dummy
    (
      readCounters
    ) where

import           Cardano.BM.Data (Counter, TraceTransformer (..))

readCounters :: TraceTransformer -> IO [Counter]
readCounters NoTrace             = return []
readCounters Neutral             = return []
readCounters UntimedTrace        = return []
readCounters DropOpening         = return []
readCounters (ObservableTrace _) = return []
\end{code}

