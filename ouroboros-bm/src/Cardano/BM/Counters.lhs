
\subsection{Cardano.BM.Counters}

Here the platform is chosen on which we compile this program.

Currently, we only support |Linux| with its 'proc' filesystem.


\begin{code}
{-# LANGUAGE CPP #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.BM.Counters
    (
      Platform.readCounters
    ) where

#ifdef LINUX
import qualified Cardano.BM.Counters.Linux as Platform
#else
import qualified Cardano.BM.Counters.Dummy as Platform
#endif
\end{code}

