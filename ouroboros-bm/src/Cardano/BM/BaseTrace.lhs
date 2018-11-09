
\subsection{BaseTrace}

%if False
\begin{code}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.BaseTrace
    (
      BaseTrace (..)
    , natTrace
    , noTrace
    , traceWith
    ) where

import           Data.Functor.Contravariant (Contravariant (..), Op (..))
\end{code}
%endif

\subsubsection{Contravariant}

A covariant is a functor: F A $\to$ F B \\
A contravariant is a functor: F B $\to$ F A \\

|Op| implements "|getOp :: b -> a|", which when applied to a |BaseTrace|, yields "|s -> m ()|".
\begin{code}

newtype BaseTrace m s = BaseTrace { runTrace :: Op (m ()) s }

\end{code}

\subsubsection{contramap}
A covariant functor defines the function "|map :: (a -> b) -> a -> b|".
In case of a contravariant functor, it is the function "|contramap :: (a -> b) -> b -> a|" which is defined.

In the following instance, runTrace extracts type "|Op (m ()) s|" to which contramap applies |f|, 
thus "|f -> m()|". The constructor |BaseTrace| restores "|Op (m ()) f|". 
\begin{code}

instance Contravariant (BaseTrace m) where
    contramap f = BaseTrace . contramap f . runTrace

\end{code}

\subsubsection{traceWith}
Accepts a |Trace| and some payload |s|. First it gets the contravariant from the |Trace|
as type "|Op (m ()) s|" and runs the payload through "|getOp :: b -> a|" which translates to:
"|s -> m ()|".

\begin{code}

traceWith :: BaseTrace m s -> s -> m ()
traceWith = getOp . runTrace

\end{code}

\subsubsection{natTrace}
Natural transformation from monad |m| to monad |n|.
\begin{code}

natTrace :: (forall x . m x -> n x) -> BaseTrace m s -> BaseTrace n s
natTrace nat (BaseTrace (Op tr)) = BaseTrace $ Op $ nat . tr

\end{code}

\subsubsection{noTrace}
Ignores all inputs to the Trace. The function is "|\_ -> m ()|"" as argument to |Op| which
results in the type "|Op (m ()) _|" in |BaseTrace|.
\begin{code}

noTrace :: Applicative m => BaseTrace m a
noTrace = BaseTrace $ Op $ const (pure ())

\end{code}
