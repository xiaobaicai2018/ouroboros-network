\subsection{Cardano.BM.ToObject}

%if False
\begin{code}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.BM.ToObject
       ( ToObject (..)
       ) where

import           Data.Aeson (Object, ToJSON (..), Value (..))

\end{code}
%endif

\begin{code}
-- copied from katip
class ToObject a where
    toObject :: a -> Object
    default toObject :: ToJSON a => a -> Object
    toObject v = case toJSON v of
      Object o -> o
      _        -> mempty

instance ToObject ()
instance ToObject Object

\end{code}
