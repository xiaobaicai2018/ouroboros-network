\subsection{Cardano.BM.Output.Internal}

%if False
\begin{code}

module Cardano.BM.Output.Internal
       ( FileDescription (..)
       , prefixPath
       , prtoutException
       ) where

import           Control.Exception (Exception (..))
import           System.FilePath (takeDirectory)

data FileDescription = FileDescription {
                         filePath   :: !FilePath }
                       deriving (Show)

prefixPath :: FileDescription -> FilePath
prefixPath = takeDirectory . filePath

-- mkFileDescription :: FilePath -> FilePath -> FileDescription
-- mkFileDescription bp fp = FileDescription (bp </> fp)

-- | display message and stack trace of exception on stdout
prtoutException :: Exception e => String -> e -> IO ()
prtoutException msg e = do
    putStrLn msg
    putStrLn ("exception: " ++ displayException e)

\end{code}
