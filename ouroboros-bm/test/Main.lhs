\documentclass[11pt,twoside]{report}
\pagestyle{headings}

\usepackage{kpfonts}
\usepackage[margin=1in]{geometry}
\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{colortbl}
\usepackage{hyperref}

%include polycode.fmt

\begin{document}
\title{Testing benchmarking and logging}
\author{Alexander Diemand}
\date{November 2018}
\maketitle

\tableofcontents

\begin{abstract}
abstract ...
\end{abstract}

\chapter{Testing}

\section{Test main entry point}

\begin{code}
module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Aggregation (tests)
import qualified Cardano.BM.Test.STM (tests)
import qualified Cardano.BM.Test.Trace (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-bm"
  [ Cardano.BM.Test.Aggregation.tests
  , Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  ]
\end{code}

%include Cardano//BM//Arbitrary//Aggregation.lhs

%include Cardano//BM//Test//Aggregation.lhs
%include Cardano//BM//Test//STM.lhs
%include Cardano//BM//Test//Trace.lhs

\end{document}
