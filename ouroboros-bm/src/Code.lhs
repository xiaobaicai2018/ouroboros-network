\documentclass[11pt,twoside]{report}
\pagestyle{headings}

\usepackage{kpfonts}
\usepackage[margin=1in]{geometry}
\usepackage[pdfpagelabels]{hyperref}
\usepackage{todonotes}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{colortbl}
\usepackage{hyperref}

%include polycode.fmt

\title{Cardano.BM - benchmarking and logging}
\author{Alexander Diemand
  \and
        Andreas Triantafyllos}
\date{November 2018}

\begin{document}

\hypersetup{pageanchor=false}
\begin{titlepage}
\maketitle
\end{titlepage}

\begin{abstract}
abstract ...
\end{abstract}

\newpage

\hypersetup{pageanchor=true}
\pagenumbering{arabic}

\tableofcontents

\newpage

\chapter{Cardano BM}

\section{Introduction}

introduction ...

\section{Examples}

examples ...

\section{Code listings}

%if False
\begin{code}
module Code
where

import qualified Cardano.BM.Aggregation
import qualified Cardano.BM.BaseTrace
import qualified Cardano.BM.Controller
import qualified Cardano.BM.Counters
import qualified Cardano.BM.Data
import qualified Cardano.BM.STM
import qualified Cardano.BM.Trace
 
\end{code}
%endif

%include Cardano//BM//Aggregation.lhs

%include Cardano//BM//STM.lhs

%include Cardano//BM//BaseTrace.lhs

%include Cardano//BM//Trace.lhs

%include Cardano//BM//Controller.lhs

%include Cardano//BM//Counters.lhs

%include Cardano//BM//Data.lhs

\end{document}
