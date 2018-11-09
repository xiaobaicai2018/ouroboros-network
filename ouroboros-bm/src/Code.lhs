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

% \begin{code}
% module Code
%   (
%   ) where
% 
% import qualified Cardano.BM.Aggregation
% import qualified Cardano.BM.STM
% import qualified Cardano.BM.Trace
% 
% \end{code}

%include Cardano//BM//Aggregation.lhs

%include Cardano//BM//STM.lhs

%include Cardano//BM//BaseTrace.lhs

%include Cardano//BM//Trace.lhs

\end{document}
