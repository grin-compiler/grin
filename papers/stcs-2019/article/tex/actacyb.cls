%% actacyb.cls -- Style file for Acta Cybernetica articles
%%
%% This file may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License, either version 1.2
%% of this license or (at your option) any later version.
%% The latest version of this license is in
%%    http://www.latex-project.org/lppl.txt
%% and version 1.2 or later is part of all distributions of LaTeX
%% version 1999/12/01 or later.
%%
% TODO
% * transform this to a dtx to allow automatic documentation with docstrip

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{actacyb}[2008/05/15 v1.21 Acta Cybernetica article class]
\RequirePackage{amsthm,amssymb,amsmath,lineno,vruler,ifthen}
\LoadClass[10pt,twoside,a4paper]{article}
\newcounter{actasectionstyle}
\setcounter{actasectionstyle}{1}

\DeclareOption{theoremcontinuousnumbering}
{
  \setcounter{actasectionstyle}{1}
}

\DeclareOption{theoremsectionnumbering}
{
  \setcounter{actasectionstyle}{2}
}

\DeclareOption{notnumberedtheorems}
{
  \newtheorem*{nntheorem}{Theorem}
  \newtheorem*{nndefinition}{Definition}
  \newtheorem*{nnlemma}{Lemma}
  \newtheorem*{nnproposition}{Proposition}
  \newtheorem*{nncorollary}{Corollary}
  \newtheorem*{nnclaim}{Claim}
  \theoremstyle{definition}
  \newtheorem*{nnexample}{Example}
%  \theoremstyle{remark}
  \newtheorem*{nnremark}{Remark}
  \theoremstyle{plain}
}

\ExecuteOptions{theoremcontinuousnumbering}
\ProcessOptions

\RequirePackage{geometry}
\geometry{a4paper,includeheadfoot,textwidth=28cc,textheight=42cc}

\newcommand{\draft}{\setlength\overfullrule{5pt}}

\newcommand{\referee}{\linenumbers\modulolinenumbers[1]
 %\setvruler[24.7pt][1][2][1][0][30pt][30pt][0pt][\textheight]
}

\newcommand{\ac@lastpage@putlabel}{
  %\addtocounter{page}{-1}
  \immediate\write\@auxout{\string\newlabel{LastPage}{{}{\thepage}}}
  %\addtocounter{page}{1}
}
\AtEndDocument{\ac@lastpage@putlabel}

\newcommand{\ac@volume}{00}
\newcommand{\ac@year}{0000}
\newcommand{\ac@pages}{\thepage--\pageref{LastPage}.}
\newcommand{\ac@headingtitle}{}
\newcommand{\ac@headingauthor}{}
\newcommand{\page}[1]{\setcounter{page}{#1}}

\newcommand{\acta}[2]{
  \renewcommand{\ac@volume}{#1}
  \renewcommand{\ac@year}{#2}
}

\def\eatone#1{}
\def\eatzero#1{#1}
\def\hideinheader#1{#1}
\def\and{~and~}
\def\aand{and~}



\newcommand{\keywords}[1]{\vspace*{\baselineskip}{\noindent\textbf{Keywords:} #1}}

\newcommand{\email}[1]{\mbox{E-mail:} \texttt{#1}}

\renewcommand{\labelenumii}{\theenumii)}
\renewcommand{\p@enumii}{\theenumi.}

\newcommand{\received}[1]{\AtEndDocument{\smallskip
\centerline{\textit{Received #1}}}
}


\ifthenelse{\value{actasectionstyle} = 1}
{
\newtheorem{theorem}{Theorem}
\newtheorem{definition}{Definition}
\newtheorem{lemma}{Lemma}
\newtheorem{proposition}{Proposition}
\newtheorem{corollary}{Corollary}
\newtheorem{claim}{Claim}
\theoremstyle{definition}
\newtheorem{example}{Example}
%\theoremstyle{remark}
\newtheorem{remark}{Remark}
\theoremstyle{plain}
}
{
\newtheorem{theorem}{Theorem}[section]
\newtheorem{definition}{Definition}[section]
\newtheorem{lemma}{Lemma}[section]
\newtheorem{proposition}{Proposition}[section]
\newtheorem{corollary}{Corollary}[section]
\newtheorem{claim}{Claim}[section]
\theoremstyle{definition}
\newtheorem{example}{Example}[section]
%\theoremstyle{remark}
\newtheorem{remark}{Remark}[section]
\theoremstyle{plain}
}

%\sloppy
