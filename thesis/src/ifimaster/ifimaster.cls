%% Identification part:

\NeedsTeXFormat{LaTeX2e}[1995/12/01]  % Any version, really!

\ProvidesClass{template/ifimaster}[2010/01/17 v1.0 Ifi/UiO document class]

%% Initial code part:

%% Option declaration part:
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\ExecuteOptions{}
\ProcessOptions \relax

%% Base class load part:

\LoadClass[a4paper,12pt,openright]{report}

%% Adjustments:

\addtolength{\topmargin}{-1\baselineskip}
\addtolength{\textheight}{4\baselineskip}

\pretolerance = 2000
\tolerance = 5000   \hbadness = \tolerance

%% Required packages:

\IfFileExists{uiofont2010.sty}% Official text font (if installed)
  {\RequirePackage{uiofont2010}}%
  {\RequirePackage{mathpazo}}

%% Document parts:

\newcommand{\frontmatter}{\cleardoublepage \pagenumbering{roman}}
\newcommand{\mainmatter}{\cleardoublepage \pagenumbering{arabic}}
\newcommand{\backmatter}{\cleardoublepage}

\endinput
