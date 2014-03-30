%%================================================
%%  authesis.cls
%%
%%  Aberdeen MX4020 Report class file
%%
%%================================================
%% First Version
%% JP 26-11-04
%%
%%================================================

\NeedsTeXFormat{LaTeX2e}[1994/01/01]
\ProvidesClass{mx4020}[2004/11/04]


%=================================================
% Packages incorporated
%=================================================

\RequirePackage{ifthen}     % conditionals

\RequirePackage{amssymb}    % AMS symbols

\RequirePackage{setspace}   % double spacing etc

\RequirePackage{epsfig}     % graphics display (for logo)


%--------------------------------------------------
% load amsbook.cls as the base class, passing
% options to it.
%--------------------------------------------------

%-- always a4paper 12pt and rhs equation numbers,


\PassOptionsToClass{12pt,a4paper,reqno,oneside}{amsbook}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{amsbook}}
\ProcessOptions

\LoadClass{amsbook}


%-------------------------------------------------
% basic page sizes etc
%=================================================


\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}
\setlength{\voffset}{-1cm}
\setlength{\hoffset}{-1.2cm}
\setlength{\textwidth}{6in}
\setlength{\textheight}{9in}
\setlength{\headsep}{0.3in}


%%%======================================================
%%%======================================================
%%   FRONT MATTER
%%%======================================================
%%%======================================================


%========================================================
% Title Page and Preliminary matter
%========================================================

\newcommand{\Th@title}{banana}
\newcommand{\Th@author}{banana}
\newcommand{\Th@subdate}{banana}
\newcommand{\Th@supervisor}{banana}

\newcommand{\Title}[1]{\renewcommand{\Th@title}{{#1}}}
\newcommand{\Author}[1]{\renewcommand{\Th@author}{{#1}}}
\newcommand{\Year}[1]{\renewcommand{\Th@subdate}{{#1}}}
\newcommand{\Supervisor}[1]{\renewcommand{\Th@supervisor}{{#1}}}



\newcommand{\mxfrontpage}{%
\thispagestyle{empty}
%  ----Header
\begin{minipage}[t][1in][c]{\textwidth}
\begin{center}{\bf\Large\sc University of Aberdeen\\Department of Mathematical Sciences}\end{center}
\bigskip
\begin{center}{\bf\Large\sc MX4020 Project Report}\end{center}
\bigskip
\begin{center}{\bf\large\sc \Th@subdate}\end{center}
\end{minipage}
%
\vspace*{1in}
% ---- unilogo
\begin{center}
\epsfig{file=phdlogo.eps,width=1in}
\end{center}
%
\vspace*{1in}

% ----Author
\begin{center}{\bf\Huge \Th@title}\end{center}
\vspace*{2.5in}
\leftline{\bf\Large {\sc Author} : \Th@author}
\bigskip
% --- Supervisor
\leftline{\bf\Large {\sc Supervisor} : \Th@supervisor}

\newpage
\declaration
\newpage
}

%======================================================
% Official Declaration (signature) Page
%======================================================

\newcommand{\declaration}
{\thispagestyle{empty}
 \phantom{A}
 \vspace{1in}
 \begin{center}{\Large \bf Declaration}\end{center}
 \vspace*{0.75in}
 \begin{quote}
 I hereby declare that this report has been composed by me.
 I also declare that all sources of information have been
  specifically acknowledged and all quotations distinguished
 by quotation marks.

 \vspace*{1in}

 \hspace{2.5in}(signed) ...............................

 \end{quote}
 \newpage
 }




%======================================================
% Summary  Page
%======================================================


\newenvironment{Summary}{%
\thispagestyle{empty}
 \begin{center}{\Large \bf Summary}\end{center}

 \hrule

 \vspace*{0.75in}
 \begin{quote}}{\end{quote}\newpage}


%%%======================================================
%%  END OF FRONT MATTER
%%%======================================================


\newcommand{\StartThesis}%
{
  \setcounter{page}{0}
  \setcounter{secnumdepth}{2} %+++++
  \tableofcontents
  \mainmatter

  \newcommand{\T@headline} % text of headline
   {{\small\sc \chaptername\, \thechapter\hfill%
      Page\quad\thepage}}
 % \newcommand{\T@footline}{\begin{minipage}{\textwidth}\rule{\linewidth}{0.1mm}\\{\small Author: \Th@author\hfill}\end{minipage}}
   \newcommand{\ps@headline}{%
   \renewcommand{\@oddhead}{\T@headline}
   \renewcommand{\@oddfoot}{}
   \renewcommand{\@evenfoot}{}
   }
 \pagestyle{headline}
 \onehalfspacing
}



%======================================================
% Equation Numbering :  chapter.number
%======================================================

\numberwithin{equation}{chapter}
\renewcommand{\theequation}{\thechapter.\arabic{equation}}


%======================================
%  Table Numbering within Chapters
%   Chapter number as prefix
%   (26/10/01)
%======================================

\newcommand{\tableprefix}{\renewcommand{\thetable}{\thechapter.\arabic{table}}}

%=======================================================
% Sundries
%=======================================================


% -- theorem structures (chapter indexed)

\theoremstyle{plain}
\newtheorem*{stheorem}{Theorem}
\newtheorem*{slemma}{Lemma}
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{lemma}{Lemma}[chapter]
\newtheorem{corollary}{Corollary}[chapter]
\newtheorem{proposition}{Proposition}[chapter]
\newtheorem{conjecture}{Conjecture}[chapter]

\theoremstyle{definition}
\newtheorem{definition}{Definition}[chapter]
\newtheorem{problem}{Problem}[chapter]
\newtheorem{example}{Example}[chapter]

% these are not numbered

\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{note}{Note}
\newtheorem*{notation}{Notation}

% -- blackboard bold

\newcommand{\field}[1]{\mathbb{#1}}
\newcommand{\Z}{\field{Z}}
\renewcommand{\P}{\field{P}}
\newcommand{\R}{\field{R}}
\newcommand{\C}{\field{C}}
\newcommand{\Q}{\field{Q}}

%-- change default inequality symbols to AMS type

\renewcommand{\le}{\leqslant}
\renewcommand{\ge}{\geqslant}



\endinput
