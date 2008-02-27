%%
%% This is file `kth-mag.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% kthesis.dtx  (with options: `classfile,exjobb,avhandling')
%% 
%% IMPORTANT NOTICE:
%% 
%% For the copyright see the source file.
%% 
%% Any modified versions of this file must be renamed
%% with new filenames distinct from kth-mag.cls.
%% 
%% For distribution of the original source see the terms
%% for copying and modification in the file kthesis.dtx.
%% 
%% This generated file may be distributed as long as the
%% original source files, as listed above, are part of the
%% same distribution. (The sources need not necessarily be
%% in the same archive or directory.)
\NeedsTeXFormat{LaTeX2e}[2001/01/01]
\ProvidesClass{kth-mag}
              [2004/12/14 v2.1 Document Class for KTH Theses]
\newif\if@kthesis@electronic \@kthesis@electronicfalse
\DeclareOption{electronic}{\@kthesis@electronictrue}
\def\@kthesis@setpapersize@afour{%
  \setstocksize{297mm}{210mm}
  \settrimmedsize{297mm}{210mm}{*}
  \settypeblocksize{49pc}{33pc}{*}}
\def\@kthesis@setpapersize@gfive{%
  \if@kthesis@electronic
    \setstocksize{239mm}{169mm}
    \settrimmedsize{239mm}{169mm}{*}
    \ifpdf
    \else
      \special{papersize=169mm,239mm}
    \fi
  \else
    \setstocksize{297mm}{210mm}
    \settrimmedsize{239mm}{169mm}{*}
    \setlength{\trimtop}{\stockheight}
    \addtolength{\trimtop}{-\paperheight}
    \setlength{\trimtop}{0.5\trimtop}
    \setlength{\trimedge}{\stockwidth}
    \addtolength{\trimedge}{-\paperwidth}
    \ifpdf
      \AtBeginDocument{%
        \setlength{\pdfpagewidth}{\stockwidth}
        \setlength{\pdfpageheight}{\stockheight}}
    \fi
  \fi
  \settypeblocksize{43pc}{30pc}{*}}
\DeclareOption{g5paper}{\def\kthesis@trimmedsize{gfive}}
\DeclareOption{a3paper}{\OptionNotUsed}
\DeclareOption{a4paper}{\def\kthesis@trimmedsize{afour}}
\DeclareOption{a6paper}{\OptionNotUsed}
\DeclareOption{a5paper}{\OptionNotUsed}
\DeclareOption{b3paper}{\OptionNotUsed}
\DeclareOption{b4paper}{\OptionNotUsed}
\DeclareOption{b5paper}{\OptionNotUsed}
\DeclareOption{b6paper}{\OptionNotUsed}
\DeclareOption{letterpaper}{\OptionNotUsed}
\DeclareOption{legalpaper}{\OptionNotUsed}
\DeclareOption{executivepaper}{\OptionNotUsed}
\DeclareOption{ebook}{\OptionNotUsed}
\DeclareOption{landscape}{\OptionNotUsed}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
\ExecuteOptions{g5paper}
\ProcessOptions\relax
\LoadClass[a4paper]{memoir}
\AtBeginDocument{\RequirePackage{graphics}}
\@nameuse{@kthesis@setpapersize@\kthesis@trimmedsize}
\setlrmargins{*}{*}{*}
\setulmargins{*}{*}{*}
\checkandfixthelayout
\def\subtitle{\gdef\thesubtitle}
\def\trita{\gdef\thetrita}
\def\kthlogo{\gdef\thekthlogo}
\def\blurb{\gdef\theblurb}
\let\thesubtitle\relax
\def\thekthlogo{kth_svv}
\let\theblurb\relax
\def\maketitle{%
  \begingroup
  \sffamily
  \pagestyle{empty}
  \renewcommand*{\thepage}{title-\arabic{page}}
  \noindent
  \parbox[b][0.6\textheight]{\textwidth}{\centering\noindent
    \huge\bfseries\thetitle\par
    \vspace{2cm}
    \large\mdseries\thesubtitle\par
    \vfill
    \large\MakeUppercase{\theauthor}\par
  }\par
  \vfill\noindent
  \parbox[b]{\textwidth}{\centering\noindent
    \theblurb\par
  }
  \vfill
  \centerline{\thetrita}
  \rmfamily
  \clearpage
  \endgroup
}
\endinput
%%
%% End of file `kth-mag.cls'.
