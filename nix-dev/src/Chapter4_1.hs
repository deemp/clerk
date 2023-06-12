{- D -}

module Chapter4_1 where

{- E -}

{-
\chapter{Implementation}
\label{chap:impl}

\texttt{clerk} can be used to produce a styled spreadsheet with some data and formulas on it. These formulas are evaluated when the document is loaded into a target spreadsheet system.

The library supports the following features:

\begin{itemize}
  \item Typed cell references. Example: \texttt{Ref Double};
  \item Type-safe arithmetic operations with them. Example: \texttt{(a :: Ref Double) + (b :: Ref Double)} produces a \texttt{Ref Double};
  \item Constructing expressions with given types. Example: \texttt{("SUM" [a .: b]):: Expr Double} translates to \texttt{SUM(A1:B1)} (actual value depends on the values of \texttt{a} and \texttt{b});
  \item Conditional styles, formatting, column widths.
\end{itemize}

\Cref{sec:ex1} demonstrates the formula syntax and \Cref{sec:ex2} provides an example of working with the library's data types.

\section{Example 1. Formulas}
\label{sec:ex1}

This section demonstrates the formula syntax via several examples.

\subsection{Imports}

These are the necessary imports.

\begin{mycode}
-}

import Clerk
import Data.Text (Text)

{-
\end{mycode}

\subsection{Sample formulas}

Formulas consist of references, functions, and values.
Here, I pretend that there are values with given types and that I can get references to them. I compose formulas using these references.

\begin{mycode}
-}

r1 :: Ref Int
r1 = mkRef "B4"

r2 :: Ref Int
r2 = mkRef "E6"

r3 :: Ref Int
r3 = mkRef "G8"

t1 :: Text
t1 = showFormula $ ref r2

-- >>>t1
-- "E6"

t2 :: Text
t2 = showFormula $ (r1 .* r2) .+ r1 .^ r2 .- r3

-- >>>t2
-- "B4*E6+B4^E6/G8"

{-
\end{mycode}
-}
