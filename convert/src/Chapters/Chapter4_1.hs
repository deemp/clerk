{- d -}
module Chapters.Chapter4_1 where
{- e -}

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

import Clerk hiding (mkRef)
import Data.Text (Text)
import Examples.Helpers(mkRef, showFormula)
{-
\end{mycode}

\subsection{Sample formulas}

Formulas consist of references, functions, and values.
Here, I pretend that there are values with given types and that I can get references to them. I compose formulas using these references.

\begin{mycode}
-}

r1 :: Ref Double
r1 = mkRef @"B4"

r2 :: Ref Double
r2 = mkRef @"E6"

r3 :: Ref Double
r3 = mkRef @"G8"

t1 :: Text
t1 = showFormula $ formulaRef r2

-- >>>t1
-- "E6"

{-
Finally, I construct a longer expression and look at its representation.
I convert a literal value to a formula via `val`.
-}

t2 :: Text
t2 = showFormula $ r1 .* r2 .* val 3 .+ (r1 .** r2) ./ r3

-- >>>t2
-- "B4*E6*3.0+B4^E6/G8"

{-
Of course, I can mix differently typed references in expressions when necessary.
For this case, I have an unsafe `as` function.
-}

r4 :: Ref Double
r4 = mkRef @"T6"

t3 :: Text
t3 = showFormula $ as @Double ((r4 .* r4 .* val 3) .+ r1 .** r2 ./ r3)

-- >>>t3
-- "T6*T6*1.5e-323+B4^E6/G8"

{-
This `as` function should not be abused, though. If I need an `Int` instead of a `Double`, I can explicitly use an Excel function.
-}

round_ :: forall a. Formula a -> Formula Int
round_ x = fun "ROUND" [x]

t4 :: Formula Int
t4 = round_ (r1 .** r2 ./ r3)

-- >>>:t t4
-- t4 :: Formula Int

t5 :: Text
t5 = showFormula t4

-- >>> t5
-- "ROUND(B4^E6/G8)"
