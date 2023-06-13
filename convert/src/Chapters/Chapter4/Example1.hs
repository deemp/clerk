{- D -}

module Chapters.Chapter4.Example1 where

{- E -}

{-
\chapter{Examples}
\label{chap:impl}

This chapter presents several examples of the \clerk library usage.

The library was used to produce styled spreadsheets with data and formulas on it. The formulas were evaluated when the document was loaded into a target spreadsheet system.

The library supported the following features:

\begin{itemize}
  \item Typed cell references (\cref{example1:typedReferences});
  \item Type-safe arithmetic operations with cell references (\cref{example1:typedArithmetic});
  \item Typed formulas (\cref{example1:typedFunctions});
  \item Conditional styles, formatting, column widths;
  \item Context-aware formula rendering.
\end{itemize}

\Cref{sec:ex1} demonstrates the formula syntax and \Cref{sec:ex2} provides an example of working with the library data types.

\section{Example 1. Formulas}
\label{sec:ex1}

This section demonstrates the formula syntax and several examples of formula properties.

\subsection{Formula syntax}

Formulas consist of references (relative cell addresses), functions, and values.

I constructed a EBNF of a rendered cell formula (\Cref{example1:ebnf}) and made the following assumptions:

\begin{itemize}
  \item a \hs{<number>} is an integer or decimal number (\hs{3 or 3.14});
  \item a \hs{<string>} is an ordinary string in a spreadsheet system (\hs{"abc"});
  \item \hs{<cell formula>} is a formula interpretable by a spreadsheet system (\hs{=A1+B2});
\end{itemize}


\begin{figure}[h]
  \begin{multicols}{2}
    \begin{grammar}
      <letter> ::= [A-Z]

      <column> ::= <letter>+

      <digit> ::= [0-9]

      <row> ::= 0<digit>+ $\vert$ 1<digit>*

      <reference> ::= <column><row>

      <function name> ::= $\newline$
      [A-Za-z][A-Za-z0-9]*

      \columnbreak

      <value> ::= <number> | <string>

      <operator> ::= + $\vert$ - $\vert$ * $\vert$ /
      \alt $\leq$ $\vert$ $\geq$ $\vert$

      <function> ::= $\newline$ <function name>((<expression>, ))

      <formula> ::= <function> \alt <reference> \alt <value>

      <cell formula> ::= =<formula>
    \end{grammar}
  \end{multicols}
  \caption{Simplified formula EBNF}
  \label{example1:ebnf}
\end{figure}


\subsection{Imports}

First, I imported the necessary modules (\cref{example1:imports}).
Usually, formula representation depends on a certain context.
This context contains the location of a formula within a spreadsheet.
However, in the example below, I did not use contexts.
Instead, I imported from the module \hs{Examples.Helpers} several functions that use a default context and make formulas look concise.

\begin{listing}[!h]
  \begin{minted}{haskell}
-}

import Clerk hiding (mkRef)
import Data.Text (Text)
import Examples.Helpers(mkRef, showFormula)

{-
\end{minted}
  \caption{Imports}
  \label{example1:imports}
\end{listing}


\subsection{Typed cell references}

The \clerk library supports typed cell references. Such a cell reference contains the coordinates of a cell within a spreadsheet and the type of a value in that cell.
-}

{- TODO were explained in the previous chapter for more details
-}

{-
I constructed several references to \hs{Double} values (\cref{example1:typedReferences}). Moreover, I provided each cell address as type-level \hs{Symbol}s so that the GHC can check the syntax of an address at compile time. If a user writes an invalid address, the compiler will show a type error and will refuse to compile the program.

\begin{listing}[!h]
  \begin{minted}{haskell}
-}

r1 :: Ref Double
r1 = mkRef @"B4"

r2 :: Ref Double
r2 = mkRef @"E6"

r3 :: Ref Double
r3 = mkRef @"G8"

{-
\end{minted}
  \caption{Typed references}
  \label{example1:typedReferences}
\end{listing}

\newpage

\subsection{Sample formulas}
Next, I converted one of the references to a formula and rendered it (\cref{example1:renderRef}).

\begin{listing}[!h]
  \begin{minted}{haskell}
-}

f1 :: Formula Double
f1 = formulaRef r2

s1 :: Text
s1 = showFormula f1

-- >>>t1
-- "E6"

{-
\end{minted}
  \caption{Rendered reference}
  \label{example1:renderRef}
\end{listing}

Following that, I constructed an arithmetic formula and looked at its representation (\cref{example1:typedArithmetic}). I used \hs{val 3} to bring a literal value into this formula.

\begin{listing}[!h]
  \begin{minted}{haskell}
-}

f2 :: Formula Double
f2 = r1 .* r2 .* val 3 .+ r1 .** r2 ./ r3

s2 :: Text
s2 = showFormula f2

-- >>>t2
-- "(((B4)*(E6))*(3.0))+(((B4)^(E6))/(G8))"

{-
\end{minted}
  \caption{Long formula}
  \label{example1:typedArithmetic}
\end{listing}

\newpage

In \cref{example1:typedArithmetic}, a formula \hs{f2} composed from \hs{Ref Double} had the type \hs{Formula Double}. In some cases, a user may want to change the type of an expression or a formula. In \cref{example1:unsafe}, I unsafely changed the type of a formula \hs{f4} to \hs{Formula Double} using the function \hs{as}. Though the expression typechecked, it contained a large meaningless integer. The type change was not guaranteed to produce a valid value.

\begin{listing}[!h]
  \begin{minted}[breakanywhere, breakbytokenanywhere=false]{haskell}
-}

r4 :: Ref Int
r4 = mkRef @"T6"

f3 :: Formula Int
f3 = r4 .* r4

s3 :: Text
s3 = showFormula $ f4 .^ as @Int f2

-- >>>t3
-- "((T6)*(T6))^((((B4)*(E6))*(4613937818241073152))+(((B4)^(E6))/(G8)))"    

{-
\end{minted}
  \caption{Unsafe type change}
  \label{example1:unsafe}
\end{listing}

\subsection{Typed functions}

For a typed function, it is possible to set:
\begin{inlinelist}
  \item the function name;
  \item the types of its arguments;
  \item the type of the function result.
\end{inlinelist}

In \cref{example1:typedFunctions}, I defined a sample typed function \hs{round_}.

\begin{listing}[!h]
  \begin{minted}{haskell}
-}

round_ :: Formula Double -> Formula Double -> Formula Int
round_ x y = fun "ROUND" [x .+ y]

t4 :: Formula Double -> Formula Int
t4 = round_ (r1 .** r2 ./ r3)

f4 :: Formula Int
f4 = t4 f1

s4 :: Text
s4 = showFormula f4

-- >>> s4
-- "ROUND(B4^E6/G8+E6)"

{-
\end{minted}
  \caption{Typed function}
  \label{example1:typedFunctions}
\end{listing}

As other Haskell functions, the \hs{round_} function can be partially applied.
In \hs{t4}, the \hs{round_} function is applied to a single value.
Generally, \hs{round_} can be applied to a list of arguments to produce a list of partially applied functions.
-}
