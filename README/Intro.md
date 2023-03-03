# clerk

`clerk` provides a Haskell eDSL in a library for declarative spreadsheet generation. `clerk` is built on top of the [xlsx](https://hackage.haskell.org/package/xlsx) package and extends upon the [work](https://youtu.be/1xGoa-zEOrQ) of Nickolay Kudasov by making the tables' layout more flexible.

## Features

`clerk` can be used to produce a styled spreadsheet with some data and formulas on it. These formulas are evaluated when the document is loaded into a target spreadsheet system.

The library supports:

- Typed cell references. Example: `CellRef Double`.
- Type-safe arithmetic operations with them. Example: `(a :: CellRef Double) + (b :: CellRef Double)` produces a `CellRef Double`.
- Constructing expressions with given types. Example: `(e :: Expr Double) = "SUM" |$| [a |:| b]`, `e` translates to `SUM(A1:B1)` (actual value depends on the values of `a` and `b`).
- Conditional styles, formatting, column widths.

The examples below demonstrate these features.
