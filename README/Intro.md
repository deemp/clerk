# clerk

`clerk` is a library for declarative spreadsheet generation using a Haskell eDSL.

It extends upon the [work](https://youtu.be/1xGoa-zEOrQ) of Kudasov by making the tables' layout more flexible.

## Features

`clerk` supports

- typed cell references - `Cell Double`
- type-safe arithmetic operations - `(a :: Cell Double) + (b :: Cell Double)`
- range references - `A1:B2`
- formulas - `SUM(A1:A3)`
- conditional styles, formatting, column widths

The example below demonstrates these features.
