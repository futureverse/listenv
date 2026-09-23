# TODO

## Bugs

- `length<-.listenv` does not clear the stale `dim`/`dimnames`
  attributes when resizing a dimensioned (matrix/array) listenv, unlike
  base R which drops `dim` on `length<-`. E.g.:

  ``` r

  x <- as.listenv(1:6)
  dim(x) <- c(2, 3)
  length(x) <- 4
  ```

  leaves `dim(x) == c(2, 3)` while the underlying map now has only four
  elements. `as.list(x)` then throws “dims \[product 6\] do not match
  the length of object \[4\]”, and `print(x)` reports a nonsensical
  matrix. (`R/listenv.R:217`)

- The multi-dimensional branch of `` `[[.listenv` `` (`nidxs > 1`) never
  checks that the resolved index resolves to a single element, unlike
  the single-dimension branch, which stops with “Subsetting of more than
  one element at a time is not allowed”. E.g. on a dimensioned listenv,
  `x[[1:2, 1]]` fails with an opaque internal error (“‘length = 2’ in
  coercion to ‘logical(1)’”) instead of a clear user-facing error.
  (`R/listenv.R:457`)

## Cleanup

- [`parse_env_subset()`](https://listenv.futureverse.org/reference/parse_env_subset.md)
  (`R/parse_env_subset.R:192`) reimplements the same multi-dimensional
  linear-index scaling algorithm already provided by the internal
  `to_index()` helper (`R/listenv.R`), instead of reusing it. The two
  implementations already differ (this one silently maps out-of-range
  indices to `NA_integer_` instead of erroring), so a fix to one is easy
  to miss applying to the other.

- The block resolving implicit/missing indices (`x[]`, `x[, 2]`) is
  duplicated almost verbatim between `` `[.listenv` ``
  (`R/listenv.R:503`) and `` `[<-.listenv` `` (`R/listenv.R:898`). A fix
  to one copy but not the other can leave getter and setter semantics
  inconsistent for the same syntax.

- A roxygen `#' @keywords internal` line is placed inside the body of
  `` `$.listenv` `` (`R/listenv.R:348`), after the opening brace,
  instead of above the function, so it is dead documentation that
  roxygen2 silently ignores. The real tag is already correctly placed
  above at line 346.
