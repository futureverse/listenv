# Helper function to infer target from expression and environment

Helper function to infer target from expression and environment

## Usage

``` r
parse_env_subset(
  expr,
  envir = parent.frame(),
  substitute = TRUE,
  is_variable = TRUE
)
```

## Arguments

- expr:

  An expression.

- envir:

  An environment.

- substitute:

  If `TRUE`, then the expression is
  [`base::substitute()`](https://rdrr.io/r/base/substitute.html):ed,
  otherwise not.

- is_variable:

  (logical) If TRUE and an element name is specified, then the name is
  checked to be a valid variable name.

## Value

A named list with elements:

- `envir`:

  An environment (defaults to argument `envir`)

- `name`:

  A character vector. ...

- `op`:

  ...

- `subset`:

  A list of `NULL`. ...

- `idx`:

  An integer vector or `NULL`. ...

- `exists`:

  A logical vector of length `length(idx)` with `TRUE` and `FALSE`
  values.

- `code`:

  The deparsed expression `expr` coerced to a single character string.
