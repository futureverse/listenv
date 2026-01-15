# Set the dimension of an object

Set the dimension of an object

## Usage

``` r
dim_na(x) <- value
```

## Arguments

- x:

  An R object, e.g. a list environment, a matrix, an array, or a data
  frame.

- value:

  A numeric vector coerced to integers. If one of the elements is
  missing, then its value is inferred from the other elements (which
  must be non-missing) and the length of `x`.

## Value

An object with the dimensions set, similar to what
[`dim(x) <- value`](https://rdrr.io/r/base/dim.html) returns.

## Examples

``` r
x <- 1:6
dim_na(x) <- c(2, NA)
print(dim(x))  ## [1] 2 3
#> [1] 2 3
```
