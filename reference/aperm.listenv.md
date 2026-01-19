# Transpose a 'listenv' array by permuting its dimensions

Transpose a 'listenv' array by permuting its dimensions

## Usage

``` r
# S3 method for class 'listenv'
aperm(a, perm, ...)

# S3 method for class 'listenv'
t(x)
```

## Arguments

- a, x:

  (listenv) The list environment to be transposed

- perm:

  (integer vector) An index vector of length `dim(a)`

- ...:

  Additional arguments passed to
  [`base::aperm()`](https://rdrr.io/r/base/aperm.html).

## Value

Returns a list environment with permuted dimensions

## See also

These functions work like
[`base::aperm()`](https://rdrr.io/r/base/aperm.html) and
[`base::t()`](https://rdrr.io/r/base/t.html).

## Examples

``` r
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(letters[1:2], LETTERS[1:3])
print(x)
#> A ‘listenv’ matrix with 6 elements (unnamed) arranged in 2x3 rows (‘a’, ‘b’) and columns (‘A’, ‘B’, ‘C’).

x <- t(x)
print(x)
#> A ‘listenv’ matrix with 6 elements (unnamed) arranged in 3x2 rows (‘A’, ‘B’, ‘C’) and columns (‘a’, ‘b’).

x <- aperm(x, perm = 2:1)
print(x)
#> A ‘listenv’ matrix with 6 elements (unnamed) arranged in 2x3 rows (‘a’, ‘b’) and columns (‘A’, ‘B’, ‘C’).
```
