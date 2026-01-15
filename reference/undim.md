# Removes the dimension of an object

Removes the dimension of an object

## Usage

``` r
undim(x, ...)
```

## Arguments

- x:

  An object with or without dimensions

- ...:

  Not used.

## Value

The object with the dimension attribute removed.

## Details

This function does `attr(x, "dim") <- NULL`, which automatically also
does `attr(x, "dimnames") <- NULL`. However, other attributes such as
names attributes are preserved, which is not the case if one do
`dim(x) <- NULL`.
