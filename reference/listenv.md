# Create a list environment

Create a list environment

## Usage

``` r
listenv(...)

as.listenv(...)
```

## Arguments

- ...:

  (optional) Named and/or unnamed objects to be assigned to the list
  environment.

## Value

An environment of class `listenv`.

## Examples

``` r
x <- listenv(c = 2, a = 3, d = "hello")
print(names(x))
#> [1] "c" "a" "d"
names(x)[2] <- "A"
x$b <- 5:8

y <- as.list(x)
str(y)
#> List of 4
#>  $ c: num 2
#>  $ A: num 3
#>  $ d: chr "hello"
#>  $ b: int [1:4] 5 6 7 8

z <- as.listenv(y)
```
