# Get name of variable for a specific element of list environment

Get name of variable for a specific element of list environment

## Usage

``` r
get_variable(...)

# S3 method for class 'listenv'
get_variable(x, name, mustExist = FALSE, create = !mustExist, ...)
```

## Arguments

- x:

  A list environment.

- name:

  The name or index of element of interest.

- mustExist:

  If `TRUE`, an error is generated if `name` does not exist.

- create:

  If `TRUE`, element `name` is created if missing.

## Value

The name of the underlying variable
