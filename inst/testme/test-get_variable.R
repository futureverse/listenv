library("listenv")

ovars <- ls(envir = globalenv())
oopts <- options(warn = 1)

x <- listenv()
length(x) <- 3L
names(x) <- c("a", "b", "c")
stopifnot(length(x) == 3L)
print(mapping(x))

var <- get_variable(x, "a")
stopifnot(!is.na(var))
stopifnot(length(x) == 3L)
print(mapping(x))

var <- get_variable(x, "b")
stopifnot(!is.na(var))
stopifnot(length(x) == 3L)
print(mapping(x))

var <- get_variable(x, "c")
stopifnot(!is.na(var))
stopifnot(length(x) == 3L)
print(mapping(x))

var <- get_variable(x, "d")
stopifnot(!is.na(var))
stopifnot(length(x) == 4L)
print(mapping(x))

var <- get_variable(x, 4L)
stopifnot(!is.na(var))
stopifnot(length(x) == 4L)
print(mapping(x))

x$b <- 2
var <- get_variable(x, "b")
stopifnot(!is.na(var))
stopifnot(length(x) == 4L)
print(mapping(x))

var <- get_variable(x, length(x) + 1L)
stopifnot(length(x) == 5L)
print(names(x))
print(mapping(x))

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Allocation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
length(x) <- 3L
print(x[[1]])
print(x[[2]])
print(x[[3]])

## Out-of-bound subsetting
res <- tryCatch(x[[0]], error = identity)
stopifnot(inherits(res, "error"))

## Out-of-bound subsetting
res <- tryCatch(x[[4]], error = identity)
stopifnot(inherits(res, "error"))

print(get_variable(x, 1L, mustExist = FALSE))
print(get_variable(x, 2L, mustExist = FALSE))
print(get_variable(x, 3L, mustExist = FALSE))

## Out-of-bound element
res <- tryCatch(var <- get_variable(x, 0L, mustExist = TRUE), error = identity)
stopifnot(inherits(res, "error"))

## Out-of-bound element
res <- tryCatch(var <- get_variable(x, length(x) + 1L, mustExist = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
length(x) <- 3L
names(x) <- c("a", "b", "c")

## Non-existing element
res <- tryCatch(var <- get_variable(x, "z", mustExist = TRUE), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(var <- get_variable(x, c("a", "b")), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(var <- get_variable(x, 1 + 2i), error = identity)
stopifnot(inherits(res, "error"))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Odds and ends
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## get_variable with 'mustExist = TRUE' on named list environment
x <- listenv(a = 1, b = 2)
var <- get_variable(x, "a", mustExist = TRUE)
stopifnot(!is.na(var))

## get_variable with 'create = FALSE' on numeric index
x <- listenv()
length(x) <- 3L
var <- get_variable(x, 2L, create = FALSE)
stopifnot(!is.na(var))
stopifnot(length(x) == 3L)

## get_variable with numeric index expanding the mapping
x <- listenv()
length(x) <- 2L
var <- get_variable(x, 5L)
stopifnot(length(x) == 5L)

## get_variable with named character on existing name
x <- listenv(a = 1, b = 2)
var <- get_variable(x, "a")
stopifnot(!is.na(var))

## get_variable with named character creating new name
x <- listenv(a = 1)
var <- get_variable(x, "z")
stopifnot(!is.na(var))
stopifnot(length(x) == 2L)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-dimensional get_variable - exceptions
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- as.listenv(1:6)
dim(x) <- c(2, 3)

## Wrong number of indices
res <- tryCatch(get_variable(x, c(1, 2, 3)), error = identity)
stopifnot(inherits(res, "error"))

## Missing values in index
res <- tryCatch(get_variable(x, c(NA_integer_, 1L)), error = identity)
stopifnot(inherits(res, "error"))

## Out-of-range index
res <- tryCatch(get_variable(x, c(3, 1)), error = identity)
stopifnot(inherits(res, "error"))


## Cleanup
options(oopts)
rm(list = setdiff(ls(envir = globalenv()), ovars), envir = globalenv())
