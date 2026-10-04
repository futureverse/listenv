library("listenv")

ovars <- ls(envir = globalenv())
if (exists("x")) rm(list = "x")
if (exists("y")) rm(list = "y")

## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-dimensional subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** parse_env_subset() on multi-dim listenv ...")

x <- listenv()
length(x) <- 6
dim(x) <- c(2, 3)

target <- parse_env_subset(x[2], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx  == 2, !target$exists)

target <- parse_env_subset(x[[2]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx  == 2, !target$exists)

target <- parse_env_subset(x[1, 2], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, !target$exists)

target <- parse_env_subset(x[[1, 2]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, !target$exists)

x[[1, 2]] <- 1.2
target <- parse_env_subset(x[1, 2], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, target$exists)

target <- parse_env_subset(x[[1, 2]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, target$exists)

target <- parse_env_subset(x[1, 4], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), is.na(target$idx), !target$exists)

target <- parse_env_subset(x[[1, 4]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), is.na(target$idx), !target$exists)

target <- parse_env_subset(x[1, 1:2], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x),
          length(target$idx) == 2L, all(target$idx == c(1,3)),
	  length(target$exists) == 2L, all(target$exists == c(FALSE, TRUE)))

target <- parse_env_subset(x[1, -3], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x),
          length(target$idx) == 2L, all(target$idx == c(1,3)),
	  length(target$exists) == 2L, all(target$exists == c(FALSE, TRUE)))

## Assert that x[[1, 4]] is not the same as x[[c(1, 4)]]
target <- parse_env_subset(x[[1, 4]], substitute = TRUE)
str(target)
target2 <- parse_env_subset(x[[c(1, 4)]], substitute = TRUE)
str(target2)
target$code <- target2$code <- NULL
stopifnot(!isTRUE(all.equal(target2, target)))


dimnames(x) <- list(c("a", "b"), c("A", "B", "C"))
print(x)

target <- parse_env_subset(x[["a", 2]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, target$exists)

target <- parse_env_subset(x[["a", "B"]], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, target$exists)

target <- parse_env_subset(x["a", "B"], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 3, target$exists)

target <- parse_env_subset(x["a", 1:3], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), length(target$idx) == 3,
          all(target$idx == c(1, 3, 5)),
          all(target$exists == c(FALSE, TRUE, FALSE)))

target <- parse_env_subset(x["a", ], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), length(target$idx) == 3,
          all(target$idx == c(1, 3, 5)),
          all(target$exists == c(FALSE, TRUE, FALSE)))

target <- parse_env_subset(x["a", -1], substitute = TRUE)
str(target)
stopifnot(identical(target$envir, x), length(target$idx) == 2,
          all(target$idx == c(3, 5)),
          all(target$exists == c(TRUE, FALSE)))

## Multi-dimensional subsetting with out-of-bounds and named elements
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
names(x) <- letters[1:6]

target <- parse_env_subset(x[1:2, c(1, 5)], substitute = TRUE)
str(target)
stopifnot(
  identical(target$envir, x),
  is.matrix(target$idx),
  identical(dim(target$idx), c(2L, 2L)),
  all(target$idx[, 1] == c(1, 2)),
  all(is.na(target$idx[, 2])),
  is.matrix(target$exists),
  identical(dim(target$exists), c(2L, 2L)),
  all(target$exists[, 1]),
  !any(target$exists[, 2])
)

target <- parse_env_subset(x[1, 5], substitute = TRUE)
str(target)
stopifnot(
  identical(target$envir, x),
  length(target$idx) == 1L,
  is.na(target$idx),
  length(target$exists) == 1L,
  !target$exists
)

## Symbols are evaluated for [, as for [[
x <- listenv()
length(x) <- 6
dim(x) <- c(2, 3)
x[[2, 1]] <- 2.1
j <- 2L
target <- parse_env_subset(x[j, 1], substitute = TRUE)
str(target)
stopifnot(target$idx == 2, target$exists)
target <- parse_env_subset(x[j, ], substitute = TRUE)
str(target)
stopifnot(all(target$idx == c(2, 4, 6)),
          all(target$exists == c(TRUE, FALSE, FALSE)))
rm(list = "j")

## Named dimensions must not give dimnames on 'idx' and 'exists'
for (dim in list(c(r = 2, c = 3), c(d1 = 2, d2 = 3, d3 = 4))) {
  x <- listenv()
  length(x) <- prod(dim)
  dim(x) <- dim
  if (length(dim) == 2) {
    exprs <- list(quote(x[[1, 2]]), quote(x[, 2]), quote(x[1, 4]))
    idxs <- list(3, c(3, 4), NA)
  } else {
    exprs <- list(quote(x[[1, 2, 3]]), quote(x[, 2, 4]), quote(x[-1, 1:2, 4]))
    idxs <- list(15, c(21, 22), c(20, 22))
  }
  for (kk in seq_along(exprs)) {
    target <- parse_env_subset(exprs[[kk]], substitute = FALSE)
    str(target)
    stopifnot(
      is.null(dimnames(target$idx)),
      is.null(dimnames(target$exists)),
      identical(as.vector(target$idx), as.numeric(idxs[[kk]]))
    )
  }
}

message("*** parse_env_subset() on multi-dim listenv ... DONE")


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** parse_env_subset() on multi-dim listenv - exceptions ...")

x <- listenv()

## Multidimensional subsetting on 'x' without dimensions
res <- tryCatch(target <- parse_env_subset(x[[1, 2]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

## Multi-dimensional subsetting
x <- listenv()
length(x) <- 6
dim(x) <- c(2, 3)


## Zero indices are invalid for [[, as for lists
res <- tryCatch(target <- parse_env_subset(x[[0]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(target <- parse_env_subset(x[[1, 0]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(target <- parse_env_subset(x[[0, 2]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

## ... but select nothing for [, as for lists
target <- parse_env_subset(x[1, 0], substitute = TRUE)
stopifnot(length(target$idx) == 0L, length(target$exists) == 0L)

## Fractional indices are truncated toward zero, as for arrays
target <- parse_env_subset(x[[1.5, 2]], substitute = TRUE)
stopifnot(target$idx == 3L)

res <- tryCatch(target <- parse_env_subset(x[[1, 0.5]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"),
          grepl("zero", conditionMessage(res)))

## An empty subset selects all elements of a dimension for [, but a
## NULL subset selects nothing, as for arrays
target <- parse_env_subset(x[1, ], substitute = TRUE)
stopifnot(length(target$idx) == 3L)

target <- parse_env_subset(x[1, NULL], substitute = TRUE)
stopifnot(length(target$idx) == 0L, length(target$exists) == 0L)

## ... whereas both are invalid for [[, as for arrays
res <- tryCatch(target <- parse_env_subset(x[[1, ]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(target <- parse_env_subset(x[[1, NULL]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(target <- parse_env_subset(x[[1, 2, 3]], substitute = TRUE),
                error = identity)
stopifnot(inherits(res, "error"))

## Incorrect number of subscripts
x <- listenv()
length(x) <- 24
dim(x) <- c(2, 3, 4)
exprs <- list(
  quote(x[[1, 2]]),
  quote(x[1, 2]),
  quote(x[1, ]),
  quote(x[[1, 2, 3, 4]]),
  quote(x[1, 2, 3, 4])
)
for (expr in exprs) {
  nsubscripts <- length(expr) - 2L
  res <- tryCatch(parse_env_subset(expr, substitute = FALSE), error = identity)
  msg <- sprintf("Incorrect number of subscripts (%d) for a list environment with 3 dimensions", nsubscripts)
  stopifnot(
    inherits(res, "error"),
    grepl(msg, conditionMessage(res), fixed = TRUE)
  )
}

## All errors report the code, but not the call
x <- listenv()
length(x) <- 6
dim(x) <- c(2, 3)
y <- listenv()
length(y) <- 3
e <- new.env()
exprs <- list(
  quote(x[[1, 0]]),
  quote(x[[1, -1]]),
  quote(y[[1, 2]]),
  quote(y[[0]]),
  quote(y[[-1]]),
  quote(y[c(-1, 1)]),
  quote(e[["a", "b"]]),
  quote(e[[c("a", "b")]]),
  quote(e[[1]])
)
for (expr in exprs) {
  code <- paste(deparse(expr), collapse = "")
  res <- tryCatch(parse_env_subset(expr, substitute = FALSE), error = identity)
  stopifnot(
    inherits(res, "error"),
    grepl(sQuote(code), conditionMessage(res), fixed = TRUE),
    is.null(conditionCall(res))
  )
}
rm(list = c("x", "y", "e"))

message("*** parse_env_subset() on multi-dim listenv - exceptions ... DONE")


## Cleanup
rm(list = setdiff(ls(envir = globalenv()), ovars), envir = globalenv())
