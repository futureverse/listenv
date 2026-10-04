library("listenv")

ovars <- ls(envir = globalenv())
oopts <- options(warn = 1)
with_r_330 <- function(expr) {
  if (getRversion() < "3.3.0") return()
  eval(substitute(expr), envir = parent.frame(), enclos = baseenv())
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Allocation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
print(x)
stopifnot(length(x) == 0)
stopifnot(is.null(names(x)))
with_r_330({
  stopifnot(identical(lengths(x), integer(0L)))
})

## Named, empty list environment
x <- listenv()
names(x) <- character(0L)
print(x)

x <- listenv(a = 1)
print(x)
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("a")))
stopifnot(identical(x$a, 1))
with_r_330({
  stopifnot(identical(lengths(x), c(a = 1L)))
})

x <- listenv(a = 1, b = 2:3)
print(x)
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$a, 1), identical(x$b, 2:3))
with_r_330({
  stopifnot(identical(lengths(x), c(a = 1L, b = 2L)))
  stopifnot(identical(lengths(x, use.names = FALSE), c(1L, 2L)))
})

x <- listenv(b = 2:3, .a = 1)
print(x)
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("b", ".a")))
stopifnot(identical(x$.a, 1), identical(x$b, 2:3))
with_r_330({
  stopifnot(identical(lengths(x), c(b = 2L, .a = 1L)))
})

x <- listenv(length = 3, a = 1)
print(x)
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("length", "a")))
stopifnot(identical(x$length, 3), identical(x$a, 1))
with_r_330({
  stopifnot(identical(lengths(x), c(length = 1L, a = 1L)))
})



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Single-element assignments and subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 0)

x$a <- 1
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("a")))
stopifnot(identical(x$a, 1), is.null(x$b))

x$b <- 2
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$b, 2))

x$a <- 0
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x[["a"]], 0))

x$"a" <- 1
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$a, 1))
with_r_330({
  stopifnot(identical(lengths(x), c(a = 1L, b = 1L)))
})

x[["a"]] <- 0
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))


key <- "b"
x[[key]] <- 3
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$b, 3), identical(x[["b"]], 3), identical(x[[key]], 3))

x[[3]] <- 3.14
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "")))
stopifnot(identical(x[[3]], 3.14))

names(x) <- c("a", "b", "c")
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(x[[3]], 3.14), identical(x[["c"]], 3.14),
          identical(x$c, 3.14))
with_r_330({
  stopifnot(identical(lengths(x), c(a = 1L, b = 1L, c = 1L)))
})



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-element subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assert than no false names are introduced
x <- listenv()
x[1:3] <- list(1, NULL, 3)
print(x)
stopifnot(is.null(names(x)))
with_r_330({
  stopifnot(identical(lengths(x), c(1L, 0L, 1L)))
})

y <- x[]
print(y)
stopifnot(length(y) == length(x))
stopifnot(all.equal(y, x))
stopifnot(!identical(y, x))
stopifnot(is.null(names(y)))
with_r_330({
  stopifnot(identical(lengths(y), c(1L, 0L, 1L)))
})

y <- x[1]
print(y)
stopifnot(is.null(names(y)))

y <- x[2:3]
print(y)
stopifnot(is.null(names(y)))
with_r_330({
  stopifnot(identical(lengths(y), c(0L, 1L)))
})

y <- x[-1]
print(y)
stopifnot(is.null(names(y)))
with_r_330({
  stopifnot(identical(lengths(y), c(0L, 1L)))
})

x[c("c", ".a", "b")] <- list(NULL, 3, 1)
print(x)
stopifnot(identical(names(x), c("", "", "", "c", ".a", "b")))
with_r_330({
  stopifnot(identical(lengths(x), c(1L, 0L, 1L, c = 0L, .a = 1L, b = 1L)))
})

y <- as.list(x)
str(y)
stopifnot(identical(names(y), c("", "", "", "c", ".a", "b")))

y <- as.list(x, all.names = FALSE)
str(y)
stopifnot(identical(names(y), c("", "", "", "c", "b")))

y <- as.list(x, sorted = TRUE)
str(y)
stopifnot(identical(names(y), c("", "", "", ".a", "b", "c")))

y <- as.list(x, all.names = FALSE, sorted = TRUE)
str(y)
stopifnot(identical(names(y), c("", "", "", "b", "c")))

## as.list(x, all.names = FALSE) on unnamed listenv should keep all elements
x <- as.listenv(1:3)
y <- as.list(x, all.names = FALSE)
stopifnot(
  length(y) == 3L,
  identical(y, as.list(x))
)

x <- listenv()
x[c("a", "b", "c")] <- list(1, NULL, 3)
stopifnot(length(x) == 3)

y <- x[NULL]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list()))

y <- x[integer(0L)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list()))

y <- x["a"]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1)))

y <- x[c("a", "c")]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1, c = 3)))

y <- x[c("c", "a")]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(c = 3, a = 1)))

y <- x[c(1, 3)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1, c = 3)))

y <- x[-2]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1, c = 3)))

y <- x[-c(1, 3)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(b = NULL)))

y <- x[rep(1L, times = 6L)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, rep(list(a = 1), times = 6L)))

y <- x[c(4, 3)]
print(y)
stopifnot(identical(names(y), c("", "c")))
z <- as.list(y)
print(z)
stopifnot(identical(names(z), c("", "c")))
stopifnot(identical(z, list(NULL, c = 3)))

y <- x[1:10]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, c(as.list(x), rep(list(NULL), times = 7L))))


y <- x[c(TRUE, FALSE, TRUE)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1, c = 3)))

y <- x[c(TRUE, FALSE)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list(a = 1, c = 3)))

y <- x[TRUE]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, as.list(x)))

y <- x[FALSE]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, list()))

y <- x[rep(TRUE, times = 5L)]
print(y)
z <- as.list(y)
print(z)
stopifnot(identical(z, c(as.list(x), list(NULL), list(NULL))))
with_r_330({
  stopifnot(identical(lengths(z), c(a = 1L, b = 0L, c = 1L, 0L, 0L)))
})


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Local access
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2, c = 3.14)

y <- local({
  x[[3]]
})
stopifnot(identical(y, 3.14))

y <- local({
  x[3]
})
stopifnot(identical(y[[1]], 3.14))

y <- local({
  ii <- 3
  x[[ii]]
})
stopifnot(identical(y, 3.14))

y <- local({
  ii <- 3
  x[ii]
})
stopifnot(identical(y[[1]], 3.14))


local({
  x[[3]] <- 42L
})
y <- x[[3]]
stopifnot(identical(y, 42L))

local({
  x[3] <- 3.14
})
y <- x[[3]]
stopifnot(identical(y, 3.14))

local({
  ii <- 3
  x[ii] <- 42L
})
y <- x[[3]]
stopifnot(identical(y, 42L))

local({
  ii <- 3
  x[[ii]] <- 3.14
})
y <- x[[3]]
stopifnot(identical(y, 3.14))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Removing elements
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x[["a"]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("b", "c")))

x[[3L]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("b", "c")))

x[[2L]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("b")))

x$b <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 0)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assigning NULL
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x[2L] <- list(NULL)
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("", "")))

x["c"] <- list(NULL)
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("", "", "c")))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assigning multiple elements at once
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[c("a", "b", "c")] <- 1:3
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a = 1L, b = 2L, c = 3L)))
stopifnot(identical(unlist(x), c(a = 1L, b = 2L, c = 3L)))

x[] <- 3:1
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a = 3L, b = 2L, c = 1L)))

x[c("c", "b")] <- 2:3
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a = 3L, b = 3L, c = 2L)))

x[c("a", "c")] <- 1L
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a = 1L, b = 3L, c = 1L)))

x[c("d", "e")] <- 4:5
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 5)
stopifnot(identical(names(x), c("a", "b", "c", "d", "e")))
stopifnot(identical(as.list(x), list(a = 1L, b = 3L, c = 1L, d = 4L, e = 5L)))


x <- listenv()
x[c("a", "b")] <- 1:2
x[c(TRUE, FALSE)] <- 2L
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(as.list(x), list(a = 2L, b = 2L)))

x[c(TRUE)] <- 1L
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(as.list(x), list(a = 1L, b = 1L)))

x[c(TRUE, FALSE, TRUE, FALSE)] <- 3L
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "")))
stopifnot(identical(as.list(x), list(a = 3L, b = 1L, 3L)))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Removing multiple elements at once
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- as.listenv(1:6)
names(x) <- letters[seq_along(x)]
y <- as.list(x)
stopifnot(identical(as.list(x), y))

x[2] <- NULL
y[2] <- NULL
stopifnot(identical(as.list(x), y))

x[4:3] <- NULL
y[4:3] <- NULL
stopifnot(identical(as.list(x), y))

x[rep(2, times = 10)] <- NULL
y[rep(2, times = 10)] <- NULL
stopifnot(identical(as.list(x), y))

## Erase all elements
y[] <- NULL
x[] <- NULL
stopifnot(identical(as.list(x), y))

x <- as.listenv(1:6)
names(x) <- letters[seq_along(x)]
y <- as.list(x)
stopifnot(identical(as.list(x), y))

# Every other by logical indexing
x[c(TRUE, FALSE)] <- NULL
y[c(TRUE, FALSE)] <- NULL
stopifnot(identical(as.list(x), y))

x[c("b", "f")] <- NULL
y[c("b", "f")] <- NULL
stopifnot(identical(as.list(x), y))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Expanding
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
for (ii in 1:3) {
  x[[ii]] <- letters[ii]
  print(x[[ii]])
}
print(x)
names(x) <- sprintf("item%d", seq_along(x))
print(x)

y <- as.list(x)
str(y)
stopifnot(identical(names(y), c("item1", "item2", "item3")))
stopifnot(identical(y[[1]], "a"), identical(y[[2]], "b"),
          identical(y[[3]], "c"))
x[[2]] <- "B"
stopifnot(identical(x$item2, "B"))


x <- listenv()
x[[1]] <- { 1 }
x[[3]] <- { "Hello world!" }
stopifnot(length(x) == 3)
stopifnot(identical(seq_along(x), seq_len(length(x))))
print(x)
names(x) <- c("a", "b", "c")
print(x)
x$b <- TRUE
stopifnot(identical(x[[1]], 1))
stopifnot(identical(x[[2]], TRUE))
stopifnot(identical(x$b, TRUE))
stopifnot(identical(x[["b"]], TRUE))
y <- as.list(x)
str(y)
stopifnot(length(y) == 3)


## Mixed names and indices
x <- listenv()
x$a <- 1
x[[3]] <- 3
print(names(x))
stopifnot(identical(names(x), c("a", "", "")))

# First element (should be named "a")
var <- get_variable(x, "a")
stopifnot(var == "a")
var <- get_variable(x, 1)
stopifnot(var == "a")

# Third element (should be a temporary name)
var <- get_variable(x, 3)
stopifnot(var != "c")
names(x) <- c("a", "b", "c")
var <- get_variable(x, 3)
stopifnot(var != "c")
var <- get_variable(x, "c")
stopifnot(var != "c")

## Second element (should become "b", because it was never used
#                  before it was "named" "b")
x$b <- 2
var <- get_variable(x, 2)
stopifnot(var == "b")
var <- get_variable(x, "b")
stopifnot(var == "b")


## Names where as.integer(names(x)) are integers
x <- listenv()
x[["1"]] <- 1
x[["3"]] <- 3
print(names(x))
stopifnot(identical(names(x), c("1", "3")))


## Expand and shrink
x <- listenv(a = 1, b = 2)
length(x) <- 2L
stopifnot(length(x) == 2L)

x <- listenv()
stopifnot(length(x) == 0L)
length(x) <- 3L
stopifnot(length(x) == 3L)
stopifnot(is.null(names(x)))

names(x) <- c("a", "b", "c")
x$a <- 2
stopifnot(identical(x$a, 2))
x[c("a", "c")] <- c(2, 1)
stopifnot(identical(x$a, 2), identical(x$c, 1))

length(x) <- 4L
stopifnot(length(x) == 4L)
stopifnot(identical(names(x), c("a", "b", "c", "")))

length(x) <- 1L
stopifnot(length(x) == 1L)
stopifnot(identical(names(x), c("a")))
stopifnot(identical(x$a, 2))

length(x) <- 0L
stopifnot(length(x) == 0L)
stopifnot(length(names(x)) == 0) # Actually, character(0), cf. lists


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Flatten
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
for (recursive in c(FALSE, TRUE)) {
  x <- list(); x$a <- list(B = 1:3); x$b <- list(C = 1:3, D = 4:5)
  y1 <- unlist(x, recursive = recursive)

  x <- listenv(); x$a <- list(B = 1:3); x$b <- list(C = 1:3, D = 4:5)
  y2 <- unlist(x, recursive = recursive)
  stopifnot(identical(y2, y1))
} # for (recursive ...)

x <- listenv(); x$a <- list(B = 1:3); x$b <- as.listenv(list(C = 1:3, D = 4:5))
y3 <- unlist(x, recursive = TRUE)
stopifnot(identical(y3, y1))

x <- listenv()
y <- unlist(x)
stopifnot(length(y) == 0)
stopifnot(is.null(y))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Comparisons
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(c = NULL, .a = 3, b = 1)
print(x)

## A list environment is always equal to itself
stopifnot(all.equal(x, x))

## List environments emulate lists
stopifnot(all.equal(x, list(c = NULL, .a = 3, b = 1)))
stopifnot(all.equal(x, list(c = NULL, .a = 3, b = 1), sorted = TRUE))
stopifnot(all.equal(x, list(.a = 3, b = 1, c = NULL), sorted = TRUE))

stopifnot(all.equal(x, list(c = NULL, b = 1), all.names = FALSE))
stopifnot(all.equal(x, list(.a = 3, c = NULL, b = 1), all.names = FALSE))
stopifnot(all.equal(x, list(b = 1, c = NULL), all.names = FALSE, sorted = TRUE))

res <- all.equal(x, list(b = 1, c = NULL), sorted = FALSE)
stopifnot(!isTRUE(res))

res <- all.equal(x, list(b = 1, c = NULL), all.names = FALSE)
stopifnot(!isTRUE(res))

## Assert listenv() -> as.list() -> as.listenv() equality
y <- as.list(x)
stopifnot(identical(names(y), names(x)))
z <- as.listenv(y)
stopifnot(identical(names(z), names(y)))
stopifnot(all.equal(x, y))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Warnings
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[1:3] <- 1:3
res <- tryCatch(x[1:2] <- 1:4, warning = function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(inherits(res, "try-warning"))

res <- tryCatch(x[1:3] <- 1:2, warning = function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(inherits(res, "try-warning"))

res <- tryCatch(x[integer(0L)] <- 1, warning = function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(!inherits(res, "try-warning"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
length(x) <- 3L
names(x) <- c("a", "b", "c")

res <- tryCatch(names(x) <- c("a", "b"), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[1:2]], error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[0]], error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[length(x) + 1]], error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[1 + 2i]], error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[1 + 2i], error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[1 + 2i]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[1 + 2i] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[integer(0L)]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[1:2]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[Inf]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[0]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[-1]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[character(0L)]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[c("a", "b")]] <- 1, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x[[""]] <- 1, error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## print() - various cases
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## print with 1 element
x <- listenv(a = 1)
print(x)

## print with no named elements
x <- listenv()
names(x) <- character(0L)
print(x)

## print matrix with all dimnames
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(c("r1", "r2"), c("c1", "c2", "c3"))
print(x)

## print matrix with only row names
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(c("r1", "r2"), NULL)
print(x)

## print matrix with only column names
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(NULL, c("c1", "c2", "c3"))
print(x)

## print matrix with NULL dimnames
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(NULL, NULL)
print(x)

## print matrix with no dimnames
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
print(x)

## print 3d-array with all dimnames
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
dimnames(x) <- list(letters[1:2], letters[1:3], letters[1:4])
print(x)

## print 3d-array with partial dimnames
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
dimnames(x) <- list(letters[1:2], NULL, letters[1:4])
print(x)

## print 3d-array with no dimnames
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
print(x)

## print 3d-array with NULL dimnames
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
dimnames(x) <- list(NULL, NULL, NULL)
print(x)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## map() is defunct
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
res <- tryCatch(map(x), error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Additional exception handling for assign/remove helpers
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
length(x) <- 3L
names(x) <- c("a", "b", "c")

## remove_by_name: non-existing name
x$nonexistent <- NULL

## remove_by_index: out of range (no-op)
x[[10L]] <- NULL

## [<- with zero-length replacement value
res <- tryCatch({ x[1:2] <- list() }, error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## all.equal.listenv - identical objects
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
stopifnot(isTRUE(all.equal(x, x)))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [[ with non-existing character name returns NULL
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
stopifnot(is.null(x[["nonexistent"]]))

## [[ on unassigned (NA placeholder) element returns NULL
x <- listenv()
length(x) <- 3L
stopifnot(is.null(x[[2]]))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [.listenv and [<-.listenv dimension mismatch
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- as.listenv(1:6)
dim(x) <- c(2, 3)

## [.listenv: wrong number of dimensions
res <- tryCatch(x[1, 2, 3], error = identity)
stopifnot(inherits(res, "error"))

## [<-.listenv: wrong number of dimensions
res <- tryCatch(x[1, 2, 3] <- 1, error = identity)
stopifnot(inherits(res, "error"))

## [<-.listenv: multi-dim NULL with wrong number of non-missing dims
res <- tryCatch(x[, ] <- NULL, error = identity)
stopifnot(inherits(res, "error"))

## [.listenv: mixed negative and positive subscripts
res <- tryCatch(x[c(-1, 1)], error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## as.listenv.environment
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
e <- new.env(parent = emptyenv())
e$x <- 1
e$y <- 2
x <- as.listenv(e)
stopifnot(length(x) == 2)
y <- as.list(x)
stopifnot(all(sort(names(y)) == c("x", "y")))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [[ with non-existing name returns NULL
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
stopifnot(is.null(x[["nonexistent"]]))

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Negative length error
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
res <- tryCatch(length(x) <- -1, error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## map() is defunct
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1)
res <- tryCatch(listenv::map(x), error = identity)
stopifnot(inherits(res, "error"))

## Test deprecated path of map()
Sys.setenv(R_LISTENV_MAP_DEPRECATED = "deprecated")
res <- withCallingHandlers(listenv::map(x), warning = function(w) {
  invokeRestart("muffleWarning")
})
stopifnot(is.character(res))
Sys.unsetenv("R_LISTENV_MAP_DEPRECATED")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Mixing positive and negative subscripts in [
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[1:3] <- 1:3
res <- tryCatch(x[c(1, -1)], error = identity)
stopifnot(inherits(res, "error"))

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [<- with zero-length replacement
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[1:3] <- 1:3
res <- tryCatch(x[1] <- list(), error = identity)
stopifnot(inherits(res, "error"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [[i, j]] on listenv without dimensions
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
res <- tryCatch(x[[1, 2]], error = identity)
stopifnot(inherits(res, "error"))
res <- tryCatch(x[[1, 2]] <- 3, error = identity)
stopifnot(inherits(res, "error"))

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## [[<- NULL with invalid names or indices
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(a = 1, b = 2)
for (name in list(character(0L), c("a", "b"), "")) {
  res <- tryCatch(x[[name]] <- NULL, error = identity)
  stopifnot(inherits(res, "error"))
}
for (i in list(integer(0L), 1:2, NA_real_, Inf, 0, -1)) {
  res <- tryCatch(x[[i]] <- NULL, error = identity)
  stopifnot(inherits(res, "error"))
}
stopifnot(identical(names(x), c("a", "b")), length(x) == 2L)


message("* x[[name]] with more or less than one name is an error ...")
x <- listenv(a = 1, b = 2)
for (name in list(character(0L), c("a", "b"), c("a", "c"))) {
  res <- tryCatch(x[[name]], error = identity)
  stopifnot(
    inherits(res, "error"),
    grepl("more than one element", conditionMessage(res))
  )
}


## Fractional indices are truncated toward zero, as for lists
message("* Fractional indices ...")
exprs <- list(
  quote(x[[1.5]]),
  quote(x[[3.9]]),
  quote(x[c(0.5, 2.9)]),
  quote(x[-1.5]),
  quote(x[c(-0.5, -1.5)]),
  quote(x[[1.5]] <- 9),
  quote(x[[0.5]] <- 9),
  quote(x[c(1.5, 3.9)] <- 9)
)
## For lists, x[[0.5]] gives x[[1]] in R (< 4.2.0)
if (getRversion() >= "4.2.0") {
  exprs <- c(exprs, list(
    quote(x[[0.5]])
  ))
}
for (expr in exprs) {
  message(sprintf("- %s", paste(deparse(expr), collapse = "")))
  is_assign <- identical(expr[[1]], as.symbol("<-"))
  x <- list(a = 1, b = 2, c = 3)
  truth <- tryCatch({
    res <- eval(expr)
    if (is_assign) x else res
  }, error = identity)
  x <- as.listenv(list(a = 1, b = 2, c = 3))
  res <- tryCatch({
    res <- eval(expr)
    if (is_assign) x else res
  }, error = identity)
  if (inherits(truth, "error")) {
    stopifnot(inherits(res, "error"))
  } else {
    if (inherits(res, "listenv")) res <- as.list(res)
    stopifnot(identical(res, truth))
  }
}

## x[[0.5]] is an error, because it is x[[0]], regardless of R version
x <- as.listenv(list(a = 1, b = 2, c = 3))
res <- tryCatch(x[[0.5]], error = identity)
stopifnot(inherits(res, "error"))
message("* Fractional indices ... DONE")


## A missing index gives NULL for [[, as for lists
message("* Missing [[ index ...")
x <- as.listenv(list(a = 1, b = 2, c = 3))
for (i in list(NA, NA_integer_, NA_real_, NaN, NA_character_)) {
  message(sprintf("- x[[%s]]", deparse(i)))
  stopifnot(is.null(x[[i]]))
}
message("* Missing [[ index ... DONE")


## Zero indices are dropped for [<-, as for lists
message("* Zero indices in [<- ...")
exprs <- list(
  quote(x[0] <- 9),
  quote(x[c(0, 2)] <- 9),
  quote(x[c(2, 0)] <- 9),
  quote(x[c(0, 0)] <- 9),
  quote(x[c(0, 2)] <- c(8, 9)),
  quote(x[c(0.5, 2)] <- 9),
  quote(x[c(0, 2)] <- NULL),
  quote(x[0] <- NULL)
)
for (expr in exprs) {
  message(sprintf("- %s", paste(deparse(expr), collapse = "")))
  x <- list(a = 1, b = 2, c = 3)
  truth_warning <- NULL
  withCallingHandlers({
    eval(expr)
  }, warning = function(w) {
    truth_warning <<- w
    invokeRestart("muffleWarning")
  })
  truth <- x
  x <- as.listenv(list(a = 1, b = 2, c = 3))
  res_warning <- NULL
  withCallingHandlers({
    eval(expr)
  }, warning = function(w) {
    res_warning <<- w
    invokeRestart("muffleWarning")
  })
  res <- as.list(x)
  stopifnot(
    identical(res, truth),
    is.null(res_warning) == is.null(truth_warning)
  )
}
message("* Zero indices in [<- ... DONE")


## Negative indices select all other elements for [<-, as for lists
message("* Negative indices in [<- ...")
exprs <- list(
  quote(x[-1] <- 9),
  quote(x[c(-1, -3)] <- 9),
  quote(x[c(-1, -1)] <- 9),
  quote(x[c(-1, 0)] <- 9),
  quote(x[-4] <- 9),
  quote(x[-1.5] <- 9),
  quote(x[-1] <- c(8, 9)),
  quote(x[-1] <- c(7, 8, 9)),
  quote(x[-1] <- NULL),
  quote(x[c(-1, -2, -3)] <- NULL),
  quote(x[c(-1, 2)] <- 9),
  quote(x[c(-1, NA)] <- 9)
)
for (expr in exprs) {
  message(sprintf("- %s", paste(deparse(expr), collapse = "")))
  x <- list(a = 1, b = 2, c = 3)
  truth_warning <- NULL
  truth <- tryCatch(withCallingHandlers({
    eval(expr)
    x
  }, warning = function(w) {
    truth_warning <<- w
    invokeRestart("muffleWarning")
  }), error = identity)
  x <- as.listenv(list(a = 1, b = 2, c = 3))
  res_warning <- NULL
  res <- tryCatch(withCallingHandlers({
    eval(expr)
    as.list(x)
  }, warning = function(w) {
    res_warning <<- w
    invokeRestart("muffleWarning")
  }), error = identity)
  if (inherits(truth, "error")) {
    stopifnot(
      inherits(res, "error"),
      ## A failed assignment must not modify 'x'
      identical(as.list(x), list(a = 1, b = 2, c = 3))
    )
  } else {
    stopifnot(
      identical(res, truth),
      is.null(res_warning) == is.null(truth_warning)
    )
  }
}
message("* Negative indices in [<- ... DONE")


## Missing indices give NULL elements for [, as for lists
message("* Missing indices in [ ...")
exprs <- list(
  quote(x[c(1, NA)]),
  quote(x[NA_real_]),
  quote(x[NA_integer_]),
  quote(x[c(NA_real_, NA_real_)]),
  quote(x[c(2, NaN)]),
  quote(x[c(0, NA)]),
  quote(x[c(1.5, NA)]),
  quote(x[c(-1, NA)])
)
for (named in c(TRUE, FALSE)) {
  for (expr in exprs) {
    message(sprintf("- %s (named = %s)", paste(deparse(expr), collapse = ""), named))
    x <- list(a = 1, b = 2, c = 3)
    if (!named) names(x) <- NULL
    truth <- tryCatch(eval(expr), error = identity)
    x <- as.listenv(x)
    res <- tryCatch(as.list(eval(expr)), error = identity)
    if (inherits(truth, "error")) {
      stopifnot(inherits(res, "error"))
    } else {
      stopifnot(identical(res, truth))
    }
  }
}
message("* Missing indices in [ ... DONE")


## Missing indices are ignored for [<-, as for lists, but only if
## the replacement value is of length one
message("* Missing indices in [<- ...")
exprs <- list(
  quote(x[c(1, NA)] <- 9),
  quote(x[NA_real_] <- 9),
  quote(x[NA_integer_] <- 9),
  quote(x[c(2, NaN)] <- 9),
  quote(x[c(1, NA, NA)] <- 9),
  quote(x[c(1, NA, NA)] <- list(9)),
  quote(x[c(1, NA)] <- c(8, 9)),
  quote(x[c(NA_real_, NA_real_)] <- c(8, 9)),
  quote(x[c(1, NA)] <- NULL),
  quote(x[NA_real_] <- NULL),
  quote(x[c(-1, NA)] <- 9)
)
for (expr in exprs) {
  message(sprintf("- %s", paste(deparse(expr), collapse = "")))
  x <- list(a = 1, b = 2, c = 3)
  truth <- tryCatch({
    eval(expr)
    x
  }, error = identity)
  x <- as.listenv(list(a = 1, b = 2, c = 3))
  res <- tryCatch({
    eval(expr)
    as.list(x)
  }, error = identity)
  if (inherits(truth, "error")) {
    stopifnot(
      inherits(res, "error"),
      ## A failed assignment must not modify 'x'
      identical(as.list(x), list(a = 1, b = 2, c = 3))
    )
  } else {
    stopifnot(identical(res, truth))
  }
}
message("* Missing indices in [<- ... DONE")


## Missing logical indices give NULL elements for [, as for lists
message("* Missing logical indices in [ and [<- ...")
exprs <- list(
  quote(x[NA]),
  quote(x[c(TRUE, NA)]),
  quote(x[c(NA, FALSE, TRUE)]),
  quote(x[c(TRUE, TRUE, TRUE, NA)]),
  quote(x[NA] <- 9),
  quote(x[c(TRUE, NA)] <- 9),
  quote(x[c(TRUE, NA)] <- c(8, 9)),
  quote(x[c(TRUE, NA)] <- NULL)
)
for (named in c(TRUE, FALSE)) {
  for (expr in exprs) {
    message(sprintf("- %s (named = %s)", paste(deparse(expr), collapse = ""), named))
    is_assign <- (as.character(expr[[1]]) == "<-")
    x <- list(a = 1, b = 2, c = 3)
    if (!named) names(x) <- NULL
    x0 <- x
    truth <- tryCatch({
      res <- eval(expr)
      if (is_assign) x else res
    }, error = identity)
    x <- as.listenv(x0)
    res <- tryCatch({
      res <- eval(expr)
      as.list(if (is_assign) x else res)
    }, error = identity)
    if (inherits(truth, "error")) {
      stopifnot(
        inherits(res, "error"),
        ## A failed assignment must not modify 'x'
        identical(as.list(x), x0)
      )
    } else {
      stopifnot(identical(res, truth))
    }
  }
}
message("* Missing logical indices in [ and [<- ... DONE")


## Cleanup
options(oopts)
rm(list = setdiff(ls(envir = globalenv()), ovars), envir = globalenv())
