library("listenv")

message("*** aperm() and t() ...")

for (ndim in 0:5) {
  message("- Number of dimensions: ", ndim)

  if (ndim == 0) {
    n <- 3L
    X_truth <- as.list(seq_len(n))
    names(X_truth) <- letters[seq_len(n)]
  } else {
    dim <- seq_len(ndim) + 2L
    dimnames <- lapply(dim, FUN = function(n) letters[seq_len(n)])
    X_truth <- as.list(seq_len(prod(dim)))
    dim(X_truth) <- dim
    dimnames(X_truth) <- dimnames
  }
  
  X <- as.listenv(X_truth)
  stopifnot(identical(as.list(X), X_truth))
  if (ndim <= 1L) {
    stopifnot(!is.null(names(X)) && !is.null(names(X_truth)))
    stopifnot(identical(names(X), names(X_truth)))
  } else {
    stopifnot(is.null(names(X)) && is.null(names(X_truth)))
  }

  if (ndim > 0) {
    message("- aperm()")
    for (kk in 1:10) {
      perm <- sample(seq_len(ndim), replace = FALSE)
      X_truth <- aperm(X_truth, perm = perm)
      X <- aperm(X, perm = perm)
      stopifnot(identical(as.list(X), X_truth))
    }
  }

  if (ndim <= 2) {
    message("- t()")
    X_truth <- t(X_truth)
    X <- t(X)
    ## For comparision: t(<listenv>) preserves element names
    names(X) <- NULL
    stopifnot(identical(as.list(X), X_truth))
  }
} ## for (ndim ...)

message("*** aperm() and t() ... DONE")


message("*** aperm() and t() - exceptions ...")

## aperm on non-array
x <- as.listenv(1:3)
res <- try(aperm(x, perm = 1), silent = TRUE)
stopifnot(inherits(res, "try-error"))

## aperm with wrong 'perm' length
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
res <- try(aperm(x, perm = 1:3), silent = TRUE)
stopifnot(inherits(res, "try-error"))

## aperm with out-of-range 'perm'
res <- try(aperm(x, perm = c(1, 3)), silent = TRUE)
stopifnot(inherits(res, "try-error"))

## aperm with duplicated 'perm'
res <- try(aperm(x, perm = c(1, 1)), silent = TRUE)
stopifnot(inherits(res, "try-error"))

## aperm identity (no-op)
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- list(c("r1", "r2"), c("c1", "c2", "c3"))
y <- aperm(x, perm = 1:2)
stopifnot(identical(as.list(y), as.list(x)))


message("*** t.listenv - 1D array ...")
x <- as.listenv(1:3)
dim(x) <- 3L
y <- t(x)
stopifnot(identical(dim(y), c(1L, 3L)))

message("*** t.listenv - error for 3D array ...")
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
res <- try(t(x), silent = TRUE)
stopifnot(inherits(res, "try-error"))

message("*** aperm() and t() - exceptions ... DONE")

