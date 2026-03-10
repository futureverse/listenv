stopf <- listenv:::stopf
warnf <- listenv:::warnf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stopf()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
res <- tryCatch(stopf("Error: %d", 42), error = identity)
stopifnot(inherits(res, "error"))
stopifnot(res$message == "Error: 42")
stopifnot(!is.null(res$call))

res <- tryCatch(stopf("Error: %d", 42, call. = FALSE), error = identity)
stopifnot(is.null(res$call))

my_call <- call("foo", x = 1)
res <- tryCatch(stopf("Error: %d", 42, call. = my_call), error = identity)
stopifnot(identical(res$call, my_call))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# warnf()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
res <- tryCatch(warnf("Warning: %d", 42), warning = identity)
stopifnot(inherits(res, "warning"))
stopifnot(res$message == "Warning: 42")
stopifnot(!is.null(res$call))

res <- tryCatch(warnf("Warning: %d", 42, call. = FALSE), warning = identity)
stopifnot(is.null(res$call))

res <- tryCatch(warnf("Warning: %d", 42, call. = my_call), warning = identity)
stopifnot(identical(res$call, my_call))

res <- tryCatch(warnf("Warning: %d", 42, immediate. = TRUE), warning = identity)
stopifnot(inherits(res, "warning"))
