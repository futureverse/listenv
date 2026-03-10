printf <- function(...) cat(sprintf(...))
hpaste <- listenv:::hpaste
stop_if_not <- listenv:::stop_if_not

# Some vectors
x <- 1:6
y <- 10:1
z <- LETTERS[x]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# hpaste()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Abbreviation of output vector
printf("x = %s.\n", hpaste(x))
## x = 1, 2, 3, ..., 6.

printf("x = %s.\n", hpaste(x, max_head = 2))
## x = 1, 2, ..., 6.

printf("x = %s.\n", hpaste(x, max_head = 3)) # Default
## x = 1, 2, 3, ..., 6.

# It will never output 1, 2, 3, 4, ..., 6
printf("x = %s.\n", hpaste(x, max_head = 4))
## x = 1, 2, 3, 4, 5 and 6.

# Showing the tail
printf("x = %s.\n", hpaste(x, max_head = 1, max_tail = 2))
## x = 1, ..., 5, 6.

# Turning off abbreviation
printf("y = %s.\n", hpaste(y, max_head = Inf))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

## ...or simply
printf("y = %s.\n", paste(y, collapse = ", "))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Adding a special separator before the last element
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Change last separator
printf("x = %s.\n", hpaste(x, last_collapse = " and "))
## x = 1, 2, 3, 4, 5 and 6.


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Edge cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_if_not(length(hpaste(character(0L))) == 0L)
stop_if_not(identical(hpaste("a", collapse = NULL), "a"))
stop_if_not(identical(hpaste(1:3, sep = "-", last_collapse = " and "), "1, 2 and 3"))
stop_if_not(identical(hpaste(1:3, collapse = " | ", last_collapse = " & "), "1 | 2 & 3"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stop_if_not()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_if_not(TRUE)
stop_if_not(TRUE, TRUE)

res <- tryCatch(stop_if_not(FALSE), error = identity)
stop_if_not(inherits(res, "error"))
stop_if_not(grepl("FALSE", res$message), grepl("is not TRUE", res$message))

res <- tryCatch(stop_if_not(NA, TRUE), error = identity)
stop_if_not(inherits(res, "error"))

res <- tryCatch(stop_if_not(TRUE, FALSE), error = identity)
stop_if_not(inherits(res, "error"))
stop_if_not(grepl("FALSE", res$message), grepl("is not TRUE", res$message))

res <- tryCatch(stop_if_not(1 == 2), error = identity)
stop_if_not(inherits(res, "error"))
stop_if_not(grepl("1 == 2", res$message), grepl("is not TRUE", res$message))

res <- tryCatch(stop_if_not(nchar("abc") == 2), error = identity)
stop_if_not(inherits(res, "error"))
stop_if_not(grepl("nchar\\(\"abc\"\\) == 2", res$message), grepl("is not TRUE", res$message))

# Long expressions
res <- tryCatch(stop_if_not(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 == 0), error = identity)
stop_if_not(inherits(res, "error"))
print(res$message)
stop_if_not(grepl("[...]", res$message))

stop_if_not()
