#' Helper function to infer target from expression and environment
#'
#' @param expr An expression.
#' 
#' @param envir An environment.
#' 
#' @param substitute If `TRUE`, then the expression is [base::substitute()]:ed,
#' otherwise not.
#'
#' @param is_variable (logical) Ignored. Any name is a valid variable
#' name, as for [base::assign()].
#' 
#' @return A named list with elements:
#' \describe{
#'  \item{`envir`}{An environment (defaults to argument `envir`)}
#'  \item{`name`}{A character vector. ...}
#'  \item{`op`}{...}
#'  \item{`subset`}{A list of subsetting indices or `NULL`. ...}
#'  \item{`idx`}{An integer vector or `NA_integer_`. ...}
#'  \item{`exists`}{A logical vector of length `length(idx)` with `TRUE`
#'        and `FALSE` values.}
#'  \item{`code`}{The deparsed expression `expr` coerced to a single character
#'        string.}
#' }
#'
#' @export
#' @keywords internal
parse_env_subset <- function(expr, envir = parent.frame(), substitute = TRUE, is_variable = TRUE) {
  if (substitute) expr <- substitute(expr)
  code <- paste(deparse(expr), collapse = "")

  res <- list(envir = envir, name = "", op = NULL, subset = NULL,
              idx = NA_integer_, exists = NA, code = code)

  if (is.symbol(expr)) {
    ## Element specified as a symbol
    res$name <- deparse(expr)
  } else if (is.character(expr)) {
    ## Element specified as a name
    if (length(expr) > 1L) {
      stopf("Does not specify a single name, but %d: %s",
            length(expr), hpaste(sQuote(expr)), call. = FALSE)
    }
    res$name <- expr
  } else if (is.numeric(expr)) {
    ## Element specified as a subset of envir
    if (length(expr) > 1L) {
      stopf("Does not specify a single index, but %d: %s",
            length(expr), hpaste(sQuote(expr)), call. = FALSE)
    }
    res$subset <- list(expr)
  } else {
    n <- length(expr)
    stop_if_not(n >= 2L)

    if (n >= 3L) {
      ## Assignment to environment via $ and [[
      op <- as.character(expr[[1]])
      res$op <- op
      if (op == "$" && n > 3L) {
        stopf("Invalid syntax: %s", sQuote(code), call. = FALSE)
      } else if (!is.element(op, c("$", "[[", "["))) {
        stopf("Invalid syntax: %s", sQuote(code), call. = FALSE)
      }

      ## Target
      objname <- deparse(expr[[2]])
      if (!exists(objname, envir = envir, inherits = TRUE)) {
        stopf("Object %s not found: %s", sQuote(objname), sQuote(code),
              call. = FALSE)
      }

      obj <- get(objname, envir = envir, inherits = TRUE)
      if (!is.environment(obj)) {
        stopf("Subsetting can not be done on a %s; only to an environment: %s",
              sQuote(mode(obj)), sQuote(code), call. = FALSE)
      }
      res$envir <- obj

      ## Check whether an empty symbol or not
      is_empty <- local({
        symbol <- alist(empty=)
	function(x) identical(x, symbol$empty)
      })
      
      ## Subset
      subset <- list()
      for (kk in 3:n) {
        if (is_empty(expr[[kk]])) {
          subset_kk <- NULL
        } else {
          subset_kk <- expr[[kk]]
        }
        if (is.symbol(subset_kk)) {
          subset_kk <- deparse(subset_kk)
          ## Symbols are names for x$name, but variables for x[[i]] and x[i]
          if (op != "$") {
            if (!exists(subset_kk, envir = envir, inherits = TRUE)) {
              stopf("Object %s not found: %s",
                    sQuote(subset_kk), sQuote(code), call. = FALSE)
            }
            subset_kk <- get(subset_kk, envir = envir, inherits = TRUE)
          }
        } else if (is.language(subset_kk)) {
          subset_kk <- eval(subset_kk, envir = envir, enclos = baseenv())
        }
        if (is_empty(expr[[kk]])) {
          ## An empty subset, e.g. x[], selects all elements
          subset[kk - 2L] <- list(NULL)
        } else {
          ## ... whereas a NULL subset, e.g. x[NULL], selects nothing
          if (is.null(subset_kk)) subset_kk <- integer(0L)
          subset[[kk - 2L]] <- subset_kk
        }
      }

      res$subset <- subset
    } # if (n >= 3)
  } # if (is.symbol(expr))


  ## Validate subsetting, e.g. x[[1]], x[["a"]], and x$a, iff any
  subset <- res$subset
  if (!is.null(subset)) {
    if (!is.list(subset)) {
      stopf("INTERNAL ERROR (expected 'subset' to be a list): %s",
            sQuote(code), call. = FALSE)
    }
    if (length(subset) == 0L) {
      stopf("Subsetting of at least one element is required: %s",
            sQuote(code), call. = FALSE)
    }

    op <- res$op
    if (is.null(op)) op <- "[["

    for (kk in seq_along(subset)) {
      subset_kk <- subset[[kk]]
      if (op == "[[" && length(subset_kk) == 0L) {
        if (is.null(subset_kk)) {
          stopf("Invalid subset for dimension #%d. Subset must not be missing: %s",
                kk, sQuote(code), call. = FALSE)
        }
        stopf("Invalid subset for dimension #%d. Subset must not be empty: %s",
              kk, sQuote(code), call. = FALSE)
      } else if (is.null(subset_kk)) {
      } else if (any(is.na(subset_kk))) {
        stopf("Invalid subsetting for dimension #%d. Subset must not contain missing values: %s",
              kk, sQuote(code), call. = FALSE)
      } else if (is.character(subset_kk)) {
        if (!all(nzchar(subset_kk))) {
          stopf("Invalid subset for dimension #%d. Subset must not contain empty names: %s",
                kk, sQuote(code), call. = FALSE)
        }
      } else if (is.numeric(subset_kk)) {
        ## Fractional indices are truncated toward zero, as for lists
        subset[[kk]] <- trunc(subset_kk)
      } else {
        stopf("Invalid subset for dimension #%d of type %s: %s",
	      kk, sQuote(typeof(subset_kk)), sQuote(code), call. = FALSE)
      }
    } # for (kk ...)
    res$subset <- subset

    ## Special: listenv:s
    envir <- res$envir
    stop_if_not(is.environment(envir))

    if (inherits(envir, "listenv")) {
      names <- names(envir)
      map <- mapping(envir)
      dim <- dim(envir)

      ## Multi-dimensional subsetting?
      if (length(subset) > 1L) {
        if (is.null(dim)) {
          stopf("Multi-dimensional subsetting on list environment without dimensions: %s", sQuote(code), call. = FALSE)  #nolint
        }
        if (length(subset) != length(dim)) {
          stopf("Incorrect number of subscripts (%d) for a list environment with %d dimensions: %s",  #nolint
                length(subset), length(dim), sQuote(code), call. = FALSE)
        }
        ## Expand NULL indices, and validate [[ indices
        for (kk in seq_along(subset)) {
          subset_kk <- subset[[kk]]
          if (is.null(subset_kk)) {
            subset[[kk]] <- seq_len(dim[kk])
          } else if (op == "[[" && is.numeric(subset_kk)) {
            if (any(subset_kk == 0)) {
              stopf("Invalid (zero) indices for dimension #%d (%s): %s",
                    kk, hpaste(subset_kk), sQuote(code), call. = FALSE)
            } else if (any(subset_kk < 0)) {
              stopf("Invalid (negative) indices for dimension #%d (%s): %s",
                    kk, hpaste(subset_kk), sQuote(code), call. = FALSE)
            }
          }
        }

        ## Map to linear indices, where out-of-bounds indices become NA
        idx <- to_index(envir, subset, on_out_of_bound = "NA")
        dimnames(idx) <- NULL
        names(dim(idx)) <- NULL

        res$idx <- idx
        res$name <- names[res$idx]
      } else {
        subset <- subset[[1L]]
        if (is.numeric(subset)) {
          i <- subset
          n <- length(envir)
          if (any(i < 0)) {
            if (op == "[[") {
              stopf("Invalid (negative) indices (%s): %s",
                    hpaste(i), sQuote(code), call. = FALSE)
            } else if (any(i > 0)) {
              stopf("Only 0's may be mixed with negative subscripts: %s",
                    sQuote(code), call. = FALSE)
            }
            ## Drop elements
            i <- setdiff(seq_len(n), -i)
          }
          ## Drop zeros?
          keep <- which(i != 0)
          if (length(keep) != length(i)) {
            if (op == "[[") {
              stopf("Invalid (zero) indices (%s): %s",
                    hpaste(i), sQuote(code), call. = FALSE)
            }
            i <- i[keep]
          }
          res$idx <- i
          res$name <- names[i]
        } else if (is.character(subset)) {
          res$idx <- match(subset, names)
        } else if (is.null(subset)) {
	  res$idx <- seq_len(length(envir))
	}
      }
    } else {
      if (length(subset) > 1L) {
        stopf("Invalid subset: %s", sQuote(code), call. = FALSE)
      }
      subset <- subset[[1L]]
      if (length(subset) > 1L) {
        stopf("Wrong arguments for subsetting an environment: %s",
	      sQuote(code), call. = FALSE)
      }
      if (!is.character(subset)) {
        stopf("Wrong arguments for subsetting an environment: %s",
	      sQuote(code), call. = FALSE)
      }
    }
    
    if (is.character(subset)) {
      res$name <- subset
    }
  }

  if (length(res$name) == 0L) res$name <- ""

  ## Identify index?
  if (inherits(res$envir, "listenv")) {
    envir <- res$envir
    if (length(res$name) == 1L && any(is.na(res$idx)) && nzchar(res$name)) {
      res$idx <- match(res$name, names(envir))
    }
    res$exists <- !is.na(res$idx) & !is.na(mapping(envir)[res$idx])
  }

  ## Validate
  if (is.null(dim) && length(res$subset) == 1 && identical(res$op, "[")) {
    if (any(is.na(res$idx) & !nzchar(res$name))) {
      stopf("Invalid subset: %s", sQuote(code), call. = FALSE)
    }
  }

  unknown <- which(is.na(res$exists))
  if (length(unknown) > 0) {
    ## Variables, e.g. 'a', may be inherited, but not elements of an
    ## environment, e.g. x$a and x[["a"]]
    inherits <- is.null(res$op)
    res$exists[unknown] <- sapply(unknown, FUN = function(idx) {
      exists(res$name[idx], envir = res$envir, inherits = inherits)
    })
  }

  ## Sanity check
  stop_if_not(is.environment(res$envir))
  stop_if_not(is.character(res$name))
  stop_if_not(is.null(res$subset) || is.list(res$subset))
  stop_if_not(is.null(res$idx) || all(is.numeric(res$idx)))
  stop_if_not(is.logical(res$exists), !anyNA(res$exists))
  stop_if_not(length(res$exists) == length(res$idx))

  res
}
