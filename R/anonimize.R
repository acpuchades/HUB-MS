anonimize <- function(x, ...) {
  UseMethod("anonimize", x)
}

anonimize.default <- function(x) x
