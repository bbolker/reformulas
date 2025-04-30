## suppose we have two formulas

f1 <- y ~ x1 + x2 + s(x3)
f2 <- y ~ x1 + x2 + s
## unrealistic but perhaps challenging
f3 <- y ~ x1 + x2 + foo(s(x3))

## our function should recognize f1, but not f2, as containing
## a term of the form s(...)

rfun <- function(expr) {
    if (length(expr) == 1) return(NULL)  ## we've hit bottom
    if (identical(expr[[1]], quote(s))) return(TRUE)
    lapply(expr[-1], rfun)
}
rfun(f1)  ## not really what we want ...

## this works ... (isTRUE(NULL) is FALSE)
isTRUE(unlist(rfun(f1)))
isTRUE(unlist(rfun(f2)))
isTRUE(unlist(rfun(f3)))

## alternatively, set a top-level flag ...

has_s <- FALSE
rfun2 <- function(expr) {
    if (length(expr) == 1) return(NULL)  ## we've hit bottom
    if (identical(expr[[1]], quote(s))) {
        assign("has_s", TRUE, envir = environment(rfun2))
        return(NULL)
    }
    lapply(expr[-1], rfun2)
}
invisible(rfun2(f1))
print(has_s)
