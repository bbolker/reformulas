##' Remove the random-effects terms from a mixed-effects formula,
##' thereby producing the fixed-effects formula.
##'
##' @title Omit terms separated by vertical bars in a formula
##' @param term the right-hand side of a mixed-model formula
##' @return the fixed-effects part of the formula
##' @section Note: This function is called recursively on individual
##' terms in the model, which is why the argument is called \code{term} and not
##' a name like \code{form}, indicating a formula.
##' @examples
##' nobars(Reaction ~ Days + (Days|Subject)) ## => Reaction ~ Days
##' @seealso \code{\link{formula}}, \code{\link{model.frame}}, \code{\link{model.matrix}}.
##' @family utilities
##' @keywords models utilities
##' @importFrom methods is
##' @importFrom stats as.formula reformulate terms
##' @export
nobars <- function(term) {
    e <- environment(term)
    nb <- nobars_(term)  ## call recursive version
    if (is(term,"formula") && length(term)==3 && is.symbol(nb)) {
        ## called with two-sided RE-only formula:
        ##    construct response~1 formula
        nb <- reformulate("1", response=deparse(nb))
    }
    ## called with one-sided RE-only formula, or RHS alone
    if (is.null(nb)) {
        nb <- if (is(term,"formula")) ~1 else 1
    }
    environment(nb) <- e
    nb
}

nobars_ <- function(term)
{
    if (!anyBars(term)) return(term)
    if (isBar(term)) return(NULL)
    if (isAnyArgBar(term)) return(NULL)
    if (length(term) == 2) {
        nb <- nobars_(term[[2]])
        if(is.null(nb)) return(NULL)
        term[[2]] <- nb
        return(term)
    }
    nb2 <- nobars_(term[[2]])
    nb3 <- nobars_(term[[3]])
    if (is.null(nb2)) return(nb3)
    if (is.null(nb3)) return(nb2)
    term[[2]] <- nb2
    term[[3]] <- nb3
    term
}

isBar <- function(term) {
    if(is.call(term)) {
        if((term[[1]] == as.name("|")) || (term[[1]] == as.name("||"))) {
            return(TRUE)
        }
    }
    FALSE
}

isAnyArgBar <- function(term) {
    if ((term[[1]] != as.name("~")) && (term[[1]] != as.name("("))) {
        for(i in seq_along(term)) {
            if(isBar(term[[i]])) return(TRUE)
        }
    }
    FALSE
}

anyBars <- function(term) {
    any(c('|','||') %in% all.names(term))
}
