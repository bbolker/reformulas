##' From the result of \code{\link{findbars}} applied to a model formula and
##' and the evaluation frame, create the model matrix, etc. associated with
##' random-effects terms.  See the description of the returned value for a
##' detailed list.
##'
##' @title Create list of structures needed for models with random effects
##' @param bars a list of parsed random-effects terms
##' @param fr a model frame in which to evaluate these terms
##' @param drop.unused.levels (logical) drop unused factor levels?
##' @param reorder.terms arrange random effects terms in decreasing order of number of groups (factor levels)?
##' @param reorder.vars arrange columns of individual random effects terms in alphabetical order?
##' @param calc.lambdat (logical) compute \code{Lambdat} and \code{Lind} components? (At present these components
##' are needed for \code{lme4} machinery but not for \code{glmmTMB}, and may be large in some cases; see Bates \emph{et al.} 2015
##' @return a list with components
##' \item{Zt}{transpose of the sparse model matrix for the random effects}
##'  \item{Ztlist}{list of components of the transpose of the
##'    random-effects model matrix, separated by random-effects term}
##' \item{Lambdat}{transpose of the sparse relative covariance factor}
##' \item{Lind}{an integer vector of indices determining the mapping of the
##'     elements of the \code{theta} to the \code{"x"} slot of \code{Lambdat}}
##' \item{theta}{initial values of the covariance parameters}
##' \item{lower}{lower bounds on the covariance parameters}
##' \item{flist}{list of grouping factors used in the random-effects terms}
##' \item{cnms}{a list of column names of the random effects according to
##'     the grouping factors}
##' \item{Gp}{a vector indexing the association of
##'    elements of the conditional mode vector
##'    with random-effect terms; if \code{nb} is the vector of numbers
##'    of conditional modes per term (i.e. number of groups times number
##'  of effects per group), \code{Gp} is \code{c(0,cumsum(nb))}
##'     (and conversely \code{nb} is \code{diff(Gp)})}
##' \item{nl}{names of the terms (in the same order as \code{Zt},
##'     i.e. reflecting the \code{reorder.terms} argument)}
##' @importFrom Matrix sparseMatrix drop0
## (no methods found in package 'Matrix' for rbind ... ???)
##' @importMethodsFrom Matrix coerce t diag
##' @importFrom Rdpack reprompt
##' @family utilities
##' @references \insertRef{lme4}{reformulas})
##' @export
mkReTrms <- function(bars, fr, drop.unused.levels=TRUE,
                     reorder.terms=TRUE,
                     reorder.vars=FALSE,
                     calc.lambdat = TRUE) {
  if (!length(bars))
    stop("No random effects terms specified in formula",call.=FALSE)
  stopifnot(is.list(bars), vapply(bars, is.language, NA),
            inherits(fr, "data.frame"))
  names(bars) <- barnames(bars)
  term.names <- vapply(bars, deparse1, "")
      ## get component blocks
      blist <- lapply(bars, mkBlist, fr, drop.unused.levels,
                      reorder.vars = reorder.vars)
      nl <- vapply(blist, `[[`, 0L, "nl")   # no. of levels per term
                                        # (in lmer jss:  \ell_i)
      
      ## order terms stably by decreasing number of levels in the factor
      if (reorder.terms) {
          if (any(diff(nl) > 0)) {
              ord <- rev(order(nl))
              blist      <- blist     [ord]
              nl         <- nl        [ord]
              term.names <- term.names[ord]
          }
      }
      Ztlist <- lapply(blist, `[[`, "sm")
      Zt <- do.call(rbind, Ztlist)  ## eq. 7, JSS lmer paper
      names(Ztlist) <- term.names
      q <- nrow(Zt)

      ## Create and install Lambdat, Lind, etc.  This must be done after
      ## any potential reordering of the terms.
      cnms <- lapply(blist, `[[`, "cnms")   # list of column names of the
                                            # model matrix per term
      nc <- lengths(cnms)                   # no. of columns per term
                                            # (in lmer jss:  p_i)
      nth <- as.integer((nc * (nc+1))/2)    # no. of parameters per term
                                            # (in lmer jss:  ??)
      nb <- nc * nl                         # no. of random effects per term
                                            # (in lmer jss:  q_i)
  ## eq. 5, JSS lmer paper
  if (sum(nb) != q) {
      stop(sprintf("total number of RE (%d) not equal to nrow(Zt) (%d)",
                   sum(nb),q))
  }
  boff <- cumsum(c(0L, nb))             # offsets into b
  thoff <- cumsum(c(0L, nth))           # offsets into theta
  ## FIXME: should this be done with cBind and avoid the transpose
  ## operator?  In other words should Lambdat be generated directly
  ## instead of generating Lambda first then transposing?
  if (calc.lambdat) {
      mk_b <-function(i) {
          mm <- matrix(seq_len(nb[i]), ncol = nc[i],
                       byrow = TRUE)
          dd <- diag(nc[i])
          ltri <- lower.tri(dd, diag = TRUE)
          ii <- row(dd)[ltri]
          jj <- col(dd)[ltri]
          ## unused: dd[cbind(ii, jj)] <- seq_along(ii)
          data.frame(i = as.vector(mm[, ii]) + boff[i],
                     j = as.vector(mm[, jj]) + boff[i],
                     x = as.double(rep.int(seq_along(ii),
                                           rep.int(nl[i], length(ii))) +
                                   thoff[i]))
      }
      Lambdat <- t(do.call(sparseMatrix,
                           do.call(rbind,
                                   lapply(seq_along(blist), mk_b))))
      Lind <- as.integer(Lambdat@x)
  } else {
      Lambdat <- Lind <- NULL
  }
  thet <- numeric(sum(nth))
  ll <- list(Zt = drop0(Zt), theta = thet, Lind = Lind,
             Gp = unname(c(0L, cumsum(nb))))
  ## lower bounds on theta elements are 0 if on diagonal, else -Inf
  ll$lower <- -Inf * (thet + 1)
  if (calc.lambdat) {
      ll$lower[unique(diag(Lambdat))] <- 0
      Lambdat@x[] <- ll$theta[ll$Lind]  # initialize elements of Lambdat
  }      
  ll$theta[] <- is.finite(ll$lower) # initial values of theta are 0 off-diagonal, 1 on
  ll$Lambdat <- Lambdat
  # massage the factor list
  fl <- lapply(blist, `[[`, "ff")
  # check for repeated factors
  fnms <- names(fl)
  if (length(fnms) > length(ufn <- unique(fnms))) {
    fl <- fl[match(ufn, fnms)]
    asgn <- match(fnms, ufn)
  } else asgn <- seq_along(fl)
  names(fl) <- ufn
  ## DON'T need fl to be a data.frame ...
  ## fl <- do.call(data.frame, c(fl, check.names = FALSE))
  attr(fl, "assign") <- asgn
  ll$flist <- fl
  ll$cnms <- cnms
  ll$Ztlist <- Ztlist
  ll$nl <- nl
  ll
} ## {mkReTrms}


##' @param x a language object of the form  effect | groupvar
##' @param frloc model frame
##' @param drop.unused.levels (logical)
##' @return list containing grouping factor, sparse model matrix, number of levels, names
##' @importFrom Matrix KhatriRao fac2sparse sparse.model.matrix
##' @importFrom stats model.matrix
##' @noRd
mkBlist <- function(x,frloc, drop.unused.levels=TRUE,
                    reorder.vars=FALSE) {
    frloc <- factorize(x,frloc)
    ## try to evaluate grouping factor within model frame ...
    ff0 <- replaceTerm(x[[3]], quote(`:`), quote(`%i%`))
    ff <- try(eval(substitute(makeFac(fac),
                              list(fac = ff0)),
                   frloc), silent = TRUE)
    if (inherits(ff, "try-error")) {
        stop("couldn't evaluate grouping factor ",
             deparse1(x[[3]])," within model frame:",
             "error =",
             c(ff),
             " Try adding grouping factor to data ",
             "frame explicitly if possible",call.=FALSE)
    }
    if (all(is.na(ff)))
        stop("Invalid grouping factor specification, ",
             deparse1(x[[3]]),call.=FALSE)
    ## NB: *also* silently drops <NA> levels - and mkReTrms() and hence
    ##     predict.merMod() have relied on that property  :
    if (drop.unused.levels) ff <- factor(ff, exclude=NA)
    nl <- length(levels(ff))
    ## this section implements eq. 6 of the JSS lmer paper
    ## model matrix based on LHS of random effect term (X_i)
    ##    x[[2]] is the LHS (terms) of the a|b formula
    has.sparse.contrasts <- function(x) {
      cc <- attr(x, "contrasts")
      !is.null(cc) && is(cc, "sparseMatrix")
    }
    any.sparse.contrasts <- any(vapply(frloc, has.sparse.contrasts, FUN.VALUE = TRUE))
    mMatrix <- if (!any.sparse.contrasts) model.matrix else sparse.model.matrix
    mm <- mMatrix(eval(substitute( ~ foo, list(foo = x[[2]]))), frloc)
    if (reorder.vars) {
        mm <- mm[colSort(colnames(mm)),]
    }
    ## this is J^T (see p. 9 of JSS lmer paper)
    ## construct indicator matrix for groups by observations
    ## use fac2sparse() rather than as() to allow *not* dropping
    ## unused levels where desired
    sm <- fac2sparse(ff, to = "d",
                     drop.unused.levels = drop.unused.levels)
    sm <- KhatriRao(sm, t(mm))
    dimnames(sm) <- list(
        rep(levels(ff),each=ncol(mm)),
        rownames(mm))
    list(ff = ff, sm = sm, nl = nl, cnms = colnames(mm))
}

##' @noRd
##' @param bars result of findbars
barnames <- function(bars) vapply(bars, function(x) deparse1(x[[3]]), "")

makeFac <- function(x,char.only=FALSE) {
    if (!is.factor(x) && (!char.only || is.character(x))) factor(x) else x
}

factorize <- function(x,frloc,char.only=FALSE) {
    ## convert grouping variables to factors as necessary
    ## TODO: variables that are *not* in the data frame are
    ##  not converted -- these could still break, e.g. if someone
    ##  tries to use the : operator
    ## TODO: some sensible tests for drop.unused.levels
    ##       (not actually used, but could come in handy)
    for (i in all.vars(RHSForm(x))) {
        if (!is.null(curf <- frloc[[i]]))
            frloc[[i]] <- makeFac(curf,char.only)
    }
    return(frloc)
}

colSort <- function(x) {
    termlev <- vapply(strsplit(x,":"),length,integer(1))
    iterms <- split(x,termlev)
    iterms <- sapply(iterms,sort,simplify=FALSE)
    ## make sure intercept term is first
    ilab <- "(Intercept)"
    if (ilab %in% iterms[[1]]) {
        iterms[[1]] <- c(ilab,setdiff(iterms[[1]],ilab))
    }
    unlist(iterms)
}

## infix interaction operator (more careful)
`%i%` <- function(f1, f2, fix.order = TRUE) {
    if (!is.factor(f1) || !is.factor(f2)) stop("both inputs must be factors")
    f12 <- paste(f1, f2, sep = ":")
    ## explicitly specifying levels is faster in any case ...
    u <- which(!duplicated(f12))
    if (!fix.order) return(factor(f12, levels = f12[u]))
    ## deal with order of factor levels
    levs_rank <- length(levels(f2))*as.numeric(f1[u])+as.numeric(f2[u])
    return(factor(f12, levels = (f12[u])[order(levs_rank)]))
}

## was called "replaceForm" there but replaceTerm is better
## (decide on camelCase vs snake_case!)
replaceTerm <- function(term,target,repl) {
    if (identical(term,target)) return(repl)
    if (!inForm(term,target)) return(term)
    if (length(term) == 2) {
        return(substitute(OP(x),list(OP=replaceTerm(term[[1]],target,repl),
                                     x=replaceTerm(term[[2]],target,repl))))
    }
    return(substitute(OP(x,y),list(OP=replaceTerm(term[[1]],target,repl),
                                   x=replaceTerm(term[[2]],target,repl),
                                   y=replaceTerm(term[[3]],target,repl))))
}

