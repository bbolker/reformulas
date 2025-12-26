
##' Format columns corresponding to std. dev. and/or variance.
##' @param reStdDev a vector of standard deviations.
##' @param use.c a character vector indicating which scales to include.
##' @param digits number of significant digits.
##' @param formatter formatting function.
##' @param ... additional arguments to formatter.
##' @export
## FIXME: avoid repeating defaults
format_sdvar <- function(reStdDev, use.c = "Std.Dev.", formatter=format,
                         digits = max(3, getOption("digits") - 2), ...) {
  res <- list()
  if("Variance" %in% use.c)
    res <- c(res,
             list(Variance = formatter(unlist(reStdDev)^2, digits = digits, ...)))
  if("Std.Dev." %in% use.c)
    res <- c(res, list(`Std.Dev.`=formatter(unlist(reStdDev),   digits = digits, ...)))
  mat <- do.call(cbind, res)
  colnames(mat) <- names(res)
  rownm <- names(res[[1]])  %||% ""
  mat <- cbind(Name = rownm, mat)
  rownames(mat) <- NULL
  return(mat)
}


##' @rdname format_sdvar
##' @param x a square numeric matrix
##' @param maxdim maximum number of rows/columns to display
##' @param digits digits for format
##' @param maxlen maximum number of rows to display
##' @param ... additional parameters
## FIXME: avoid repeating defaults
##' @export
format_corr <- function(x, maxdim=Inf, digits=2, maxlen = 10, ...) {
  UseMethod("format_corr")
}

##' @rdname format_sdvar
##' @export
get_sd <- function(x, ...) {
  UseMethod("get_sd")
}

##' @export
get_sd.default <- function(x, ...) {
  attr(x, "stddev")
}

##' @export
get_sd.vcmat_ar1 <- function(x, ...) {
  attr(x, "stddev")[1]
}

##' @export
get_sd.vcmat_homcs <- get_sd.vcmat_ar1

##' @export
get_sd.vcmat_homdiag <- get_sd.vcmat_ar1

corr_missing <- function(x) {
  identical(c(x), NaN) || identical(c(x), NA_real_)
}

##' @export
format_corr.default <- function(x, maxdim = Inf, digits=2, ...) {
  if (length(x)==0) return("")
  x <- attr(x, "correlation")
  x <- as(x, "matrix")
  extra_rows <- (nrow(x) > maxdim)
  newdim <- min(maxdim, nrow(x))
  if (corr_missing(x)) {
    cc <- matrix("(not stored)")
  } else {
    x <- x[1:newdim, 1:newdim, drop = FALSE]
    cc <- format(round(x, digits), nsmall = digits)
    cc[upper.tri(cc, diag = TRUE)] <- ""  ## empty upper triangle
    if (extra_rows) cc <- rbind(cc, "...")
  }
  cc
}

#' @export
format_corr.vcmat_diag <- function(x, maxdim = Inf, digits=2, ...) {
  ## empty correlation
  return(matrix(""))
}

#' @export
format_corr.vcmat_homdiag <- format_corr.vcmat_diag

## generic formatting function: name *without* a dot so roxygen doesn't mistake it for
## an S3 method
format_corr_vcmat_onecorr <- function(x, maxdim = Inf, digits=2, ..., tag = "") {
  x <- attr(x, "correlation")
  cc <- if (corr_missing(x)) {
          "(not stored)"
        } else if (length(x) == 1) {
          format(round(x, digits), nsmall = digits)
        } else {
          format(round(x[2,1], digits), nsmall = digits)
        }
  return(matrix(paste(cc, sprintf("(%s)", tag))))
}

#' @export
format_corr.vcmat_ar1 <- function(x, maxdim = Inf, digits=2, ...) {
  format_corr_vcmat_onecorr(x, maxdim = maxdim, digits = digits, ..., tag  = "ar1")
}

#' @export
format_corr.vcmat_hetar1 <- format_corr.vcmat_ar1

#' @export
format_corr.vcmat_cs <- function(x, maxdim = Inf, digits=2, ...) {
  format_corr_vcmat_onecorr(x, maxdim = maxdim, digits = digits, ..., tag  = "cs")
}

#' @export
format_corr.vcmat_homcs <- format_corr.vcmat_cs

## FIXME: get specials for ou, compsymm, spatial matrices, etc..

##' "\code{format()}" the \code{VarCorr} matrix of the random effects -- for
##' \code{print()}ing and \code{show()}ing
##'
##' @title Format the 'VarCorr' Matrix of Random Effects
##' @param varcor a \code{VarCorr} (-like) matrix with attributes.
##' @param digits the number of significant digits for standard deviations and variances.
##' @param corr_digits the number of significant digits for correlations.
##' @param comp character vector of length one or two indicating which
##' columns out of "Variance" and "Std.Dev." should be shown in the
##' formatted output.
##' @param formatter the \code{\link{function}} to be used for
##' formatting the standard deviations and or variances (but
##' \emph{not} the correlations which (currently) are always formatted
##' as "0.nnn".
##' @param useScale whether to report a scale parameter (e.g. residual standard deviation).
##' @param maxdim maximum dimensions (numbers of standard deviations/variances and number of
##' rows of correlation matrices) to report per random effects term.
##' @param ... optional arguments for \code{formatter(*)} in addition
##' to the first (numeric vector) and \code{digits}.
##' @return a character matrix of formatted VarCorr entries from \code{varcor}.
##' @export
##' @importFrom methods as
formatVC <- function(varcor, digits = max(3, getOption("digits") - 2),
                     corr_digits = max(2, digits-2),
                     maxdim = 10,
                     comp = "Std.Dev.", formatter = format,
                     useScale = attr(varcor, "useSc"),
                     ...)
{
  comp_opts <- c("Variance", "Std.Dev.")
  if(anyNA(mcc <- pmatch(comp, comp_opts))) {
    stop("Illegal 'comp': ", comp[is.na(mcc)])
  }
  use.c <- comp_opts[mcc]
  if (length(use.c) == 0) {
    stop("Must report either standard deviations or variances")
  }
  
  termnames <- names(varcor)
  
  ## get std devs (wait until after processing useScale to create output matrices)
  ## ugh, want to restrict lengths of sd that get reported: do we need methods/special
  ##   cases for this as well?
  
  reStdDev <- lapply(varcor, get_sd)
  
  ## get corr outputs
  corr_out <- lapply(varcor, format_corr, digits = corr_digits, maxdim = maxdim)
  
  if(useScale) {
    reStdDev <- c(reStdDev,
                  list(Residual = unname(attr(varcor, "sc"))))
    termnames <- c(termnames, "Residual")
    ## dummy correlation for Residual
    corr_out <- c(corr_out, list(matrix("")))
  }
  
  ## in order to get everything formatted consistently we have to collapse the std devs to a single
  ## vector, format them all at once, then split them back up (e.g. to insert extra spaces where necessary)
  
  trunc_rows <- function(x) {
    if (nrow(x) > maxdim) {
      x <- rbind(x[1:maxdim,,drop = FALSE], rep("...", ncol(x)))
    }
    return(x)
  }
  
  formatted_sdvar <- format_sdvar(unlist(unname(reStdDev)), digits = digits, comp = comp_opts, formatter = formatter, use.c = use.c)
  ## split back into chunks
  sdvar_out <- split.data.frame(formatted_sdvar,
                                rep(seq(length(reStdDev)), lengths(reStdDev)))
  sdvar_out <- lapply(sdvar_out, trunc_rows)
  
  names(sdvar_out) <- names(reStdDev)
  
  ## stick it all back together, properly spaced
  assemble_sdcor(sdvar_out, corr_out, termnames)
}

pad_blank <- function(m, max_rows=0, max_cols=0) {
  m <- as.matrix(m) ## handle scalar case
  if ((xrows <- (max_rows - nrow(m))) > 0) {
    m <- rbind(m, matrix("", nrow = xrows, ncol = ncol(m)))
  }
  if ((xcols <- (max_cols - ncol(m))) > 0) {
    m <- cbind(m, matrix("", ncol = xcols, nrow = nrow(m)))
  }
  return(m)
}

## patch together sd/var info, correlation info, group names
assemble_sdcor <- function(sdvar_out, corr_out, termnames) {
  
  sdvar_rows <- vapply(sdvar_out, nrow, numeric(1))
  corr_rows <- vapply(corr_out, nrow, numeric(1))
  max_rows <- pmax(sdvar_rows, corr_rows)
  
  nt <- length(corr_out)
  corr_cols <- vapply(corr_out, ncol, numeric(1))
  max_cols <- rep(max(corr_cols), nt)
  
  termnames_out <- mapply(pad_blank, termnames, max_rows, SIMPLIFY = FALSE)
  termnames_out <- do.call(rbind, termnames_out)
  colnames(termnames_out) <- "Groups"
  
  sdvar_out <- mapply(pad_blank, sdvar_out, max_rows, max_cols = 0, SIMPLIFY = FALSE)
  sdvar_out <- do.call(rbind, sdvar_out)
  
  corr_out <- mapply(pad_blank, corr_out, max_cols = max_cols, max_rows = max_rows, SIMPLIFY = FALSE)
  corr_out <- do.call(rbind, corr_out)
  if (all(corr_out == "")) {
    corr_out <- NULL
  } else {
    colnames(corr_out) <- c("Corr", rep("", ncol(corr_out)-1))
  }
  ## FIXME: should we enable column names here? spacing, abbrev, etc to worry about
  ##  (first, making sure that null correlation matrices are unnamed)
  
  res <- cbind(termnames_out, sdvar_out, corr_out)
  rownames(res) <- rep("", nrow(res))
  
  return(res)
  
}


## not used (and don't want roxygen to get confused

## once we decide what to do with this we can implement some nicer vcmat_* printing ...

## print.vcmat <- function(x, ...) {
##   class(x) <- class(x)[-1]  ## strip vcmat_* class attribute
##   NextMethod(.Generic)
## }

## ##' @export
## print.vcmat_us <- print.vcmat
## ##' @export
## print.vcmat_ar1 <- print.vcmat
## ##' @export
## print.vcmat_hetar1 <- print.vcmat
## ##' @export
## print.vcmat_diag <- print.vcmat
## ##' @export
## print.vcmat_homdiag <- print.vcmat
## ##' @export
## print.vcmat_cs <- print.vcmat
## ##' @export
## print.vcmat_homcs <- print.vcmat

