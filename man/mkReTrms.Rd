% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mkReTrms.R
\name{mkReTrms}
\alias{mkReTrms}
\title{Create list of structures needed for models with random effects}
\usage{
mkReTrms(
  bars,
  fr,
  drop.unused.levels = TRUE,
  reorder.terms = TRUE,
  reorder.vars = FALSE,
  calc.lambdat = TRUE,
  sparse = NULL
)
}
\arguments{
\item{bars}{a list of parsed random-effects terms}

\item{fr}{a model frame in which to evaluate these terms}

\item{drop.unused.levels}{(logical) drop unused factor levels?}

\item{reorder.terms}{arrange random effects terms in decreasing order of number of groups (factor levels)?}

\item{reorder.vars}{arrange columns of individual random effects terms in alphabetical order?}

\item{calc.lambdat}{(logical) compute \code{Lambdat} and \code{Lind} components? (At present these components
are needed for \code{lme4} machinery but not for \code{glmmTMB}, and may be large in some cases; see Bates \emph{et al.} 2015}

\item{sparse}{(logical) set up sparse model matrices?}
}
\value{
a list with components
\item{Zt}{transpose of the sparse model matrix for the random effects}
\item{Ztlist}{list of components of the transpose of the
random-effects model matrix, separated by random-effects term}
\item{Lambdat}{transpose of the sparse relative covariance factor}
\item{Lind}{an integer vector of indices determining the mapping of the
elements of the \code{theta} to the \code{"x"} slot of \code{Lambdat}}
\item{theta}{initial values of the covariance parameters}
\item{lower}{lower bounds on the covariance parameters}
\item{flist}{list of grouping factors used in the random-effects terms}
\item{cnms}{a list of column names of the random effects according to
the grouping factors}
\item{Gp}{a vector indexing the association of
elements of the conditional mode vector
with random-effect terms; if \code{nb} is the vector of numbers
of conditional modes per term (i.e. number of groups times number
of effects per group), \code{Gp} is \code{c(0,cumsum(nb))}
(and conversely \code{nb} is \code{diff(Gp)})}
\item{nl}{names of the terms (in the same order as \code{Zt},
i.e. reflecting the \code{reorder.terms} argument)}
}
\description{
From the result of \code{\link{findbars}} applied to a model formula and
and the evaluation frame, create the model matrix, etc. associated with
random-effects terms.  See the description of the returned value for a
detailed list.
}
\details{
\code{Lambdat}, \code{Lind}, \code{theta}, \code{lower} are likely to
be useful only for \code{lme4}; the other terms can be generally useful for
constructing mixed-effect models
}
\examples{
## (silly/impractical formula, for illustration only)
form <- mpg ~ 1 + (1 + hp | cyl) + (1|gear) + (factor(cyl)|gear)
fr <- model.frame(subbars(form), data = mtcars)
rterms <- mkReTrms(findbars(form), fr)
names(rterms)
## block sizes (latent variables per block) of each term
(nperblock <- lengths(rterms$cnms))
## latent variables per term
(nperterm <- diff(rterms$Gp))
with(rterms, identical(unname(nl*nperblock), nperterm))
}
\references{
\insertRef{lme4}{reformulas})
}
\seealso{
Other utilities: 
\code{\link{expandDoubleVerts}()},
\code{\link{nobars}()},
\code{\link{subbars}()}
}
\concept{utilities}
