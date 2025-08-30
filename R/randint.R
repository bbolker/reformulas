#' Remove random slopes from a formula, while retaining random intercepts.
#' See: https://github.com/bbolker/reformulas/issues/11#issuecomment-3221120343.
#'
#' @param form A formula
#' @return The new formula
#' @export
#' @examples
#' f <- ~ 1 + a  + b + (a | f) + (1 + a | g) + (a + b | h ) + (1 + a + b | i)
#' reformulas_randint(f)
randint <- function(form) {
   fixed <- reformulas::nobars(form)
   bars <- reformulas::findbars(form) 
   for (i in seq_along(bars)) {
        bars[[i]][[2]] <- 1
   }
   reformulas::addForm(fixed, Reduce(reformulas::addForm0, bars))
}
