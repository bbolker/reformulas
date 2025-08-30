#' Remove all random slopes from a formula, while retaining random intercepts.
#'
#' @param form A formula
#' @return The new formula
#' @export
#' @examples
#' f <- ~ 1 + a  + b + (a | f) + (1 + a | g) + (a + b | h ) + (1 + a + b | i)
#' randint(f)
randint <- function(form) {
   fixed <- nobars(form)
   bars <- findbars(form) 
   for (i in seq_along(bars)) {
        bars[[i]][[2]] <- 1
   }
   ## was: addForm0(fixed, Reduce(addForm0, bars)), but
   ##  this fails to brace-protect the first RE term
   Reduce(addForm0, c(list(fixed), bars))
}
