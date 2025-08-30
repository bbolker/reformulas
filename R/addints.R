#' Add interaction terms in a formula.
#' See: https://stackoverflow.com/q/79750252/1719931.
#' 
#' @param form A formula.
#' @param treat Treatment variable (string).
#' @param controls A character vector of control variables.
#' @return A formula with interaction terms added between `treat` and each variable in `controls`.
#' @export
#' @examples
#' reformulas_addints(mpg ~ cyl + gear, "cyl", c("gear"))
#' reformulas_addints(mpg ~ cyl + gear + disp, "cyl", c("gear", "disp"))
#' reformulas_addints(mpg ~ cyl + gear + disp + hp, "cyl", c("gear", "disp"))
#' reformulas_addints(mpg ~ cyl + gear, "cyl", c("gears"))
#' reformulas_addints(mpg ~ cyl + cyl*gear, "cyl", c("gear"))
addints <- function(form, treat, controls) {
  stopifnot (inherits(form, "formula"))
  terms <- terms(form)
  
  variables <- attr(terms, "term.labels")
  
  stopifnot(controls %in% variables, treat %in% variables)
  
  for (control in controls) {
    new <- as.formula(paste("~ . + ", control, ":", treat))
    form <- update(form, new)
  }
  
  form
}
