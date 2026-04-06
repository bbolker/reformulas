library(reformulas)
form1 <- y~diag(covariates|dummy)
form2 <- y~diag(covariates|1)
findbars_x(form1)
findbars_x(form2) ## NULL
## expandAllGrpVars is throwing this away?
