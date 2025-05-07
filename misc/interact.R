library(reformulas)
dd <- expand.grid(z=factor(1:3), temp = 1:5)
dd$y <- rnorm(nrow(dd))
form <- y ~ 1 + (1|z:factor(temp))
fr <- model.frame(subbars(form), data = dd)
mkReTrms(findbars(form), fr)
## problem is that model frame gets 'factor(temp)' as a column name,
## not 'temp', so can't evaluate "factor(temp)" in
##   eval(substitute(makeFac(fac), list(fac = ff0)), frloc)
