library(reformulas)

varChr <- paste0("varname_",outer(letters,letters,paste0)[1:100])
rvars <- varChr[1:9]
form <- as.formula(paste("y ~",paste(varChr,collapse="+"),
                         "+",
                         paste0("(",paste(rvars,collapse="+"),"|f)")))
ff <- reOnly(form)
environment(ff) <- .GlobalEnv
expect_equal(ff,
             ~(us(varname_aa + varname_ba + varname_ca + varname_da + varname_ea +
                  varname_fa + varname_ga + varname_ha + varname_ia | f)))

