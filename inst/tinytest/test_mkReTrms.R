## argh, can't (yet) replicate 

if (requireNamespace("lme4") && requireNamespace("glmmTMB")) {
    ## devtools::load_all("~/R/pkgs/reformulas")
    dd <- expand.grid(f = factor(1:101), rep1 = factor(1:2), rep2 = 1:2)
    dd$y <- glmmTMB::simulate_new(~1 + (rep1|f),
                                  seed = 101,
                                  newdata = dd,
                                  newparams = list(beta = 1,
                                                   theta = c(0,0,1),
                                                   betadisp = 0),
                                  family = gaussian)[[1]]
    debug(reformulas::mkReTrms)
    m1 <- lme4::lmer( y ~ 1 + (1|f), data = dd)
    p1 <- predict(m1)
    p2 <- predict(m1, newdata = dd)
}

## devtools::load_all("~/R/pkgs/reformulas")
dd <- read.csv("inst/testdata/lme4_GH631.csv")
model <- lme4::lmer(response ~ condition_bystanders +
                        (condition_bystanders|ID), 
                    data = dd)
d0 <- unique(dd[c("condition_bystanders", "ID")])
dp <- with(d0, expand.grid(condition_bystanders, ID)) |>
    setNames(c("condition_bystanders", "ID"))
## debug(reformulas::mkReTrms)
pp <- predict(model, newdata = dp)
