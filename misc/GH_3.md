
Testing https://github.com/bbolker/reformulas/issues/3

Want to get lme4 commit 9bafccbb31 (from 2021-06-21 and a compatible Matrix package

https://hackmd.io/@dushoff/theobioSummerLearning#containers-Docker-etc,
https://rocker-project.org/images/versioned/r-ver.html

```
docker run --rm -ti rocker/r-ver:4.1.0
```

https://stackoverflow.com/questions/22907231/how-to-copy-files-from-host-to-docker-container

docker ps --> confident_feynman (current name)

```
docker cp data_\(1\).csv confident_feynman:/
```

In R:

```r
install.packages("remotes")
remotes::install_version("lme4", "1.1.27")
library(lme4)
data <- read.csv("data_(1).csv")
model <- lmer(response ~ condition_bystanders + (condition_bystanders|ID), 
              data = data)
d <- as.data.frame(expand.grid(unique(data$condition_bystanders), unique(data$ID)))
colnames(d) <- c("condition_bystanders", "ID")
predict(model, newdata = d)
```

Fails in `mkNewReTrms` → `mkReTrms` → `mkBlist` → `model.matrix.default`

```r
undebug(lme4:::predict.merMod)
undebug(lme4:::mkNewReTrms)
undebug(lme4:::mkReTrms)
debug(lme4:::mkBlist)

ee <- eval(substitute(~foo, list(foo = x[[2]])))
## may have sparse contrasts; model.matrix.default **will not work** with sparse contrast matrices
## call in nrows? https://github.com/r-devel/r-svn/blob/2c0916d455958379652d0debb71c9cab72a912f4/src/main/util.c#L81-L94
## where do sparse contrast matrices come from in the first place? are they ever useful?
## this is from 'sparse option for mkNewReTrms`:
##   https://github.com/lme4/lme4/commit/9a3cbe153f8fb26ac5282e16c55fde71c8574611
```
