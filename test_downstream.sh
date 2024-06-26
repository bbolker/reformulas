#! /bin/bash
cd ..
R CMD build reformulas
export TARBALL=`ls -t reformulas*.tar.gz | head -1`
echo $TARBALL
R CMD INSTALL $TARBALL

## test lme4
R CMD build --compact-vignettes=both lme4
export TARBALL=`ls -t lme4*.tar.gz | head -1`
R CMD check $TARBALL

## test glmmTMB
R CMD build glmmTMB/glmmTMB
export TARBALL=`ls -t glmmTMB*.tar.gz | head -1`
R CMD check $TARBALL

