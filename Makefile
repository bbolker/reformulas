downstream:
cd ..; R CMD build reformulas; export TARBALL=`ls -t reformulas*.tar.gz | head -1`; echo $(TARBALL); R CMD INSTALL $(TARBALL)
	cd ..; R CMD build --compact-vignettes lme4; export TARBALL=`ls -t lme4*.tar.gz | head -1`; R CMD check $(TARBALL)
	cd ..; R CMD build glmmTMB/glmmTMB; export TARBALL=`ls -t glmmTMB*.tar.gz | head -1`; R CMD check $(TARBALL)

