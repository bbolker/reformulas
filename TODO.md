## to do

- figure out how to avoid copying `.valid_covstruct`
- add `subbars`, `mkReTrms` (with options)
- test with `lme4` and `glmmTMB` branches
- push to CRAN ... ??
- tests!
- sort out licensing: glmmTMB is AGPL-3, lme4 is GPL ... ??? I *think* it's OK to make `reformulas` GPL-3 ... ? https://www.gnu.org/licenses/license-list.en.html
- merge/check `noSpecials` and `no_specials` ?

> Please note that the GNU AGPL is not compatible with GPLv2. It is also technically not compatible with GPLv3 in a strict sense: you cannot take code released under the GNU AGPL and convey or modify it however you like under the terms of GPLv3, or vice versa. However, you are allowed to combine separate modules or source files released under both of those licenses in a single project, which will provide many programmers with all the permission they need to make the programs they want. See section 13 of both licenses for details.

I think this is OK in any case since they are in separate packages?? But see [here](https://opensource.stackexchange.com/questions/4414/if-my-r-package-uses-gpl-packages-does-mine-automatically-inherit-gpl) ... maybe `reformulas` should be AGPL?


