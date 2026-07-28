library(reformulas)

## null_model() is currently internal (@noRd, not exported).
## Uncomment these tests once it is exported.

# expect_equal(reformulas:::null_model(y ~ x + (1|g) + (x|h)),
#              y ~ 1 + ((1 | g) + (x | h)))
# expect_equal(reformulas:::null_model(y ~ x),
#              y ~ 1)
# expect_equal(reformulas:::null_model(cbind(a, b) ~ x + (1|g)),
#              cbind(a, b) ~ 1 + (1 | g))
# expect_error(reformulas:::null_model(~ x + (1|g)))
