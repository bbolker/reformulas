## check usage of reformulas:: functions downstream

library(purrr)
library(dplyr)

rdeps <- tools::package_dependencies("reformulas", reverse = TRUE)$reformulas
names(rdeps) <- rdeps  ## for map_dfr

tdir <- tempdir()
a1 <- available.packages()

## tarball names
tarballs <- sapply(rdeps, function(x) {
    sprintf("%s_%s.tar.gz", x, a1[x,"Version"])
})

## download
for (tt in tarballs) {
    url <- sprintf("%s/src/contrib/%s",
                   getOption("repos")[["CRAN"]], tt)
    download.file(url, destfile = file.path(tdir, tt))
}

cdir <- setwd(tdir)

## unpack
for (tt in tarballs) {
    system(sprintf("tar zxf %s", tt))
}

## need to check both NAMESPACE file and R code
dd_ns <- purrr::map_dfr(rdeps,
               ~ data.frame(import = readLines(file.path(.x, "NAMESPACE"))),
               .id = "pkg") |>
    dplyr::filter(stringr::str_detect(import, "reformulas")) |>
    mutate(across(import, ~ stringr::str_extract(.x, "(?<=importFrom\\([\"']?reformulas[\"']?,)[^()]+"))) |>
    mutate(across(import, ~ stringr::str_remove_all(.x, "[ \"']"))) |>
    group_by(pkg) |>
    distinct()

## FIXME: also check .r files?
dd_code <- purrr::map_dfr(rdeps,
                      function(x) data.frame(import = system(sprintf("grep -h reformulas:: %s/R/*.R", x), intern=TRUE)),
                      .id = "pkg") |>
    mutate(across(import, ~ stringr::str_extract(.x, "(?<=reformulas::)[^()]+"))) |>
    group_by(pkg) |>
    distinct()

setwd(cdir)
