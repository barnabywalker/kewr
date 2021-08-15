#' precompile articles that take a long time to run
library(knitr)
library(here)

# remove the figures folder for regeneration
if (dir.exists(here("vignettes/articles/figure"))) {
  unlink(here("vignettes/articles/figure"), recursive=TRUE)
}

# Conservation status on the Tree of Life
knit("vignettes/articles/conservation-status-treeoflife.Rmd.orig",
     "vignettes/articles/conservation-status-treeoflife.Rmd")

# move any figures that have been created to the articles folder
file.rename(here("figure"), here("vignettes/articles/figure"))
