# libraries ----
library(rredlist)  # make requests to IUCN Red List
library(dplyr)     # manipulate data
library(purrr)     # map functions over lists

# set key ----
key <- Sys.getenv("IUCN_REDLIST_KEY")

# download all assessments ----
all_assessments <- rl_sp(all=TRUE, key=key)
all_assessments <- map_dfr(all_assessments, ~.x$result)

# filter for just angiosperms ----
angiosperm_assessments <-
  all_assessments %>%
  filter(class_name %in% c("MAGNOLIOPSIDA", "LILIOPSIDA")) %>%
  select(taxonid, scientific_name, taxonomic_authority, category)

# convert to tibble for ease
angiosperm_assessments <- as_tibble(angiosperm_assessments)

usethis::use_data(angiosperm_assessments, overwrite = TRUE)
