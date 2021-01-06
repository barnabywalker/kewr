# libraries ----
library(rredlist)  # make requests to IUCN Red List
library(dplyr)     # manipulate data
library(purrr)     # map functions over lists

# set key ----
key <- Sys.getenv("IUCN_REDLIST_KEY")

# download danish assessments ----
iso_code <- "DK"
assessments <- rl_sp_country(iso_code, key=key)

# download full assessment info for each taxon ----
taxa_idx <- unique(assessments$result$taxonid)

# wrap the search function to add a wait, so IUCN don't get upset
f <- function(taxonid) {
  Sys.sleep(0.1)
  rl_search(id=taxonid, key=key)
}

# might still need a few retries to get everything without an error
full_assessments <- map(taxa_idx, f)
full_assessments <- map_dfr(full_assessments, ~.x$result)

# narrow down to vascular plants ----
danish_plants <-
  full_assessments %>%
  filter(phylum == "TRACHEOPHYTA") %>%
  select(taxonid, scientific_name, authority, category)

# convert to a tibble for ease
danish_plants <- as_tibble(danish_plants)

# save to data folder ----
usethis::use_data(danish_plants, overwrite = TRUE)
