####################################
##        DATA ANALYSIS
##     DIET COMPOSITION
####################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)

#-----------------------------------------------------------
# Diet computation species

diet_species <- stomac_soles %>%
  mutate(
    total_N = sum(N_tot_in_tractus, na.rm = TRUE),
    abundance_pct = N_tot_in_tractus / total_N
  ) |>
  group_by(Faunistic_grp, Class, ScientificName_accepted) |>
  summarise(diet_portion = sum(abundance_pct, na.rm = TRUE), .groups = "drop") |>
  rename(taxon = Faunistic_grp)

usethis::use_data(diet_species, overwrite = TRUE)

#-----------------------------------------------------------
# Diet computation taxon

diet_taxon <- stomac_soles %>%
  mutate(
    total_N = sum(N_tot_in_tractus, na.rm = TRUE),
    abundance_pct = N_tot_in_tractus / total_N
  ) |>
  group_by(Faunistic_grp) |>
  summarise(diet_portion = sum(abundance_pct, na.rm = TRUE), .groups = "drop") |>
  rename(taxon = Faunistic_grp)

diet <- diet_taxon$diet_portion
names(diet) <- diet_taxon$taxon

usethis::use_data(diet, overwrite = TRUE)

#-----------------------------------------------------------
# Proportions of each prey "grp" in sole's diet

diet_comp <- tibble(taxon = c("Polychaeta", "Crustacea", "Bivalvia"),
                    diet_portion = diet) |>
  mutate(diet_portion = round(diet_portion, digits = 3))

usethis::use_data(diet_comp, overwrite = TRUE)
