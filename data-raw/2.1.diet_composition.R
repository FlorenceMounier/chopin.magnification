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
# Diet computation

taxa <- stomac_soles |> distinct(Faunistic_grp) |>  pull()
abundance_tot <- sum(stomac_soles$N_tot_in_tractus, na.rm = T)
diet <- c(NA)
for (taxon in 1:length(taxa)) {
  diet[taxon] <- sum(stomac_soles[stomac_soles$Faunistic_grp == taxa[taxon], ]$N_tot_in_tractus, na.rm = T) / abundance_tot
}
names(diet) <- taxa

usethis::use_data(diet, overwrite = TRUE)

#-----------------------------------------------------------
# Proportions of each prey "grp" in sole's diet

diet_comp <- tibble(taxon = c("Polychaeta", "Crustacea", "Bivalvia"),
                    diet_portion = diet) |>
  mutate(diet_portion = round(diet_portion, digits = 3))

usethis::use_data(diet_comp, overwrite = TRUE)
