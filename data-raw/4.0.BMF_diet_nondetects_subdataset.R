############################################################
##   Diet-based BMF computation with non-detects for PFASs
##                    DATA SELECTION
############################################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)

#-----------------------------------------------------------
# Data selection

## Subdataset of contamination levels of PFASs for each species

contam_PFAS_ng_gdw <- contam |>
  select(species, alim, labels, grp,
         sommePFAS_ng_gdw, sommePFAS_ng_gdw_censored,
         starts_with(PFAS),
         -matches("ALL"),
         -matches("normalised"),
         -matches("gww")) |>
  select(-starts_with("FOSAA")) |>
  mutate(type = case_when(species == "Solea_solea" ~ "Sole",
                          TRUE ~ "Prey")) |>
  drop_na()
names(contam_PFAS_ng_gdw) <- names(contam_PFAS_ng_gdw) |> str_remove("_ng_gdw")

usethis::use_data(contam_PFAS_ng_gdw, overwrite = TRUE)
