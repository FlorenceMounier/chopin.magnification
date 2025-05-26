###########################################
##   BMF
##   Isotopes and PCB ng/glw subdatasets
###########################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)


#-----------------------------------------------------------
# Data selection

## Subdataset of isotopes and contamination levels of PCB congeners for each species

contam_PCB_ng_glw <- contam |>
  select(species, alim, labels, grp, d15N,
         starts_with("CB") & ends_with("ng_glw"),
         -contains("187"), -contains("194")) |>
  mutate(type = case_when(species == "Solea_solea" ~ "Sole",
                          TRUE ~ "Prey")) |>
  drop_na(CB153_ng_glw)
names(contam_PCB_ng_glw) <- names(contam_PCB_ng_glw) |> str_remove("_ng_glw")

usethis::use_data(contam_PCB_ng_glw, overwrite = TRUE)

## Subdataset of isotopes and sum PCB contamination levels for each species

contam_sumPCB_ng_glw <- contam |>
  select(species, alim, labels, grp, d15N, sommePCB_ng_glw) |>
  mutate(type = case_when(species == "Solea_solea" ~ "Sole",
                          TRUE ~ "Prey")) |>
  drop_na(sommePCB_ng_glw)

usethis::use_data(contam_sumPCB_ng_glw, overwrite = TRUE)
