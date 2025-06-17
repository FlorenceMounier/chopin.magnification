###################################################
##   Trophic-level normalized BMF computation
###################################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)

#-----------------------------------------------------------
# 1. Compute trophic levels

d15N_base <- min(isotopes$d15N, na.rm = TRUE)
delta_d15N <- 3.4
lambda <- 2

trophic_level <- function(d15N_consumer){
  TL <- ((d15N_consumer - d15N_base) / delta_d15N) + lambda
  return(TL)
}

# For predator-prey BMF_TL
TL_species <- isotopes |>
  group_by(species) |>
  summarise(d15N_median = median(d15N, na.rm = TRUE)) |>
  mutate(TL = trophic_level(d15N_consumer = d15N_median))

# For comparison between TL-BMF scenarios
TL_prey <- TL_species |>
  filter(species != "Solea_solea") |>
  summarise(d15N_median = median(d15N_median, na.rm = TRUE)) |>
  mutate(TL = trophic_level(d15N_consumer = d15N_median))

# For diet-based BMF_TL
d15N_median_taxa <- isotopes |>
  group_by(grp) |>
  summarise(d15N_median = median(d15N, na.rm = TRUE)) |>
  pivot_wider(names_from = grp, values_from = d15N_median)

TL_taxa <- isotopes |>
  group_by(grp) |>
  summarise(d15N_median = median(d15N, na.rm = TRUE)) |>
  mutate(TL = trophic_level(d15N_consumer = d15N_median)) |>
  select(-d15N_median) |>
  pivot_wider(names_from = grp, values_from = TL)

d15N_diet <- d15N_median_taxa$Bivalvia * diet[["Mollusca"]] +
  d15N_median_taxa$Crustacea * diet[["Arthropoda"]] +
  d15N_median_taxa$Polychaeta * diet[["Annelida"]]
names(d15N_diet) <- "diet"
trophic_level(d15N_diet)

TL_diet <- TL_taxa$Bivalvia * diet["Mollusca"] +
           TL_taxa$Crustacea * diet[["Arthropoda"]] +
           TL_taxa$Polychaeta * diet[["Annelida"]]
names(TL_diet) <- "diet"

table_d15N_TL <- rbind(tibble(species = names(d15N_median_taxa),
             d15N_median = as.numeric(d15N_median_taxa[1,]),
             TL = as.numeric(TL_taxa[1,])),
      tibble(species = "diet", d15N_median = d15N_diet, TL = TL_diet),
      TL_species)

write_xlsx(x = table_d15N_TL,
           path = "inst/results/BMF_computation_PCB_ng_glw/3.BMF_TL_species_all_PCB_ng_glw_d15N_TL.xlsx")

#-----------------------------------------------------------
# 2. Compute species BM_TL

BMF_TL_species_all_PCB_ng_glw <- BMF_species_all_PCB_ng_glw |>
  select(-starts_with("BMF")) |>
  left_join(TL_species) |>
  mutate(
    BMF_TL_min = 10^(log10(min_sole / max) / (TL_taxa$Actinopterygii - TL)),
    BMF_TL_median = 10^(log10(median_sole / median) / (TL_taxa$Actinopterygii - TL)),
    BMF_TL_max = 10^(log10(max_sole / min) / (TL_taxa$Actinopterygii - TL))) |>
  fct_reorder_PCB() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_TL_species_all_PCB_ng_glw, overwrite = TRUE)

write_xlsx(x = BMF_TL_species_all_PCB_ng_glw,
           path = "inst/results/BMF_computation_PCB_ng_glw/3.BMF_TL_species_all_PCB_ng_glw.xlsx")


#-----------------------------------------------------------
# 3. Mean BMF_TL species computation

BMF_TL_species_PCB_ng_glw_summarised <- BMF_TL_species_all_PCB_ng_glw |>
  group_by(PCB) |>
  summarise(min = median_plus(BMF_TL_min),
            median = median_plus(BMF_TL_median),
            max = median_plus(BMF_TL_max), .groups = "drop") |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_TL_species_PCB_ng_glw_summarised, overwrite = TRUE)

write_xlsx(x = BMF_TL_species_PCB_ng_glw_summarised,
           path = "inst/results/BMF_computation_PCB_ng_glw/3.BMF_TL_species_PCB_ng_glw_summarised.xlsx")

# Export dataset of TL-BMF species for comparison

BMF_TL_species_all_PCB_ng_glw_compare <- BMF_TL_species_PCB_ng_glw_summarised |>
  mutate(type = "TL_species")

usethis::use_data(BMF_TL_species_all_PCB_ng_glw_compare, overwrite = TRUE)


#-----------------------------------------------------------
# 4. Compute diet BM_TL

BMF_TL_diet_all_PCB_ng_glw <- BMF_diet_all_PCB_ng_glw |>
  select(-starts_with("BMF")) |>
  mutate(
    BMF_TL_min = 10^(log10(min_sole / max_diet) / (TL_taxa$Actinopterygii - TL_diet)),
    BMF_TL_median = 10^(log10(median_sole / median_diet) / (TL_taxa$Actinopterygii - TL_diet)),
    BMF_TL_max = 10^(log10(max_sole / min_diet) / (TL_taxa$Actinopterygii - TL_diet))
    ) |>
  fct_reorder_PCB() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_TL_diet_all_PCB_ng_glw, overwrite = TRUE)

write_xlsx(x = BMF_TL_diet_all_PCB_ng_glw,
           path = "inst/results/BMF_computation_PCB_ng_glw/3.BMF_TL_diet_all_PCB_ng_glw.xlsx")

### Export diet TL-BMFs for comparison

BMF_TL_diet_all_PCB_ng_glw_compare <- BMF_TL_diet_all_PCB_ng_glw |>
  select(PCB, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_TL_"), .cols = starts_with("BMF_TL_")) |>
  mutate(type = "TL_diet")

usethis::use_data(BMF_TL_diet_all_PCB_ng_glw_compare, overwrite = TRUE)

