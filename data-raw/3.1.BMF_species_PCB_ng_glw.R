###########################################
##   BMF species computation
###########################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)


#-----------------------------------------------------------
# 1. Species statistics on contamination levels

## Species statistics on contamination levels for PCB congeners

contam_PCB_ng_glw_species_stats <- contam_PCB_ng_glw |>
  group_by(type, grp, labels, species) |>
  summarise(across(
    starts_with("CB"),
    list(
      min = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      max = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ),
  .groups = "drop") |>
  pivot_longer(cols = starts_with("CB"),
               names_to = c("PCB", ".value"),
               names_sep = "_")


## Species statistics on contamination levels for sum PCB

contam_sumPCB_ng_glw_species_stats <- contam_sumPCB_ng_glw |>
  group_by(type, grp, labels, species) |>
  summarise(min = quantile(sommePCB_ng_glw, probs = 0.25, digits = 2),
            median = median_plus(sommePCB_ng_glw),
            max = quantile(sommePCB_ng_glw, probs = 0.75, digits = 2),
            .groups = "drop") |>
  mutate(PCB = "sumPCB")


## Species statistics on contamination levels for all PCB type

contam_all_PCB_ng_glw_species_stats <- full_join(
  contam_PCB_ng_glw_species_stats,
  contam_sumPCB_ng_glw_species_stats)


#-----------------------------------------------------------
# 2. BMF species computation

## BMF species statistics for all PCB type

# Isolate prey and sole contamination statistics
contam_all_PCB_ng_glw_species_stats_prey <- contam_all_PCB_ng_glw_species_stats |>
  filter(type == "Prey") |>
  arrange(grp, species, PCB)
contam_all_PCB_ng_glw_species_stats_sole <- contam_all_PCB_ng_glw_species_stats |>
  filter(type == "Sole") |>
  arrange(PCB)

# Create base table for BMFs species
BMF_all_PCB_ng_glw_species <- contam_all_PCB_ng_glw_species_stats_prey

# Number of prey
n_prey <- length(BMF_all_PCB_ng_glw_species |> distinct(species) |> pull())

# Replicate sole stats of contamination next to prey stats of contamination
BMF_all_PCB_ng_glw_species$min_sole <- rep.int(
  contam_all_PCB_ng_glw_species_stats_sole$min,
  times = n_prey)
BMF_all_PCB_ng_glw_species$median_sole <- rep.int(
  contam_all_PCB_ng_glw_species_stats_sole$median,
  times = n_prey)
BMF_all_PCB_ng_glw_species$max_sole <- rep.int(
  contam_all_PCB_ng_glw_species_stats_sole$max,
  times = n_prey)

# Compute BMF stats from prey and sole stats of contamination
BMF_species_all_PCB_ng_glw <- BMF_all_PCB_ng_glw_species |>
  mutate(BMF_min = min_sole / max,
         BMF_median = median_sole / median,
         BMF_max = max_sole / min) |>
  fct_reorder_PCB() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

# Export as data in the package
usethis::use_data(BMF_species_all_PCB_ng_glw, overwrite = TRUE)

# Export dataset as excel file
write_xlsx(
  x = BMF_species_all_PCB_ng_glw,
  path = "inst/results/BMF_computation_PCB_ng_glw/1.BMF_species_all_PCB_ng_glw.xlsx")


#-----------------------------------------------------------
# 3. Mean BMF species computation

BMF_species_PCB_ng_glw_summarised <- BMF_species_all_PCB_ng_glw |>
  group_by(PCB) |>
  summarise(min = min_plus(BMF_min),
            median = mean_plus(BMF_median),
            max = max_plus(BMF_max)) |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_species_PCB_ng_glw_summarised, overwrite = TRUE)

write_xlsx(x = BMF_species_PCB_ng_glw_summarised,
           path = "inst/results/BMF_computation_PCB_ng_glw/1.BMF_species_PCB_ng_glw_summarised.xlsx")

BMF_species_all_PCB_ng_glw_compare <- BMF_species_PCB_ng_glw_summarised |>
  mutate(type = "species") |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_species_all_PCB_ng_glw_compare, overwrite = TRUE)
