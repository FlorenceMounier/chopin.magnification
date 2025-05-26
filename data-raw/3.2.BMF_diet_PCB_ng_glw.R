###########################################
##   BMF diet computation
###########################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)


#-----------------------------------------------------------
# 0. Proportions of each prey "grp" in sole's diet

diet_comp <- tibble(taxon = c("Polychaeta", "Crustacea", "Bivalvia"),
                    diet_portion = diet) |>
  mutate(diet_portion = round(diet_portion, digits = 3))

usethis::use_data(diet_comp, overwrite = TRUE)


#-----------------------------------------------------------
# 1. Taxon statistics on contamination levels


## Taxon statistics on contamination levels for PCB congeners

contam_PCB_ng_glw_taxon_stats <- contam_PCB_ng_glw |>
  group_by(type, grp) |>
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

## Taxon statistics on contamination levels for sum PCB

contam_sumPCB_ng_glw_taxon_stats <- contam_sumPCB_ng_glw |>
  group_by(type, grp) |>
  summarise(min = quantile(sommePCB_ng_glw, probs = 0.25, digits = 2),
            median = median_plus(sommePCB_ng_glw),
            max = quantile(sommePCB_ng_glw, probs = 0.75, digits = 2),
            .groups = "drop") |>
  mutate(PCB = "sumPCB")

## Taxon statistics on contamination levels for all PCB type

contam_all_PCB_ng_glw_taxon_stats <- full_join(
  contam_PCB_ng_glw_taxon_stats,
  contam_sumPCB_ng_glw_taxon_stats) |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

## Export dataset as excel file

write_xlsx(
  x = contam_all_PCB_ng_glw_taxon_stats,
  path = "inst/results/BMF_computation_PCB_ng_glw/2.contam_all_PCB_ng_glw_taxon_stats.xlsx")


#-----------------------------------------------------------
# 2. Diet statistics on contamination levels for all PCB type

contam_all_PCB_ng_glw_diet_stats <- contam_all_PCB_ng_glw_taxon_stats |>
  filter(type == "Prey") |>
  left_join(diet_comp, by = c("grp" = "taxon")) |>
  mutate(across(
    .cols = c(min, median, max),
    ~ .x * diet_portion
  )) |>
  group_by(PCB) |>
  summarise(across(
    .cols = c(min, median, max),
    ~ sum(.x))) |>
  arrange(PCB)


#-----------------------------------------------------------
# 3. BMF diet computation

### Get sole contamination stats

contam_all_PCB_ng_glw_diet_stats_sole <- contam_all_PCB_ng_glw_taxon_stats |>
  filter(type == "Sole") |>
  select(-type, -grp) |>
  arrange(PCB)

### Compute diet BMFs

BMF_diet_all_PCB_ng_glw <- full_join(contam_all_PCB_ng_glw_diet_stats_sole, contam_all_PCB_ng_glw_diet_stats,
                                     by = "PCB", suffix = c("_sole", "_diet")) |>
  mutate(BMF_diet_min = min_sole / max_diet,
         BMF_diet_median = median_sole / median_diet,
         BMF_diet_max = max_sole / min_diet) |>
  fct_reorder_PCB() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_all_PCB_ng_glw, overwrite = TRUE)

write_xlsx(x = BMF_diet_all_PCB_ng_glw,
           path = "inst/results/BMF_computation_PCB_ng_glw/2.BMF_diet_all_PCB_ng_glw.xlsx")

BMF_diet_all_PCB_ng_glw_compare <- BMF_diet_all_PCB_ng_glw |>
  select(PCB, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_diet_"), .cols = starts_with("BMF_diet_")) |>
  mutate(type = "diet") |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_all_PCB_ng_glw_compare, overwrite = TRUE)
