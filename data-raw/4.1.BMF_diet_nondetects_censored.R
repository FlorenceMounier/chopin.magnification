############################################################
##   Diet-based BMF computation with non-detects for PFASs
##                 CENSORED SCENARIO
############################################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)


#-----------------------------------------------------------
# 0. Proportions of each prey "grp" in sole's diet

data(diet_comp)


#-----------------------------------------------------------
# 1. Taxon statistics on contamination levels for selected PFASs

# Select censored dataset
contam_PFAS_ng_gdw_censored <- contam_PFAS_ng_gdw |>
  select("species", "alim", "labels", "grp", "type", matches("censored")) |>
  rename_with(~ str_remove_all(., "_censored"))

# Compute taxon statistics
contam_PFAS_ng_gdw_censored_taxon_stats <- contam_PFAS_ng_gdw_censored |>
  rename(sumPFAS = sommePFAS) |>
  group_by(type, grp) |>
    summarise(across(where(is.numeric),
      list(
        min = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop") |>
    pivot_longer(cols = where(is.numeric),
                 names_to = c("PFAS", ".value"),
                 names_sep = "_")

# Export dataset as excel file
write_xlsx(x = contam_PFAS_ng_gdw_censored_taxon_stats,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/1.contam_PFAS_ng_gdw_censored.xlsx")

#-----------------------------------------------------------
# 2. Diet statistics on contamination levels

contam_PFAS_ng_gdw_censored_diet_stats <- contam_PFAS_ng_gdw_censored_taxon_stats |>
  filter(type == "Prey") |>
  left_join(diet_comp, by = c("grp" = "taxon")) |>
  mutate(across(.cols = c(min, median, max), ~ .x * diet_portion)) |>
  group_by(PFAS) |>
  summarise(across(.cols = c(min, median, max), ~ sum(.x))) |>
  arrange(PFAS)

#-----------------------------------------------------------
# 3. BMF diet computation

### Get sole contamination stats

contam_PFAS_ng_gdw_censored_diet_stats_sole <- contam_PFAS_ng_gdw_censored_taxon_stats |>
  filter(type == "Sole") |>
  select(-type, -grp) |>
  arrange(PFAS)

### Compute diet BMFs

BMF_diet_PFAS_ng_gdw_censored <- full_join(contam_PFAS_ng_gdw_censored_diet_stats_sole,
                                  contam_PFAS_ng_gdw_censored_diet_stats,
                                     by = "PFAS", suffix = c("_sole", "_diet")) |>
  mutate(BMF_diet_min = min_sole / max_diet,
         BMF_diet_median = median_sole / median_diet,
         BMF_diet_max = max_sole / min_diet) |>
  fct_reorder_PFAS() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_PFAS_ng_gdw_censored, overwrite = TRUE)

write_xlsx(x = BMF_diet_PFAS_ng_gdw_censored,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/1.BMF_diet_PFAS_ng_gdw_censored.xlsx")

BMF_diet_PFAS_ng_gdw_censored_compare <- BMF_diet_PFAS_ng_gdw_censored |>
  select(PFAS, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_diet_"), .cols = starts_with("BMF_diet_")) |>
  mutate(type = "diet")

usethis::use_data(BMF_diet_PFAS_ng_gdw_censored_compare, overwrite = TRUE)
