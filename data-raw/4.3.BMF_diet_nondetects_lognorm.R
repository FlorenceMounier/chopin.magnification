############################################################
##   Diet-based BMF computation with non-detects for PFASs
##            LOG NORMAL REGRESSION SCENARIO
############################################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)


#-----------------------------------------------------------
# 0. Deal with censored data

# Select uncensored dataset
contam_PFAS_ng_gdw_censored <- contam_PFAS_ng_gdw |>
  select("grp", "type", !matches("censored"), -sommePFAS)

# Split in two groups for log-normal regression
contam_PFAS_ng_gdw_censored_prey <- contam_PFAS_ng_gdw_censored |> filter(type == "Prey")
contam_PFAS_ng_gdw_censored_sole <- contam_PFAS_ng_gdw_censored |> filter(type == "Sole")

# Draw random values according to the truncated distribution [0, LOQ] for prey and sole
for(contam in PFAS){
  contam_PFAS_ng_gdw_censored_prey[[paste0(contam, "_uncensored")]] <- impute_censored_with_truncated_fit(
    contam_PFAS_ng_gdw_censored_prey,
    var = contam,
    var_cens = paste0(contam, "_cen")
  )

  contam_PFAS_ng_gdw_censored_sole[[paste0(contam, "_uncensored")]] <- impute_censored_with_truncated_fit(
    data = contam_PFAS_ng_gdw_censored_sole,
    var = contam,
    var_cens = paste0(contam, "_cen")
  )
}

contam_PFAS_ng_gdw_uncensored <- full_join(contam_PFAS_ng_gdw_censored_sole,
                                           contam_PFAS_ng_gdw_censored_prey) |>
  rowwise() |>
  mutate(sumPFAS_uncensored = sum(c_across(ends_with("uncensored")))) |>
  ungroup()

# Export dataset as excel file
write_xlsx(x = contam_PFAS_ng_gdw_uncensored,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/3.contam_PFAS_ng_gdw_uncensored.xlsx")

# Exploration
infLOQ_Br_PFOS <- contam_PFAS_ng_gdw_uncensored |>
  select("grp", "type", contains("Br-PFOS")) |>
  filter(`Br-PFOS_cen` == TRUE)
ggplot(infLOQ_Br_PFOS) +
  aes(y = `Br-PFOS_uncensored`, fill = type) +
  geom_boxplot() +
  geom_hline(yintercept = infLOQ_Br_PFOS$`Br-PFOS`[1])


#-----------------------------------------------------------
# 1. Taxon statistics on contamination levels for selected PFASs

# Compute taxon statistics
contam_PFAS_ng_gdw_uncensored_taxon_stats <- contam_PFAS_ng_gdw_uncensored |>
  select(grp, type, ends_with("uncensored")) |>
  group_by(type, grp) |>
    summarise(across(where(is.numeric),
      list(
        min = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        median = ~ median(.x),
        max = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop") |>
  rename_with(~ str_remove_all(., "_uncensored")) |>
    pivot_longer(cols = where(is.numeric),
                 names_to = c("PFAS", ".value"),
                 names_sep = "_")

# Export dataset as excel file
write_xlsx(x = contam_PFAS_ng_gdw_uncensored_taxon_stats,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/3.contam_PFAS_ng_gdw_uncensored_taxon_stats.xlsx")

#-----------------------------------------------------------
# 2. Diet statistics on contamination levels

contam_PFAS_ng_gdw_uncensored_diet_stats <- contam_PFAS_ng_gdw_uncensored_taxon_stats |>
  filter(type == "Prey") |>
  left_join(diet_comp, by = c("grp" = "taxon")) |>
  mutate(across(.cols = c(min, median, max), ~ .x * diet_portion)) |>
  group_by(PFAS) |>
  summarise(across(.cols = c(min, median, max), ~ sum(.x))) |>
  arrange(PFAS)

#-----------------------------------------------------------
# 3. BMF diet computation

### Get sole contamination stats

contam_PFAS_ng_gdw_uncensored_diet_stats_sole <- contam_PFAS_ng_gdw_uncensored_taxon_stats |>
  filter(type == "Sole") |>
  select(-type, -grp) |>
  arrange(PFAS)

### Compute diet BMFs

BMF_diet_PFAS_ng_gdw_uncensored <- full_join(contam_PFAS_ng_gdw_uncensored_diet_stats_sole,
                                             contam_PFAS_ng_gdw_uncensored_diet_stats,
                                             by = "PFAS", suffix = c("_sole", "_diet")) |>
  mutate(BMF_diet_min = min_sole / max_diet,
         BMF_diet_median = median_sole / median_diet,
         BMF_diet_max = max_sole / min_diet) |>
  fct_reorder_PFAS() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_PFAS_ng_gdw_uncensored, overwrite = TRUE)

write_xlsx(x = BMF_diet_PFAS_ng_gdw_uncensored,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/3.BMF_diet_PFAS_ng_gdw_uncensored.xlsx")

BMF_diet_PFAS_ng_gdw_uncensored_compare <- BMF_diet_PFAS_ng_gdw_uncensored |>
  select(PFAS, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_diet_"), .cols = starts_with("BMF_diet_")) |>
  mutate(type = "uncensored")

usethis::use_data(BMF_diet_PFAS_ng_gdw_uncensored_compare, overwrite = TRUE)
