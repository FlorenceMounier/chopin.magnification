############################################################
##   Diet-based BMF computation with non-detects for PFASs
##      ROS Regression on Order Statistics SCENARIO
############################################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(chopin.magnification)
library(writexl)
library(NADA)


#-----------------------------------------------------------
# 0. Deal with censored data

## Select uncensored dataset
contam_PFAS_ng_gdw_censored <- contam_PFAS_ng_gdw |>
  select("grp", "type", !matches("censored"), -sommePFAS)

contam_PFAS_ng_gdw_censored_sole <- contam_PFAS_ng_gdw_censored |> filter(grp == "Actinopterygii")
contam_PFAS_ng_gdw_censored_bivalvia <- contam_PFAS_ng_gdw_censored |> filter(grp == "Bivalvia")
contam_PFAS_ng_gdw_censored_crustacea <- contam_PFAS_ng_gdw_censored |> filter(grp == "Crustacea")
contam_PFAS_ng_gdw_censored_polychaeta <- contam_PFAS_ng_gdw_censored |> filter(grp == "Polychaeta")

#-----------------------------------------------------------
# 1. Taxon statistics on contamination levels for selected PFASs using ROS

res_ros <- function(data, var){
  ros_model <- NADA::ros(obs = data[[var]],
                         censored = data[[paste0(var, "_cen")]])

  quantiles <- NADA::quantile(x = ros_model, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  return(quantiles)
}

contam_PFAS_ng_gdw_ros_taxon_stats <- c()

for(contam in PFAS){
  contam_PFAS_ng_gdw_ros_taxon_stats <- rbind(
    contam_PFAS_ng_gdw_ros_taxon_stats,
    c("PFAS" = contam, "grp" = "Actinopterygii", type = "Sole", res_ros(data = contam_PFAS_ng_gdw_censored_sole, var = contam)),
    c("PFAS" = contam, "grp" = "Bivalvia", type = "Prey", res_ros(data = contam_PFAS_ng_gdw_censored_bivalvia, var = contam)),
    c("PFAS" = contam, "grp" = "Crustacea", type = "Prey", res_ros(data = contam_PFAS_ng_gdw_censored_crustacea, var = contam)),
    c("PFAS" = contam, "grp" = "Polychaeta", type = "Prey", res_ros(data = contam_PFAS_ng_gdw_censored_polychaeta, var = contam))
  )
}

contam_PFAS_ng_gdw_ros_taxon_stats <- contam_PFAS_ng_gdw_ros_taxon_stats |>
  as_tibble() |>
  rename(min = "25%",
         median = "50%",
         max = "75%") |>
  mutate(across(.cols = c(min, median, max), ~ as.numeric(.x)))

# Export dataset as excel file
write_xlsx(x = contam_PFAS_ng_gdw_ros_taxon_stats,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/5.contam_PFAS_ng_gdw_ros_taxon_stats.xlsx")

#-----------------------------------------------------------
# 2. Diet statistics on contamination levels

contam_PFAS_ng_gdw_ros_diet_stats <- contam_PFAS_ng_gdw_ros_taxon_stats |>
  filter(type == "Prey") |>
  left_join(diet_comp, by = c("grp" = "taxon")) |>
  mutate(across(.cols = c(min, median, max), ~ .x * diet_portion)) |>
  group_by(PFAS) |>
  summarise(across(.cols = c(min, median, max), ~ sum(.x))) |>
  arrange(PFAS)

#-----------------------------------------------------------
# 3. BMF diet computation

### Get sole contamination stats

contam_PFAS_ng_gdw_ros_diet_stats_sole <- contam_PFAS_ng_gdw_ros_taxon_stats |>
  filter(type == "Sole") |>
  select(-type, -grp) |>
  arrange(PFAS)

### Compute diet BMFs

BMF_diet_PFAS_ng_gdw_ros <- full_join(contam_PFAS_ng_gdw_ros_diet_stats_sole,
                                      contam_PFAS_ng_gdw_ros_diet_stats,
                                      by = "PFAS", suffix = c("_sole", "_diet")) |>
  mutate(BMF_diet_min = min_sole / max_diet,
         BMF_diet_median = median_sole / median_diet,
         BMF_diet_max = max_sole / min_diet) |>
  fct_reorder_PFAS() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_PFAS_ng_gdw_ros, overwrite = TRUE)

BMF_diet_PFAS_ng_gdw_ros_compare <- BMF_diet_PFAS_ng_gdw_ros |>
  select(PFAS, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_diet_"), .cols = starts_with("BMF_diet_")) |>
  mutate(type = "ros")

usethis::use_data(BMF_diet_PFAS_ng_gdw_ros_compare, overwrite = TRUE)
