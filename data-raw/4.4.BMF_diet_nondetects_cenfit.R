############################################################
##   Diet-based BMF computation with non-detects for PFASs
##             CENFIT Kaplan-Meier SCENARIO
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

# Select uncensored dataset
contam_PFAS_ng_gdw_censored <- contam_PFAS_ng_gdw |>
  select("grp", "type", !matches("censored"), -sommePFAS)

# Compute an empirical cumulative distribution function (ECDF) for censored data using the Kaplan-Meier method for each taxa

res_cenfit_taxa <- function(var){
  cenfit_model <- NADA::cenfit(obs = contam_PFAS_ng_gdw_censored[[var]],
                               censored = contam_PFAS_ng_gdw_censored[[paste0(var, "_cen")]],
                               groups = as.factor(contam_PFAS_ng_gdw_censored$grp))

  quantiles <- NADA::quantile(x = cenfit_model, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  results <- rbind(c("PFAS" = var, "grp" = "Actinopterygii", type = "Sole", quantiles[[1]]),
                   c("PFAS" = var, "grp" = "Bivalvia", type = "Prey", quantiles[[2]]),
                   c("PFAS" = var, "grp" = "Crustacea", type = "Prey", quantiles[[3]]),
                   c("PFAS" = var, "grp" = "Polychaeta", type = "Prey", quantiles[[4]]))
  return(results)
}

contam_PFAS_ng_gdw_cenfit_taxon_stats <- c()

for(contam in PFAS){
  contam_PFAS_ng_gdw_cenfit_taxon_stats <- rbind(contam_PFAS_ng_gdw_cenfit_taxon_stats,
                                     res_cenfit_taxa(contam))
}

contam_PFAS_ng_gdw_cenfit_taxon_stats <- contam_PFAS_ng_gdw_cenfit_taxon_stats |>
  as_tibble() |>
  rename(min = "25%",
         median = "50%",
         max = "75%") |>
  mutate(across(.cols = c(min, median, max), ~ as.numeric(.x)))

# Export dataset as excel file
write_xlsx(x = contam_PFAS_ng_gdw_cenfit_taxon_stats,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/4.contam_PFAS_ng_gdw_cenfit_taxon_stats.xlsx")

#-----------------------------------------------------------
# 2. Diet statistics on contamination levels

contam_PFAS_ng_gdw_cenfit_diet_stats <- contam_PFAS_ng_gdw_cenfit_taxon_stats |>
  filter(type == "Prey") |>
  left_join(diet_comp, by = c("grp" = "taxon")) |>
  mutate(across(.cols = c(min, median, max), ~ .x * diet_portion)) |>
  group_by(PFAS) |>
  summarise(across(.cols = c(min, median, max), ~ sum(.x))) |>
  arrange(PFAS)

#-----------------------------------------------------------
# 3. BMF diet computation

### Get sole contamination stats

contam_PFAS_ng_gdw_cenfit_diet_stats_sole <- contam_PFAS_ng_gdw_cenfit_taxon_stats |>
  filter(type == "Sole") |>
  select(-type, -grp) |>
  arrange(PFAS)

### Compute diet BMFs

BMF_diet_PFAS_ng_gdw_cenfit <- full_join(contam_PFAS_ng_gdw_cenfit_diet_stats_sole,
                                         contam_PFAS_ng_gdw_cenfit_diet_stats,
                                             by = "PFAS", suffix = c("_sole", "_diet")) |>
  mutate(BMF_diet_min = min_sole / max_diet,
         BMF_diet_median = median_sole / median_diet,
         BMF_diet_max = max_sole / min_diet) |>
  fct_reorder_PFAS() |>
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, digits = 2)))

usethis::use_data(BMF_diet_PFAS_ng_gdw_cenfit, overwrite = TRUE)

write_xlsx(x = BMF_diet_PFAS_ng_gdw_cenfit,
           path = "inst/results/BMF_computation_PFAS_ng_gdw/4.BMF_diet_PFAS_ng_gdw_cenfit.xlsx")

BMF_diet_PFAS_ng_gdw_cenfit_compare <- BMF_diet_PFAS_ng_gdw_cenfit |>
  select(PFAS, starts_with("BMF")) |>
  rename_with(~ str_remove(.x, "BMF_diet_"), .cols = starts_with("BMF_diet_")) |>
  mutate(type = "cenfit")

usethis::use_data(BMF_diet_PFAS_ng_gdw_cenfit_compare, overwrite = TRUE)
