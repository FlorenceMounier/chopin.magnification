####################################
##       COMPOUND SELECTION
####################################

#-----------------------------------------------------------
# Loadings

## Load packages

library(chopin.magnification)
library(tidyverse)

#-----------------------------------------------------------
# PCB

# Calculation of Detection Percentages by Compound

detection_PCB_benthos <- benthos_contam[,paste0(PCB_ALL, "_ng_gdw")] |>
  drop_na() |>
  summarise(across(everything(), ~ sum(!is.na(.x) & .x!=0 & !is.nan(.x))/n())) |>
  pivot_longer(cols = everything(), names_to = "contaminant", values_to = "detection_benthos") |>
  mutate(detection_benthos = round(detection_benthos*100))

detection_PCB_soles <- soles_contam[,paste0(PCB_ALL, "_ng_gdw")] |>
  drop_na() |>
  summarise(across(everything(), ~ sum(!is.na(.x) & .x!=0)/n())) |>
  pivot_longer(cols = everything(), names_to = "contaminant", values_to = "detection_soles") |>
  mutate(detection_soles = round(detection_soles*100))

detection_PCB <- full_join(detection_PCB_benthos, detection_PCB_soles, by = "contaminant")

# Selection =
# (1) quantification >= 50% in either soles or benthos
# (2) at least >= 20% in soles and benthos
PCB <- detection_PCB |>
  filter(detection_benthos >= 50 | detection_soles >= 50) |>
  filter(detection_benthos >= 20 & detection_soles >= 20) |>
  select(contaminant) |>
  pull() |>
  str_remove("_ng_gdw")

#-----------------------------------------------------------
# PFAS

# Calculation of Detection Percentages by Compound
detection_PFAS_benthos <- benthos_contam[,paste0(PFAS_ALL, "_ng_gdw_censored")] |>
  drop_na() |>
  summarise(across(everything(), ~ sum(!is.na(.x) & .x!=0)/n())) |>
  pivot_longer(cols = everything(), names_to = "contaminant", values_to = "detection_benthos") |>
  mutate(detection_benthos = round(detection_benthos*100))

detection_PFAS_soles <- soles_contam[,paste0(PFAS_ALL, "_ng_gdw_censored")]  |>
  drop_na() |>
  summarise(across(everything(), ~ sum(!is.na(.x) & .x!=0)/n())) |>
  pivot_longer(cols = everything(), names_to = "contaminant", values_to = "detection_soles") |>
  mutate(detection_soles = round(detection_soles*100))

detection_PFAS <- full_join(detection_PFAS_benthos, detection_PFAS_soles, by = "contaminant")

# Selection =
# (1) quantification >= 50% in either soles or benthos
# (2) at least >= 20% in soles and benthos
PFAS <- detection_PFAS |>
  filter(detection_benthos >= 50 | detection_soles >= 50) |>
  filter(detection_benthos >= 20 & detection_soles >= 20) |>
  select(contaminant) |>
  pull() |>
  str_remove("_ng_gdw_censored")


#-----------------------------------------------------------
# Combined results
table_detection_rate <- rbind(detection_PCB, detection_PFAS) |>
  mutate(contaminant = contaminant |> str_remove("_ng_gdw_censored|_ng_gdw"))

usethis::use_data(table_detection_rate, overwrite = TRUE)
# --------------------------------------------------------------
# Save dataset
write_csv(x = table_detection_rate, file = "data-raw/detection_rates.csv")

# --------------------------------------------------------------
# Output data

usethis::use_data(PCB, overwrite = TRUE)
usethis::use_data(PFAS, overwrite = TRUE)
