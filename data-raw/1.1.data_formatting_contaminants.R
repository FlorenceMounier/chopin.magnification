####################################
##        DATA FORMATTING
##          CONTAMINANTS
####################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(readxl)
library(tidyverse)

## Load data
contam_prop <- read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                    sheet= "INFO_contam")
contam_prop$LOQ_biote <- round(as.numeric(contam_prop$LOQ_biote), digits = 2)
contam_prop$logKow <- as.numeric(contam_prop$logKow)

#-----------------------------------------------------------
# Read the list of studied chemicals and their properties

## Custom function for data selection
wrangle_contam <- function(grp_contam, grp_type, out_var){
  contam_prop |>
  filter(!!sym(grp_type) == grp_contam) |>
  pull(!!sym(out_var))
  }

## PCB
PCB_ALL <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "chemical")
PCB_ALL_lab <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "chem_label") # labels for graphics
log_Kow_PCB_ALL <- wrangle_contam(grp_contam = "PCB", grp_type = "family", out_var = "logKow")

PCB_indicators <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
PCB_dioxineL <- c("CB77", "CB81", "CB105", "CB114", "CB118", "CB123", "CB126", "CB156", "CB157", "CB167", "CB169", "CB189")
inter_dioxineL <- intersect(PCB_ALL, PCB_dioxineL)

## PFAS
PFAS_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "chemical")
PFAS_ALL_lab <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "chem_label") # labels for graphics
n_C_PFAS_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "n_C") # Number of carbon atoms
subfamilies_PFAS_ALL <- wrangle_contam(grp_contam = "PFAS", grp_type = "family", out_var = "sub_family_TAG") # subfamilies

#-----------------------------------------------------------
# Output data

usethis::use_data(contam_prop, overwrite = TRUE)

usethis::use_data(PCB_ALL, overwrite = TRUE)
usethis::use_data(PCB_ALL_lab, overwrite = TRUE)
usethis::use_data(log_Kow_PCB_ALL, overwrite = TRUE)
usethis::use_data(PCB_indicators, overwrite = TRUE)
usethis::use_data(PCB_dioxineL, overwrite = TRUE)
usethis::use_data(inter_dioxineL, overwrite = TRUE)

usethis::use_data(PFAS_ALL, overwrite = TRUE)
usethis::use_data(PFAS_ALL_lab, overwrite = TRUE)
usethis::use_data(subfamilies_PFAS_ALL, overwrite = TRUE)
usethis::use_data(n_C_PFAS_ALL, overwrite = TRUE)
