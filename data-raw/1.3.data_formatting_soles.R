####################################
##       DATA FORMATTING
##            SOLES
####################################


#-----------------------------------------------------------
# Loadings

## Load packages

library(chopin.magnification)
library(tidyverse)
library(readxl)

## Load data
soles_contam = read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                          sheet = "soles")


# --------------------------------------------------------------
# Sole metadata

soles_metadata <- tibble("species" = "Solea_solea",
                         "labels" = "S. solea",
                         "grp" = "Actinopterygii",
                         "alim" = "Omnivore")

# --------------------------------------------------------------
# Calculation of dry weight

soles_contam$mass_tot_gdw = soles_contam$mass_tot_gww*((100-soles_contam$water_percent)/100)

# --------------------------------------------------------------
# Handling <LOQ values for PFASs

# Raw data are censored data in ng/gdw
colnames(soles_contam)[colnames(soles_contam) %in% PFAS_ALL] <- paste0(
  colnames(soles_contam)[colnames(soles_contam) %in% PFAS_ALL], "_ng_gdw_censored"
)

# Create logical vector for non-detects (if [ng_gdw_censored]=0 "_cen" ~ TRUE; else ~ FALSE)
soles_contam <- soles_contam |>
  mutate(
    PFPeA_cen = case_when(PFPeA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFHxA_cen = case_when(PFHxA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFHpA_cen = case_when(PFHpA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFOA_cen  = case_when(PFOA_ng_gdw_censored  == 0 ~ TRUE, TRUE ~ FALSE),
    PFNA_cen = case_when(PFNA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFDA_cen = case_when(PFDA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFUnDA_cen = case_when(PFUnDA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFDoDA_cen = case_when(PFDoDA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFTrDA_cen = case_when(PFTrDA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFTeDA_cen = case_when(PFTeDA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    FOSA_cen = case_when(FOSA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    FOSAA_cen = case_when(FOSAA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    MeFOSAA_cen = case_when(MeFOSAA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    EtFOSAA_cen = case_when(EtFOSAA_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFBS_cen = case_when(PFBS_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFHxS_cen = case_when(PFHxS_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    PFHpS_cen = case_when(PFHpS_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    `L-PFOS_cen` = case_when(`L-PFOS_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `Br-PFOS_cen` = case_when(`Br-PFOS_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    PFDS_cen = case_when(PFDS_ng_gdw_censored == 0 ~ TRUE, TRUE ~ FALSE),
    `4:2 FTSA_cen` = case_when(`4:2 FTSA_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `6:2 FTSA_cen` = case_when(`6:2 FTSA_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `8:2 FTSA_cen` = case_when(`8:2 FTSA_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `10:2 FTSA_cen` = case_when(`10:2 FTSA_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `6:2 diPAP_cen` = case_when(`6:2 diPAP_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE),
    `8:2 diPAP_cen` = case_when(`8:2 diPAP_ng_gdw_censored` == 0 ~ TRUE, TRUE ~ FALSE)
  )

# Create uncensored data by replacing 0 values with LOQ/2
soles_contam <- soles_contam |>
  mutate(
    PFPeA_ng_gdw = case_when(PFPeA_ng_gdw_censored == 0 ~ 0.39/2, TRUE ~ PFPeA_ng_gdw_censored),
    PFHxA_ng_gdw = case_when(PFHxA_ng_gdw_censored == 0 ~ 0.81/2, TRUE ~ PFHxA_ng_gdw_censored),
    PFHpA_ng_gdw = case_when(PFHpA_ng_gdw_censored == 0 ~ 0.77/2, TRUE ~ PFHpA_ng_gdw_censored),
    PFOA_ng_gdw  = case_when(PFOA_ng_gdw_censored == 0 ~ 1.66/2, TRUE ~ PFOA_ng_gdw_censored),
    PFNA_ng_gdw = case_when(PFNA_ng_gdw_censored == 0 ~ 0.06/2, TRUE ~ PFNA_ng_gdw_censored),
    PFDA_ng_gdw = case_when(PFDA_ng_gdw_censored == 0 ~ 0.56/2, TRUE ~ PFDA_ng_gdw_censored),
    PFUnDA_ng_gdw = case_when(PFUnDA_ng_gdw_censored == 0 ~ 0.17/2, TRUE ~ PFUnDA_ng_gdw_censored),
    PFDoDA_ng_gdw = case_when(PFDoDA_ng_gdw_censored == 0 ~ 0.02/2, TRUE ~ PFDoDA_ng_gdw_censored),
    PFTrDA_ng_gdw = case_when(PFTrDA_ng_gdw_censored == 0 ~ 0.03/2, TRUE ~ PFTrDA_ng_gdw_censored),
    PFTeDA_ng_gdw = case_when(PFTeDA_ng_gdw_censored == 0 ~ 0.05/2, TRUE ~ PFTeDA_ng_gdw_censored),
    FOSA_ng_gdw = case_when(FOSA_ng_gdw_censored == 0 ~ 0.23/2, TRUE ~ FOSA_ng_gdw_censored),
    FOSAA_ng_gdw = case_when(FOSAA_ng_gdw_censored == 0 ~ 0.78/2, TRUE ~ FOSAA_ng_gdw_censored),
    MeFOSAA_ng_gdw = case_when(MeFOSAA_ng_gdw_censored == 0 ~ 0.15/2, TRUE ~ MeFOSAA_ng_gdw_censored),
    EtFOSAA_ng_gdw = case_when(EtFOSAA_ng_gdw_censored == 0 ~ 0.10/2, TRUE ~ EtFOSAA_ng_gdw_censored),
    PFBS_ng_gdw = case_when(PFBS_ng_gdw_censored == 0 ~ 0.55/2, TRUE ~ PFBS_ng_gdw_censored),
    PFHxS_ng_gdw = case_when(PFHxS_ng_gdw_censored == 0 ~ 1.64/2, TRUE ~ PFHxS_ng_gdw_censored),
    PFHpS_ng_gdw = case_when(PFHpS_ng_gdw_censored == 0 ~ 1.21/2, TRUE ~ PFHpS_ng_gdw_censored),
    `L-PFOS_ng_gdw` = case_when(`L-PFOS_ng_gdw_censored` == 0 ~ 0.73/2, TRUE ~ `L-PFOS_ng_gdw_censored`),
    `Br-PFOS_ng_gdw` = case_when(`Br-PFOS_ng_gdw_censored` == 0 ~ 0.73/2, TRUE ~ `Br-PFOS_ng_gdw_censored`),
    PFDS_ng_gdw = case_when(PFDS_ng_gdw_censored == 0 ~ 0.24/2, TRUE ~ PFDS_ng_gdw_censored),
    `4:2 FTSA_ng_gdw` = case_when(`4:2 FTSA_ng_gdw_censored` == 0 ~ 0.14/2, TRUE ~ `4:2 FTSA_ng_gdw_censored`),
    `6:2 FTSA_ng_gdw` = case_when(`6:2 FTSA_ng_gdw_censored` == 0 ~ 0.03/2, TRUE ~ `6:2 FTSA_ng_gdw_censored`),
    `8:2 FTSA_ng_gdw` = case_when(`8:2 FTSA_ng_gdw_censored` == 0 ~ 0.03/2, TRUE ~ `8:2 FTSA_ng_gdw_censored`),
    `10:2 FTSA_ng_gdw` = case_when(`10:2 FTSA_ng_gdw_censored` == 0 ~ 0.19/2, TRUE ~ `10:2 FTSA_ng_gdw_censored`),
    `6:2 diPAP_ng_gdw` = case_when(`6:2 diPAP_ng_gdw_censored` == 0 ~ 3.45/2, TRUE ~ `6:2 diPAP_ng_gdw_censored`),
    `8:2 diPAP_ng_gdw` = case_when(`8:2 diPAP_ng_gdw_censored` == 0 ~ 0.24/2, TRUE ~ `8:2 diPAP_ng_gdw_censored`)
  )


# --------------------------------------------------------------
# Output data

usethis::use_data(soles_contam, overwrite = TRUE)
usethis::use_data(soles_metadata, overwrite = TRUE)

