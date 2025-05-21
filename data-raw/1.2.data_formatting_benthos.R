####################################
##       DATA FORMATTING
##           BENTHOS
####################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(tidyverse)
library(readxl)
library(chopin.magnification)

## Load data
benthos_contam <- read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                             sheet = "benthos")

# class zone and species to factor
benthos_contam$zone <- as.factor(benthos_contam$zone)
benthos_contam$species <- as.factor(benthos_contam$species)

# --------------------------------------------------------------
# Add variable "alim" for feeding mode

feeding_mode <- list(
  "Suspensivore" = c("Abra_alba", "Cerastoderma_edule", "Corbula_gibba",
              "Donax_vittatus", "Ensis_directus",
              "Owenia_fusiformis", "Spisula_subtruncata"),
  "Deposivore" = c("Corophium_volutator", "Lagis_koreni", "Lanice_conchilega",
             "Limecola_balthica", "Nucula_nitidosa", "Scrobicularia_plana"),
  "Omnivore" = c("Crangon_crangon", "Hediste_diversicolor", "Nephtys_sp")
)
benthos_contam$alim = rep.int(NA, times = dim(benthos_contam)[1])
for(l in 1:length(feeding_mode)){
  for(s in 1:length(feeding_mode[[l]])){
    benthos_contam$alim[which(as.character(benthos_contam$species) == as.character(feeding_mode[[l]][s]))] = names(feeding_mode)[l]
  }
}

# --------------------------------------------------------------
# Add variable "labels" for graph species labels

prey_labels <- list(
  "Bivalvia" = c(
    "Abra a." = "Abra_alba",
    "Spisula s." = "Spisula_subtruncata",
    "Limecola b." = "Limecola_balthica",
    "Cerastoderma e." = "Cerastoderma_edule",
    "Donax v." = "Donax_vittatus",
    "Corbula g." = "Corbula_gibba",
    "Scrobicularia p." = "Scrobicularia_plana",
    "Nucula n." = "Nucula_nitidosa",
    "Ensis d." = "Ensis_directus"
  ),
  "Polychaeta" = c(
    "Nephtys sp." = "Nephtys_sp",
    "Owenia f." = "Owenia_fusiformis",
    "Lagis k." = "Lagis_koreni",
    "Lanice c." = "Lanice_conchilega",
    "Hediste d." = "Hediste_diversicolor"
  ),
  "Crustacea" = c("Crangon c." = "Crangon_crangon", "Corophium v." =
                    "Corophium_volutator")
)
benthos_contam$labels = rep.int(NA, times = dim(benthos_contam)[1])
for(l in 1:length(prey_labels)){
  for(s in 1:length(prey_labels[[l]])){
    benthos_contam$labels[which(as.character(benthos_contam$species) == as.character(prey_labels[[l]][s]))] = names(prey_labels[[l]])[s]
    }
}

# --------------------------------------------------------------
# Add variable "grp" for species taxa

prey_taxa = list(
  "Bivalvia" = c("Abra_alba","Spisula_subtruncata", "Limecola_balthica",
                 "Cerastoderma_edule", "Donax_vittatus", "Corbula_gibba",
                 "Scrobicularia_plana","Nucula_nitidosa","Ensis_directus"),
  "Polychaeta" = c("Nephtys_sp", "Owenia_fusiformis", "Lagis_koreni",
                   "Lanice_conchilega", "Hediste_diversicolor"),
  "Crustacea" = c("Crangon_crangon", "Corophium_volutator")
)

benthos_contam$grp = rep.int(NA, times = dim(benthos_contam)[1])
for(l in 1:length(prey_taxa)){
  for(s in 1:length(prey_taxa[[l]])){
    benthos_contam$grp[which(as.character(benthos_contam$species) == as.character(prey_taxa[[l]][s]))] = names(prey_taxa)[l]
  }
}

# --------------------------------------------------------------
# Create a dataset with species metadata
benthos_metadata <- benthos_contam |> select(species, labels, grp, alim) |> distinct()

# --------------------------------------------------------------
# Handling <LOQ values for PFASs

# Raw data are censored data in ng/gdw
colnames(benthos_contam)[colnames(benthos_contam) %in% PFAS_ALL] <- paste0(
  colnames(benthos_contam)[colnames(benthos_contam) %in% PFAS_ALL], "_ng_gdw_censored"
)

# Create logical vector for non-detects (if [ng_gdw_censored]=0 "_cen" ~ TRUE; else ~ FALSE)
benthos_contam <- benthos_contam |>
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
benthos_contam <- benthos_contam |>
  mutate(
    PFPeA_ng_gdw = case_when(PFPeA_ng_gdw_censored == 0 ~ 0.39/2, TRUE ~ PFPeA_ng_gdw_censored),
    PFHxA_ng_gdw = case_when(PFHxA_ng_gdw_censored == 0 ~ 0.81/2, TRUE ~ PFHxA_ng_gdw_censored),
    PFHpA_ng_gdw = case_when(PFHpA_ng_gdw_censored == 0 ~ 0.77/2, TRUE ~ PFHpA_ng_gdw_censored),
    PFOA_ng_gdw  = case_when(PFOA_ng_gdw_censored  == 0 ~ 1.66/2, TRUE ~ PFOA_ng_gdw_censored),
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
# Handling <LOD values for HBCDD isomers

# Raw data in ng/gdw are not censored

# List of samples with values equal to the LOD for each isomer
a_HBCDD_LOQ_values_samples <- c("CP14", "CP06", "CP52", "T2 FN8 crevettes", "CP53")
b_HBCDD_LOQ_values_samples <- c("CP12", "CP14", "CP06", "CP52", "CP41", "CP44", "T2 FN8 crevettes", "CP43", "CP53")
g_HBCDD_LOQ_values_samples <- c("CP14", "CP06", "CP52", "CP41", "CP44", "T2 FN8 crevettes", "CP53")

benthos_contam <- benthos_contam |>
  mutate(

    # Isomer alpha
    `a-HBCDD_ng_gdw_censored` = case_when(
      is.na(`a-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% a_HBCDD_LOQ_values_samples ~ 0,
      TRUE ~ `a-HBCDD_ng_gdw`
    ), `a-HBCDD_cen` = case_when(
      is.na(`a-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% a_HBCDD_LOQ_values_samples ~ TRUE,
      TRUE ~ FALSE

    # Isomer beta
    ), `b-HBCDD_ng_gdw_censored` = case_when(
      is.na(`b-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% b_HBCDD_LOQ_values_samples ~ 0,
      TRUE ~ `b-HBCDD_ng_gdw`
    ), `b-HBCDD_cen` = case_when(
      is.na(`b-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% b_HBCDD_LOQ_values_samples ~ TRUE,
      TRUE ~ FALSE

    # Isomer gamma
    ), `g-HBCDD_ng_gdw_censored` = case_when(
      is.na(`g-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% g_HBCDD_LOQ_values_samples ~ 0,
      TRUE ~ `g-HBCDD_ng_gdw`
    ), `g-HBCDD_cen` = case_when(
      is.na(`g-HBCDD_ng_gdw`) ~ NA,
      sample_TAG %in% g_HBCDD_LOQ_values_samples ~ TRUE,
      TRUE ~ FALSE
    )
  )

# --------------------------------------------------------------
# Output data

usethis::use_data(benthos_metadata, overwrite = TRUE)
usethis::use_data(benthos_contam, overwrite = TRUE)
