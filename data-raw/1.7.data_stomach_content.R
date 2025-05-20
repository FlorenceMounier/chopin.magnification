####################################
##       DATA FORMATTING
##       STOMACH CONTENT
####################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(readxl)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

## Load data
stomac_soles <- read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                             sheet = "soles_stomach_content")

#-----------------------------------------------------------
# Data cleaning

# Remove contents different from preys
# (parasites, detritus, fish eggs and scale, unidentified or empty)

content_to_delete <- c("Déchet", "Parasite", "RAS", "Divers", "Divers_débris",
                       "Non identifié", "Ponte_nd",
                       "Animalia 1", "Animalia 4", "Animalia 5",
                       "Pleuronectes platessa_écailles",
                       "Actinopterygii_écaille",
                       "Solea solea_écaille")

stomac_soles <- stomac_soles |>
  filter(Statut %!in% content_to_delete) |>
  filter(ScientificName_accepted %!in% content_to_delete) |>
  filter(Faunistic_grp %in% c("Annelida", "Arthropoda", "Mollusca"))

## Deal with data class
stomac_soles$N_tot_in_tractus <- as.numeric(stomac_soles$N_tot_in_tractus)

## Keep only necessary variables
stomac_soles <- stomac_soles |>
  select(Fish_TAG, Faunistic_grp, ScientificName_accepted, N_tot_in_tractus)

#-----------------------------------------------------------
# Table of species names and corresponding taxa

vect_species <- unique(stomac_soles$ScientificName_accepted)
vect_taxon <- c()
for(species in unique(stomac_soles$ScientificName_accepted)){
  vect_taxon = c(vect_taxon,
                 stomac_soles$Faunistic_grp[which(stomac_soles$ScientificName_accepted==species)][1])
}

table_species <- tibble("taxon" = vect_taxon, "species" = vect_species) |> arrange(taxon)

# --------------------------------------------------------------
# Save dataset
write_csv(x = stomac_soles, file = "data-raw/stomac_soles.csv")

#-----------------------------------------------------------
# Output data

usethis::use_data(stomac_soles, overwrite = TRUE)
usethis::use_data(table_species, overwrite = TRUE)
