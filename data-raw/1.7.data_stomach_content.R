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
stomac_soles <- stomac_soles |>
  filter(Statut %!in% c("déchet", "Parasite", "RAS")) |>
  filter(Faunistic_grp %in% c("Annelida", "Arthropoda", "Mollusca"))

## Deal with data class
stomac_soles$N_tot_in_tractus <- as.numeric(stomac_soles$N_tot_in_tractus)

## Keep only necessary variables
stomac_soles <- stomac_soles |>
  select(Fish_TAG, Faunistic_grp, Class, ScientificName_accepted, N_tot_in_tractus)


#-----------------------------------------------------------
# Table of species names and corresponding taxa

vect_species <- unique(stomac_soles$ScientificName_accepted)
vect_taxon <- c()
vect_class <- c()
for(species in vect_species){
  vect_taxon = c(vect_taxon,
                 stomac_soles$Faunistic_grp[which(stomac_soles$ScientificName_accepted==species)][1])
  vect_class = c(vect_class,
                  stomac_soles$Class[which(stomac_soles$ScientificName_accepted==species)][1])
}

table_species <- tibble("taxon" = vect_taxon,
                        "class" = vect_class,
                        "species" = vect_species) |>
  arrange(taxon)

# --------------------------------------------------------------
# Save dataset
write_csv(x = stomac_soles, file = "data-raw/stomac_soles.csv")

#-----------------------------------------------------------
# Output data

usethis::use_data(stomac_soles, overwrite = TRUE)
usethis::use_data(table_species, overwrite = TRUE)

