####################################
##       DATA FORMATTING
##           ISOTOPES
####################################


#-----------------------------------------------------------
# Loadings

## Load packages
library(readxl)
library(tidyverse)
library(chopin.magnification)

## Load data
benthos_isotopes_CAPES <- read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                             sheet = "isotopes_benth_CAPES")
soles_isotopes_CAPES <- read_excel("inst/CHOPIN_CAPES_RAW_DATABASE.xlsx",
                                 sheet = "isotopes_fish_CAPES")

#-----------------------------------------------------------
# BENTHOS

# Data cleaning for CAPES research project

  ## Group Nepthys species in "Neptys_sp"
  nepthys_sp <- c("Nephtys", "Nephtys_assimilis", "Nephtys_caeca",
                  "Nephtys_cirrosa", "Nephtys_hombergii")
  benthos_isotopes_CAPES <- benthos_isotopes_CAPES |>
    mutate(species = case_when(species %in% nepthys_sp ~ "Nephtys_sp", TRUE ~ species))

  ## Add benthos metada (label, taxon, feeding mode)
  benthos_isotopes_CAPES$source <- "CAPES"
  benthos_isotopes_CAPES <- left_join(benthos_isotopes_CAPES, benthos_metadata)
  benthos_isotopes_CAPES <- benthos_isotopes_CAPES |>
    select(species, labels, grp, alim, d13C, d15N, source)

# Data selection from CHOPIN research project
benthos_isotopes_CHOPIN <- benthos_contam |> drop_na(d13C)
benthos_isotopes_CHOPIN$source <- "CHOPIN"
benthos_isotopes_CHOPIN <- left_join(benthos_isotopes_CHOPIN, benthos_metadata)
benthos_isotopes_CHOPIN <- benthos_isotopes_CHOPIN |>
  select(species, labels, grp, alim, d13C, d15N, source)

# Join data from CAPES & CHOPIN
benthos_isotopes <- full_join(benthos_isotopes_CHOPIN, benthos_isotopes_CAPES)

#-----------------------------------------------------------
# SOLES

# Data cleaning for CAPES research project
soles_isotopes_CAPES <- soles_isotopes_CAPES |> select(d15N, d13C)

# Data selection from CHOPIN research project
soles_isotopes_CHOPIN <- soles_contam |> drop_na(d13C)
soles_isotopes_CHOPIN$source <- "CHOPIN"
soles_isotopes_CHOPIN <- soles_isotopes_CHOPIN |> select(d13C, d15N)

# Join data from CAPES & CHOPIN
soles_isotopes <- full_join(soles_isotopes_CHOPIN, soles_isotopes_CAPES)
soles_isotopes$species <- "Solea_solea"
soles_isotopes$labels <- "S. solea"
soles_isotopes$grp <- "Actinopterygii"
soles_isotopes$alim <- "Omnivore"

#-----------------------------------------------------------
# Join BENTHOS & SOLES

isotopes <- full_join(benthos_isotopes, soles_isotopes)

# --------------------------------------------------------------
# Save dataset
write_csv(x = isotopes, file = "data-raw/isotopes.csv")

# --------------------------------------------------------------
# Output data

usethis::use_data(isotopes, overwrite = TRUE)
