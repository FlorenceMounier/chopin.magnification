####################################
##       DATA COMPUTATION
##     FOR SELECTED COMPOUNDS
####################################

#-----------------------------------------------------------
# Loadings

## Load packages

library(chopin.magnification)
library(tidyverse)

#-----------------------------------------------------------
# Contaminant metadata for selected compounds

PCB_lab <- PCB_ALL_lab[which(PCB_ALL %in% PCB)]
log_Kow_PCB <- log_Kow_PCB_ALL[which(PCB_ALL %in% PCB)]

PFAS_lab <- PFAS_ALL_lab[which(PFAS_ALL %in% PFAS)]
subfamilies_PFAS <- subfamilies_PFAS_ALL[which(PFAS_ALL %in% PFAS)]
n_C_PFAS <- n_C_PFAS_ALL[which(PFAS_ALL %in% PFAS)]

HBCDD_lab <- HBCDD_ALL_lab[which(HBCDD_ALL %in% HBCDD)]
log_Kow_HBCDD <- log_Kow_HBCDD_ALL[which(HBCDD_ALL %in% HBCDD)]


#-----------------------------------------------------------
# Joined data of contamination for soles and benthos

## Reducted datasets aux sommes de contaminant, commun sole-benthos

sub_benthos_contam <- benthos_contam |>
  select(-c(year, season, date_sampling, zone, station, comment))|>
  rename(lip_percent_dw = lip_dw_percent)

sub_soles_contam <- soles_contam |>
  mutate(dw_percent = 100-water_percent) |>
  select(-c(year, season, Date_prlvt, zone, grp, sample_type,
            length_TL_cm, length_TL_cm_sd, length_SL_cm, mass_tot_gww,
            mass_tot_gww_sd, FultonK_gwwTL3, FultonK_sd_gwwTL3, water_percent,
            "a-HBCDD_pg_gdw", "b-HBCDD_pg_gdw", "g-HBCDD_pg_gdw")) |>
  mutate(species = soles_metadata$species, alim = soles_metadata$alim,
         labels = soles_metadata$labels, grp = soles_metadata$grp)

setdiff(names(sub_benthos_contam), names(sub_soles_contam))

full_join(sub_benthos_contam, sub_soles_contam)

#-------------------------------------------------------------------------
# PCB

# Calculation of concentrations normalized by the sum in ng/gdw
contam$sommePCB_ng_gdw = apply(contam[,paste(PCB,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum)
contam[, paste(PCB, "normalised_sum_ng_gdw", sep = "_")] <- contam[, paste(PCB, "_ng_gdw", sep = "")] / contam$sommePCB_ng_gdw

# Calculations of concentrations and their sum in ng/glw
for(c in 1:length(PCB)){
  contam[,paste(PCB[c],"_ng_glw",sep="")] = contam[,paste(PCB[c],"_ng_gdw",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommePCB_ng_glw = apply(contam[,paste(PCB,"_ng_glw",sep="")], MARGIN = 1, FUN = sum)

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(PCB)){
  contam[,paste(PCB[c],"_ng_gww",sep="")] = contam[,paste(PCB[c],"_ng_gdw",sep="")]*(contam$dw_percent/100)
}
contam$sommePCB_ng_gww = apply(contam[,paste(PCB,"_ng_gww",sep="")], MARGIN = 1, FUN = sum)

#-------------------------------------------------------------------------
# PFAS

# Calculation of concentrations normalized by the sum in ng/gdw
contam$sommePFAS_ng_gdw = apply(contam[,paste(PFAS,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum)
contam[, paste(PFAS, "normalised_sum_ng_gdw", sep = "_")] <- contam[, paste(PFAS,"_ng_gdw",sep="")] /  contam$sommePFAS_ng_gdw

contam$sommePFAS_ng_gdw_censored = apply(contam[,paste(PFAS,"_ng_gdw_censored",sep="")], MARGIN = 1, FUN = sum)
contam[, paste(PFAS, "normalised_sum_ng_gdw_censored", sep = "_")] <- contam[, paste(PFAS,"_ng_gdw_censored",sep="")] / contam$sommePFAS_ng_gdw_censored

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(PFAS)){
  contam[,paste(PFAS[c],"_ng_gww",sep="")] = contam[,paste(PFAS[c],"_ng_gdw",sep="")]*(contam$dw_percent/100)
}
contam$sommePFAS_ng_gww = apply(contam[,paste(PFAS,"_ng_gww",sep="")], MARGIN = 1, FUN = sum)

for(c in 1:length(PFAS)){
  contam[,paste(PFAS[c],"_ng_gww_censored",sep="")] = contam[,paste(PFAS[c],"_ng_gdw_censored",sep="")]*(contam$dw_percent/100)
}
contam$sommePFAS_ng_gww_censored = apply(contam[,paste(PFAS,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum)

#-------------------------------------------------------------------------
# HBCDD

# Calculation of concentrations normalized by the sum in ng/gdw
contam$sommeHBCDD_ng_gdw = apply(contam[,paste(HBCDD,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum)
contam[, paste(HBCDD, "normalised_sum_ng_gdw", sep = "_")] <- contam[, paste(HBCDD, "_ng_gdw", sep = "")] / contam$sommeHBCDD_ng_gdw

contam$sommeHBCDD_ng_gdw_censored = apply(contam[,paste(HBCDD,"_ng_gdw_censored",sep="")], MARGIN = 1, FUN = sum)
contam[, paste(HBCDD, "normalised_sum_ng_gdw_censored", sep = "_")] <- contam[, paste(HBCDD, "_ng_gdw_censored", sep = "")] / contam$sommeHBCDD_ng_gdw_censored

# Calculations of concentrations and their sum in ng/glw
for(c in 1:length(HBCDD)){
  contam[,paste(HBCDD[c],"_ng_glw",sep="")] = contam[,paste(HBCDD[c],"_ng_gdw",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommeHBCDD_ng_glw = apply(contam[,paste(HBCDD,"_ng_glw",sep="")], MARGIN = 1, FUN = sum)

for(c in 1:length(HBCDD)){
  contam[,paste(HBCDD[c],"_ng_glw_censored",sep="")] = contam[,paste(HBCDD[c],"_ng_gdw_censored",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommeHBCDD_ng_glw_censored = apply(contam[,paste(HBCDD,"_ng_glw_censored",sep="")], MARGIN = 1, FUN = sum)

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(HBCDD)){
  contam[,paste(HBCDD[c],"_ng_gww",sep="")] = contam[,paste(HBCDD[c],"_ng_gdw",sep="")]*(contam$dw_percent/100)
}
contam$sommeHBCDD_ng_gww = apply(contam[,paste(HBCDD,"_ng_gww",sep="")], MARGIN = 1, FUN = sum)

for(c in 1:length(HBCDD)){
  contam[,paste(HBCDD[c],"_ng_gww_censored",sep="")] = contam[,paste(HBCDD[c],"_ng_gdw_censored",sep="")]*(contam$dw_percent/100)
}
contam$sommeHBCDD_ng_gww_censored = apply(contam[,paste(HBCDD,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum)

# --------------------------------------------------------------
# Save dataset
write_csv(x = contam, file = "data-raw/contam.csv")

# --------------------------------------------------------------
# Output data

usethis::use_data(PCB_lab, overwrite = TRUE)
usethis::use_data(log_Kow_PCB, overwrite = TRUE)

usethis::use_data(PFAS_lab, overwrite = TRUE)
usethis::use_data(subfamilies_PFAS, overwrite = TRUE)
usethis::use_data(n_C_PFAS, overwrite = TRUE)

usethis::use_data(HBCDD_lab, overwrite = TRUE)
usethis::use_data(log_Kow_HBCDD, overwrite = TRUE)

usethis::use_data(contam, overwrite = TRUE)
