####################################
##    DATA JOINING & COMPUTING
##        SOLES & BENTHOS
####################################


#-----------------------------------------------------------
# Loadings

## Load packages

library(chopin.magnification)
library(tidyverse)

#-----------------------------------------------------------
# Join metadata

metadata <- full_join(soles_metadata, benthos_metadata)

#-----------------------------------------------------------
# Join data of contamination for soles and benthos

## Reduce datasets

sub_benthos_contam <- benthos_contam |>
  select(-c(year, season, date_sampling, zone, station, comment))|>
  rename(lipid_percent_dw = lip_dw_percent)

sub_soles_contam <- soles_contam |>
  mutate(dw_percent = 100-water_percent) |>
  select(-c(year, season, Date_prlvt, zone, grp, sample_type,
            length_TL_cm, length_TL_cm_sd, length_SL_cm, mass_tot_gww,
            mass_tot_gww_sd, FultonK_gwwTL3, FultonK_sd_gwwTL3, water_percent,
            "a-HBCDD_pg_gdw", "b-HBCDD_pg_gdw", "g-HBCDD_pg_gdw")) |>
  mutate(species = soles_metadata$species, alim = soles_metadata$alim,
         labels = soles_metadata$labels, grp = soles_metadata$grp)

## Join datasets
contam <- full_join(sub_benthos_contam, sub_soles_contam) |>
  left_join(metadata) |>
  mutate(grp = factor(grp, levels = c("Actinopterygii", "Bivalves", "Crustaces", "Polychetes")))


#-----------------------------------------------------------
# Calculation of concentrations normalized by the sum within a family (ng/g dw)

# PCB
contam$sommePCB_ALL_ng_gdw <- apply(contam[,paste(PCB_ALL,"_ng_gdw",sep="")],
                                            MARGIN = 1, FUN = sum, na.rm = TRUE)
contam[, paste(PCB_ALL, "normalised_sum_ALL_ng_gdw", sep = "_")] <- contam[, paste(PCB_ALL, "_ng_gdw", sep = "")] / contam$sommePCB_ALL_ng_gdw

# PFAS
contam$sommePFAS_ALL_ng_gdw <- apply(contam[,paste(PFAS_ALL,"_ng_gdw",sep="")],
                                             MARGIN = 1, FUN = sum, na.rm = TRUE)
contam[, paste(PFAS_ALL, "normalised_sum_ALL_ng_gdw", sep = "_")] <- contam[, paste(PFAS_ALL,"_ng_gdw",sep="")] /  contam$sommePFAS_ALL_ng_gdw

# PFAS censored
contam$sommePFAS_ALL_ng_gdw_censored <- apply(contam[,paste(PFAS_ALL,"_ng_gdw_censored",sep="")],
                                                      MARGIN = 1, FUN = sum, na.rm = TRUE)
contam[, paste(PFAS_ALL, "normalised_sum_ALL_ng_gdw_censored", sep = "_")] <- contam[, paste(PFAS_ALL,"_ng_gdw_censored",sep="")] / contam$sommePFAS_ALL_ng_gdw_censored

# HBCDD
contam$sommeHBCDD_ALL_ng_gdw <- apply(contam[,paste(HBCDD_ALL,"_ng_gdw",sep="")],
                                              MARGIN = 1, FUN = sum, na.rm = TRUE)
contam[, paste(HBCDD, "normalised_sum_ALL_ng_gdw", sep = "_")] <- contam[, paste(HBCDD_ALL, "_ng_gdw", sep = "")] / contam$sommeHBCDD_ALL_ng_gdw

# HBCDD censored
contam$sommeHBCDD_ALL_ng_gdw_censored <- apply(contam[,paste(HBCDD_ALL,"_ng_gdw_censored",sep="")],
                                                       MARGIN = 1, FUN = sum, na.rm = TRUE)
contam[, paste(HBCDD, "normalised_sum_ALL_ng_gdw_censored", sep = "_")] <- contam[, paste(HBCDD_ALL, "_ng_gdw_censored", sep = "")] / contam$sommeHBCDD_ALL_ng_gdw_censored


# --------------------------------------------------------------
# Calculation of concentrations and sums by family in lipid weight (ng/g lw)
# For PCB and HBCDD only

# PCB
for(c in 1:length(PCB_ALL)){
  contam[,paste(PCB_ALL[c],"_ng_glw",sep="")] <- contam[,paste(PCB_ALL[c],"_ng_gdw",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommePCB_ALL_ng_glw = apply(contam[,paste(PCB_ALL,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
contam$sommePCBindicators_ng_glw = apply(contam[,paste(PCB_indicators,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
contam$sommePCBdioxineL_ng_glw = apply(contam[,paste(inter_dioxineL,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# HBCDD
for(c in 1:length(HBCDD_ALL)){
  contam[,paste(HBCDD_ALL[c],"_ng_glw",sep="")] <- contam[,paste(HBCDD_ALL[c],"_ng_gdw_censored",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommeHBCDD_ALL_ng_glw = apply(contam[,paste(HBCDD_ALL,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# HBCDD censored
for(c in 1:length(HBCDD)){
  contam[,paste(HBCDD[c],"_ng_glw_censored",sep="")] <- contam[,paste(HBCDD[c],"_ng_gdw",sep="")]/(contam$lipid_percent_dw/100)
}
contam$sommeHBCDD_ng_glw_censored = apply(contam[,paste(HBCDD,"_ng_glw_censored",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# --------------------------------------------------------------
# Calculation of concentrations and sums in wet weight (ng/g ww)

# PCB
for(c in 1:length(PCB_ALL)){
  contam[,paste(PCB_ALL[c],"_ng_gww",sep="")] = contam[,paste(PCB_ALL[c],"_ng_gdw",sep="")]*((contam$dw_percent)/100)
}
contam$sommePCB_ALL_ng_gww = apply(contam[,paste(PCB_ALL,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
contam$sommePCBindicators_ng_gww = apply(contam[,paste(PCB_indicators,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
contam$sommePCBdioxineL_ng_gww = apply(contam[,paste(inter_dioxineL,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# PFAS
for(c in 1:length(PFAS_ALL)){
  contam[,paste(PFAS_ALL[c],"_ng_gww",sep="")] = contam[,paste(PFAS_ALL[c],"_ng_gdw",sep="")]*((contam$dw_percent)/100)
}
contam$sommePFAS_ALL_ng_gww = apply(contam[,paste(PFAS_ALL,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# PFAS censored
for(c in 1:length(PFAS_ALL)){
  contam[,paste(PFAS_ALL[c],"_ng_gww_censored",sep="")] = contam[,paste(PFAS_ALL[c],"_ng_gdw_censored",sep="")]*((contam$dw_percent)/100)
}
contam$sommePFAS_ALL_ng_gww_censored = apply(contam[,paste(PFAS_ALL,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# HBCDD
for(c in 1:length(HBCDD_ALL)){
  contam[,paste(HBCDD_ALL[c],"_ng_gww",sep="")] = contam[,paste(HBCDD_ALL[c],"_ng_gdw",sep="")]*((contam$dw_percent)/100)
}
contam$sommeHBCDD_ALL_ng_gww = apply(contam[,paste(HBCDD_ALL,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# HBCDD censored
for(c in 1:length(HBCDD_ALL)){
  contam[,paste(HBCDD_ALL[c],"_ng_gww_censored",sep="")] = contam[,paste(HBCDD_ALL[c],"_ng_gdw_censored",sep="")]*((contam$dw_percent)/100)
}
contam$sommeHBCDD_ALL_ng_gww_censored = apply(contam[,paste(HBCDD_ALL,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# --------------------------------------------------------------
# Save dataset
write_csv(x = contam, file = "data-raw/contam.csv")

# --------------------------------------------------------------
# Output data

usethis::use_data(contam, overwrite = TRUE)
usethis::use_data(metadata, overwrite = TRUE)
