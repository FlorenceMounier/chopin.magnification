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

#-------------------------------------------------------------------------
# PCB

# Calculation of concentrations normalized by the sum in ng/gdw
benthos_contam$sommePCB_ng_gdw = apply(benthos_contam[,paste(PCB,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
benthos_contam[, paste(PCB, "normalised_sum_ng_gdw", sep = "_")] <- benthos_contam[, paste(PCB, "_ng_gdw", sep = "")] / benthos_contam$sommePCB_ng_gdw

soles_contam$sommePCB_ng_gdw = apply(soles_contam[,paste(PCB,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
soles_contam[, paste(PCB, "normalised_sum_ng_gdw", sep = "_")] <- soles_contam[, paste(PCB, "_ng_gdw", sep = "")] / soles_contam$sommePCB_ng_gdw

# Calculations of concentrations and their sum in ng/glw
for(c in 1:length(PCB)){
  benthos_contam[,paste(PCB[c],"_ng_glw",sep="")] <- benthos_contam[,paste(PCB[c],"_ng_gdw",sep="")]/(benthos_contam$lip_dw_percent/100)
}
benthos_contam$sommePCB_ng_glw = apply(benthos_contam[,paste(PCB,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

for(c in 1:length(PCB)){
  soles_contam[,paste(PCB[c],"_ng_glw",sep="")] = soles_contam[,paste(PCB[c],"_ng_gdw",sep="")]/(soles_contam$lipid_percent_dw/100)
}
soles_contam$sommePCB_ng_glw = apply(soles_contam[,paste(PCB,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(PCB)){
  benthos_contam[,paste(PCB[c],"_ng_gww",sep="")] = benthos_contam[,paste(PCB[c],"_ng_gdw",sep="")]*(benthos_contam$dw_percent/100)
}
benthos_contam$sommePCB_ng_gww = apply(benthos_contam[,paste(PCB,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

for(c in 1:length(PCB)){
  soles_contam[,paste(PCB[c],"_ng_gww",sep="")] = soles_contam[,paste(PCB[c],"_ng_gdw",sep="")]*((100-soles_contam$water_percent)/100)
}
soles_contam$sommePCB_ng_gww = apply(soles_contam[,paste(PCB,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

#-------------------------------------------------------------------------
# PFAS

# Calculation of concentrations normalized by the sum in ng/gdw
benthos_contam$sommePFAS_ng_gdw = apply(benthos_contam[,paste(PFAS,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
benthos_contam[, paste(PFAS, "normalised_sum_ng_gdw", sep = "_")] <- benthos_contam[, paste(PFAS,"_ng_gdw",sep="")] /  benthos_contam$sommePFAS_ng_gdw

benthos_contam$sommePFAS_ng_gdw_censored = apply(benthos_contam[,paste(PFAS,"_ng_gdw_censored",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
benthos_contam[, paste(PFAS, "normalised_sum_ng_gdw_censored", sep = "_")] <- benthos_contam[, paste(PFAS,"_ng_gdw_censored",sep="")] / benthos_contam$sommePFAS_ng_gdw_censored

soles_contam$sommePFAS_ng_gdw = apply(soles_contam[,paste(PFAS,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
soles_contam[,paste(PFAS,"normalised_sum_ng_gdw",sep="_")] = soles_contam[,paste(PFAS,"_ng_gdw",sep="")]/soles_contam$sommePFAS_ng_gdw

soles_contam$sommePFAS_ng_gdw_censored = apply(soles_contam[,paste(PFAS,"_ng_gdw_censored",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
soles_contam[,paste(PFAS,"normalised_sum_ng_gdw_censored",sep="_")] = soles_contam[,paste(PFAS,"_ng_gdw_censored",sep="")]/soles_contam$sommePFAS_ng_gdw_censored

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(PFAS)){
  benthos_contam[,paste(PFAS[c],"_ng_gww",sep="")] = benthos_contam[,paste(PFAS[c],"_ng_gdw",sep="")]*(benthos_contam$dw_percent/100)
}
benthos_contam$sommePFAS_ng_gww = apply(benthos_contam[,paste(PFAS,"_ng_gww",sep="")], MARGIN = 1, FUN = sum)

for(c in 1:length(PFAS)){
  benthos_contam[,paste(PFAS[c],"_ng_gww_censored",sep="")] = benthos_contam[,paste(PFAS[c],"_ng_gdw_censored",sep="")]*(benthos_contam$dw_percent/100)
}
benthos_contam$sommePFAS_ng_gww_censored = apply(benthos_contam[,paste(PFAS,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum)


for(c in 1:length(PFAS)){
  soles_contam[,paste(PFAS[c],"_ng_gww",sep="")] = soles_contam[,paste(PFAS[c],"_ng_gdw",sep="")]*((100-soles_contam$water_percent)/100)
}
soles_contam$sommePFAS_ng_gww = apply(soles_contam[,paste(PFAS,"_ng_gww",sep="")], MARGIN = 1, FUN = sum)

for(c in 1:length(PFAS)){
  soles_contam[,paste(PFAS[c],"_ng_gww_censored",sep="")] = soles_contam[,paste(PFAS[c],"_ng_gdw_censored",sep="")]*((100-soles_contam$water_percent)/100)
}
soles_contam$sommePFAS_ng_gww_censored = apply(soles_contam[,paste(PFAS,"_ng_gww_censored",sep="")], MARGIN = 1, FUN = sum)

#-------------------------------------------------------------------------
# HBCDD

# Calculation of concentrations normalized by the sum in ng/gdw
benthos_contam$sommeHBCDD_ng_gdw = apply(benthos_contam[,paste(HBCDD,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
benthos_contam[, paste(HBCDD, "normalised_sum_ng_gdw", sep = "_")] <- benthos_contam[, paste(HBCDD, "_ng_gdw", sep = "")] / benthos_contam$sommeHBCDD_ng_gdw

soles_contam$sommeHBCDD_ng_gdw = apply(soles_contam[,paste(HBCDD,"_ng_gdw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)
soles_contam[, paste(HBCDD, "normalised_sum_ng_gdw", sep = "_")] <- soles_contam[, paste(HBCDD, "_ng_gdw", sep = "")] / soles_contam$sommeHBCDD_ng_gdw

# Calculations of concentrations and their sum in ng/glw
for(c in 1:length(HBCDD)){
  benthos_contam[,paste(HBCDD[c],"_ng_glw",sep="")] = benthos_contam[,paste(HBCDD[c],"_ng_gdw",sep="")]/(benthos_contam$lip_dw_percent/100)
}
benthos_contam$sommeHBCDD_ng_glw = apply(benthos_contam[,paste(HBCDD,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

for(c in 1:length(HBCDD)){
  soles_contam[,paste(HBCDD[c],"_ng_glw",sep="")] = soles_contam[,paste(HBCDD[c],"_ng_gdw",sep="")]/(soles_contam$lipid_percent_dw/100)
}
soles_contam$sommeHBCDD_ng_glw = apply(soles_contam[,paste(HBCDD,"_ng_glw",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# Calculations of concentrations and their sum in ng/gww
for(c in 1:length(HBCDD)){
  benthos_contam[,paste(HBCDD[c],"_ng_gww",sep="")] = benthos_contam[,paste(HBCDD[c],"_ng_gdw",sep="")]*(benthos_contam$dw_percent/100)
}
benthos_contam$sommeHBCDD_ng_gww = apply(benthos_contam[,paste(HBCDD,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

for(c in 1:length(HBCDD)){
  soles_contam[,paste(HBCDD[c],"_ng_gww",sep="")] = soles_contam[,paste(HBCDD[c],"_ng_gdw",sep="")]*((100-soles_contam$water_percent)/100)
}
soles_contam$sommeHBCDD_ng_gww = apply(soles_contam[,paste(HBCDD,"_ng_gww",sep="")], MARGIN = 1, FUN = sum, na.rm = TRUE)

# --------------------------------------------------------------
# Save dataset
write_csv(x = benthos_contam, file = "data-raw/benthos_contam.csv")
write_csv(x = soles_contam, file = "data-raw/sole_contam.csv")

# --------------------------------------------------------------
# Output data

usethis::use_data(PCB_lab, overwrite = TRUE)
usethis::use_data(log_Kow_PCB, overwrite = TRUE)

usethis::use_data(PFAS_lab, overwrite = TRUE)
usethis::use_data(subfamilies_PFAS, overwrite = TRUE)
usethis::use_data(n_C_PFAS, overwrite = TRUE)

usethis::use_data(HBCDD_lab, overwrite = TRUE)
usethis::use_data(log_Kow_HBCDD, overwrite = TRUE)

usethis::use_data(benthos_contam, overwrite = TRUE)
usethis::use_data(soles_contam, overwrite = TRUE)
