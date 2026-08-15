#####################################################################
# Libraries
#####################################################################
library(tidyverse)

#####################################################################
# Load uncleaned dataset
#####################################################################
df_uncleaned <- read.csv("../data_sheets/li600_uncleaned.csv") %>%
  dplyr::select(, -(X.1:X.270))
head(df_uncleaned)

#####################################################################
# Extract measurements, save as new file
#####################################################################
df_uncleaned %>%
  filter(configName == "TT25_psf") %>%
  mutate(dust_status = ifelse(Date == "8/4/26" | Date == "8/5/26", 
                              "pre_dust", "post_dust")) %>%
  dplyr::select(id, Date, dust_status, gsw:VPDleaf, Fm., PhiPS2, 
                ETR, rh_r, Tleaf, flow, Qamb) %>%
  write.csv("../data_sheets/li600_cleaned.csv", row.names = F)





