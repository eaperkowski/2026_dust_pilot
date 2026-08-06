#####################################################################
# Libraries
#####################################################################
library(tidyverse)

#####################################################################
# Load uncleaned dataset
#####################################################################
df_uncleaned <- read.csv("../data_sheets/li600_uncleaned.csv")
head(df_uncleaned)

#####################################################################
# Extract measurements, save as new file
#####################################################################
df_uncleaned %>%
  group_by(id, Date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  slice(-1) %>%
  dplyr::select(id, Date, gsw:VPDleaf, Fm., PhiPS2, 
                ETR, rh_r, Tleaf, flow, Qamb) %>%
  mutate(dust_status = ifelse(Date == "8/4/26" | Date == "8/5/26", 
                              "pre_dust", "post_dust")) %>%
  write.csv("../data_sheets/li600_cleaned.csv", row.names = F)



