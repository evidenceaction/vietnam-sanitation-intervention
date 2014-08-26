library(dplyr)
library(plyr)
library(foreign)
library(stringr)

# Merging Ruth's province data --------------------------------------------

comm.1.data <- read.csv("~/Data/Vietnam ADM/Commune_1_Shape.csv")
comm.2.data <- read.csv("~/Data/Vietnam ADM/Communes_2_oxford.csv")

merged.data <-
  comm.2.data %>%
  mutate(District=str_trim(District)) %>%
  merge(comm.1.data[, c("Commune.ID", "Province.Name", "District.Name", "Commune.Name")],
        by.x=c("Province", "District", "Commune"), by.y=c("Province.Name", "District.Name", "Commune.Name"),
        all.x=TRUE, all.y=FALSE) %>%
  rename(c("Commune.ID"="ADM.Commune.ID"))

write.csv(merged.data, file="merged_all.csv")
merged.data %>% filter(!is.na(ADM.Commune.ID)) %>% write.csv(file="merged_matched.csv")
merged.data %>% filter(is.na(ADM.Commune.ID)) %>% write.csv(file="merged_unmatched.csv")
