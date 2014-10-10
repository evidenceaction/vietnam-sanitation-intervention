library(plyr)
library(dplyr)
library(foreign)
library(stringr)
library(rgeos)
library(maptools)
library(rgdal)

source("util.R")

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


# Merging vietnamese communes names with working data set -----------------

working.data <- read.csv("Clean_Communes_All_IDs.csv", stringsAsFactors=FALSE)
names(working.data) <- gsub("_|\\.+", ".", names(working.data)) %>% gsub("\\.$", "", .)
working.data <- working.data %>%
  mutate(Commune.Shape.ID=as.numeric(Commune.Shape.ID))

target.prov.ids <- working.data$Province.Shape.ID %>% unique
duplicated.commune.ids <- working.data$Commune.Shape.ID[!is.na(working.data$Commune.Shape.ID) & duplicated(working.data$Commune.Shape.ID)]

load("~/Data/Vietnam ADM/VNM_adm4.RData")
vn.comm.adm <- gadm
vn.comm.adm@data <- vn.comm.adm@data %>% rename(c("ID_2"="province.id", 
                                                  "NAME_2"="province.name", 
                                                  "ID_4"="commune.id",
                                                  "NAME_4"="commune.name")) %>% 
  filter(province.id %in% target.prov.ids)
rm(gadm)
duplicated.commune.ids.2 <- vn.comm.adm$commune.id[duplicated(vn.comm.adm$commune.id)]

working.data <- merge(working.data, 
      vn.comm.adm %>% select(commune.id, commune.name) %>% rename(c("commune.name"="Commune.Vietnamese")), 
      by.x="Commune.Shape.ID", by.y="commune.id", 
      all.x=TRUE, all.y=FALSE,
      incomparables=union(duplicated.commune.ids, duplicated.commune.ids.2))

write.table(working.data, file="Clean_Communes_All_IDs_2.csv", row.names=FALSE, col.names=gsub("\\.", " ", names(working.data)), sep=",")

# Adding polygon areas for communes ---------------------------------------

working.data <- read.csv("Clean_Communes_All_IDs.csv", stringsAsFactors=FALSE)
names(working.data) <- gsub("_|\\.+", ".", names(working.data)) %>% gsub("\\.$", "", .)

target.prov.ids <- working.data$Province.Shape.ID %>% unique
duplicated.commune.ids <- working.data$Commune.Shape.ID[!is.na(working.data$Commune.Shape.ID) & duplicated(working.data$Commune.Shape.ID)]

load("~/Data/Vietnam ADM/VNM_adm4.RData")
vn.comm.adm <- spTransform(gadm, utm.crs)
vn.comm.adm@data <- vn.comm.adm@data %>% rename(c("ID_2"="province.id", 
                                                  "NAME_2"="province.name", 
                                                  "ID_4"="commune.id",
                                                  "NAME_4"="Commune.Vietnamese"))  
vn.comm.adm <- vn.comm.adm[vn.comm.adm$province.id %in% target.prov.ids, ]
rm(gadm)
duplicated.commune.ids.2 <- vn.comm.adm$commune.id[duplicated(vn.comm.adm$commune.id)]

vn.comm.adm$commune.area.sqm <- laply(vn.comm.adm@polygons, function(polygon) polygon@area)

working.data <- merge(working.data, 
      vn.comm.adm[, c("commune.id", "Commune.Vietnamese", "commune.area.sqm")],
      by.x="Commune.Shape.ID", by.y="commune.id", 
      all.x=TRUE, all.y=FALSE,
      incomparables=union(duplicated.commune.ids, duplicated.commune.ids.2))

write.table(working.data, file="Clean_Communes_All_IDs_2.csv", row.names=FALSE, col.names=gsub("\\.", " ", names(working.data)), sep=",")

# Some more areas ---------------------------------------------------------

readShapeSpatial("Admin_4_newID_shapefile/admin4updated.shp") %>%
  (l(shp ~ {
    proj4string(shp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 
    return(shp)
  })) %>%
  spTransform(utm.crs) %>%
  (l(shp ~ {
    shp@data$area <- laply(shp@polygons, function(polygon) polygon@area)
    return(shp@data)
  })) %>%
  write.csv(file="Admin_4_newID_shapefile/admin4updated.csv")
