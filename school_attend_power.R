library(sp)
library(plyr)
library(dplyr)
library(rgeos)
library(foreach)

get.boundary.adm <- function(in.adm, bounding.adm) {
  which.mask <- foreach(index=seq_len(nrow(in.adm)), .combine=c) %do% {
    gTouches(in.adm[index, ], bounding.adm)
  }
  
  return(in.adm[which.mask, ])
}

load("~/Data/Vietnam ADM/VNM_adm2.RData")
vn.province.adm <- gadm
rm(gadm)

intervene.province.ids <- c(31, 45, 49, 57) 
candidate.control.ids <- c(22, 34)

intervene.provinces.adm <- vn.province.adm[vn.province.adm$ID_2 %in% intervene.province.ids, ] 
nonintervene.provinces.adm <- vn.province.adm[!vn.province.adm$ID_2 %in% intervene.province.ids, ] 

adj.provinces.adm <- get.boundary.adm(vn.province.adm, intervene.provinces.adm)

load("~/Data/Vietnam ADM/VNM_adm3.RData")
vn.district.adm <- gadm
rm(gadm)

intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% intervene.province.ids, ]

adj.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% adj.provinces.adm$ID_2, ]

adj.district.adm <- get.boundary.adm(adj.district.adm, intervene.provinces.adm)

bound.intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% intervene.provinces.adm$ID_2, ]
bound.intervene.district.adm <- get.boundary.adm(bound.intervene.district.adm, adj.provinces.adm)

load("~/Data/Vietnam ADM/VNM_adm4.RData")
vn.comm.adm <- gadm
rm(gadm)

vn.comm.adm@data <- rename(vn.comm.adm@data, c("ID_2"="province.id",
                                     "NAME_2"="province.name", 
                                     "NAME_3"="district.name",
                                     "NAME_4"="commune.name")) %>%
  mutate(province.name=factor(province.name))

intervene.comm.adm <- vn.comm.adm[vn.comm.adm$province.id %in% intervene.province.ids, ]

adj.comm.adm <- vn.comm.adm[vn.comm.adm$ID_3 %in% adj.district.adm$ID_3, ]
adj.comm.adm <- get.boundary.adm(adj.comm.adm, intervene.provinces.adm)

bound.intervene.comm.adm <- vn.comm.adm[vn.comm.adm$ID_4 %in% intervene.comm.adm$ID_4, ]
bound.intervene.comm.adm <- get.boundary.adm(bound.intervene.comm.adm, adj.provinces.adm)

save(list=ls(pattern="adm$"), file="intervene_adm.RData")
