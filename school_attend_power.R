library(sp)
library(plyr)
library(dplyr)
library(rgeos)
library(rgdal)
library(foreach)
library(ICC)
library(lme4)
library(foreign)
library(ggplot2)

# Geographic stuff --------------------------------------------------------

get.boundary.adm <- function(in.adm, bounding.adm, max.distance=2000) {
  which.mask <- foreach(index=seq_len(nrow(in.adm)), .combine=c) %do% {
#     gTouches(in.adm[index, ], bounding.adm)
    gDistance(in.adm[index, ], bounding.adm) <= max.distance
  }
  
  return(in.adm[which.mask, ])
}

utm.crs <- CRS("+proj=utm +zone=48 +ellps=WGS84") # use this CRS so I can propery measure distances

load("~/Data/Vietnam ADM/VNM_adm2.RData")
vn.province.adm <- spTransform(gadm, utm.crs)
rm(gadm)

intervene.province.ids <- c(31, 45, 49, 57) 
control.province.ids <- c(34) # 22

intervene.provinces.adm <- vn.province.adm[vn.province.adm$ID_2 %in% intervene.province.ids, ] 
# nonintervene.provinces.adm <- vn.province.adm[!vn.province.adm$ID_2 %in% intervene.province.ids, ] 
control.provinces.adm <- vn.province.adm[vn.province.adm$ID_2 %in% control.province.ids, ] 

bound.intervene.provinces.adm <- get.boundary.adm(intervene.provinces.adm, control.provinces.adm) 
# adj.provinces.adm <- get.boundary.adm(vn.province.adm, intervene.provinces.adm)

load("~/Data/Vietnam ADM/VNM_adm3.RData")
vn.district.adm <- spTransform(gadm, utm.crs)
rm(gadm)

intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% intervene.province.ids, ]

control.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% control.provinces.adm$ID_2, ] %>% 
  get.boundary.adm(bound.intervene.provinces.adm)

# adj.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% adj.provinces.adm$ID_2, ]
# adj.district.adm <- get.boundary.adm(adj.district.adm, intervene.provinces.adm)

bound.intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% bound.intervene.provinces.adm$ID_2, ] %>%
  get.boundary.adm(control.provinces.adm)
# bound.intervene.district.adm <- get.boundary.adm(bound.intervene.district.adm, adj.provinces.adm)

load("~/Data/Vietnam ADM/VNM_adm4.RData")
vn.comm.adm <- spTransform(gadm, utm.crs) 
rm(gadm)

vn.comm.adm@data <- rename(vn.comm.adm@data, c("ID_2"="province.id",
                                     "NAME_2"="province.name", 
                                     "NAME_3"="district.name",
                                     "NAME_4"="commune.name")) %>%
  mutate(province.name=factor(province.name))

intervene.comm.adm <- vn.comm.adm[vn.comm.adm$province.id %in% intervene.province.ids, ]

control.comm.adm <- vn.comm.adm[vn.comm.adm$ID_3 %in% control.district.adm$ID_3, ] %>% 
  get.boundary.adm(intervene.provinces.adm)

# adj.comm.adm <- vn.comm.adm[vn.comm.adm$ID_3 %in% adj.district.adm$ID_3, ]
# adj.comm.adm <- get.boundary.adm(adj.comm.adm, intervene.provinces.adm)

bound.intervene.comm.adm <- vn.comm.adm[vn.comm.adm$ID_3 %in% bound.intervene.district.adm$ID_3, ] %>%
  get.boundary.adm(control.provinces.adm)
# bound.intervene.comm.adm <- vn.comm.adm[vn.comm.adm$ID_4 %in% intervene.comm.adm$ID_4, ]
# bound.intervene.comm.adm <- get.boundary.adm(bound.intervene.comm.adm, adj.provinces.adm)

plot(rbind(bound.intervene.comm.adm, control.comm.adm))
plot(control.comm.adm, add=T, col="red")

save(list=ls(pattern="adm$"), file="intervene_adm.RData")

# Calculating power -------------------------------------------------------

mean.num.schools.commune <- 1.7
miguel.kremer.residual <- 0.273 / sqrt(1 - 0.23)
num.control.commune <- nrow(control.comm.adm)
num.treat.commune <- nrow(bound.intervene.comm.adm)

calc.mde <- function(sig.level=0.05, power=0.8, alloc.frac=0.5, num.clust=300, clust.size=25, icc=0.04, residual=miguel.kremer.residual) {
  M <- qnorm(sig.level/2, lower.tail=FALSE) + qnorm(power)

  (M / sqrt(alloc.frac * (1 - alloc.frac) * num.clust)) * sqrt(icc + (((1 - icc) / clust.size) * residual))
}  

hm.data <- read.dta("~/Data/VDHS/2005/VNPR53FL.DTA", convert.underscore=TRUE, convert.factors=FALSE) %>% # Household member data
  rename(c("hv001"="cluster.num", 
           "hv002"="hh.num",
           "shprovin"="province",
           "hv105"="age",
           "hv110"="still.in.school")) %>%
  filter(province %in% c(8, 25))  # Phu Tho and Tuyen Quang

lmer(still.in.school ~ (1|cluster.num), data=hm.data, REML=FALSE) %>% 
  summary

icc <- 0.002/(0.002 + 0.2)

calc.vn.attend.mde <- calc.mde(num.clust=round(mean.num.schools.commune * (num.control.commune + num.treat.commune)), 
                               alloc.frac=num.treat.commune/(num.control.commune + num.treat.commune), 
                               icc=icc, 
                               residual=sd(hm.data$still.in.school)))

(mde <- calc.vn.attend.mde(clust.size=100)
(mde/sd(hm.data$still.in.school))

