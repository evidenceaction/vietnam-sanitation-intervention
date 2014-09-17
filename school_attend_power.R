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
library(maptools)

# Geographic stuff --------------------------------------------------------

load("~/Data/Vietnam ADM/VNM_adm2.RData")
longlat.crs <- CRS(proj4string(gadm))
vn.province.adm <- spTransform(gadm, utm.crs)
rm(gadm)

intervene.province.ids <- c(31, 45, 49, 57) 
control.province.ids <- c(34) # 22

intervene.provinces.adm <- vn.province.adm[vn.province.adm$ID_2 %in% intervene.province.ids, ] 
control.provinces.adm <- vn.province.adm[vn.province.adm$ID_2 %in% control.province.ids, ] 

bound.intervene.provinces.adm <- get.boundary.adm(intervene.provinces.adm, control.provinces.adm) 

load("~/Data/Vietnam ADM/VNM_adm3.RData")
vn.district.adm <- spTransform(gadm, utm.crs)
rm(gadm)

intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% intervene.province.ids, ]

control.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% control.provinces.adm$ID_2, ] %>% 
  get.boundary.adm(bound.intervene.provinces.adm)

bound.intervene.district.adm <- vn.district.adm[vn.district.adm$ID_2 %in% bound.intervene.provinces.adm$ID_2, ] %>%
  get.boundary.adm(control.provinces.adm)

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
# all.control.comm.adm <- vn.comm.adm[vn.comm.adm$province.id %in% control.provinces.adm$ID_2, ] 
# control.comm.adm <- get.boundary.adm(all.control.comm.adm, intervene.provinces.adm)

bound.intervene.comm.adm <- vn.comm.adm[vn.comm.adm$ID_3 %in% bound.intervene.district.adm$ID_3, ] %>% 
  get.boundary.adm(bound.intervene.provinces.adm)
# all.bound.intervene.comm.adm <- vn.comm.adm[vn.comm.adm$province.id %in% bound.intervene.provinces.adm$ID_2, ]
# bound.intervene.comm.adm <- get.boundary.adm(all.bound.intervene.comm.adm, control.provinces.adm)

save(list=ls(pattern="adm$"), file="intervene_adm.RData")

# Plots -------------------------------------------------------------------

water.areas <- readShapeSpatial("~/Data/Vietnam ADM/VNM_water_areas_dcw.shp") 
water.lines <- readShapeSpatial("~/Data/Vietnam ADM/VNM_water_lines_dcw.shp")

bound.intervene.provinces.adm <- spTransform(bound.intervene.provinces.adm, longlat.crs)
control.provinces.adm <- spTransform(control.provinces.adm, longlat.crs)
bound.intervene.comm.adm <- spTransform(bound.intervene.comm.adm, longlat.crs)
control.comm.adm <- spTransform(control.comm.adm, longlat.crs)
# all.bound.intervene.comm.adm <- spTransform(all.bound.intervene.comm.adm, longlat.crs)
# all.control.comm.adm <- spTransform(all.control.comm.adm, longlat.crs)

water.color <- "deepskyblue3"

plot(rbind(bound.intervene.provinces.adm, control.provinces.adm), border="grey48", lty="dashed", lwd=2)
plot(rbind(bound.intervene.comm.adm, control.comm.adm), add=TRUE)
plot(water.lines, col=water.color, add=TRUE)
plot(water.areas, col=water.color, border=water.color, add=TRUE)
plot(rbind(bound.intervene.provinces.adm, control.provinces.adm), border="grey48", lty="dashed", lwd=2, add=TRUE)
plot(rbind(bound.intervene.comm.adm, control.comm.adm), add=TRUE)
plot(control.comm.adm, add=TRUE, border="red")

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

calc.vn.attend.mde <- function(clust.size) 
  calc.mde(num.clust=round(mean.num.schools.commune * (num.control.commune + num.treat.commune)), 
           alloc.frac=num.treat.commune/(num.control.commune + num.treat.commune), 
           clust.size=clust.size,
           icc=icc, 
           residual=sd(hm.data$still.in.school))

(mde <- calc.vn.attend.mde(clust.size=100))
(mde/sd(hm.data$still.in.school))

qplot(c(1, 300), stat="function", fun=calc.vn.attend.mde, geom="line", color="MDE") +
  stat_function(aes(color="MDE/SD"), fun=function(x) calc.vn.attend.mde(x)/sd(hm.data$still.in.school)) +
  coord_cartesian(ylim=c(0.0,0.5)) +
  scale_color_discrete("")
