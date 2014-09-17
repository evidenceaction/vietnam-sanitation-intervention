library(sp)
library(plyr)
library(dplyr)
library(magrittr)
library(rgeos)
library(rgdal)
library(foreach)
library(ICC)
library(lme4)
library(foreign)
library(maptools)

# Geographic Constants ----------------------------------------------------

utm.crs <- CRS("+proj=utm +zone=48 +ellps=WGS84") # use this CRS to propery measure distances
boundary.max.dist <- 5000 # meters

# Budget Constants ---------------------------------------------------------------

financial.budget <- 250000
time.budget <- 35 

num.sth.teams.per.province <- 2
team.size <- 3 
num.target.provinces <- 4
num.commune.visits <- 2
child.compliance <- 0.7 # fraction of children who return STH sample

travel.between.time.hrs <- 1 
travel.to.time.hrs <- 2
lab.time.hrs <- 0.5 # 30 min of 1 lab technician per child
day.length <- 12 - (travel.to.time.hrs * 2) + travel.between.time.hrs # length of work day - time to and from communes + double counted travel between time (below) 

financial.clust.cost <- 80 * 2 # Doubling because Ruth believes the cost estimates we got don't include per diem expenses for survey team 
financial.child.cost <- 2.5 + 10      # $2.5/child (STH) + $10/child (HH survey)

time.clust.cost.1.hrs <- travel.between.time.hrs + 2 # Travelling one way to communes + registration and selection (assumed fixed)  
time.child.cost.1.hrs <- 0 
time.clust.cost.2.hrs <- travel.between.time.hrs # Travelling one way to communes 
time.child.cost.2.hrs <- 0.04 + (lab.time.hrs/team.size) # 0.04 hr STH sampling/child + lab time

# Border Budget Constants -------------------------------------------------

# MISSING: attendance spot checks: travel and checking

border.financial.clust.cost <- 80            
border.financial.child.cost <- 2.5 + 10      # $2.5/child (STH) + $10/child (HH survey) 

# Power Functions ---------------------------------------------------------------

calc.mde <- function(sig.level=0.05, power=0.8, alloc.frac=0.5, icc=1/14, residual=0.3, num.clust, clust.size) {
  M <- qnorm(sig.level/2, lower.tail=FALSE) + qnorm(power)

  (M / sqrt(alloc.frac * (1 - alloc.frac) * num.clust)) * sqrt(icc + (((1 - icc) / clust.size) * residual))
}  

vn.calc.mde <- function(clust.size, num.clust) calc.mde(icc=0.04, num.clust=num.clust, clust.size=clust.size, residual=0.16)

mean.num.schools.commune <- 1.7

# Budget Functions --------------------------------------------------------

calc.cost <- function(num.clust, clust.size, 
                      clust.cost=financial.clust.cost,
                      child.cost=financial.child.cost) {
  num.clust * clust.cost + num.clust * clust.size * child.cost 
}

cost.budget.curve <- function(num.clust, budget, 
                      clust.cost=financial.clust.cost,
                      child.cost=financial.child.cost) {
  (budget - num.clust * clust.cost)/(num.clust * child.cost)  
}

# province.day.time.budget.curve <- function(num.clust.per.day, 
#                                            clust.cost,
#                                            child.cost,
#                                            num.teams=num.sth.teams.per.province) { 
#   if (child.cost == 0) {
#     if ((floor(num.clust.per.day / num.teams) * clust.cost) > day.length) {
#       return(NA)
#     } else {
#       return(Inf)
#     }
#   } else {
#     return(max(((day.length * num.teams / num.clust.per.day) - clust.cost)/child.cost, 0))
#   }
# }

# get.province.day.clusters <- function(clust.size, 
#                                       clust.cost,
#                                       child.cost,
#                                       num.teams.per.province=num.sth.teams.per.province,
#                                       max.clust=1600) {
#   tryCatch({
#     uniroot(function(num.clust) clust.size - province.day.time.budget.curve(num.clust=num.clust, 
#                                                                             clust.cost=clust.cost, 
#                                                                             child.cost=child.cost, 
#                                                                             num.teams=num.teams.per.province),
#             interval=c(1, max.clust)) %>% use_series(root) %>% floor %>% max(0) 
#   }, error=function(e) NA)
# }

# time.budget.curve <- function(num.clust, budget,
#                            num.teams.per.province=num.sth.teams.per.province,
#                            num.provinces=num.target.provinces) { 
#   max(province.day.time.budget.curve(floor(num.clust/(num.provinces * num.teams.per.province * budget)), 
#                                 clust.cost=time.clust.cost.1.hrs,
#                                 child.cost=time.child.cost.1.hrs,
#                                 num.teams=num.teams.per.province) +
#       province.day.time.budget.curve(floor(num.clust/(num.provinces * num.teams.per.province * budget)), 
#                                 clust.cost=time.clust.cost.2.hrs,
#                                 child.cost=time.child.cost.2.hrs,
#                                 num.teams=num.teams.per.province), 
#       0)
# }

calc.time.cost <- function(num.clust, 
                           clust.size, 
                           num.clust.per.day.1,
                           num.clust.per.day.2,
                           num.teams.per.province=num.sth.teams.per.province,
                           num.provinces=num.target.provinces) {
  (num.clust / (num.clust.per.day.1 * num.teams.per.province * num.provinces)) +
    (num.clust / (num.clust.per.day.2 * num.teams.per.province * num.provinces)) 
}

# Geographic Functions ----------------------------------------------------

get.boundary.adm <- function(in.adm, bounding.adm, max.distance=boundary.max.dist) {
  which.mask <- foreach(index=seq_len(nrow(in.adm)), .combine=c) %do% {
    gDistance(in.adm[index, ], bounding.adm) <= max.distance 
  }
  
  return(in.adm[which.mask, ])
}

