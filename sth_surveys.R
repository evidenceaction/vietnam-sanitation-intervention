library(plyr)
library(dplyr)
library(magrittr)
library(foreach)
library(lme4)
library(ggplot2)

sth.infection.types <- c("as", "tr", "hk")

hanam.sth.data <- read.csv("~/Data/Vietnam STH/hanam.csv") %>%
  mutate(sex=ifelse(is.na(male), 1, 2) %>% factor(levels=1:2, labels=c("female", "male")),
         male=ifelse(is.na(male), 0, male),
         female=ifelse(is.na(female), 0, female),
         age=male + female) %>%
  select(-c(male, female, TT, Pin.1, Pin.2)) %>%
  cbind(ldply(foreach(it=sth.infection.types) %do% paste(it, 1:2, sep="."), 
              function(cols, slide.data) slide.data[, cols] %>% rowMeans(na.rm=TRUE) %>% multiply_by(24),
              slide.data=.) %>% 
          t %>%
          set_colnames(paste0(sth.infection.types, "epg"))) %>%
  cbind(select(., ends_with("epg")) %>% is_greater_than(0) %>% set_colnames(paste0(sth.infection.types, "infect"))) %>%
  cbind(select(., ends_with("infect")) %>% equals(TRUE) %>% ifelse(1, 0) %>% set_colnames(paste0(sth.infection.types, "infect.bin"))) %>%
  mutate(sthinfect=select(., ends_with("infect.bin")) %>% rowSums %>% is_greater_than(0),
         sthinfect.bin=ifelse(sthinfect == TRUE, 1, 0),
         sthepg=NA)

infection.levels <- data.frame(infection.type=sth.infection.types,
                               moderate.epg=c(5000, 1000, 2000),
                               high.epg=c(50000, 10000, 4000))

hanam.sth.long.data <- hanam.sth.data %>%
  select(-matches("\\.\\d$")) %>%
  (l(.data ~ reshape(.data, 
                     direction="long", 
                     varying=llply(.v.names, function(vn, it) paste0(it, vn), .infection.types),
                     v.names=.v.names,
                     times=.infection.types,
                     timevar="infection.type",
                     idvar="student.ID",
                     sep="")) %>%
     add_args(.infection.types=c(sth.infection.types, "sth"),
              .v.names=c("infect", "infect.bin", "epg"))) %>%
  merge(infection.levels, by="infection.type", all.x=TRUE) %>%
  mutate(hi.infect=epg > moderate.epg,
         hi.infect.bin=ifelse(hi.infect == TRUE, 1, 0))

hanam.sth.long.data %>% 
  group_by(infection.type, school.ID) %>%
  summarize(school.name=first(as.character(school)),
            num.obs=n(),
            prevalence=mean(infect.bin), 
            hi.prevalence=mean(hi.infect.bin), 
            hi.prevalence.cond=hi.prevalence/prevalence,
            mean.epg=mean(epg)) 

hanam.sth.long.data %>% 
  filter(infection.type == "sth") %>%
  lmer(infect ~ (1|school.ID), data=., REML=FALSE) %>%
  summary

ggplot(hanam.sth.long.data) +
geom_freqpoly(aes(epg, color=infection.type)) +
  facet_wrap(~ school.ID)