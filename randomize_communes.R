library(plyr)
library(dplyr)

target.prov.names <- c("Phu Tho", "Hoa Binh", "Thanh Hoa", "Nghe An")
num.clusters <- 800

commune.data <- read.csv("communes.csv") 
names(commune.data) <- sub("_", ".", names(commune.data))

commune.data <- commune.data %>%
  filter(Prov.EName %in% target.prov.names) %>%
  mutate(total.pop=sum(COMPOPULA),
         total.num.communes=n()) %>%
  group_by(Prov.EName, Dist.EName) %>%
  mutate(dist.pop=sum(COMPOPULA),
         dist.num.communes=n(),
         dist.pop.weight=dist.pop/total.pop,
         num.survey.treated.clusters=round(dist.pop.weight*num.clusters/2)) %>%
  ddply(.(Prov.EName, Dist.EName), function(dist.df) {
    dist.df$treated <- dist.df$ID %in% sample(dist.df$ID, ceiling(nrow(dist.df)/2))
    
    dist.df$num.survey.treated.clusters <- min(dist.df$num.survey.treated.clusters, sum(!dist.df$treated))
    
    dist.df$survey[dist.df$treated] <- dist.df$ID[dist.df$treated] %in% (filter(dist.df, treated) %>% use_series(ID) %>% sample(dist.df$num.survey.treated.clusters))
    dist.df$survey[!dist.df$treated] <- dist.df$ID[!dist.df$treated] %in% (filter(dist.df, !treated) %>% use_series(ID) %>% sample(dist.df$num.survey.treated.clusters))
    
    return(dist.df)
  }) %>%
  group_by(Prov.EName, Dist.EName) %>%
  mutate(num.treated=sum(treated),
         num.survey=sum(survey))

dist.data <- commune.data %>%
  mutate(total.pop=sum(COMPOPULA)) %>%
  group_by(Prov.EName, Dist.EName) %>%
  summarize(pop=sum(COMPOPULA),
            num.communes=n(),
            num.treated=sum(treated),
            total.pop=first(total.pop)) %>%
  mutate(pop.weight=pop/total.pop,
         num.survey.treated.clusters=round(pop.weight*num.clusters/2)) %>%
  ungroup %>%


