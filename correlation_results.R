# look at high level correlations between mean lats/depths and estimated affinities

library(ggplot2)
library(dplyr)

region=c("goa","wc")[2]
df = read.csv(paste0("output/",region,"_output.csv"),stringsAsFactors = FALSE) %>% 
  dplyr::filter(depth_effect==TRUE) %>% 
  dplyr::select(-X,-depth_effect)

# pull in summary data
summary = read.csv(paste0("output/summary_statistics_",region,".csv"),
  stringsAsFactors = FALSE) %>% 
  dplyr::select(-X)

df = dplyr::left_join(df,summary)

# cluster analysis

# look at o2 data
cor = dplyr::filter(df,covariate=="o2") %>% 
  dplyr::select(species,low,hi,range,mean_depth,
    mean_lat,mean_lon,weighted_depth,weighted_lat,
    weighted_lon)
cor = cor[complete.cases(cor),]
scale_cor = cor
for(i in 2:ncol(cor)) {scale_cor[,i] = scale(cor[,i])}

# naive clustering of some variables
dmat=dist(scale_cor[,c("low","range","weighted_depth","weighted_lat")])
hcl = hclust(dist(scale_cor[,c("low","range","weighted_depth","weighted_lat")]))

# 1.4 ml / L is break for dissolved oxygen


# look at temp data
cor = dplyr::filter(df,covariate=="temp") %>% 
  dplyr::select(species,low,hi,range,mean_depth,
    mean_lat,mean_lon,weighted_depth,weighted_lat,
    weighted_lon)
cor = cor[complete.cases(cor),]
