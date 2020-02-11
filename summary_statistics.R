# code to summarize mean lat, lon, depth, etc by species
library(dplyr)

dat = readRDS("survey_data/joined_nwfsc_data.rds")

g = dplyr::group_by(dat, species) %>% 
  summarize(mean_depth = mean(depth[which(cpue_kg_km2>0)],na.rm=T),
    mean_lat = mean(latitude_dd[which(cpue_kg_km2>0)],na.rm=T),
    mean_lon = mean(longitude_dd[which(cpue_kg_km2>0)],na.rm=T),
    weighted_depth = sum(depth*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T),
    weighted_lat = sum(latitude_dd*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T),
    weighted_lon = sum(longitude_dd*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T))

write.csv(as.data.frame(g), file="output/summary_statistics_wc.csv")

dat = readRDS("survey_data/joined_goa_data.rds")

g = dplyr::group_by(dat, species) %>% 
  summarize(mean_depth = mean(depth[which(cpue_kg_km2>0)],na.rm=T),
    mean_lat = mean(latitude_dd[which(cpue_kg_km2>0)],na.rm=T),
    mean_lon = mean(longitude_dd[which(cpue_kg_km2>0)],na.rm=T),
    weighted_depth = sum(depth*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T),
    weighted_lat = sum(latitude_dd*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T),
    weighted_lon = sum(longitude_dd*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2,na.rm=T))

write.csv(as.data.frame(g), file="output/summary_statistics_goa.csv")
