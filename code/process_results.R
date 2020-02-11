library(ggplot2)
library(dplyr)

region=c("goa","wc")[1]
df = readRDS(paste0("output/",region,"/models.RDS"))

for(i in 1:nrow(df)) {
  fname = paste0("output/",region,"/model_",i,".rds")
  if(file.exists(fname)) {
    
  m = readRDS(fname)
  sd_report <- summary(m$sd_report)
  params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])
  df$low[i] = params$Estimate[1]
  df$low_se[i] = params$`Std. Error`[1]
  df$hi[i] = params$Estimate[2]
  df$hi_se[i] = params$`Std. Error`[2]
  df$range[i] = params$Estimate[3]
  df$range_se[i] = params$`Std. Error`[3]
  }
}

# remove junk columns
df = dplyr::select(df, -spatial_only, -time_varying)

# save results
write.csv(df, file=paste0("output/",region,"_output.rds"))
#saveRDS(df,file=paste0("output/",region,"_output.rds"))

pdf(paste0("plots/",region,"-temp_range.pdf"))
level_order = dplyr::filter(df, !is.na(range), covariate=="temp",
  depth_effect == TRUE, range_se < 1) %>%
  dplyr::arrange(range) %>% select(species)
dplyr::filter(df, !is.na(range), covariate=="temp", range_se < 1) %>% 
ggplot(aes(factor(species, level=level_order$species), range,col=depth_effect)) +
  geom_pointrange(aes(ymin=range-2*range_se, 
    ymax=range+2*range_se)) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature range - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-temp_low.pdf"))
level_order = dplyr::filter(df, !is.na(low), covariate=="temp",
  depth_effect == TRUE, low_se < 1) %>%
  dplyr::arrange(low) %>% select(species)
dplyr::filter(df, !is.na(low), covariate=="temp", low_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), low,col=depth_effect)) +
  geom_pointrange(aes(ymin=low-2*low_se, 
    ymax=low+2*low_se)) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature low bound - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-temp_hi.pdf"))
level_order = dplyr::filter(df, !is.na(hi), covariate=="temp",
  depth_effect == TRUE, hi_se < 1) %>%
  dplyr::arrange(hi) %>% select(species)
dplyr::filter(df, !is.na(hi), covariate=="temp", hi_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), hi,col=depth_effect)) +
  geom_pointrange(aes(ymin=hi-2*hi_se, 
    ymax=hi+2*hi_se)) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature upper bound - ",region," survey"))
dev.off()

# o2 plots here - wc and bc species
pdf(paste0("plots/",region,"-o2_range.pdf"))
level_order = dplyr::filter(df, !is.na(range), covariate=="o2",
  depth_effect == TRUE, range_se < 1) %>%
  dplyr::arrange(range) %>% select(species)
dplyr::filter(df, !is.na(range), covariate=="o2", range_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), range,col=depth_effect)) +
  geom_pointrange(aes(ymin=range-2*range_se, 
    ymax=range+2*range_se)) + ylim(0,10) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 range - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-o2_low.pdf"))
level_order = dplyr::filter(df, !is.na(low), covariate=="o2",
  depth_effect == TRUE, low_se < 1) %>%
  dplyr::arrange(low) %>% select(species)
dplyr::filter(df, !is.na(low), covariate=="o2", low_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), low,col=depth_effect)) +
  geom_pointrange(aes(ymin=low-2*low_se, 
    ymax=low+2*low_se)) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 low bound - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-temp_hi.pdf"))
level_order = dplyr::filter(df, !is.na(hi), covariate=="o2",
  depth_effect == TRUE, hi_se < 1) %>%
  dplyr::arrange(hi) %>% select(species)
dplyr::filter(df, !is.na(hi), covariate=="o2", hi_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), hi,col=depth_effect)) +
  geom_pointrange(aes(ymin=hi-2*hi_se, 
    ymax=hi+2*hi_se)) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 upper bound - ",region," survey"))
dev.off()


# pull in the catch data and try to look for attributes that
# might be useful in predicting breadth
raw = readRDS("survey_data/joined_nwfsc_data.rds")
summary = dplyr::group_by(raw,species) %>% 
  summarize(mean_lat = sum(latitude_dd*cpue_kg_km2)/sum(cpue_kg_km2),
    mean_depth = sum(depth*cpue_kg_km2)/sum(cpue_kg_km2),
    mean_o2 = sum(o2*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2[which(!is.na(o2))]),
    mean_temp = sum(temp*cpue_kg_km2,na.rm=T)/sum(cpue_kg_km2[which(!is.na(temp))]),
    mean_cpue = mean(cpue_kg_km2),
    depth_range = diff(range(depth[which(cpue_kg_km2>0)])),
    o2_range=diff(range(o2[which(cpue_kg_km2>0)],na.tm=T)),
    temp_range=diff(range(temp[which(cpue_kg_km2>0)],na.rm=T)))

df = dplyr::left_join(df,summary)

d = dplyr::filter(df, !is.na(range), 
  covariate=="temp",range_se < 1, 
  hi_se < 1, low_se < 1, depth_effect == TRUE)

# the high end of the 'env breadth' is very correlated with depth
plot(log(d$mean_depth), d$hi)

# Not great correlation, but also some correspondence between
# which species have high estimated ranges + those that have
# high empirical ranges
plot(d$range, log(d$depth_range))



summary(lm(range ~ log(mean_lat) + I(log(mean_lat)^2),data=d))
summary(gam(range ~ s(log(mean_lat)),data=d))