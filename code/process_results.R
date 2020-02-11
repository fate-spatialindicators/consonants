library(ggplot2)
library(dplyr)

region=c("goa","wc")[2]
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
write.csv(df, file=paste0("output/",region,"_output.csv"))
#saveRDS(df,file=paste0("output/",region,"_output.rds"))

pdf(paste0("plots/",region,"-temp_range.pdf"))
level_order = dplyr::filter(df, !is.na(range), covariate=="temp",
  depth_effect == TRUE, range_se < 1) %>%
  dplyr::arrange(range) %>% select(species)
dplyr::filter(df, !is.na(range), covariate=="temp", depth_effect==TRUE, range_se < 1) %>% 
ggplot(aes(factor(species, level=level_order$species), range)) +
  geom_pointrange(aes(ymin=range-2*range_se, 
    ymax=range+2*range_se), col="darkblue") +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature range - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-temp_low.pdf"))
level_order = dplyr::filter(df, !is.na(low), covariate=="temp",
  depth_effect == TRUE, low_se < 1) %>%
  dplyr::arrange(low) %>% select(species)
dplyr::filter(df, !is.na(low), covariate=="temp", depth_effect==TRUE,low_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), low)) +
  geom_pointrange(aes(ymin=low-2*low_se, 
    ymax=low+2*low_se),col="darkblue") +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature low bound - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-temp_hi.pdf"))
level_order = dplyr::filter(df, !is.na(hi), covariate=="temp",
  depth_effect == TRUE, hi_se < 1) %>%
  dplyr::arrange(hi) %>% select(species)
dplyr::filter(df, !is.na(hi), covariate=="temp", depth_effect==TRUE,hi_se < 1) %>% 
  ggplot(aes(factor(species, level=level_order$species), hi)) +
  geom_pointrange(aes(ymin=hi-2*hi_se, 
    ymax=hi+2*hi_se),col="darkblue") +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("Temperature upper bound - ",region," survey"))
dev.off()

# o2 plots here - wc and bc species
pdf(paste0("plots/",region,"-o2_range.pdf"))
level_order = dplyr::filter(df, !is.na(range), covariate=="o2",
  depth_effect == TRUE, range_se < 1) %>%
  dplyr::arrange(range) %>% select(species)
dplyr::filter(df, !is.na(range), covariate=="o2", range_se < 1,depth_effect == TRUE) %>% 
  ggplot(aes(factor(species, level=level_order$species), range)) +
  geom_pointrange(aes(ymin=range-2*range_se, 
    ymax=range+2*range_se),col="darkblue") + ylim(0,10) +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 range - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-o2_low.pdf"))
level_order = dplyr::filter(df, !is.na(low), covariate=="o2",
  depth_effect == TRUE, low_se < 1) %>%
  dplyr::arrange(low) %>% select(species)
dplyr::filter(df, !is.na(low), covariate=="o2", low_se < 1,depth_effect == TRUE) %>% 
  ggplot(aes(factor(species, level=level_order$species), low)) +
  geom_pointrange(aes(ymin=low-2*low_se, 
    ymax=low+2*low_se),col="darkblue") +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 low bound - ",region," survey"))
dev.off()

pdf(paste0("plots/",region,"-o2_hi.pdf"))
level_order = dplyr::filter(df, !is.na(hi), covariate=="o2",
  depth_effect == TRUE, hi_se < 1) %>%
  dplyr::arrange(hi) %>% select(species)
dplyr::filter(df, !is.na(hi), covariate=="o2", hi_se < 1,depth_effect == TRUE) %>% 
  ggplot(aes(factor(species, level=level_order$species), hi)) +
  geom_pointrange(aes(ymin=hi-2*hi_se, 
    ymax=hi+2*hi_se),col="darkblue") +
  coord_flip() + xlab("Species") + ylab("Range") + 
  ggtitle(paste0("o2 upper bound - ",region," survey"))
dev.off()


