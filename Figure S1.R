library(tidyverse)
library(ggplot2)
library(ggsidekick)
df = read.csv("output/wc_output.csv")
#saveRDS(df,file=paste0("output/",region,"_output.rds"))

wc_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
wc_df$region = "USA West Coast"

df = read.csv("output/goa_output.csv")
goa_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
goa_df$region = "Gulf of Alaska"

df = rbind(wc_df, goa_df) %>% 
  dplyr::rename("Depth included"="TRUE",
    "Depth omitted"="FALSE",
    "Species"="species") %>% 
  dplyr::mutate(`Depth included` = log(`Depth included`),
    `Depth omitted` = log(`Depth omitted`))

pdf("Figure_S1_range_sensitivity.pdf")
ggplot(df, aes(`Depth included`,`Depth omitted`)) + 
  geom_point(col="dark blue",size=2,alpha=0.6) + 
  facet_wrap(~region) + theme_sleek()
dev.off()

