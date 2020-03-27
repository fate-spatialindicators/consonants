library(tidyverse)
library(ggplot2)

df = read.csv("output/wc_output.csv")
#saveRDS(df,file=paste0("output/",region,"_output.rds"))

wc_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
wc_df$region = "West coast"

df = read.csv("output/goa_output.csv")
goa_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
goa_df$region = "Gulf of Alaska"

df = rbind(wc_df, goa_df) %>% 
  dplyr::rename("Depth included"="TRUE",
    "Depth omitted"="FALSE",
    "Species"="species")

ggplot(df, aes(`Depth included`,`Depth omitted`)) + 
  geom_point() + scale_x_continuous(trans="log") + 
  scale_y_continuous(trans="log") + 
  facet_wrap(~region)


