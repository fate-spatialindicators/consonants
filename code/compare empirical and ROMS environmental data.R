library(tidyverse)

# get survey data

ROMS.names <- c("ROMS_oxygen_bottom_era5_monthly","ROMS_temp_bottom_era5_monthly")
ROMS.RDS.names <- paste0("survey_data/joined_nwfsc_data",ROMS.names,".rds")

#dat = readRDS("survey_data/joined_nwfsc_data.rds")
dat_temp_bottom <- readRDS(ROMS.RDS.names[2])
dat_oygen_bottom <- readRDS(ROMS.RDS.names[1])
# lapply(ROMS.RDS.names, function(filenames){
#   readRDS(ROMS.RDS.names[i])
# })
dat <- dat_temp_bottom %>%
  left_join(dat_oygen_bottom)

dat_2003_2010 <- dplyr::filter(dat, year%in%seq(2003,2010))

# visual comparison of empirical and ROMS env data 

plot_theme <-   theme_minimal()+
  theme(text=element_text(family="sans",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
theme_set(plot_theme)

#################################################
################## bottom temp ##################
#################################################

ggplot(data = dat_2003_2010, 
                            aes(x=temp, 
                                y = ROMS_temp_bottom_era5_monthly, 
                                colour=as.factor(year)
                                )
                            ) +
  geom_point() +
  geom_abline(data = dplyr::filter(dat, year%in%seq(2003,2010)),
              slope=1) +
  labs(x="Empirical bottom temperature",y="ROMS bottom temperature\n(monthly average)")

ggsave(here::here('plots','empirical v ROMS bottom temp.png'))

# bottom temp, faceted
ggplot(data = dat_2003_2010, 
       aes(x=temp, 
           y = ROMS_temp_bottom_era5_monthly,
           colour = as.factor(year)
       )
) +
  geom_point() +
  geom_abline(data = dplyr::filter(dat, year%in%seq(2003,2010)),
              slope=1) +
  facet_wrap(~year, nrow=5) +
  labs(x="Empirical bottom temperature",y="ROMS bottom temperature\n(monthly average)")

ggsave(here::here('plots','empirical v ROMS bottom temp, faceted by year.png'))

#################################################
################## bottom O2 ##################
#################################################

ggplot(data = dat_2003_2010, 
       aes(x=temp, 
           y = ROMS_oxygen_bottom_era5_monthly, 
           colour=as.factor(year)
       )
) +
  geom_point() +
  geom_abline(data = dplyr::filter(dat, year%in%seq(2003,2010)),
              slope=1) +
  labs(x="Empirical bottom oxygen",y="ROMS bottom oxygen\n(monthly average)")

ggsave(here::here('plots','empirical v ROMS bottom oxygen.png'))

# bottom temp, faceted
ggplot(data = dat_2003_2010, 
       aes(x=temp, 
           y = ROMS_oxygen_bottom_era5_monthly,
           colour = as.factor(year)
       )
) +
  geom_point() +
  geom_abline(data = dplyr::filter(dat, year%in%seq(2003,2010)),
              slope=1) +
  facet_wrap(~year, nrow=5) +
  labs(x="Empirical bottom temperature",y="ROMS bottom oxygen\n(monthly average)")

ggsave(here::here('plots','empirical v ROMS bottom oxygen, faceted by year.png'))
