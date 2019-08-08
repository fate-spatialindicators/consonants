library(dplyr)
library(ggplot2)

dat = readRDS("survey_data/joined_nwfsc_data.rds")
f = dplyr::filter(dat, common_name == unique(dat$common_name)[1])
f$level = "75-100%"
f$level[which(f$latitude_dd < quantile(f$latitude_dd,0.75,na.rm=T))]="50-75%"
f$level[which(f$latitude_dd < quantile(f$latitude_dd,0.5,na.rm=T))]="25-50%"
f$level[which(f$latitude_dd < quantile(f$latitude_dd,0.25,na.rm=T))]="0-25%"

pdf("plots/plot_wc_bottom_temp_by_latitude.pdf")
group_by(f, year,level) %>% 
  summarize(m = mean(degc,na.rm=T), low=quantile(degc,0.025,na.rm=T), hi = quantile(degc,0.975,na.rm=T)) %>% 
  ggplot(aes(year, m)) + 
  geom_ribbon(aes(ymin=low,ymax=hi),alpha=0.3) + 
  geom_line() + 
  ylab("Bottom temperature") + xlab("Year") + 
  facet_wrap(~level,nrow=1)
dev.off()