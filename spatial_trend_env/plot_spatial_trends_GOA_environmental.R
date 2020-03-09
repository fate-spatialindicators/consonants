library(ggplot2)
library(viridis)
library(scales)

load("goa_temp.Rdata")

# get coastlines and transform to projection and scale of data
shore <- rnaturalearth::ne_countries(continent = "north america", scale = "medium", returnclass = "sp")
shore <- sp::spTransform(shore, CRS = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
shore <- fortify(shore)
shore$long <- shore$long/1000
shore$lat <- shore$lat/1000

ex_year = 2003 #or should it be 2005?

plot_map_raster <- function(dat, column = "omega_s") {
  ggplot(dat, aes_string("X", "Y", fill = column)) +
    annotation_map(shore, color = "black", fill = "white", size=0.1) +
    geom_raster() +
    xlab("Eastings (km)") +
    ylab("Northings (km)")
}

# extract all coefficients
sr_se <- summary(temp_model$sd_report)[,"Std. Error"]
b_j <- unname(temp_model$model$par[grep("b_j", names(temp_model$model$par))])
b_j_se <- unname(sr_se[grep("b_j", names(sr_se))])
mm <- cbind(b_j, b_j_se)
colnames(mm) <- c("coef.est", "coef.se")
row.names(mm) <- colnames(model.matrix(temp_model$formula, temp_model$data))
# get mean density, when using all GOA survey years
pred_temp$mean <- dplyr::filter(pred_temp,year==ex_year)$omega_s + 0.2*dplyr::filter(pred_temp,year==ex_year)$zeta_s + b_j[10] + pred_temp$log_depth_scaled*b_j[1] + pred_temp$log_depth_scaled2*b_j[2]

# make plot of mean, with color scale centered on mean of means
pdf("spatial_trend_env/spatial_trends_GOA_mean_temp.pdf", width = 9, height = 3)
plot_map_raster(dplyr::filter(pred_temp,year==ex_year), "mean") +
  scale_fill_gradient2(low = muted("blue"), high = muted("red"), midpoint = 6.238697) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right") +
  ggtitle("Mean temperature (degrees C)")
dev.off()

# make plot of spatial trend (small over whole time series but try predicting in time chunks?)
pdf("spatial_trend_env/spatial_trends_GOA_temp.pdf", width = 9, height = 3)
plot_map_raster(dplyr::filter(pred_temp,year==ex_year), "zeta_s") +
  scale_fill_gradient2(low = muted("blue"), high = muted("red")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right") +
  ggtitle("Temperature trends (degrees C/yr)")
dev.off()

# make plot of predictions from full model (all fixed + random effects)
pdf("spatial_trend_env/spatial_trends_GOA_year_temp.pdf", width = 9, height = 5)
plot_map_raster(pred_temp, "est") +
  facet_wrap(~year) +
  coord_fixed() +
  #scale_fill_viridis_c() +
  scale_fill_gradient2(low = muted("blue"), high = muted("red"), midpoint = 6.238697) +
  theme(legend.title = element_blank()) +
  ggtitle("Temperature predictions (degrees C)")
dev.off()