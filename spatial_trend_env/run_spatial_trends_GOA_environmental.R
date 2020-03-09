#devtools::install_github("pbs-assess/sdmTMB")
library(ggplot2)
library(raster)
library(rasterize)
library(sp)
library(sdmTMB)
library(dplyr)

dat = readRDS("spatial_trend_env/data/AK_BTS.rds")

dat = dplyr::filter(dat, SURVEY == "GOA") %>% 
  dplyr::mutate(ID = paste(LATITUDE,LONGITUDE,DATE)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(LAT = LATITUDE[1],LON=LONGITUDE[1],
    DATE=DATE[1],YEAR=YEAR[1],BOTTOM_DEPTH=BOTTOM_DEPTH[1],
    TEMP = GEAR_TEMPERATURE[1],DAY=DAY[1],MONTH=MONTH[1])

names(dat) = tolower(names(dat))

# UTM transformation
dat_ll = dat
coordinates(dat_ll) <- c("lon", "lat")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm = spTransform(dat_ll, 
  CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# convert back from sp object to data frame
dat = as.data.frame(dat_utm)
# convert to km
dat = dplyr::rename(dat, longitude = lon, 
  latitude = lat)
dat$latitude = dat$latitude /1000
dat$longitude = dat$longitude /1000

# try to add in seasonal component
dat$jday = date::mdy.date(dat$month, dat$day, dat$year) -
  date::mdy.date(1, 1, dat$year)
dat$jday_scaled = scale(dat$jday)
dat$jday_scaled2 = dat$jday_scaled ^ 2

dat = dplyr::filter(dat,bottom_depth>0) %>% 
  dplyr::mutate(log_depth_scaled = scale(log(bottom_depth)),
    log_depth_scaled2 = log_depth_scaled ^ 2)

# fit first model to raw geospatial data
c_spde <- make_spde(dat$longitude, dat$latitude, n_knots = 300)
# fit model with ~ gaussian response to temperature
temp_model <- sdmTMB(formula = temp ~ -1 + log_depth_scaled +
    log_depth_scaled2 + as.factor(year) + jday_scaled + jday_scaled2,
  data = dat,
  time = "year", spde = c_spde, anisotropy = TRUE,
  silent = TRUE, spatial_trend = TRUE, spatial_only = FALSE,#family = tweedie(link = "log"),
  control = sdmTMBcontrol(step.min = 0.01, step.max = 1))

# load prediction grid
goa_grid <- readRDS("spatial_trend_env/data/GOA_predict_data.rds")
goa_grid <- goa_grid %>% filter(year == 2019) %>% dplyr::select(-year)
goa_grid$loc = seq(1,nrow(goa_grid))

# truncate limits based on haul filters for OR above
df = expand.grid(loc=unique(goa_grid$loc),
                 year = unique(dat$year))
df = left_join(df,goa_grid,by="loc")
df$jday_scaled = 0
df$jday_scaled2 = 0

pred_temp = predict(temp_model,
                    newdata=df,
                    xy_cols=c("X","Y"),
                    return_tmb_object = FALSE)
pred_temp_tmb = predict(temp_model,
                    newdata=df,
                    xy_cols=c("X","Y"),
                    return_tmb_object = TRUE)

save(temp_model,pred_temp,pred_temp_tmb,file="spatial_trend_env/goa_temp.Rdata")

