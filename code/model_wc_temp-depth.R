library(mgcv)

hauls = readRDS("survey_data/wcbts_haul_2019-08-01.rds")
hauls$year = as.numeric(substr(hauls$date_yyyymmdd,1,4))
hauls$month = as.numeric(substr(hauls$date_yyyymmdd,5,6))
hauls$day = as.numeric(substr(hauls$date_yyyymmdd,7,8))
hauls$jday = date::mdy.date(hauls$month, hauls$day, hauls$year) - date::mdy.date(1, 1, hauls$year)

m = gam(temperature_at_gear_c_der ~ -1+as.factor(year)+
    s(longitude_dd,latitude_dd) + s(log(depth_hi_prec_m),bs="cr")+
    as.factor(month), data=hauls)
