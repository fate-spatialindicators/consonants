library(mgcv)

hauls = readRDS("survey_data/joined_goa_data.rds")

hauls = hauls[which(hauls$species==""),]

m = gam(temp ~ -1+as.factor(year)+
    s(longitude_dd,latitude_dd) + s(log(depth),bs="cr")+
    as.factor(month), data=hauls)

library(randomForest)
hauls$year = as.factor(hauls$year)
hauls$month = as.factor(hauls$month)
hauls = hauls[which(!is.na(hauls$temp)),]
rf = randomForest(temp ~ year + longitude_dd + 
    latitude_dd + depth + month, data=hauls)
library(randomForestExplainer)
