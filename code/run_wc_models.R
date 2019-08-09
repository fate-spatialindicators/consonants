library(sdmTMB)
library(dplyr)
library(sp)

dat = readRDS("survey_data/joined_nwfsc_data.rds")

dplyr::group_by(dat, species) %>% 
  summarize(min = min(latitude[which(cpue_kg_km2 > 0)]),
    max = max(latitude[which(cpue_kg_km2 > 0)])) %>% 
  as.data.frame() %>% arrange(min)

# UTM transformation
dat_ll = dat
coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm = spTransform(dat_ll, 
  CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
# convert back from sp object to data frame
dat = as.data.frame(dat_utm)
dat = dplyr::rename(dat, longitude = longitude_dd, 
  latitude = latitude_dd)

df = expand.grid("species" = unique(dat$species),
  spatial_only=c(FALSE), 
  depth_effect = c(TRUE,FALSE),
  time_varying = c(FALSE),
  covariate = c("o2","temp")
)
saveRDS(df, "output/wc/models.RDS")

for(i in 1:nrow(df)) {
  
  # filter by species, and select range within occurrences
  sub = dplyr::filter(dat, 
    species == df$species[i]) %>% 
    dplyr::filter(latitude > min(latitude[which(cpue_kg_km2>0)]),
      latitude <= max(latitude[which(cpue_kg_km2>0)]),
      longitude > min(longitude[which(cpue_kg_km2>0)]),
      longitude < max(longitude[which(cpue_kg_km2>0)]))
  
  # rescale variables
  sub$depth = scale(log(sub$depth))
  sub$o2 = scale(log(sub$o2))
  sub$temp = scale(sub$temp)

  # drop points with missing values
  sub = dplyr::filter(sub, 
    !is.na(o2),!is.na(temp),!is.na(depth))
  # filter years based on covariate
  if(df$covariate[i]=="o2") {
    sub = dplyr::filter(sub, year%in%seq(2010,2015))
  } else {
    # temp observed for all years
    sub = sub
  }
  
  # rename variables to make code generic
  sub = dplyr::rename(sub, enviro = as.character(df$covariate[i]))
  
  # make spde
  spde <- make_spde(x = sub$longitude, y = sub$latitude, 
    n_knots = 150)
  
  formula = paste0("cpue_kg_km2 ~ -1")
  if(df$depth_effect[i]==TRUE) {
    formula = paste0(formula, " + depth + I(depth^2)")
  }
  
  time_formula = "~ -1"
  if(df$time_varying[i]==TRUE) {
    time_formula = paste0(time_formula, " + ", 
      "enviro", " + I(","enviro","^2)")
    time_varying = as.formula(time_formula)
    time = "year"
  } else {
    formula = paste0(formula, " + ", 
      "enviro", " + I(","enviro","^2)")
    time_varying = NULL
    time = NULL
  }
  formula = paste0(formula, " + as.factor(year)")
  
  # fit model
  m <- sdmTMB(
    formula = as.formula(formula),
    time_varying = time_varying,
    spde = spde,
    time = time,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = df$spatial_only[i],
    quadratic_roots = TRUE
  )
  
  saveRDS(m, file=paste0("output/wc/model_",i,".rds"))
  
}
