library(sdmTMB)
library(dplyr)

dat = readRDS("survey_data/joined_nwfsc_data.rds")

df = expand.grid("species" = unique(dat$species),
  spatial_only=c(TRUE,FALSE), 
  depth_effect = c(TRUE,FALSE),
  time_varying = c(FALSE),
  covariate = c("o2","temp")
)
saveRDS(df, "output/wc/models.RDS")

for(i in 1:nrow(df)) {
  
  sub = dplyr::filter(dat, 
    species == df$species[i])
  sub$depth = scale(log(sub$depth))
  sub$o2 = scale(log(sub$o2))
  sub$temp = scale(sub$temp)

  # drop points with missing values
  sub = dplyr::filter(sub, year%in%seq(2010,2015)) %>% 
    dplyr::filter(!is.na(o2),!is.na(temp),!is.na(depth))
  
  # rename variables to make code generic
  sub = dplyr::rename(sub, enviro = as.character(df$covariate[i]))
  
  # make spde
  spde <- make_spde(x = sub$longitude_dd, y = sub$latitude_dd, 
    n_knots = 150)
  
  formula = paste0("cpue_kg_km2 ~ -1 + as.factor(year)")
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

  # fit model
  m <- sdmTMB(
    formula = as.formula(formula),
    time_varying = time_varying,
    spde = spde,
    time = time,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = df$spatial_only[i]
  )
  
  saveRDS(m, file=paste0("output/wc/model_",i,".rds"))
  
}
