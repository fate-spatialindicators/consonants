library(sdmTMB)
library(dplyr)

dat = readRDS("survey_data/joined_nwfsc_data.rds")

spp_ranked = dplyr::mutate(dat, 
  bin = ifelse(cpue_kg_km2>0,1,0)) %>% 
  group_by(common_name) %>% 
  summarize(n = sum(bin)) %>% 
  arrange(-n)

df = expand.grid("common_name" = unique(spp_ranked$common_name),
  spatial_only=c(TRUE), 
  depth_effect = c(TRUE,FALSE),
  time_varying = c(TRUE,FALSE),
  covariate = c("o2","degc")
)
saveRDS(df, "output/wc/models.RDS")

for(i in 1:nrow(df)) {
  
  sub = dplyr::filter(dat, 
    common_name == df$common_name[i])
  sub$depthm = scale(log(sub$depthm))
  sub$o2 = scale(log(sub$o2))
  sub$degc = scale(sub$degc)

  # drop points with missing values
  sub = dplyr::filter(sub, year%in%seq(2010,2015)) %>% 
    dplyr::filter(!is.na(o2),!is.na(degc),!is.na(depthm))
  
  # rename variables to make code generic
  sub = dplyr::rename(sub, enviro = as.character(df$covariate[i]))
  
  # make spde
  spde <- make_spde(x = sub$longitude_dd, y = sub$latitude_dd, 
    n_knots = 150)
  
  formula = paste0("cpue_kg_km2 ~ -1 + as.factor(year)")
  if(df$depth_effect[i]==TRUE) {
    formula = paste0(formula, " + depthm + I(depthm^2)")
  }
  
  time_formula = "~ -1"
  if(df$time_varying[i]==TRUE) {
    time_formula = paste0(time_formula, " + ", 
      "enviro", " + I(","enviro","^2)")
  } else {
    formula = paste0(formula, " + ", 
      "enviro", " + I(","enviro","^2)")
  }
    
  # fit model
  m <- sdmTMB(
    formula = as.formula(formula),
    time_varying = as.formula(time_formula),
    spde = spde,
    time = ifelse(df$time_varying[i]==TRUE, "year", NULL),
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = df$spatial_only[i]
  )
  
  saveRDS(m, file=paste0("output/wc/model_",i,".rds"))
  
}