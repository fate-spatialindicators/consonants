library(sdmTMB)
library(dplyr)

spp_ranked = dplyr::mutate(dat, 
  bin = ifelse(cpue_kg_km2>0,1,0)) %>% 
  group_by(common_name) %>% 
  summarize(n = sum(bin)) %>% 
  arrange(-n)

df = expand.grid("common_name" = unique(spp_ranked$common_name),
  spatial_only=c(TRUE,FALSE),
)

for(i in 1:nrow(spp_ranked)) {
  
  sub = dplyr::filter(dat, 
    common_name == spp_ranked$common_name[i])
  sub$depthm = scale(log(sub$depthm))
  sub$o2 = scale(log(sub$o2))
  sub$degc = scale(sub$degc)
  
  sub = dplyr::filter(sub, year%in%seq(2010,2015)) %>% 
    dplyr::filter(!is.na(o2),!is.na(degc),!is.na(depthm))
  
  spde <- make_spde(x = sub$longitude_dd, y = sub$latitude_dd, 
    n_knots = 150)
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year),
    #depthm + I(depthm^2), 
    time_varying = ~ -1 + o2 + I(o2^2),
    spde = spde,
    time="year",
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/dynamic/",spp_ranked$common_name[i],"-","o2_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year) + depthm + I(depthm^2), 
    time_varying = ~ -1 + o2 + I(o2^2),
    spde = spde,
    time="year",
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/dynamic/depth-",spp_ranked$common_name[i],"-","o2_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year) + depthm + I(depthm^2), 
    time_varying = ~ -1 + o2 + I(o2^2),
    spde = spde,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/static/depth-",spp_ranked$common_name[i],"-","o2_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year), 
    #depthm + I(depthm^2), 
    time_varying = ~ -1 + o2 + I(o2^2),
    spde = spde,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/static/",spp_ranked$common_name[i],"-","o2_nodepth.rds"))
  
  # temp variables
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year),
    #depthm + I(depthm^2), 
    time_varying = ~ -1 + degc + I(degc^2),
    spde = spde,
    time="year",
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/dynamic/",spp_ranked$common_name[i],"-","degc_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year) + depthm + I(depthm^2), 
    time_varying = ~ -1 + degc + I(degc^2),
    spde = spde,
    time="year",
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/dynamic/depth-",spp_ranked$common_name[i],"-","degc_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year) + depthm + I(depthm^2), 
    time_varying = ~ -1 + degc + I(degc^2),
    spde = spde,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/static/depth-",spp_ranked$common_name[i],"-","degc_nodepth.rds"))
  
  m <- sdmTMB(
    cpue_kg_km2 ~ -1 + as.factor(year), 
    #depthm + I(depthm^2), 
    time_varying = ~ -1 + degc + I(degc^2),
    spde = spde,
    family = tweedie(link = "log"),
    data = sub,
    anisotropy = TRUE,
    spatial_only = TRUE
  )
  saveRDS(m, file=paste0("output/static/",spp_ranked$common_name[i],"-","degc_nodepth.rds"))
  
}