devtools::install_github("pbs-assess/sdmTMB")
library(sdmTMB)
library(dplyr)
library(sp)
library(gsw)

dat = readRDS("survey_data/joined_nwfsc_data.rds")
# analyze sablefish for years and hauls with adequate oxygen and temperature data, within range of occurrence
dat = filter(dat, species == "sablefish", year%in%seq(2010,2015), 
             !is.na(temp), !is.na(o2), !is.na(sal),
             latitude_dd > min(latitude_dd[which(cpue_kg_km2>0)]),
             latitude_dd <= max(latitude_dd[which(cpue_kg_km2>0)]),
             longitude_dd > min(longitude_dd[which(cpue_kg_km2>0)]),
             longitude_dd < max(longitude_dd[which(cpue_kg_km2>0)]))

# compute metabolic index (mi) --------------------------------------------
# converted from Halle Berger matlab script

#O2 from trawl data is in ml/l - may need to be converted to umol/kg
gas_const = 8.31
partial_molar_vol = 0.000032
kelvin = 273.15
boltz = 0.000086173324

#calculate percent saturation for O2 - assumes  units of mL O2/L
# Input:       S = Salinity (pss-78)
#              T = Temp (deg C) ! use potential temp
#depth is in meters
#[umole/kg] = [ml/L]*44660/(sigmatheta(P=0,theta,S) + 1000)
dat$SA = gsw_SA_from_SP(dat$sal,dat$depth,dat$longitude_dd,dat$latitude_dd) #absolute salinity for pot T calc
dat$pt = gsw_pt_from_t(dat$SA,dat$temp,dat$depth) #potential temp at a particular depth
dat$CT = gsw_CT_from_t(dat$SA,dat$temp,dat$depth) #conservative temp
dat$sigma0 = gsw_sigma0(dat$SA,dat$CT)
dat$o2_umolkg = dat$o2*44660/(dat$sigma0+1000)
# calc o2 solubility, relies on o2 in umol/kg
gsw_O2sol_SP_pt <- function(sal,pt) {
  x = dat$sal
  pt68 = dat$pt*1.00024
  y = log((298.15 - pt68)/(273.15 + pt68))

  a0 =  5.80871
  a1 =  3.20291
  a2 =  4.17887
  a3 =  5.10006
  a4 = -9.86643e-2
  a5 =  3.80369
  b0 = -7.01577e-3
  b1 = -7.70028e-3
  b2 = -1.13864e-2
  b3 = -9.51519e-3
  c0 = -2.75915e-7
  
  O2sol = exp(a0 + y*(a1 + y*(a2 + y*(a3 + y*(a4 + a5*y)))) + x*(b0 + y*(b1 + y*(b2 + b3*y)) + c0*x))
  return(O2sol)
}

dat$O2_Sat0 = gsw_O2sol_SP_pt(dat$sal,dat$pt)

#= o2satv2a(sal,pt) #uses practical salinity and potential temp - solubity at p =1 atm
dat$press = exp(dat$depth*10000*partial_molar_vol/gas_const/(dat$temp+kelvin))
dat$O2_satdepth = dat$O2_Sat0*dat$press

#solubility at p=0
dat$sol0 = dat$O2_Sat0/0.209
dat$sol_Dep = dat$sol0*dat$press
dat$po2 = dat$o2_umolkg/dat$sol_Dep

# species-specific parameters
Ao = 1.16625e-13
Eo = 0.8736
B = 3000 # size in grams, roughly average (initial calculations used 10000g)
N = -0.208 # borrowed from cod 

dat$mi = B^N*Ao*dat$po2/exp(-1*Eo/(boltz*(dat$temp+kelvin)))

# prepare data and models -------------------------------------------------

dat <- select(dat, species, year, longitude_dd, latitude_dd, cpue_kg_km2,
              o2, temp, depth, mi)

# rescale variables
dat$depth = scale(log(dat$depth))
dat$o2 = scale(log(dat$o2))
dat$temp = scale(dat$temp)
dat$mi = scale(dat$mi)

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

# create combination of covariates and threshold responses for different models
m_df = data.frame(
  spatial_only = rep(FALSE,9), 
  depth_effect = rep(FALSE,9),
  time_varying = rep(FALSE,9),
  threshold_function = c(rep("NA",5),rep(c("linear","logistic"),2)),
  covariate1 = c("temp","o2","mi","temp","temp",rep("o2",2),rep("mi",2)),
  covariate2 = c(rep("none",3),"o2","o2",rep("none",4)),
  interaction = c(rep(FALSE,4),TRUE,rep(FALSE,4)),
  tweedie_dens = rep(NA,9) # set up vector to store performance data if using cv
)
  
# run models for each combination of settings/covariates in df ------------

use_cv = FALSE # specify whether to do cross validation or not
spde <- make_spde(x = dat$longitude, y = dat$latitude, n_knots = 250) # choose # knots

for(i in 1:nrow(df)) {
  print(paste0("model # ", i, " of ", nrow(m_df)))
  
  # rename variables to make code generic
  sub <- dplyr::rename(dat, enviro1 = as.character(m_df$covariate1[i]))
  
  # format data and formula based on combination of arguements in model settings df
  formula = paste0("cpue_kg_km2 ~ 0 + as.factor(year)")
  time_formula = "~ -1"
  time_varying = NULL
  time = "year"
  
  if(m_df$covariate2[i] != "none") {
    sub <- dplyr::rename(sub, enviro2 = as.character(m_df$covariate2[i]))
    if(m_df$interaction[i] == TRUE){
      formula = paste0(formula, " + ", "enviro1", " + ", "enviro2", " + ", "enviro1", " * ", "enviro2")
    } else {
      formula = paste0(formula, " + ", "enviro1", " + ", "enviro2")
    }
  } else {
    if(m_df$threshold_function[i] == "linear") {
      formula = paste0(formula, " + ", "breakpt(enviro1)")
    }
    if(m_df$threshold_function[i] == "logistic") {
      formula = paste0(formula, " + ", "logistic(enviro1)")
    } 
    if(m_df$threshold_function[i] == "NA")  formula = paste0(formula, " + ", "enviro1")
  }
  
  if(m_df$depth_effect[i]==TRUE) {
    formula = paste0(formula, " + depth + I(depth^2)")
  }
    
    # fit model with or without cross-validation
    if(use_cv==TRUE) {
      m <- try(sdmTMB_cv(
        formula = as.formula(formula),
        data = sub,
        x = "longitude", 
        y = "latitude",
        time = time,
        k_folds = 5,
        n_knots = 250,
        seed = 7,
        family = tweedie(link = "log"),
        anisotropy = TRUE,
        spatial_only = m_df$spatial_only[i]
      ), silent = FALSE)
      
      if(class(m)!="try-error") {
        saveRDS(m, file = paste0("output/wc/model_",i,"_MI_cv.rds"))
        m_df$tweedie_dens[i] = m$sum_loglik
      }
      
    } else {
        m <- try(sdmTMB(
          formula = as.formula(formula),
          data = sub,
          time = time,
          spde = spde,
          family = tweedie(link = "log"),
          anisotropy = TRUE,
          spatial_only = m_df$spatial_only[i]
        ), silent = FALSE)
        
      if(class(m)!="try-error") {
        saveRDS(m, file = paste0("output/wc/model_",i,"_MI.rds"))
      }
        
    }
}

saveRDS(m_df, "output/wc/models_MI.rds")

#as.list(m$sd_report, "Estimate")$b_threshold
#as.list(m$sd_report, "Std. Error")$b_threshold