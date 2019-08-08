library(nwfscSurvey)
library(sdmTMB)
library(dplyr)
library(stringr)

# bring in common names
#UrlText <- "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year>=2003&variables=common_name,scientific_name"
#DataPull <- try(jsonlite::fromJSON(UrlText))
#spec_names = group_by(DataPull, common_name) %>% 
#  dplyr::summarize(scientific_name = scientific_name[1])
#spec_names$scientific_name = tolower(spec_names$scientific_name)
#saveRDS(spec_names,"nwfsc_lookup.rds")

spec_names = readRDS("survey_data/nwfsc_species_lookup.rds")

catch = readRDS("survey_data/wcbts_catch_2019-08-01.rds")
names(catch) = tolower(names(catch))
catch$date = as.character(catch$date)
catch$trawl_id = as.numeric(catch$trawl_id)
catch$scientific_name = tolower(catch$scientific_name)
catch = dplyr::left_join(catch, spec_names)
catch$common_name = tolower(catch$common_name)

# WC names in cope and haltuch 2012
cope_haltuch = c("aurora rockfish", "big skate", "bigfin eelpout",
  "black eelpout", "brown cat shark", "california slickhead",
  "canary rockfish", "chilipepper", "darkblotched rockfish",
  "deepsea sole", "dover sole", "english sole", "giant grenadier",
  "greenstriped rockfish", "halfbanded rockfish", "lingcod", 
  "longnose skate", "longspine thornyhead", "butterfish unident.",
  "pacific flatnose", "pacific grenadier", "pacific hake",
  "pacific sanddab", "petrale sole", "pink seaperch",
  "rex sole", "sablefish", "sandpaper skate", "sharpchin rockfish",
  "shortbelly rockfish", "shortspine thornyhead", "slender sole",
  "pacific spiny dogfish", "splitnose rockfish", "spotted ratfish",
  "stripetail rockfish", "white croaker", "yellowtail rockfish")

catch = dplyr::filter(catch, common_name %in% cope_haltuch)

haul = readRDS("survey_data/wcbts_haul_2019-08-01.rds")

haul$year = as.numeric(substr(haul$date_yyyymmdd,1,4))
haul$month = as.numeric(substr(haul$date_yyyymmdd,5,6))
haul$day = as.numeric(substr(haul$date_yyyymmdd,7,8))

haul = dplyr::rename(haul, 
  o2 = o2_at_gear_ml_per_l_der,
  degc = temperature_at_gear_c_der,
  depthm=depth_hi_prec_m) %>% 
  dplyr::select(o2,degc,depthm,latitude_dd,longitude_dd,
    performance,trawl_id)

dat = dplyr::left_join(catch, haul)

saveRDS(dat, "survey_data/joined_nwfsc_data.rds")
