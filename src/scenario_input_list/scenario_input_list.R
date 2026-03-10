#setwd('Z:/Maggie/uga_snt/src/scenario_input_list/')
library(tidyverse)
library(ggplot2)
library(data.table)

orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)
site <- readRDS('new_site.RDS')

intervention_list <- list()
for(name in names(site$interventions)){
  dt <- site$interventions[[name]]$implementation %>% data.table()
  dt[,intervention := name]
  intervention_list[[name]] <- dt
}

#To get net usage given fixed distributions
get_usage <- function(adm2, dist_list){
  dist_list <- data.table::data.table(dist_list)
  dist_dt.adm2 <- dist_list[name_2 == adm2,]
  dist_dt.adm2[,distribution_timesteps := (year - 2000) * 365 + distribution_day_of_year]

  model_usage <- netz::model_distribution_to_usage(
    ##just use distribution timesteps for ease.
    usage_timesteps = dist_dt.adm2$distribution_timesteps,
    distribution = dist_dt.adm2$itn_input_dist,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life = netz::get_halflife('UGA'),
    net_loss_function = netz::net_loss_map
  )

  use_dt.adm2 <- copy(dist_dt.adm2)
  use_dt.adm2 <- use_dt.adm2[,.(country, iso3c, name_1, name_2,
                                year,
                                usage_day_of_year = distribution_day_of_year,
                                itn_use = model_usage)]
  print(adm2)
  return(data.frame(use_dt.adm2))
}

# Business as usual -------------------------------------------------------
copy_coverage <- function(int, years = 2026:2035){
  int <- data.table(int)
  need <- lapply(years, function(x) {
    y = unique(int[,.(country, iso3c, name_1, name_2)])
    y[,year := x]
  }
  )
  need <- rbindlist(need)
  int_columns <- c("tx_cov",
                   "itn_input_dist",
                   "distribution_upper",
                   "distribution_lower",
                   "irs_cov",
                   "smc_cov",
                   "pmc_cov",
                   "r21_primary_cov",
                   "r21_booster_cov",
                   "r21_booster1_cov",
                   "lsm_cov")
  int_columns <- intersect(colnames(int), int_columns)
  if(unique(int$intervention) == 'itn'){
    copy <- int[year %in% 2023:2025]
    for(new_year in c(3,6,9,12)){
      x <- copy(copy)
      x[,year := year + new_year]
      int <- rbind(int, x)
    }
    if('itn_input_dist' %in% int_columns){
      int <- data.table(int)[order(name_2, year, distribution_day_of_year)]
    }
    return(data.frame(int))
  }else{
    val_2025 <- int[year == 2025,]
    val_2025[,year := NULL]

    need <- merge(val_2025, need, by = intersect(colnames(val_2025), colnames(need)), allow.cartesian = T)
    int <- rbind(int, need)
    return(data.frame(int))
  }
}

bau_list <- lapply(intervention_list, copy_coverage)

##ITN usage
bau_itn_use <- lapply(unique(loc_map$name_2), get_usage, dist_list = bau_list[['itn']])
bau_itn_use <- rbindlist(bau_itn_use)
bau_list[['itn_dist']] <- bau_list[['itn']]
bau_list[['itn']] <- NULL
bau_list[['itn_use']] <- data.frame(bau_itn_use)

# No/ low coverage --------------------------------------------------------
remove_coverage <- function(int, years = 2026:2035){
  int <- data.table(int)
  need <- lapply(years, function(x) {
    y = unique(int[,.(country, iso3c, name_1, name_2)])
    y[,year := x]
  }
  )
  need <- rbindlist(need)
  int_columns <- c("tx_cov",
                   "itn_input_dist",
                   "distribution_upper",
                   "distribution_lower",
                   "irs_cov",
                   "smc_cov",
                   "pmc_cov",
                   "r21_primary_cov",
                   "r21_booster_cov",
                   "r21_booster1_cov",
                   "lsm_cov")
  int_columns <- intersect(colnames(int), int_columns)
  val_2025 <- int[year == 2025,]
  val_2025[,year := NULL]
  for(col in int_columns){
    val_2025[,col] <- 0
  }

  need <- merge(val_2025, need, by = intersect(colnames(val_2025), colnames(need)), allow.cartesian = T)
  int <- rbind(int, need)

  if('itn_input_dist' %in% int_columns){
    int <- data.table(int)[order(name_2, year, distribution_day_of_year)]
  }

  return(data.frame(int))
}

nolow_list <- lapply(intervention_list, remove_coverage)

##Special case for removing mass campaigns
itn <- data.table(intervention_list[["itn"]])
copy <- itn[year %in% 2023:2025]
for(new_year in c(3,6,9,12)){
  x <- copy(copy)
  x[,year := year + new_year]
  x[distribution_type == 'mass',itn_input_dist := 0]
  x[distribution_type == 'mass',distribution_lower := 0]
  x[distribution_type == 'mass',distribution_upper := 0]
  itn <- rbind(itn, x)
}
nolow_list[["itn_routine_dist"]] <- data.frame(itn)
nolow_list[["itn_dist"]] <- nolow_list[["itn"]]
nolow_list[["itn"]] <- NULL

itn_use_no_mass <- lapply(unique(loc_map$name_2), get_usage, dist_list = nolow_list[['itn_routine_dist']])
itn_use_no_mass <- rbindlist(itn_use_no_mass)

itn_use_no_nets <- lapply(unique(loc_map$name_2), get_usage, dist_list = nolow_list[['itn_dist']])
itn_use_no_nets <- rbindlist(itn_use_no_nets)

nolow_list[["itn_routine_use"]] <- data.frame(itn_use_no_mass)
nolow_list[["itn_use"]] <- data.frame(itn_use_no_nets)

# High coverage -----------------------------------------------------------
high_coverage <- function(int, years = 2026:2035){
  int <- data.table(int)
  need <- lapply(years, function(x) {
    y = loc_map[,.(country = 'Uganda', iso3c = 'UGA', name_1, name_2)]
    y[,year := x]
  }
  )
  need <- rbindlist(need)
  int_columns <- c("tx_cov",
                   "itn_input_dist",
                   "irs_cov",
                   "smc_cov",
                   "pmc_cov",
                   "r21_primary_cov",
                   "r21_booster_cov",
                   "r21_booster1_cov",
                   "lsm_cov")
  int_columns <- intersect(colnames(int), int_columns)
  val_2025 <- int[year == 2025,]
  val_2025[,year := NULL]

  need <- merge(val_2025, need, by = intersect(colnames(val_2025), colnames(need)), allow.cartesian = T,
                all.y = T)
  if("itn_input_dist" %in% int_columns){
    ##special case scenario
    copy <- int[year %in% 2023:2025]
    for(new_year in c(3,6,9,12)){
      x <- copy(copy)
      x[,year := year + new_year]
      int <- rbind(int, x)
    }
    ##this is just to get the general structure
    int <- data.table(int)[order(name_2, year, distribution_day_of_year)]

    return(data.frame(int))
  }

  for(col in int_columns){
    if(col == "lsm_cov"){
      need[,col] <- 0
    }else{
      if(any(is.na(need[,get(col)]))){
        need[is.na(get(col)), (col)] <- 0.9
      }
      ##so that we don't artifically decrease coverage
      need[get(col) < 0.9, (col) := 0.9]
    }
    if(col == 'smc_cov'){
      need[,smc_max_age := 3650]
      need[,smc_min_age := 0]

      ##fill in  round structure for locations that don't have it
      name_missing_round <- unique(need[is.na(round), name_2])
      missing_round <-  rbindlist(lapply(name_missing_round, function(x){
        data.table::data.table(name_2 = x, round = 1:5, round_day_of_year = c(91, 136, 182, 227, 273))
      }))

      need_keep <- need[!(name_2 %in% name_missing_round),]
      need <- need[name_2 %in% name_missing_round]
      need[,round := NULL] ; need[,round_day_of_year := NULL]
      need <- merge(need, missing_round, by = 'name_2', allow.cartesian = T)
      need <- rbind(need, need_keep)
    }
  }

  int <- rbind(int, need, fill = T)
  if(any(colnames(int) == 'low_level')){
    int <- int[low_level != 'adm_1']
    int[,low_level := NULL]
  }
  return(data.frame(int))
}

high_list <- lapply(intervention_list, high_coverage)

get_dist_and_usage <- function(adm2){
  use_dt.adm2 <- use_dt[name_2 == adm2,]
  dist_dt.adm2 <- dist_dt[name_2 == adm2 & distribution_timesteps < max(use_dt.adm2$usage_timesteps),]

  model_distribution <- netz::usage_to_model_distribution(
    usage = use_dt.adm2$itn_use,
    usage_timesteps = use_dt.adm2$usage_timesteps,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life = netz::get_halflife('UGA'),
    distribution_lower = dist_dt.adm2$distribution_lower,
    distribution_upper = dist_dt.adm2$distribution_upper,
    net_loss_function = netz::net_loss_map
  )

  dist_dt.adm2[,modelled_dist := model_distribution]

  model_usage <- netz::model_distribution_to_usage(
    usage_timesteps = use_dt.adm2$usage_timesteps,
    distribution = dist_dt.adm2$modelled_dist,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life = netz::get_halflife('UGA'),
    net_loss_function = netz::net_loss_map
  )
  use_dt.adm2[,modelled_usage := model_usage]

  print(adm2)
  return(list(dist = dist_dt.adm2, use = use_dt.adm2))
}


##Special case for ITN, need to infer what dist would be needed for 80% coverage
int <- site$interventions$itn$use
use_dt <- lapply(2026:2035, function(x) {
  int <- data.table::data.table(int)
  ##Assume quarterly distribution
  y = unique(int[,.(country, iso3c, name_1, name_2, usage_day_of_year = rep(seq(1,244, length.out = 4), length(unique(int$name_2))))])
  y[,year := x]
}
)
use_dt <- rbindlist(use_dt)
use_dt[,itn_use := 0.8]
use_dt <- rbind(use_dt, int)
use_dt <- use_dt[order(year),]
use_dt[,usage_timesteps := (year - 2000) * 365 + usage_day_of_year]
use_dt <- use_dt[year < 2036]

dist_dt <- data.table::data.table(high_list[["itn"]])
dist_dt <- dist_dt[order(year),]
dist_dt[,distribution_timesteps := (year - 2000) * 365 + distribution_day_of_year]
dist_dt <- dist_dt[year < 2036]

test <- lapply(unique(loc_map$name_2), get_dist_and_usage)
use <- rbindlist(lapply(test, '[[', 2))
dist <- rbindlist(lapply(test, '[[', 1))
high_list[['itn']] <- NULL
high_list[['itn_dist']] <- dist
high_list[['itn_use']] <- use[,.(country, iso3c, name_1, name_2, year, usage_day_of_year, itn_use = modelled_usage)]

# Format for runs ---------------------------------------------------------
treatment <- list()
treatment[["no"]] <- nolow_list[["treatment"]]
treatment[["bau"]] <- bau_list[["treatment"]]
treatment[["high"]] <- high_list[["treatment"]]

itn_use <- list()
itn_use[["no"]] <- nolow_list[["itn_use"]]
itn_use[["no_mass"]] <- nolow_list[["itn_routine_use"]]
itn_use[["bau"]] <- bau_list[["itn_use"]]
itn_use[["high"]] <- high_list[["itn_use"]]

itn_dist <- list()
itn_dist[["no"]] <- nolow_list[["itn_dist"]]
itn_dist[["no_mass"]] <- nolow_list[["itn_routine_dist"]]
itn_dist[["bau"]] <- bau_list[["itn_dist"]]
itn_dist[["high"]] <- high_list[["itn_dist"]]

irs <- list()
irs[["no"]] <- nolow_list[["irs"]]
irs[["bau"]] <- bau_list[["irs"]]
irs[["high"]] <- high_list[["irs"]]

smc <- list()
smc[["no"]] <- nolow_list[["smc"]]
smc[["bau"]] <- bau_list[["smc"]]
smc[["high"]] <- high_list[["smc"]]

vaccine <- list()
vaccine[["no"]] <- nolow_list[["vaccine"]]
vaccine[["bau"]] <- bau_list[["vaccine"]]
vaccine[["high"]] <- high_list[["vaccine"]]

input_strat <- list(treatment = treatment,
                    itn_use = itn_use,
                    itn_dist = itn_dist,
                    irs = irs,
                    smc = smc,
                    vaccine = vaccine)
saveRDS(input_strat, 'input_strat.RDS')
