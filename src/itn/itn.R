library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

orderly::orderly_shared_resource(
  llin.xls = "raw_data/LLIN.xls"
)

orderly::orderly_shared_resource(
  llin_mdc.csv = "raw_data/2023_LLIN_mass_distribution_coverage_data.csv"
)

orderly::orderly_shared_resource(
  net_type_2023.csv = "raw_data/ITN_2023_net_type.csv"
)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
)
orderly::orderly_shared_resource(
  net_waves.csv = "net_distribution_waves.csv"
)

orderly::orderly_shared_resource(
  distrib_cov.csv = "distribution_coverages.csv"
)

orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    loc_hierarchy.RDS = "loc_hierarchy.RDS"
  )
)


#loc_map <- readRDS(paste0(getwd(), '/archive/process_raw_data/', list.files(paste0(getwd(), '/archive/process_raw_data/'))[length(list.files(paste0(getwd(), '/archive/process_raw_data/')))], '/loc_hierarchy.RDS'))
loc_map <- readRDS("loc_hierarchy.RDS")

site <- readRDS("site.RDS")


# MAP usage estimates ------------------------------------------------------
site_itn_use <- data.table(site$interventions$itn$use)
site_pop <- data.table(site$population$population_total)

site_itn_use <- merge(site_itn_use[,.(name_1, name_2, urban_rural, year, itn_use)],
                      site_pop[,.(name_1, name_2, urban_rural, year, pop = par_pf)],
                      by = c('name_1', 'name_2', 'urban_rural', 'year'))
site_itn_use[,pop_use_itn := itn_use * pop]
site_itn_use_adm1 <- unique(site_itn_use[,.(pop_use_itn = sum(pop_use_itn),
                                       pop = sum(pop)), by = c('name_1', 'year')])
site_itn_use_adm1 <- site_itn_use_adm1[,.(name_1, name_2 = name_1, level = 'adm_1', year, pop_use_itn, pop)]
site_itn_use_adm2 <- site_itn_use[,.(name_1, name_2, level = 'adm_2', year, pop_use_itn, pop)]
site_itn_use_adm2 <- site_itn_use_adm2[,.(pop_use_itn = sum(pop_use_itn),
                                     pop = sum(pop)), by = c('name_1', 'name_2', 'level', 'year')]
site_itn_use <- rbind(site_itn_use_adm1, site_itn_use_adm2)
site_itn_use[,cov := pop_use_itn / pop]
site_itn_use[name_2 == 'Sembabule', name_2 := 'Ssembabule']

# Re-align the distribution campaigns -------------------------------------
##net_waves <- fread("shared/net_distribution_waves.csv")
net_waves <- fread("net_waves.csv")
net_waves[name_2 == 'Sembabule', name_2 := 'Ssembabule']
net_waves[!is.na(month),date := paste0(year, '/', unlist(lapply(net_waves$month, function(x) which(x == month.name))), '/01')]
net_waves[is.na(month),date := paste0(year, '/1/01')]
net_waves[,date := as.Date(date)]
net_waves[,day_of_year := lubridate::yday(date)]

##Based on site files find the distribution years
##distribution years will have higher coverage than the year before and after
site_itn_use[,year_before := shift(cov), by = c('name_1', 'name_2', 'level')]
site_itn_use[,year_after := shift(cov, type = 'lead'), by = c('name_1', 'name_2', 'level')]
site_itn_use[,peak := ifelse(cov > year_before & cov > year_after, T, F)]
site_itn_use <- unique(site_itn_use)
dist_years_site <- site_itn_use[peak == T & level == 'adm_2',.(name_2, year)]
##only have data on campaigns starting in 2012
dist_years_site <- dist_years_site[year > 2011]
##The way I've pulled out peaks may obscure some districts with continuously increasing
##itn coverage (e.g. Yumbe), will add on any earlier campaigns from net campaign data
# dist_years[,first_campaign := min(year), by = 'name_2']
# add_campaigns <- merge(net_waves[year != 2013], unique(dist_years[,.(name_2, first_campaign)]), by = 'name_2')
# dist_years <- rbind(add_campaigns[year < first_campaign,.(name_2, year)],
#                     dist_years[,.(year, name_2)])
dist_years <- unique(net_waves[,.(year, level = 'adm_2', name_2)])

get_new_net_usage <- function(dt){
  name_2_in = dt[['name_2']]
  year_in = as.integer(dt[['year']])
  level_in = dt[['level']]
  print(name_2_in)
  print(year_in)
  ##ARGS:
  ##name_2_in: district that is being adjusted
  ##year_in: distribution year that occurs in the site file

  ##Need to calculate how many days are occurring between distributions
  site_dist <- dist_years_site[name_2 == name_2_in]
  if(nrow(site_dist) == 0){
    return(NULL)
  }
  site_dist_year <- site_dist[which.min(abs(site_dist$year - year_in)),year]
  net_waves_adm2 <- net_waves[name_2 == name_2_in]
  true_dist_day <- net_waves_adm2[which.min(abs(net_waves_adm2$year - year_in)),day_of_year]
  if(length(true_dist_day) == 0){
    ##TODO: fix this eventually, as this is due to redistricting occurring.
    return(NULL)
  }

  next_dist <- net_waves_adm2[year > site_dist_year,]
  next_dist_year <- next_dist[which.min(abs(year - site_dist_year)), year]
  if(length(next_dist_year) == 0){
    ##This means that the year_in is the last date that we have data for, so make the next distribution assumed to be start of 2026
    ##TODO: Update this once we have the actual distribution plan for 2026
    next_dist_year = 2026
    next_dist_day = 1
  }else{
    next_dist_day <- next_dist[which.min(abs(year - site_dist_year)), day_of_year]
  }

  # Convert to actual dates
  current_dist <- as.Date(true_dist_day - 1, origin = paste0(site_dist_year, "-01-01"))
  next_dist <- as.Date(next_dist_day - 1, origin = paste0(next_dist_year, "-01-01"))

  # Calculate difference
  days_between <- as.numeric(next_dist - current_dist)

  ##Get monthly usages from now (current_dist date) until one month before the next dist
  if(next_dist_year == 2026){
    usage_days <- seq(0,days_between, by = 365)## timepoints for netzpackage
  }else{
    usage_days <- seq(0,days_between, by = 365)[-length(seq(1,days_between, by = 365))] ## timepoints for netzpackage
  }
  usage_dates <- usage_days + current_dist

  annual_usage = netz::model_distribution_to_usage(
                                    usage_timesteps = usage_days,
                                    ##MAP's distribution coverage estimate
                                    distribution = site_itn_use[name_2 == name_2_in & year == year_in & level == level_in,cov],
                                    distribution_timesteps = 0,
                                    ##Assume's people are keeping nets for 3 years, likely optimisitic
                                    mean_retention = 365 * 3)
  new_usage = data.table(date = usage_dates,
                         name_2 = name_2_in,
                         level = level_in,
                         useage = annual_usage)
  return(new_usage)
}

new_net_usage <- apply(dist_years, 1, get_new_net_usage)
new_net_usage <- rbindlist(new_net_usage, fill = T)
##find the first year that we have campaign data for, and for years before that use MAP estimates
new_net_usage[,min_year := min(date), by = 'name_2']
new_net_usage[,min_year := lubridate::year(min_year)]

get_historical_useage <- function(dt){
  name_2_in = dt[['name_2']]
  year_in = as.integer(dt[['year']])
  print(name_2_in)
  print(year_in)
  ##ARGS:
  ##name_2_in: district
  ##year_in: first net distribution year
 if(nrow(site_itn_use[name_2 == name_2_in & year < year_in,]) == 0){
   return(NULL)
 }
  return(site_itn_use[name_2 == name_2_in & year < year_in,.(name_2, date = as.Date(paste0(year, '-01-01')), useage = cov)])
}

hist_useage <- rbindlist(apply(unique(new_net_usage[,.(name_2, year = min_year)]), 1, get_historical_useage))
hist_useage[,source := 'MAP']
new_net_usage[,source := 'Inferred from distribution campaign dates & MAP distribution coverage']
itn_cov <- rbind(hist_useage, new_net_usage[,.(date, name_2, useage, source)])


# Campaign data for comparison --------------------------------------------
#distrib_cov <- fread("shared/distribution_coverages.csv")
distrib_cov <- fread("distrib_cov.csv")
distrib_cov[name_2 == 'Sembabule',name_2 := 'Ssembabule']
distrib_cov[,month := unlist(lapply(distrib_cov$month, function(x) which(x == month.name)))]
distrib_cov[,date := as.Date(paste0(year, '-', month, '-01'), '%Y-%m-%d')]
distrib_cov <- distrib_cov[,.(name_2, date, useage = coverage, source = 'Distribution coverage')]
itn_cov <- rbind(itn_cov, distrib_cov)

# Bind on 2023 net types --------------------------------------------------
##accepted types: pyrethroid_only, pyrethroid_pbo and pyrethroid_pyrrole
# net_type_2023 <- fread("shared/raw_data/ITN_2023_net_type.csv")
net_type_2023 <- fread("net_type_2023.csv")
setnames(net_type_2023, c('District_city', 'Status'), c('name_2', 'net_type'))
net_type_2023[name_2 == 'Sembabule',name_2 := 'Ssembabule']
itn_cov[,year := lubridate::year(date)]
itn_cov <- merge(itn_cov, net_type_2023[,year:=2023], by = c('name_2', 'year'), all.x = T)
itn_cov[net_type == 'PBO', net_type := "pyrethroid_pbo"]
itn_cov[net_type == 'NewGen', net_type := "pyrethroid_pyrrole"]
itn_cov[net_type == 'Standard', net_type := "pyrethroid_only"]
itn_cov[,year := NULL]

# Reformat in line with site file -----------------------------------------
loc_map[name_2 == 'Sembabule',name_2 := 'Ssembabule']
itn_cov <- merge(itn_cov, loc_map, by = c('name_2'))
itn_cov <- copy(itn_cov)[,.(country = 'Uganda',
                      iso3c = 'UGA',
                      name_1, name_2, urban_rural = NA,
                      year = lubridate::year(date),
                      itn_use = useage,
                      usage_day_of_year = lubridate::yday(date),
                      net_type,
                      ##Consider if the non-known mass distributions should just be routine...
                      distribution_type = 'mass',
                      distribution_day_of_year = lubridate::yday(date),
                      source)]


# Aggregate up to admin 1 level for plotting ------------------------------
pop <- data.table(site$population$population_total)[,.(country, iso3c, name_1, name_2, year, par_pf)]
pop <- pop[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]
pop[name_2 == 'Sembabule', name_2 := 'Ssembabule']
admin_1 <- merge(itn_cov, pop, by = c('country', 'iso3c', 'name_1', 'name_2', 'year'),
                 all.x = T, all.y = T)
admin_1[,pop_use_itn := itn_use * par_pf]
admin_1[,name_2 := name_1]
##for these plots just use the first day of the year
admin_1[,usage_day_of_year := 1]
admin_1[,distribution_day_of_year := 1]
admin_1[,urban_rural := NA]
admin_1 <- admin_1[,.(pop_use_itn = sum(pop_use_itn),
                      par_pf = sum(par_pf)),
                   by = c('country', 'iso3c', 'name_1', 'name_2',
                          'year', 'urban_rural', 'usage_day_of_year', 'net_type', 'distribution_type',
                          'distribution_day_of_year', 'source')]
admin_1[,itn_use := pop_use_itn / par_pf]
admin_1 <- admin_1[,.(country, iso3c, name_1, name_2,
                      level = 'adm_1',
                      year, urban_rural, usage_day_of_year, net_type,
                      distribution_type, distribution_day_of_year,
                      source, itn_use)]

itn_cov <- rbind(itn_cov[,level := 'adm_2'],
                 admin_1)
itn_cov[name_2 == 'Sembabule', name_2 := 'Ssembabule']
itn_cov[,distribution_lower := 0]
itn_cov[,distribution_upper := 1]

get_mod_dist <- function(x){
  netz::usage_to_model_distribution(
    usage = as.numeric(x[['itn_use']]),
    usage_timesteps =   as.numeric(x[['usage_day_of_year']]),
    distribution_timesteps = as.numeric(x[['distribution_day_of_year']]),
    distribution_lower = 0,
    distribution_upper = 1,
    net_loss_function = netz::net_loss_map,
    half_life = 605.9
  )
}


##this gets used as coverages within site:::add_itns()
###really not sure how this is different than itn_use...
for(name_2_in in unique(itn_cov$name_2)){
  x <- apply(itn_cov[name_2== name_2_in & source != 'Distribution coverage',], 1, get_mod_dist)
  itn_cov[name_2== name_2_in & source != 'Distribution coverage',itn_input_dist := x]
}

saveRDS(itn_cov, './llin.RDS')

