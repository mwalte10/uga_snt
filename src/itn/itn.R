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
site_itn_use[,itn_use := pop_use_itn / pop]
site_itn_use[name_2 == 'Sembabule', name_2 := 'Ssembabule']
##Assume all MAP usage estimates are from the first of the year
site_itn_use[,use_timestep := (year - 2000) * 365 + 1]

# Create data frame that contains distribution campaigns ------------------
#distrib_cov <- fread("shared/distribution_coverages.csv")
distrib_cov <- fread("distrib_cov.csv")
mda <- distrib_cov
mda[,month_ord := unlist(lapply(month, function(x) which(x == month.name)))]
mda[,date := paste0(year, '-', month_ord, '-01')]
mda[,day_of_year := lubridate::yday(as.Date(date))]
mda[,distribution_lower := 0.5]
mda[,distribution_upper := 1]
mda <- mda[,.(year, name_2, day_of_year, distribution_lower, distribution_upper)]
##Identify pre 2014 ITN campaigns
site_itn_use[,year_before := shift(itn_use), by = c('name_1', 'name_2')]
site_itn_use[,year_after := shift(itn_use, type = 'lead'), by = c('name_1', 'name_2')]
site_itn_use[,scale_peak := itn_use / ((year_before * 0.5 + 0.5 * year_after))]
site_itn_use[,peak := ifelse(itn_use > year_before & itn_use > year_after, T, F)]
peaks <- unique(site_itn_use)
peaks <- unique(peaks[peak == T & year < 2014 & scale_peak > 1.5,.(name_2, year, day_of_year = 1,
                                                                   distribution_lower = 0.5, distribution_upper = 1)])

mda <- rbind(mda,
             peaks, fill = T)
mda[,dist_timestep := (year - 2000) * 365 + day_of_year]
mda <- mda[,.(name_2, dist_timestep, distribution_lower, distribution_upper)]
##add on routine distributions, assume they happen 3 times a year and can reach 50% of the population
rt <- data.table(expand.grid(list(name_2 = as.character(unique(mda$name_2)),
                                  dist_timestep = c(2,90,180)  + 365 * (0:25),
                                  distribution_lower = 0,
                                  distribution_upper = 0.5)))
rt[,name_2 := as.character(name_2)]
dist <- rbind(mda[,type := 'mda'],
              rt[,type := 'rt'],
              fill = T)
dist <- dist[order(dist_timestep),.(name_2, dist_timestep, distribution_lower, distribution_upper, type)]

# Obtain model distributions ----------------------------------------------
get_model_inputs <- function(name_2_in, dist_dt, use_dt){
  dist_dt.adm2 <- dist_dt[name_2 == name_2_in,]
  dist_dt.adm2 <- dist_dt.adm2[order(dist_timestep),]
  use_dt.adm2 <- use_dt[name_2 == name_2_in,]
  use_dt.adm2 <- use_dt.adm2[order(use_timestep),]

  model_distribution <- netz::usage_to_model_distribution(
    usage = use_dt.adm2$itn_use,
    usage_timesteps = use_dt.adm2$use_timestep,
    distribution_timesteps = dist_dt.adm2$dist_timestep,
    half_life = netz::get_halflife('UGA'),
    distribution_lower = dist_dt.adm2$distribution_lower,
    distribution_upper = dist_dt.adm2$distribution_upper,
    net_loss_function = netz::net_loss_map
  )
  dist_dt.adm2[,itn_input_dist := model_distribution]
  setnames(dist_dt.adm2, 'dist_timestep', 'ts')

  pred <- data.table(ts = 1:(365 * 26))
  pred[,year := floor((ts-1) / 365) + 2000]
  pred$model_usage <- netz::model_distribution_to_usage(
    usage_timesteps = pred$ts,
    distribution = dist_dt.adm2$itn_input_dist,
    distribution_timesteps = dist_dt.adm2$ts,
    half_life = netz::get_halflife('UGA'),
    net_loss_function = netz::net_loss_map
  )

  pred <- merge(pred, dist_dt.adm2, by = c('ts'), all.x = T, all.y = T)
  pred[,name_2 := name_2_in]
  print(name_2_in)
  return(pred)
}

itn_cov <- lapply(unique(site_itn_use$name_2),
                  get_model_inputs, dist_dt = dist,
                  use_dt = site_itn_use)
itn_cov <- rbindlist(itn_cov)


# Campaign data for comparison --------------------------------------------
#distrib_cov <- fread("shared/distribution_coverages.csv")
distrib_cov <- fread("distrib_cov.csv")
distrib_cov[name_2 == 'Sembabule',name_2 := 'Ssembabule']
distrib_cov[,month := unlist(lapply(distrib_cov$month, function(x) which(x == month.name)))]
distrib_cov[,date := as.Date(paste0(year, '-', month, '-01'), '%Y-%m-%d')]
distrib_cov <- distrib_cov[,.(name_2, date, model_usage = coverage, source = 'Programmatic distribution coverage')]
distrib_cov[,ts := ((lubridate::year(date)) - 2000) * 365 + (lubridate::yday(date))]
distrib_cov[,year := lubridate::year(date)]
itn_cov <- rbind(itn_cov[,source := 'Modelled usage'], distrib_cov, fill = T)

# Bind on 2023 net types --------------------------------------------------
##accepted types: pyrethroid_only, pyrethroid_pbo and pyrethroid_pyrrole
# net_type_2023 <- fread("shared/raw_data/ITN_2023_net_type.csv")
net_type_2023 <- fread("net_type_2023.csv")
setnames(net_type_2023, c('District_city', 'Status'), c('name_2', 'net_type'))
net_type_2023[name_2 == 'Sembabule',name_2 := 'Ssembabule']
itn_cov <- merge(itn_cov, net_type_2023[,year:=2023], by = c('name_2', 'year'), all.x = T)
itn_cov[net_type == 'PBO', net_type := "pyrethroid_pbo"]
itn_cov[net_type == 'NewGen', net_type := "pyrethroid_pyrrole"]
itn_cov[net_type == 'Standard', net_type := "pyrethroid_only"]

# Reformat in line with site file -----------------------------------------
loc_map[name_2 == 'Sembabule',name_2 := 'Ssembabule']
itn_cov <- merge(itn_cov, loc_map, by = c('name_2'))
save = copy(itn_cov)
itn_cov <- copy(itn_cov)[,.(country = 'Uganda',
                      iso3c = 'UGA',
                      name_1, name_2,
                      urban_rural = NA,
                      year = year,
                      itn_use = model_usage,
                      model_distribution = itn_input_dist,
                      usage_day_of_year = ts - floor((ts-1)/365) * 365,
                      net_type,
                      distribution_type = type,
                      distribution_lower,
                      distribution_upper,
                      source,
                      ts)]
itn_cov[!is.na(distribution_type),distribution_day_of_year := ts - floor((ts-1)/365) * 365]
itn_cov[,ts := NULL]


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
                 admin_1, fill = T)
itn_cov[name_2 == 'Sembabule', name_2 := 'Ssembabule']


saveRDS(itn_cov, './llin.RDS')

