#setwd('Z:/Maggie/uga_snt/src/format_comparison_data/')
library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
library(cowplot)
library(geofacet)
#library(wesanderson)

orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
)

orderly::orderly_dependency(
  name = "tx_cov",
  query = "latest()",
  files = c(
    tx_cov.RDS = "tx_cov.RDS"
  )
)

orderly::orderly_dependency(
  name = "smc",
  query = "latest()",
  files = c(
    smc.RDS = "smc.RDS"
  )
)

orderly::orderly_dependency(
  name = "r21",
  query = "latest()",
  files = c(
    r21_cov.RDS = "r21.RDS"
  )
)

orderly::orderly_dependency(
  name = "irs",
  query = "latest()",
  files = c(
    irs_cov.RDS = "irs.RDS"
  )
)

orderly::orderly_dependency(
  name = "itn",
  query = "latest()",
  files = c(
    llin.RDS = "llin.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_calibration",
  query = "latest()",
  files = c(
    aggregated_results.RDS = "aggregated_results.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    inc_adj_dt.RDS = "formatted_yearly_inc_adj.RDS"
  )
)

orderly::orderly_dependency(
  name = "resistance",
  query = "latest()",
  files = c(
    insect_resist.RDS = "insect_resist.RDS"
  )
)

orderly::orderly_shared_resource(
  dhs_values.RDS = "dhs_values.RDS"
)
orderly::orderly_shared_resource(
  mis_2024.csv = "mis_2024.csv"
)
orderly::orderly_shared_resource(
  name_1_map.csv = "name_1_map.csv"
)

##load in site file
site <- readRDS("new_site.RDS")
old_site <- readRDS("site.RDS")

##load in dhs values for comparison
dhs_values <- readRDS("dhs_values.RDS")
data.table::setnames(dhs_values, c('adm_1_2025', 'adm_2_2025'), c('name_1', 'name_2'))
mis_2024 <- fread("mis_2024.csv")
dhs_values <- rbind(dhs_values, mis_2024, fill = T)
dhs_values[name_2 == 'Sembabule', name_2 := 'Ssembabule']
dhs_values[name_2 == 'Buvuma', name_1 := 'North Buganda']

name_1_map <- fread("name_1_map.csv")
dhs_values[,SurveyYear := as.integer(SurveyYear)]
dhs_values <- merge(dhs_values, name_1_map, by = c('SurveyYear', 'name_1'), all.x = T, allow.cartesian = T)
dhs_values[!is.na(name_1_2025), name_1 := name_1_2025]

loc_map <- unique(data.table::data.table(site$sites)[,.(name_1, name_2)])

# Load in comparison data -------------------------------------------------
tx_cov <- readRDS("tx_cov.RDS") %>% data.table()
smc <- readRDS("smc.RDS") %>% data.table()
r21_dt <- readRDS("r21_cov.RDS") %>% data.table()
irs_dt <- readRDS("irs_cov.RDS") %>% data.table()
llin <- readRDS("llin.RDS") %>% data.table()
resist <- readRDS("insect_resist.RDS") %>% data.table()
yearly_inc_adj <- readRDS("inc_adj_dt.RDS") %>% data.table()


# Functions -------------------------------
##Function to aggregate across admin levels
pop_dt <- data.table(old_site$population$population_by_age)[,.(country, iso3c, name_1, name_2, urban_rural,
                                                               year, age_lower, age_upper, par_pf)]
pop_weights <- function(pop_dt, ages){
  pop_dt <- pop_dt[age_lower %in% ages,]

  ##aggregate across ages
  pop_dt <- pop_dt[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural', 'year')]
  pop_dt[,par_pf_adm2 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]
  pop_dt[,par_pf_adm1 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'year')]
  pop_dt[,par_pf_adm0 := sum(par_pf), by = c('country', 'iso3c', 'year')]

  pop_dt_adm0 <- unique(pop_dt[,.(country, iso3c, low_level = 'adm_0', name_1, name_2, urban_rural, year, weight = par_pf / par_pf_adm0)])
  pop_dt_adm1 <- unique(pop_dt[,.(country, iso3c, low_level = 'adm_1', name_1, name_2, urban_rural, year, weight = par_pf / par_pf_adm1)])
  pop_dt_adm2 <- unique(pop_dt[,.(country, iso3c, low_level = 'adm_2', name_1, name_2, urban_rural, year, weight = par_pf / par_pf_adm2)])

  return(list(adm0 = pop_dt_adm0,
              adm1 = pop_dt_adm1,
              adm2 = pop_dt_adm2))
}

##Expand DHS values so that name_2's have the name_1 values
copy_dhs_name2 <- function(dhs, name_1_in){
  copy_dhs <- dhs[low_level == 'adm_1' & name_1 == name_1_in,]
  copy_dt <- data.table()
  for(name_2_in in loc_map[name_1 == name_1_in, name_2]){
    copy_dt <- rbind(copy_dt, copy_dhs[,name_2 := name_2_in])
  }
  return(copy_dt)
}


# Prevalence --------------------------------------------------------------
##name_1, name_2, low_level, year, ts, mean, lower, upper, source (MAP, MIS microscopy prevalence)
pop <- pop_weights(pop_dt, ages = 2:9)

##admin_0
prev <- data.table(old_site$prevalence)
prev <- merge(prev, pop$adm0, by = c('country', 'iso3c','name_1', 'name_2','urban_rural','year'))
prev[,prev := pfpr * weight]
prev <- prev[,.(prev= sum(prev)), by = c('country', 'iso3c', 'year')]
prev_0 <- copy(prev)

##admin_1
prev <- data.table(old_site$prevalence)
prev <- merge(prev, pop$adm1, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'))
prev[,prev := pfpr * weight]
prev <- prev[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'year')]

##admin_2
prev2 <- data.table(old_site$prevalence)
prev2 <- merge(prev2, pop$adm2, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'),
               all.x = T)
prev2[,prev := pfpr * weight]
prev2 <- prev2[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]
prev2[,country := 'Uganda'] ; prev2[,iso3c := 'UGA']

map <- rbind(
  prev_0[,.(country, iso3c, low_level = 'adm_0', name_1 = 'Uganda', name_2 = 'Uganda', year, ts = year, mean = prev, source = 'MAP')],
  prev[,.(country, iso3c, low_level = 'adm_1', name_1, name_2 = name_1, year, ts = year, mean = prev, source = 'MAP')],
  prev2[,.(country, iso3c, low_level = 'adm_0', name_1, name_2, year, ts = year, mean = prev, source = 'MAP')]
)

dhs_prev <- copy(dhs_values)
dhs_prev[,prev_type := ifelse(prev_type == 'rdt', 'RDT', 'MIS microscopy prevalence')]
##remove 2015/2016 microscopy, not collected in that survey so those zeroes are false
dhs_prev <- dhs_prev[!(prev_type == 'MIS microscopy prevalence' & SurveyYear == 2016),]
dhs_prev <- dhs_prev[!(SurveyYear == 2011),] ##only Karamoja
dhs_prev <- dhs_prev[as.integer(SurveyYear) > 2006]

dhs_prev <- dhs_prev[variable == 'prevalence' &
                         prev_type == 'MIS microscopy prevalence']

dhs_prev <- dhs_prev[,.(name_1, name_2, low_level = admin_level, year = SurveyYear, ts = SurveyYear,
                        mean, lower, upper, source =  'DHS')]
dhs_prev[is.na(name_2) & low_level == 1, name_2 := name_1]
dhs_prev[low_level == 0, name_1 := 'Uganda']
dhs_prev[low_level == 0, name_2 := 'Uganda']
dhs_prev[,low_level := paste0('adm_', low_level)]
dhs_prev[,country := 'Uganda'] ; dhs_prev[,iso3c := 'UGA']
dhs_prev2 <- rbindlist(lapply(unique(loc_map$name_1), copy_dhs_name2, dhs = dhs_prev))
dhs_prev <- rbind(dhs_prev, dhs_prev2)
dhs_prev[low_level == 'adm_2', source := 'DHS cluster']

prev <- rbind(map, dhs_prev, fill = T)
prev[,title := 'Prevalence, 0-5y']

# CCM ---------------------------------------------------------------------
tx_cov[source == 'dhis2', source := 'NMED']
tx_cov[name_2 == 'Sembabule', name_2 := 'Ssembabule']

map <- data.table(site$interventions$treatment$implementation)
pop <- pop_weights(pop_dt, unique(pop_dt$age_lower))

map_0 <- merge(map, pop$adm0, by = c('country', 'iso3c', 'year', 'name_1', 'name_2'))
map_0 <- map_0[,.(tx_cov = sum(tx_cov * weight),
                  prop_act = sum(prop_act * weight)), by = c('country', 'iso3c', 'year')]

map_1 <- merge(map, pop$adm1, by = c('country', 'iso3c', 'year', 'name_1', 'name_2'))
map_1 <- map_1[,.(tx_cov = sum(tx_cov * weight),
                  prop_act = sum(prop_act * weight)), by = c('country', 'iso3c', 'year', 'name_1')]

map_2 <- merge(map, pop$adm2, by = c('country', 'iso3c', 'year', 'name_1', 'name_2'))
map_2 <- map_2[,.(tx_cov = sum(tx_cov * weight),
                  prop_act = sum(prop_act * weight)), by = c('country', 'iso3c', 'year', 'name_1', 'name_2')]
map <- rbind(map_0[,low_level := 'adm_0'],
             map_1[,low_level := 'adm_1'],
             map_2[,low_level := 'adm_2'], fill = T)
map[low_level == 'adm_0', name_1 := 'Uganda']
map[low_level == 'adm_0', name_2 := 'Uganda']
map[low_level == 'adm_1', name_2 := name_1]
map <- map[,.(country, iso3c, low_level, name_1, name_2,
              year, ts = year,
              mean = tx_cov, prop_act,
              source = 'MAP')]

nmed <- tx_cov[source == 'NMED',.(country, iso3c, name_1, name_2, low_level, year, ts = year, mean = value, source)]

dhs_trt <- dhs_values[variable == 'trt_fever',.(country = 'Uganda', iso3c = 'UGA',
                                                year = SurveyYear, ts = SurveyYear,
                                                name_1, name_2, low_level = paste0('adm_', admin_level),
                                                mean, lower, upper, source = 'DHS')]
dhs_trt[is.na(name_2) & low_level == 'adm_1', name_2 := name_1]
dhs_trt[low_level == 'adm_0', name_1 := 'Uganda']
dhs_trt[low_level == 'adm_0', name_2 := 'Uganda']
dhs_trt2 <- rbindlist(lapply(unique(loc_map$name_1), copy_dhs_name2, dhs = dhs_trt))
dhs_trt <- rbind(dhs_trt, dhs_trt2)
dhs_trt[low_level == 'adm_2', source := 'DHS cluster']

ccm <- rbind(map, nmed, dhs_trt, fill = T)
ccm[,title := 'Treatment coverage']
ccm[is.na(prop_act), prop_act := 1]


# SMC ---------------------------------------------------------------------
smc[,age_label := paste0(smc_min_age / 365, '-', smc_max_age/365)]
smc$age_label <- factor(smc$age_label, levels = c('0-4', '5-10', '0-10'))
smc[,urban_rural := NULL]
smc <- smc[,.(country, iso3c, name_1, name_2, low_level, age_label,
              year = year, ts = year + (round_day_of_year-1) / 365,
              mean = smc_cov, upper = NA, lower = NA, source = 'NMED')]
smc[,title := 'SMC coverage']


# R21 ---------------------------------------------------------------------
r21_dt[,r21_vaccine_dose := gsub(pattern = 'Malaria_vaccine_',
                                 replacement = 'R21 dose: ',
                                 r21_vaccine_dose)]
r21_dt <- r21_dt[r21_vaccine_dose == 'R21 dose: 3',.(country, iso3c, year,
                                                     name_1, name_2, low_level,
                                                     r21_primary_cov, r21_booster1_cov)]
r21_dt <- data.table(reshape2::melt(r21_dt, id.vars = c('country', 'iso3c', 'year',
                                                        'name_1', 'name_2', 'low_level')))
r21_dt <- r21_dt[,.(country, iso3c, year, name_1, name_2, low_level,
                    variable, mean = value, source = 'NMED')]

dhs_r21 <- dhs_values[variable == 'dpt3_cov',.(country = 'Uganda', iso3c = 'UGA',
                                               year = SurveyYear, ts = SurveyYear,
                                               low_level = paste0('adm_', admin_level),
                                               name_1, name_2, mean, lower, upper, source = 'DHS')]

dhs_r21[is.na(name_2) & low_level == 'adm_1', name_2 := name_1]
dhs_r21[low_level == 'adm_0', name_1 := 'Uganda']
dhs_r21[low_level == 'adm_0', name_2 := 'Uganda']
dhs_r212 <- rbindlist(lapply(unique(loc_map$name_1), copy_dhs_name2, dhs = dhs_r21))
dhs_r21 <- rbind(dhs_r21, dhs_r212)
dhs_r21[low_level == 'adm_2', source := 'DHS cluster']

r21 <- rbind(r21_dt, dhs_r21, fill = T)
r21[,title := 'R21 coverage']


# IRS ---------------------------------------------------------------------
irs_dt[source == 'campaign_data', source := 'NMED']
irs_dt[source == 'MAP estimates', source := 'MAP']
irs_dt <- irs_dt[(source %in% c('MAP', 'NMED')),]
irs_dt <- irs_dt[,.(country, iso3c, name_1, name_2, low_level,
                    year, ts = year + (spray_day_of_year - 1)/365,
                    mean = irs_cov, source)]

dhs_irs <- dhs_values[variable == 'irs_coverage',.(country = 'Uganda', iso3c = 'UGA',
                                                   year = SurveyYear, ts = SurveyYear,
                                                   low_level = paste0('adm_', admin_level),
                                                   name_1, name_2, mean, lower, upper, source = 'DHS')]
dhs_irs[is.na(name_2) & low_level == 'adm_1', name_2 := name_1]
dhs_irs[low_level == 'adm_0', name_1 := 'Uganda']
dhs_irs[low_level == 'adm_0', name_2 := 'Uganda']
dhs_irs2 <- rbindlist(lapply(unique(loc_map$name_1), copy_dhs_name2, dhs = dhs_irs))
dhs_irs <- rbind(dhs_irs, dhs_irs2)
dhs_irs[low_level == 'adm_2', source := 'DHS cluster']

irs <- rbind(irs_dt, dhs_irs, fill = T)
irs[,title := 'IRS coverage']


# ITN ---------------------------------------------------------------------
map <- data.table(old_site$interventions$itn$use)
map <- map[,.(country, iso3c, name_1, name_2, low_level = 'adm_2',
              year, ts = year + (usage_day_of_year - 1) / 365, mean = itn_use,
              source = 'MAP')]

nmed <- llin[source == 'NMED',.(country, iso3c, name_1, name_2, year, ts = year + (usage_day_of_year - 1)/365,
                        low_level = level, mean = itn_use, source = 'NMED')]

dhs_itn <- dhs_values[variable == 'net_usage',.(country = 'Uganda', iso3c = 'UGA',
                                                year = SurveyYear, ts = SurveyYear,
                                                low_level = paste0('adm_', admin_level),
                                                name_1, name_2, mean, lower, upper, source = 'DHS')]
dhs_itn[is.na(name_2) & low_level == 'adm_1', name_2 := name_1]
dhs_itn[low_level == 'adm_0', name_1 := 'Uganda']
dhs_itn[low_level == 'adm_0', name_2 := 'Uganda']
dhs_itn2 <- rbindlist(lapply(unique(loc_map$name_1), copy_dhs_name2, dhs = dhs_itn))
dhs_itn <- rbind(dhs_itn, dhs_itn2)
dhs_itn[low_level == 'adm_2', source := 'DHS cluster']

itn <- rbind(map, nmed, dhs_itn, fill = T)
itn[,title := 'ITN usage']


# Insecticide resistance --------------------------------------------------
resist <- resist[source == 'NMED',.(country = 'Uganda', iso3c = 'UGA', name_1, name_2,
                                    year, ts = year, mean = pyrethroid_resistance, source,
                                    low_level = 'adm_2')]
resist[,title := 'Pyrethroid resistance']


# Total comparison data ---------------------------------------------------
comp_data <- list(prev = prev, ccm = ccm, smc = smc, r21 = r21, irs = irs, itn = itn)
saveRDS(comp_data, 'comp_data.RDS')
