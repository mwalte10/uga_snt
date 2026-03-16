#setwd('Z:/Maggie/uga_snt/src/gen_new_site/')
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
# remotes::install_git(url = "https://github.com/mrc-ide/site/",
#                      ref = "site-2601")
# remotes::install_git(url = "https://github.com/mrc-ide/netz/",
#                      ref = "site-2601")
# remotes::install_git(url = "https://github.com/mrc-ide/malariasimulation/",
#                      ref = "dev")
library(site)
library(netz)

##orderly resources
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
dhs_values <- dhs_values[variable == 'prevalence' &
                           prev_type == 'smear' &
                           admin_level == 1 &
                           SurveyYear %in% c(2009,2014,2018,2024),.(year = SurveyYear,
                                                                    name_1, pfpr = mean)]
site <- readRDS("site.RDS")
loc_map <- unique(data.table(site$sites)[,.(name_1, name_2)])
dhs_values <- merge(dhs_values, loc_map, by = 'name_1', all.x = T, all.y = T, allow.cartesian = T)

# site = readRDS('C:/Users/mwalters/Downloads/2026_02_18_UGA_SNT_sitefile.rds')
# tx <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
# smc <- readRDS(paste0(getwd(), '/archive/smc/', list.files(paste0(getwd(), '/archive/smc/'))[length(list.files(paste0(getwd(), '/archive/smc/')))], '/smc.RDS'))
# r21 <- readRDS(paste0(getwd(), '/archive/r21/', list.files(paste0(getwd(), '/archive/r21/'))[length(list.files(paste0(getwd(), '/archive/r21/')))], '/r21.RDS'))
# irs <- readRDS(paste0(getwd(), '/archive/irs/', list.files(paste0(getwd(), '/archive/irs/'))[length(list.files(paste0(getwd(), '/archive/irs/')))], '/irs.RDS'))
# llin <- readRDS(paste0(getwd(), '/archive/itn/', list.files(paste0(getwd(), '/archive/itn/'))[length(list.files(paste0(getwd(), '/archive/itn/')))], '/llin.RDS'))

tx <- readRDS("tx_cov.RDS")
smc <- readRDS("smc.RDS")
r21 <- readRDS("r21_cov.RDS")
irs <- readRDS("irs_cov.RDS")
llin <- readRDS("llin.RDS")
resistance <- readRDS("insect_resist.RDS")


##Assuming that all sites are rural right now, which is wrong but just want to test pipeline
prop_act <- data.table(site$interventions$treatment$implementation)[,.(prop_act = mean(prop_act)), by= c('name_2', 'year')]
tx <- tx[source == 'adj_map' &
            low_level == 'adm_2',.(country, iso3c, name_1, name_2,
                                   year, tx_cov = value)]
tx <- merge(tx, prop_act, by = c('name_2', 'year'))
tx <- tx[,.(tx_cov = mean(tx_cov),
            prop_act = mean(prop_act)), by = c('country', 'iso3c', 'year', 'name_1', 'name_2')]

site$interventions$treatment$implementation <- data.frame(tx)

##This is currently missing peak season, could merge on from site file but I have a feeling it isn't needed
smc[low_level == 'adm_2',.(country, iso3c, name_1, name_2, year,
                           smc_cov, peak_season, smc_min_age, smc_max_age, round,
                           round_day_of_year)]
site$interventions$smc$implementation <- data.frame(smc)

r21[low_level == 'adm_2' &
      r21_vaccine_dose == 'Malaria_vaccine_3',.(country, iso3c, year, name_1, name_2,
                                                r21_primary_cov, rtss_primary_cov,
                                                peak_season, rtss_booster1_cov,
                                                ##Assuming that boosters are all given on the first day of the year, which isn't true.
                                                hybrid_booster_day_of_year = 1,
                                                day_of_year = 1,
                                                r21_booster1_cov)]
r21[r21_primary_cov > 1, r21_primary_cov := 0.8]
r21[r21_booster1_cov > 1, r21_booster_cov := 0.8]
site$interventions$vaccine$implementation <- data.frame(r21)
site$interventions$vaccine$booster_spacing = round(365*1.5)


irs <- copy(irs)[low_level == 'adm_2' &
                   source == 'hybrid_input',.(country, iso3c, name_1, name_2,
                                              year, irs_cov,
                                              ##Placeholder
                                              peak_season = 222,
                                              insecticide, round, spray_day_of_year)]
site$interventions$irs$implementation <- data.frame(irs)

########################Need to do some reconfiguring here
site$interventions$itn$implementation <-  data.frame(llin[level == 'adm_2' & !is.na(distribution_type),.(country, iso3c, name_1, name_2,
                                                            year,
                                                            ##MAP has all nets as pyrethroid
                                                            net_type = ifelse(is.na(net_type),
                                                                                'pyrethroid_only',
                                                                                net_type),
                                                            itn_input_dist = model_distribution,
                                                            distribution_type = ifelse(distribution_type == 'rt', 'routine', 'mass'),
                                                            distribution_day_of_year,
                                                            distribution_lower, distribution_upper)])
site$interventions$itn$use <- data.frame(llin[level == 'adm_2' & source == 'Modelled usage',.(country, iso3c, name_1, name_2, year, itn_use, usage_day_of_year)])


site$vectors$pyrethroid_resistance <- data.frame(resistance[source == 'New fitted curves',.(iso3c, country, name_1, name_2,
                                                                                       year, pyrethroid_resistance)])


year_2000_prev <- data.table(site$prevalence)[year == 2000]
year_2000_prev <- year_2000_prev[,.(pfpr = mean(pfpr),
                                    pvpr = mean(pvpr)), by = c('country', 'iso3c', 'name_1',
                                                               'name_2', 'year')]
year_2000_prev[,year := 2000]
prev <- rbind(year_2000_prev,
              dhs_values[,.(country = 'Uganda', iso3c = 'UGA', name_1, name_2, year, pfpr, pvpr = 0)])
site$prevalence <- data.frame(prev)


# Vector species ----------------------------------------------------------
##weight vector species by populaiton size in 2025
# pop_weights <- data.table::data.table(site$population$population_total)[year == 2025,.(name_2, urban_rural, par_pf)]
# pop_weights[,total_pop := sum(par_pf), by = c('name_2')]
# pop_weights[,prop := par_pf / total_pop]
#
# vect <- data.table::data.table(site$vectors$vector_species)
# vect <- merge(vect, pop_weights[,.(name_2, urban_rural, weight = prop)], by = c('name_2', 'urban_rural'))
# vect[,prop := prop * weight]
# vect <- vect[,.(prop = sum(prop)), by = c('name_2', 'country', 'iso3c', 'name_1', 'species', 'blood_meal_rates',
#                                           'foraging_time', 'Q0', 'phi_bednets', 'phi_indoors', 'mum')]
# vect <- rbind(vect[,urban_rural := 'rural'],
#               vect[,urban_rural := 'urban'])
# site$vectors$vector_species <- data.frame(vect)

saveRDS(site, 'new_site.RDS')


