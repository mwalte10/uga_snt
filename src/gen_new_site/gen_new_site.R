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

# site = readRDS('C:/Users/mwalters/Downloads/2026_02_18_UGA_SNT_sitefile.rds')
# tx <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
# smc <- readRDS(paste0(getwd(), '/archive/smc/', list.files(paste0(getwd(), '/archive/smc/'))[length(list.files(paste0(getwd(), '/archive/smc/')))], '/smc.RDS'))
# r21 <- readRDS(paste0(getwd(), '/archive/r21/', list.files(paste0(getwd(), '/archive/r21/'))[length(list.files(paste0(getwd(), '/archive/r21/')))], '/r21.RDS'))
# irs <- readRDS(paste0(getwd(), '/archive/irs/', list.files(paste0(getwd(), '/archive/irs/'))[length(list.files(paste0(getwd(), '/archive/irs/')))], '/irs.RDS'))
# llin <- readRDS(paste0(getwd(), '/archive/itn/', list.files(paste0(getwd(), '/archive/itn/'))[length(list.files(paste0(getwd(), '/archive/itn/')))], '/llin.RDS'))

site <- readRDS("site.RDS")
tx <- readRDS("tx_cov.RDS")
smc <- readRDS("smc.RDS")
r21 <- readRDS("r21_cov.RDS")
irs <- readRDS("irs_cov.RDS")
llin <- readRDS("llin.RDS")

##Assuming that all sites are rural right now, which is wrong but just want to test pipeline

site$interventions$treatment$implementation <- data.frame(tx[source == 'adj_map' &
                                                  low_level == 'adm_2',.(country, iso3c, name_1, name_2,
                                                                        urban_rural, year, tx_cov = value,
                                                                        prop_act = 1)])

##This is currently missing peak season, could merge on from site file but I have a feeling it isn't needed
site$interventions$smc$implementation <- data.frame(smc[low_level == 'adm_2',.(country, iso3c, name_1, name_2,
                                                                               urban_rural = 'rural', year,
                                                                    smc_cov, peak_season, smc_min_age, smc_max_age, round,
                                                                    round_day_of_year)])
site$interventions$vaccine$implementation <- data.frame(r21[low_level == 'adm_2' &
                                                              r21_vaccine_dose == 'Malaria_vaccine_3',.(country, iso3c, year, name_1, name_2,
                                                                                                        urban_rural = 'rural', r21_primary_cov, rtss_primary_cov,
                                                                                   peak_season, rtss_booster1_cov,
                                                                                   ##Assuming that boosters are all given on the first day of the year, which isn't true.
                                                                                   hybrid_booster_day_of_year = 1,
                                                                                   day_of_year = 1,
                                                                                   r21_booster1_cov)])
site$interventions$vaccine$booster_spacing = round(365*1.5)

site$interventions$irs$implementation <- data.frame(irs[low_level == 'adm_2' &
                                                        source == 'hybrid_input',.(country, iso3c, name_1, name_2, urban_rural = 'rural',
                                                                                   year, irs_cov,
                                                                                   ##Placeholder
                                                                                   peak_season = 222,
                                                                                   insecticide, round, spray_day_of_year)])

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
site$interventions$itn$use <- data.frame(llin[level == 'adm_2',.(country, iso3c, name_1, name_2, year, itn_use, usage_day_of_year)])

saveRDS(site, 'new_site.RDS')


