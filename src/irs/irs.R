library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

orderly::orderly_shared_resource(
  allirs.csv = "raw_data/ALLIRS.csv"
)

orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    loc_hierarchy.RDS = "loc_hierarchy.RDS"
  )
)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)

#site <- readRDS("shared/raw_data/2026_01_19_UGA_SNT_sitefile.rds")
site <- readRDS("site.RDS")

#loc_map <- readRDS(paste0(getwd(), '/archive/process_raw_data/', list.files(paste0(getwd(), '/archive/process_raw_data/'))[length(list.files(paste0(getwd(), '/archive/process_raw_data/')))], '/loc_hierarchy.RDS'))
loc_map <- readRDS("loc_hierarchy.RDS")

# Load in programme IRS ---------------------------------------------------
#irs <- fread("./shared/raw_data/ALLIRS.csv")
irs <- fread("allirs.csv")
irs <- irs[,.(year = Year, name_2 = Districtname, insecticide = Chemicalused,
              Campaignwave, Campaignmonth,
              struct_found = as.numeric(gsub(pattern = ',',
                                             replacement = '',
                                             Structurefound)),
              struct_spray = as.numeric(gsub(pattern = ',',
                                             replacement = '',
                                             Structurespray)),
              structure_coverage = as.numeric(gsub(pattern = '%',
                                              replacement = '',
                                              Structurecov)) / 100,
              pop_coverage = as.numeric(gsub(pattern = '%',
                                             replacement = '',
                                             Popcov)) / 100)]
irs <- merge(irs, loc_map, by = c('name_2'), allow.cartesian = T)
irs[is.na(struct_found), struct_found := 0]
irs[is.na(struct_spray), struct_spray := 0]
##if reports over 100% population coverage, change to 100%.
irs[pop_coverage > 1, pop_coverage := 1]
irs <- merge(irs, data.table(site$population$population_total)[,.(par_pf = sum(par_pf)), by = c('name_1', 'name_2', 'year')],
             by = c('name_1', 'name_2', 'year'))

adm_1_pop <- data.table(site$population$population_total)[,.(adm_1_pop = sum(par_pf)), by = c('name_1', 'year')]
irs <- merge(irs, adm_1_pop, by = c('name_1', 'year'))
irs[,pop_spray := pop_coverage * par_pf]


# Admin 1 aggregation -----------------------------------------------------
##done for plotting and comparison purposes
##aggregating to admin one means we much only do population coverage,
##as we don't know how many structures are in the entire admin unit but we can est pop
irs_adm1 <- copy(irs)
irs_adm1[,name_2 := name_1]
irs_adm1 <- irs_adm1[,.(pop_spray = sum(pop_spray)), by = c('name_2', 'year',
                                                            'adm_1_pop',
                                                                  'insecticide', 'Campaignwave',
                                                                  'Campaignmonth', 'name_1')]
irs_adm1[,pop_coverage := pop_spray / adm_1_pop]
irs <- rbind(irs[,low_level := 'adm_2'],
             irs_adm1[,low_level := 'adm_1'], fill = T)


# Modify insecticides -----------------------------------------------------
## options are: ddt, actellic, bendiocarb, sumishield.
irs[grepl('Actellic', insecticide), insecticide := 'actellic']
irs[grepl('Sumishield', insecticide), insecticide := 'sumishield']
irs[grepl('SumiShield', insecticide), insecticide := 'sumishield']
##going to change fludora fusion to sumishield, as they both clothianidin
irs[grepl('Fludora Fusion', insecticide), insecticide := 'sumishield']
#https://www.medrxiv.org/content/10.64898/2026.01.15.26344175v1.full notes that actellic is used post 2023
irs[grepl('Pending info', insecticide), insecticide := 'actellic']


# Extract campaign months -------------------------------------------------
##TODO: assuming that start of second month best shows when spraying occurred
irs[,month := ifelse(!grepl('_', Campaignmonth),
                     Campaignmonth,
                     unlist(lapply(strsplit(irs[grepl('_', Campaignmonth),Campaignmonth], split = '_'), '[[', 2)))]
irs[month == 'March', month := 'Mar']
irs[month == 'June', month := 'Jun']
irs[month != '',month := unlist(lapply(month, function(x) which(x == month.abb)))]
irs[,month := as.integer(month)]

irs <- irs[,.(year, month, name_1, name_2, low_level, Campaignwave, insecticide, structure_coverage, pop_coverage)]
irs <- data.table(reshape2::melt(irs, id.vars = c('year', 'month', 'Campaignwave',
                                                  'name_1', 'name_2', 'low_level', 'insecticide')))

##Statement from MOH suggests that IRS occurred sep-oct 2025, going to put oct as the date.
#https://www.instagram.com/p/DOs51qHDEJZ/
#https://x.com/MinofHealthUG/status/1968270956838936972
irs[,date_source := ifelse(is.na(month), 'assumed', 'data')]
irs[is.na(month),month := 10]
irs[,date := paste0(year, '-', month, '-01')]
irs[,date := lubridate::as_date(date, "%Y-%m-%d")]
irs[,spray_day := lubridate::yday(date)]
irs <- irs[,.(country = 'Uganda', iso3c = 'UGA',
              name_1, name_2, low_level, year,
              variable, irs_cov = value, peak_season = NA,
              insecticide, round = Campaignwave, spray_day_of_year = spray_day)]
##Going to limit to only sources of population coverage
irs <- irs[variable == 'pop_coverage']

# Load in MAP IRS coverage estimates --------------------------------------
##stored in site file
site_irs <- data.table(site$interventions$irs$implementation)[,.(country, iso3c, name_1, name_2,
                                                                 urban_rural, year, spray_day_of_year,
                                                                 variable = 'MAP estimate coverage',
                                                                 low_level = 'adm_2',
                                                                 irs_cov, insecticide, round)]
##Get admin 1 of IRS
site_irs_adm1 <- copy(site_irs)
adm_1_pop <- data.table(site$population$population_total)[,.(par_pf = sum(par_pf)),
                                                          by = c('name_1', 'name_2', 'year', 'urban_rural')]
adm_1_pop[,adm_1_pop := sum(par_pf), by = c('name_1', 'year')]
site_irs_adm1 <- merge(site_irs_adm1, adm_1_pop, by = c('name_1', 'name_2', 'year', 'urban_rural'))
site_irs_adm1[,pop_sprayed := irs_cov * par_pf]
site_irs_adm2 <- site_irs_adm1[,.(pop_sprayed = sum(pop_sprayed),
                                  par_pf = sum(par_pf)), by = c('year', 'name_1', 'round', 'insecticide', 'name_2')]
site_irs_adm1 <- site_irs_adm1[,.(pop_sprayed = sum(pop_sprayed)), by = c('year', 'name_1', 'round', 'insecticide', 'adm_1_pop')]
site_irs_adm2[,pop_coverage := pop_sprayed / par_pf]
site_irs_adm1[,pop_coverage := pop_sprayed / adm_1_pop]
site_irs_adm2[,low_level := 'adm_2']
site_irs_adm1[,low_level := 'adm_1']
site_irs <- rbind(site_irs_adm1, site_irs_adm2, fill = T)
site_irs <- data.table(reshape2::melt(site_irs[,.(year, name_1, round, insecticide,
                                                  low_level, name_2, pop_coverage)], id.vars = c('year', 'name_1', 'round', 'insecticide', 'low_level', 'name_2')))
site_irs[,variable := 'MAP population coverage']
setnames(site_irs, 'value', 'irs_cov')
site_irs[,spray_day_of_year := 1]

# Bind all input options together -----------------------------------------
##Campaign info
irs[,source := 'campaign_data']
site_irs[,source := 'MAP estimates']
##Hybrid approach: before 2020 use MAP estimates, after 2020 use campaign data
hybrid <- rbind(irs[variable == 'pop_coverage' & year > 2019],
                site_irs[year < 2020], fill = T)
hybrid[,source := 'hybrid_input']

irs_dt <- rbind(irs, site_irs, hybrid, fill = T)
irs_dt[variable == 'pop_coverage', variable := 'Campaign population coverage']
irs_dt$variable <- factor(irs_dt$variable, levels = c('Campaign population coverage', 'MAP population coverage'))
irs_dt_adm1 <- irs_dt[low_level == 'adm_1',irs_cov := sum(irs_cov), by = c('country', 'iso3c', 'name_1', 'name_2', 'low_level',
                                                                           'year', 'variable', 'peak_season', 'round', 'spray_day_of_year')]
irs_dt <- rbind(irs_dt[low_level != 'adm_1'],
                irs_dt_adm1)
irs_dt[is.na(country), country := 'Uganda']
irs_dt[is.na(iso3c), iso3c := 'UGA']


# Ensure all years are present for hybrid input ---------------------------
##ASSUMING THAT IF IT IS MISSING FROM CAMPAIGN DATA THAT IT IS ZERO COVERAGE
expected <- data.table(rbindlist(lapply(2000:2025, cbind, loc_map)))
setnames(expected, 'V1', 'year')
expected[,irs_cov_new := 0]
expected[,source := 'hybrid_input']
irs_dt <- merge(irs_dt, expected, by = c('name_1', 'name_2', 'year', 'source'), all.x = T, all.y = T)
irs_dt[is.na(irs_cov) & irs_cov_new == 0,country := 'Uganda']
irs_dt[is.na(irs_cov) & irs_cov_new == 0,iso3c:= 'UGA']
irs_dt[is.na(irs_cov) & irs_cov_new == 0,low_level := 'adm_2']
irs_dt[is.na(irs_cov) & irs_cov_new == 0,variable := 'Assumed']
irs_dt[is.na(irs_cov) & irs_cov_new == 0,insecticide := 'actellic']
irs_dt[is.na(irs_cov) & irs_cov_new == 0,round := 1]
irs_dt[is.na(irs_cov) & irs_cov_new == 0,spray_day_of_year := 1]
irs_dt[is.na(irs_cov) & irs_cov_new == 0,irs_cov := 0]
irs_dt[,irs_cov_new := NULL]
irs_dt <- unique(irs_dt)


saveRDS(irs_dt, 'irs.RDS')
