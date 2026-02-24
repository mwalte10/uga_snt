library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

##TODO: there are some 5-7 year olds that are listed as having received SMC, do we consider that SMC or PMC?
##It is generally unclear how many doses were delivered in the april to sept period (in the raw data)
##SMC alliance notes that 5 rounds were done per year: https://www.smc-alliance.org/uganda
##information on SMC programme in UGA from givewell: https://www.givewell.org/research/grants/malaria-consortium-seasonal-malaria-chemoprevention-in-karamoja-uganda-in-2025-26-september-2024#Our_process
#############2022
##UGA info starting p58: https://files.givewell.org/files/DWDA%202009/Malaria%20Consortium/2022smcphilanthropyreport.pdf
###Table 17 has the exact dates of the five rounds of the 2022 campaign
############2023
#MC seems to suggest that children over 5 also received SMC in 2025, however this is not reflected in the data provided by chai
##https://files.givewell.org//files/DWDA%202009/Malaria%20Consortium/2%20SMC%20Coverage%20Report%202023.pdf
#have dates of the campaigns here: p19 https://files.givewell.org//files/DWDA%202009/Malaria%20Consortium/1%20SMC%20Philanthropy%20report%202023%20%281%29.pdf
####Says that 240K children supported in 2022 and 250K children supported in
############2024
##Table 0 has coverage from the 3 doses per round: https://www.malariaconsortium.org/assets/global/2024-SMC-Coverage-Report.pdf
###notes that some ineligible schildren received one dose


# Dependencies
orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
)


orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    "formatted_smc.RDS"
  )
)

orderly::orderly_shared_resource(
  mc_smc_report.csv = "raw_data/mc_smc_report.csv"
)

site <- readRDS("site.RDS")

#mc <- fread('./shared/raw_data/mc_smc_report.csv')
#smc <- readRDS(paste0(getwd(), '/archive/process_raw_data/', list.files(paste0(getwd(), '/archive/process_raw_data/'))[length(list.files(paste0(getwd(), '/archive/process_raw_data/')))], '/formatted_smc.RDS'))
##necessary columns are country, iso3c, name_1, year, smc_cov, smc_rounds, smc_min_age (days), smc_max_age (days), and smc_drug
smc <- readRDS("formatted_smc.RDS")
smc <- smc[!is.na(value)]
smc[age != '<1',smc_min_age := unlist(lapply(strsplit(age, split = '-'), '[[', 1))]
smc[age != '<1',smc_max_age := unlist(lapply(strsplit(age, split = '-'), '[[', 2))]
smc[age == '<1', smc_min_age := 0]
smc[age == '<1', smc_max_age := 1]
smc <- smc[,.(country, iso3c, name_1, name_2, periodname,
              age,
              smc_min_age = as.integer(smc_min_age) * 365,
              smc_max_age = as.integer(smc_max_age) * 365,
              low_level,
              value
)]
smc <- smc[,.(value = sum(value)), by = c('country', 'iso3c', 'name_1', 'name_2',
                                          'low_level',
                                          'periodname',
                                          'age',
                                          'smc_min_age', 'smc_max_age')]
##all periods are from april to september
smc[,year := gsub(pattern = 'April - September ', replacement = '', periodname)]
smc[,year := as.integer(year)]
smc[,period := 'April - September']
smc[,periodname := NULL]
##from gates article and smc alliance
smc[,smc_rounds := 5]
##from Malaria consortium data
smc[,smc_drug := 'sp_aq']
##Sum across ages
smc <- smc[,.(value = sum(value), smc_min_age  = min(smc_min_age), smc_max_age = max(smc_max_age)),
           by = c('country', 'iso3c', 'name_1', 'name_2', 'low_level', 'year', 'period', 'smc_rounds', 'smc_drug')]
smc[,age := paste0(smc_min_age/365,'-',smc_max_age/365)]

pop_by_age <- data.table(site$population$population_by_age)
pop_by_age <- rbind(copy(pop_by_age)[age_lower %in% 0:4,age:= '0-4'],
                    copy(pop_by_age)[age_lower %in% 0:10,age:= '0-10'],
                    copy(pop_by_age)[age_lower %in% 5:10,age:= '5-10'])
pop_by_age <- pop_by_age[!is.na(age),.(pop = sum(par_pf), low_level = 'adm_2'), by = c('country', 'iso3c', 'name_1', 'name_2',
                                                                                       'year', 'age')]



##aggregate up to admin_1 for national estimates
pop_by_age_adm1 <- copy(pop_by_age)
pop_by_age_adm1 <- pop_by_age_adm1[,.(country, iso3c, name_1, name_2 = name_1, year, age, pop, low_level = 'adm_1')]
pop_by_age_adm1 <- pop_by_age_adm1[,.(pop = sum(pop)), by = c('country', 'iso3c', 'name_1',
                                                              'name_2', 'year', 'age', 'low_level')]
pop_by_age <- rbind(pop_by_age,
                    pop_by_age_adm1)

smc <- merge(smc, pop_by_age, by = c('country', 'iso3c', 'year', 'name_1', 'name_2', 'low_level', 'age'), all.x = T)
smc[,smc_cov := value / pop]


# Cap SMC coverage at 100% --------------------------------
smc[,smc_cov := ifelse(smc_cov > 1, 1, smc_cov)]

# Assign round-specific coverages using MC surveys ------------------------
##For missing values will just assume the mean of the values that are there
mc <- fread("mc_smc_report.csv")
mc[,mean_year := mean(value, na.rm = T), by = c('year')]
mc[,value := ifelse(is.na(value), mean_year, value)]
##all MC surveys are among <5 year olds, assume this coverage holds true for all age groups
mc <- mc[,.(year, round, round_day_of_year, survey_cov = value)]
##Assume that 2025 survey coverages will be the same as 2024
mc_2025 <- copy(mc[year==2024,])
mc_2025[,year := 2025]
mc <- rbind(mc,
            mc_2025)

smc <- merge(smc, mc, by = c('year'), allow.cartesian = T)
smc[,smc_cov := smc_cov * survey_cov]

# Put in site file format -------------------------------------------------
smc <- smc[,.(country, iso3c, name_1, name_2, urban_rural = NA, year, smc_cov, peak_season = NA,
       smc_min_age, smc_max_age, round, round_day_of_year)]
smc[,low_level := ifelse(name_1 == name_2, 'adm_1', 'adm_2')]

saveRDS(smc, 'smc.RDS')
