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
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
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

pop_by_age <- data.table(site$population$population_by_age)
pop_by_age[age_lower == 0,age := '<1']
pop_by_age[age_lower %in% 1:4,age:= '1-4']
pop_by_age[age_lower %in% 5:6, age := '5-6']
pop_by_age[age_lower %in% 7:10, age := '7-10']
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
##TODO: dividing this by 5 to represent the 5 rounds, will need to confirm...
smc[,smc_cov := value / 5 / pop]
smc[,smc_cov_no_split := value / pop]

##census value
smc_2024 <- copy(smc[year == 2024 & low_level == 'adm_1'])
smc_2024 <- smc_2024[,.(value = sum(value)), by = c('country', 'iso3c', 'year', 'name_1', 'name_2', 'low_level',
                                                    'period', 'smc_rounds', 'smc_drug')]
smc_2024[,smc_min_age := 0]
smc_2024[,smc_max_age := 1460]
smc_2024[,age := '<5']
smc_2024[,smc_cov_census := value / 279921]
smc <- rbind(smc, smc_2024, fill = T)

##get total population of the distrticts included in SMC
test <- merge(pop_by_age, unique(smc[low_level == 'adm_2',.(name_2, year, age, keep = T)]), by = c('name_2', 'year', 'age'))
test <- test[,.(total_pop = sum(pop)), by = c('name_1', 'year')]
test[,pop:= NULL]
test <- merge(test, smc, by = c('year', 'name_1'))
test[,smc_treated := total_pop * value]
test <- test[,.(country, iso3c, name_1, name_2, year, smc_min_age, smc_max_age, age, round, 
                round_day_of_year,
                low_level, variable, smc_treated)]
test <- merge(test, pop_by_age_adm1[age %in% c('<1', '1-4'),.(pop = sum(pop)), by = c('year', 'name_1')], by = c('year',
                                                                                                                 'name_1'))
test[,smc_cov_mc_report := smc_treated/ pop]
test[,low_level := 'adm_1']
test <- test[,.(country, iso3c, name_1, name_2, year, smc_min_age, smc_max_age, age, round, round_day_of_year,
                low_level, smc_cov_mc_report)]

#smc <- rbind(smc, test, fill = T)


##Assume that the five rounds are equally divided across the april to september period
##just using 2025 as this would be the same for all give years
smc_days <- floor(seq(lubridate::yday(c("2025-04-01")),lubridate::yday(c("2025-09-30")), length.out  = 5))
smc_days <- data.table(round = 1:5, round_day_of_year = smc_days)
smc_cov_out <- data.table()
for(round_in in unique(smc_days$round)){
  smc_cov_out <- rbind(smc_cov_out, smc[,.(country, iso3c, name_1, name_2, smc_min_age, smc_max_age,
                                           age, year, round = round_in,
                                           low_level,
                                           smc_cov, smc_cov_no_split, smc_cov_census,
                                           round_day_of_year = smc_days[round == round_in, round_day_of_year])])
}

smc_cov_out <- rbind(smc_cov_out, test, fill = T)

##Need to turn the values into coverage, so will pull in population
##I assume this will be easiest using the site file, but will for now pull in other options
##Have <u5 population in the UPDATED pop
##have 2024 age disaggs in soecial age groups
##can change this to coverage with population by age in the site file
saveRDS(smc_cov_out, 'smc.RDS')