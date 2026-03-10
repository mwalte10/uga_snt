library(netz)
orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)
site <- readRDS('new_site.RDS')

risk_strat <- fread('Z:/Maggie/uga_snt/shared/risk_strat_district.csv')
risk_strat <- risk_strat[-1,.(name_2 = V1, risk_zone = V2)]

intervention_list <- list()
for(name in names(site$interventions)){
  dt <- site$interventions[[name]]$implementation %>% data.table()
  dt[,intervention := name]
  intervention_list[[name]] <- dt
}

need <- unique(data.table(site$sites)[,.(name_1, name_2)])
need_dt <- data.table()
for(year_in in 2026:2030){
  need[,year := year_in]
  need_dt <- rbind(need_dt, need)
}

cm <- intervention_list[[1]][year %in% 2020:2025,.(country = 'Uganda',
                                             iso3c = 'UGA',
                                             sub_region = name_1,
                                             district = name_2,
                                             year,
                                             treatment_coverage = tx_cov)]
cm[treatment_coverage > 1, treatment_coverage := 1]

itn_distrib <- intervention_list[[2]][year %in% 2020:2025,.(country = 'Uganda',
                                             iso3c = 'UGA',
                                             sub_region = name_1,
                                             district = name_2,
                                             year,
                                             distribution_type,
                                             net_type,
                                             prop_distrib = itn_input_dist)]
itn_use <- data.table(site$interventions$itn$use)[year %in% 2020:2025,]
itn_use <- itn_use[,.(itn_use = mean(itn_use)), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]

irs <- intervention_list[[3]][year %in% 2020:2025,.(country = 'Uganda', iso3c = 'UGA', sub_region = name_1, district = name_2, year,
                                                        spray_day_of_year, insecticide, irs_cov)]
irs[,origin := paste0(year, '-01-01')]
irs[,month := month(as.Date(spray_day_of_year, origin = origin))]
irs <- irs[,.(country, iso3c, sub_region, district, year, month, insecticide, irs_cov)]


smc <- intervention_list[[4]][year %in% 2020:2025 & low_level == 'adm_2',.(country = 'Uganda',
                                                    iso3c = 'UGA',
                                                    sub_region = name_1,
                                                    district = name_2,
                                                    year,
                                                    smc_cov,
                                                    smc_min_age,
                                                    smc_max_age,
                                                    round_day_of_year,
                                                    round)]
smc[,origin := paste0(year, '-01-01')]
smc[,month := month(as.Date(round_day_of_year, origin = origin))]
smc <- smc[,.(round = min(round),
       smc_cov = mean(smc_cov),
       start_month = min(month)), by = c('country', 'iso3c', 'sub_region', 'district', 'year', 'smc_min_age', 'smc_max_age')]

r21 <- intervention_list[[6]][year %in% 2020:2025 & low_level == 'adm_2' &
                                r21_vaccine_dose == 'Malaria_vaccine_3',.(country = 'Uganda',
                                                                           iso3c = 'UGA',
                                                                           sub_region = name_1,
                                                                           district = name_2,
                                                                           year,
                                                                           r21_primary_cov,
                                                                           r21_booster1_cov)]
r21[r21_primary_cov > 1,r21_primary_cov := 1]
r21[r21_booster_cov > 1,r21_booster_cov := 1]

new_scen_list <- list('case management'  = cm,
                      'itn- population usage'= itn_use,
                      'itn distribution campaigns' = itn_distrib,
                      'irs' = irs,
                      'smc' = smc,
                      'vaccine' = r21)


dir <- 'C:/Users/mwalters/OneDrive - Imperial College London/Projects/UGA_malaria/scen_builder/'
for(name in names(new_scen_list)){
  write.csv(new_scen_list[[name]], paste0(dir, name, '.csv'), row.names = F)
}

