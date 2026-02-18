library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

##TODO: is vaccine coverage the full three dose series?
##No fourth dose information
##for ref: r21 is given in 3 monthly doses with a booster (4th) dose given 1 year
##after the 3rd dose to children 6 mo to 5 years

# shared data and code files:
orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    disagg_dt.RDS = "formatted_disagg_data.RDS"
  )
)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)


site <- readRDS("site.RDS")
disagg_dt <- readRDS("disagg_dt.RDS")

##TODO: assumption that coverage is only from the 3rd dose onwards
r21_dt = disagg_dt[grepl('vaccine', variable), ]
r21_dt <- r21_dt[,.(value = sum(value)), by = c('country', 'iso3c', 'year',
                                                'name_1', 'name_2',
                                                'low_level',
                                                'variable')]

r21_dt <- r21_dt[,.(value = sum(value)), by = c('country', 'iso3c', 'year', 'name_1',
                                                'name_2', 'low_level', 'variable')]

##Make number receiving vaccine cumulative over the course of the year
##(to show that more children are increasingly becoming vaccinated)
# r21_dt[, date := as.Date(sprintf("%d-%02d-01", year, month))]
# setorder(r21_dt, country, iso3c, name_1, name_2, low_level, variable, year, date)
# r21_dt[ ,cum_val := cumsum(value),
#        by = .(country, iso3c, name_1, name_2, variable, year, low_level)]

##TODO: assumption that denominator is all 0-1 year olds
site_pop <- data.table(site$pop$population_by_age)
site_pop_adm1 <- copy(site_pop)
site_pop_adm1[,name_2 := name_1]
site_pop <- rbind(site_pop[,low_level := 'adm_2'],
                  site_pop_adm1[,low_level := 'adm_1'])

##aggregating across urbaname_1
##aggregating across urban/rural right now
site_pop <- site_pop[age_lower == 0 & age_upper == 1,.(par_pf = sum(par_pf)),
                     by = c('country', 'iso3c', 'name_1', 'name_2', 'low_level', 'year')]

r21_dt <- merge(r21_dt, site_pop, by = c('country', 'iso3c', 'year',
                                         'name_1', 'name_2', 'low_level'), all.x = T, allow.cartesian = T)

r21_dt[,r21_cov := value / par_pf]

r21_dt_2026 <- r21_dt[year == 2025,.(country, iso3c, year, name_1, name_2, low_level,
                                     variable, value = value * (12/8), par_pf = par_pf)]
r21_dt_2026[,r21_cov := value / par_pf]
r21_dt_2026[,year := 2026]
r21_dt <- rbind(r21_dt, r21_dt_2026)


# Assume that booster dropout is equal to dose 2-3 dropout ----------------
##NOTE: This is very optimistic
dropout <- r21_dt[variable %in% c('Malaria_vaccine_2', 'Malaria_vaccine_3') &
                  year == 2025,.(country, iso3c, year,
                                                                              name_1, name_2, low_level,
                                                                              variable, r21_cov)]
dropout <- dcast(dropout, country + iso3c + year + name_1 + name_2 + low_level ~ variable, value.var = 'r21_cov')
dropout[,do := Malaria_vaccine_3 / Malaria_vaccine_2]
dropout[,Malaria_vaccine_booster := Malaria_vaccine_3 * do]
booster <- dropout[,.(country, iso3c, year = 2026, name_1, name_2, low_level,
                      r21_booster1_cov = Malaria_vaccine_booster)]

# Reformat for site file --------------------------------------------------
r21_dt <- r21_dt[,.(country, iso3c, name_1, name_2, low_level, urban_rural = NA, year, r21_primary_cov = r21_cov,
                    r21_vaccine_dose = variable, rtss_primary_cov = 0, peak_season = NA,
                    rtss_booster1_cov = 0, hybrid_booster_day_of_year = NA)]
r21_dt <- merge(r21_dt, booster, by = c('country', 'iso3c', 'year', 'name_1', 'name_2', 'low_level'), all.x = T)
r21_dt[is.na(r21_booster1_cov),r21_booster1_cov := 0]

saveRDS(r21_dt, 'r21.RDS')
