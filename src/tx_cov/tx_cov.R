library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

# TODO: using treated / cases leads to some values above 100%,
# Using N1 and N2 as denominators seems to lead oto reasonable coverage values.
# Also tested using fever_suspected_malaria as a denominator
# Unclear on what the note: "The population at risk weighted mean (over pixels) treatment coverage of an effective antimalarial"
# weighted by what: pixels
# Richard used the DHS values (https://github.com/RJSheppard/SLE_SNT/blob/main/src/data_int_treatment/data_int_treatment.R#L29-L32)

# Dependencies
orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
)

orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    disagg_dt.RDS = "formatted_disagg_data.RDS"
  )
)


orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    inc_adj_dt.RDS = "formatted_yearly_inc_adj.RDS"
  )
)

site <- readRDS("site.RDS")

#country (char), iso3c (char), name_1 (char), name_2 (char), year (int), tx_cov (num)
#disagg_dt <- readRDS(paste0(getwd(), '/archive/process_raw_data/', list.files(paste0(getwd(), '/archive/process_raw_data/'))[length(list.files(paste0(getwd(), '/archive/process_raw_data/')))], '/formatted_disagg_data.RDS'))
disagg_dt <- readRDS("disagg_dt.RDS")


##Aggregate to the modelling unit (district = name_2)
agg_vars <- c('country', 'iso3c', 'year', 'month', 'name_1', 'name_2','variable', 'low_level')
disagg_dt <- disagg_dt[,.(value = sum(value)), by = agg_vars]


##From notes seems that treatment coverage can be taken as "Total_Cases_treated" / "Cases"
##Note from Natalie 14/1/2026 that it isn't expected that "Total_Cases_treated" starts so late,
##TODO: so this will need to be rerun eventually.
tx_cov <- disagg_dt[variable %in% c('Total_Cases_treated', 'Cases', 'Fever_suspected_malaria'),]
tx_cov <- data.table::dcast(tx_cov, country + iso3c + year + month + name_1 + name_2 + low_level ~ variable, value.var = 'value')
tx_cov[,tx_cov_baseline := Total_Cases_treated / Cases]
tx_cov[,tx_cov_fever := Total_Cases_treated / Fever_suspected_malaria]


####Pull in the adjusted number of cases
##TODO: Check that we for sure won't want to use these, they are very very low.
#yearly_inc_adj <- readRDS(paste0(getwd(), '/archive/process_raw_data/', list.files(paste0(getwd(), '/archive/process_raw_data/'))[length(list.files(paste0(getwd(), '/archive/process_raw_data/')))], '/formatted_yearly_inc_adj.RDS'))
yearly_inc_adj <- readRDS("inc_adj_dt.RDS")
##TODO: Check that we actually want montly granularity here

agg_vars <- c('country', 'iso3c', 'name_1', 'name_2', 'year', 'variable', 'low_level')
yearly_inc_adj <- yearly_inc_adj[variable %in% c('N1', 'N2'),]
yearly_inc_adj <- yearly_inc_adj[,.(value = sum(value)), by = agg_vars]
##get monthly distribution
month_case_dist <- unique(tx_cov[,.(country, iso3c, year, month, name_1, name_2, low_level,  Cases)])
month_case_dist[,annual_cases := sum(Cases), by = c('country', 'iso3c', 'year', 'name_1', 'name_2', 'low_level')]
month_case_dist[,per_month_prop := Cases / annual_cases]

#decompose annual adjusted cases into monthly values
monthly_inc_adj <- merge(yearly_inc_adj, month_case_dist[,.(year, month, name_1, name_2, low_level, per_month_prop)],
                         by = c('year', 'name_1', 'name_2', 'low_level'), allow.cartesian = T)
monthly_inc_adj[,adj_value := value * per_month_prop]
monthly_inc_adj <- data.table::dcast(monthly_inc_adj[,.(country, iso3c, name_1, name_2, low_level, year, month, variable, adj_value)],
                                     country + iso3c + name_1 + name_2 + low_level + year + month ~ variable,
                                     value.var = 'adj_value')
tx_cov <- merge(tx_cov[,.(country, iso3c, year, month, name_1, name_2,
                          low_level,
                          tx_cov_baseline, tx_cov_fever, Total_Cases_treated)],
                monthly_inc_adj,
                by = c('country', 'iso3c', 'name_1', 'name_2', 'low_level', 'year', 'month'))
tx_cov[,tx_cov_N1 := Total_Cases_treated / N1]
tx_cov[,tx_cov_N2 := Total_Cases_treated / N2]
test = copy(tx_cov)
test_avg <- copy(tx_cov)
test_avg <- test_avg[,.(tx_cov_N2 = mean(tx_cov_N2))]


tx_cov <- data.table(reshape2::melt(tx_cov[,.(country, iso3c, name_1, name_2, low_level,
                                              year, month,
                                              tx_cov_baseline, tx_cov_fever,
                                              tx_cov_N1, tx_cov_N2)], id.vars = c('country', 'iso3c',
                                                                                  'name_1', 'name_2',
                                                                                  'low_level',
                                                                                  'year', 'month')))


##test how different monthly, quarterly, and annual values are

# test <- tx_cov[variable == 'tx_cov_N2']
# test <- test[,.(country, iso3c, name_1, name_2, low_level,
#                 year, month, value = tx_cov_N2, N2)]
# ##replace coverages over 1 with 1
# test[,value := ifelse(value > 1, 1, value)]
# test[,q := ceiling(month / 3)]
# test[,q_mean := weighted.mean(value, w = N2), by = c('low_level', 'name_1', 'name_2', 'year', 'q')]
# test[,y_mean := weighted.mean(value, w = N2), by = c('low_level', 'name_1', 'name_2', 'year')]
# ggplot(test, aes(value, q_mean, size = N2)) + geom_point() +
#   facet_wrap(~name_1) + geom_abline(slope = 1, intercept = 0) +
#   ggpubr::stat_cor()
# ggplot(test, aes(value, y_mean, size = N2)) + geom_point() +
#   facet_wrap(~name_1) + geom_abline(slope = 1, intercept = 0) +
#   ggpubr::stat_cor()


###shift map 2020 values to align with DHIS2 data
##(crude way for right now)
##Need to clean this up, consider changing to intercept shifting to 2021
# tx_cov <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
map <- data.table(site$interventions$treatment$implementation)
map <- map[,.(name_1, name_2, urban_rural, year, tx_cov)]
pop <- data.table(site$population$population_total)[,.(country, iso3c, name_1, name_2,
                                                       urban_rural, year, par_pf)]


##averaging across the period to help for places like Amuru
tx_cov_avg <- tx_cov[variable == 'tx_cov_N2' &
                   year %in% 2020:2025,.(value = mean(value)), by = c('country',
                                                                        'iso3c',
                                                                        'name_1',
                                                                        'name_2',
                                                                        'low_level')]
tx_cov_avg[,scalar := value / test_avg$tx_cov_N2]
tx_cov_avg[,value := NULL]

test <- merge(map, tx_cov_avg, by = c('name_1', 'name_2'), allow.cartesian = T)
test[,tx_cov := tx_cov * scalar]
test <- test[,.(name_1, name_2, year, tx_cov)]

new <- rbind(test[,.(country = 'Uganda', iso3c = 'UGA', name_1,
                    name_2,
                    low_level = 'adm_2',
                    year,
                    value = tx_cov,
                    source = 'adj_map')])
saveRDS(new, 'tx_cov.RDS')







