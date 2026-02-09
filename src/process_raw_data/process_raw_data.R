library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)


# shared data and code files:
orderly::orderly_shared_resource(
  disagg_dt.csv = "raw_data/New_disagg_data_cleaned_19_12_25.csv"
)

orderly::orderly_shared_resource(
  yearly_inc_adj.xlsx = "raw_data/Yearly_adm3_with_incidence_adjustments.xlsx"
)

orderly::orderly_shared_resource(
  SMC.xls = "raw_data/SMC.xls"
)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)


##############################################################################
##Location map
##############################################################################
site <- readRDS("site.RDS")
loc_hierarchy <- data.table(name_1 = site$shape$level_2$name_1, name_2 = site$shape$level_2$name_2)
saveRDS(loc_hierarchy, 'loc_hierarchy.RDS')

##############################################################################
##Disaggregated data
##############################################################################
#disagg_dt <- fread("shared/raw_data/New_disagg_data_cleaned_19_12_25.csv")
disagg_dt <- fread("disagg_dt.csv")

disagg_dt <- data.table(reshape2::melt(disagg_dt, id.vars = c('Year', 'Month', 'pe', 'National', 'National_uid',
                                                              'Region', 'Region_uid', 'District_city',
                                                              'District/City_uid', 'DLG/Municipality/City Council',
                                                              'DLG/Municipality/City Council_uid',
                                                              'Sub County/Town Council/Division', 'Sub County/Town Council/Division_uid',
                                                              'HF', 'Health Facility_uid', 'Private Facilities',
                                                              'Facility Level', 'Reporting Status', 'Authority', 'Operational Status',
                                                              'HF_type', 'Public Facilities', 'Medical Bureaus')))
disagg_dt <- disagg_dt[!is.na(value)]
disagg_dt[grepl('10-14',variable),age := '10-14']
disagg_dt[grepl('15-19',variable),age := '15-19']
disagg_dt[grepl('25-49',variable),age := '25-49']
disagg_dt[grepl('20-24',variable),age := '20-24']
disagg_dt[grepl('0-4',variable),age := '0-4']
disagg_dt[grepl('5\\+Yrs',variable),age := '5+']
disagg_dt[grepl('0-28Dys',variable),age := '0-28Dys']
disagg_dt[grepl('20+',variable),age := '20+']
disagg_dt[grepl('5-9',variable),age := '5-9']
disagg_dt[grepl('10-19',variable),age := '10-19']
disagg_dt[grepl('29Dys-4',variable),age := '29Dys-4']
disagg_dt[grepl('50\\+',variable),age := '50+']

disagg_dt[grepl('Female', variable), sex := 'F']
disagg_dt[grepl('Male', variable), sex := 'M']
disagg_dt[is.na(sex), sex := 'B']

disagg_dt[,variable := unlist(lapply(strsplit(as.character(disagg_dt$variable), split = '|', fixed = T), '[[', 1))]


##Rename columns to match with the rest of the site file structures (lower case, month and month_name)
disagg_dt <- disagg_dt[,.(country = 'Uganda', iso3c = 'UGA',
                          year = Year, month = Month,
                          name_1 = Region, name_2 = gsub(pattern = ' District',
                                                         replacement = '',
                                                         District_city),
                          ##these aren't used but retaining in case eventually needed
                          name_3 = `DLG/Municipality/City Council`, name_4 = `Sub County/Town Council/Division`,
                          health_facility = HF, private_facilities = `Private Facilities`, facility_level = `Facility Level`,
                          reporting_status = `Reporting Status`, authority = Authority, operational_status = `Operational Status`,
                          health_facility_type = HF_type, public_facilities = `Public Facilities`,
                          medical_bureaus = `Medical Bureaus`, sex, age, variable, value)]

##Aggregate up to the admin_1 level
disagg_dt_adm1 <- copy(disagg_dt)
disagg_dt_adm1[,name_2 := name_1]
disagg_dt <- rbind(disagg_dt[,low_level := 'adm_2'],
                   disagg_dt_adm1[,low_level := 'adm_1'])
saveRDS(disagg_dt, 'formatted_disagg_data.RDS')

##############################################################################
##Adjusted data
##############################################################################
#yearly_inc_adj <- readxl::read_xlsx("shared/raw_data/Yearly_adm3_with_incidence_adjustments.xlsx") %>% data.table()
yearly_inc_adj <- readxl::read_xlsx("yearly_inc_adj.xlsx") %>% data.table()
yearly_inc_adj <- data.table(reshape2::melt(yearly_inc_adj, id.vars = c('Year', 'adm3_uid', 'adm1', 'adm1_uid', 'adm2', 'adm2_uid', 'adm3')))

##Rename columns to match with the rest of the site file structures (lower case, month and month_name)
yearly_inc_adj <- yearly_inc_adj[,.(country = 'Uganda', iso3c = 'UGA',
                                    name_1 = adm2, name_2 = gsub(pattern = ' District',
                                                                 replacement = '',
                                                                 adm3), year = Year,
                                    variable, value)]
##Confirmed with Natalie that North Central -> North Buganda and South Central -> South Buganda, change this file appropriately
yearly_inc_adj[name_1 == 'North Central', name_1 := 'North Buganda']
yearly_inc_adj[name_1 == 'South Central', name_1 := 'South Buganda']

##Aggregate up to the admin_1 level
yearly_inc_adj_adm1 <- copy(yearly_inc_adj)
yearly_inc_adj_adm1[,name_2 := name_1]
yearly_inc_adj <- rbind(yearly_inc_adj[,low_level := 'adm_2'],
                        yearly_inc_adj_adm1[,low_level := 'adm_1'])

saveRDS(yearly_inc_adj, 'formatted_yearly_inc_adj.RDS')

##############################################################################
##SMC coverage
##############################################################################
#smc <- readxl::read_xls("shared/raw_data/SMC.xls") %>% data.table()
smc <- readxl::read_xls("SMC.xls") %>% data.table()
smc <- data.table(reshape2::melt(smc, id.vars = c('orgunitlevel1', 'orgunitlevel2', 'orgunitlevel3', 'orgunitlevel4', 'organisationunitname', 'periodname')))
smc <- smc[,.(country = 'Uganda', iso3c = 'UGA',
              name_1 = orgunitlevel2, name_2 = orgunitlevel3, name_3 = orgunitlevel4,
              periodname, variable, value)]
smc[,sex := ifelse(grepl('Female', variable), 'Female', 'Male')]
smc[grepl('<1 year', variable),age := '<1']
smc[grepl('1-4 years', variable),age := '1-4']
smc[grepl('5-6 years', variable),age := '5-6']
smc[grepl('7-10 years', variable),age := '7-10']
smc <- smc[,.(country, iso3c, name_1, name_2 = gsub(pattern = ' District',
                                                    replacement = '',
                                                    name_2), name_3, periodname, sex, age, value)]
##Aggregate up to the admin_1 level
smc_adm1 <- copy(smc)
smc_adm1[,name_2 := name_1]
smc <- rbind(smc[,low_level := 'adm_2'],
             smc_adm1[,low_level := 'adm_1'])

saveRDS(smc, 'formatted_smc.RDS')
