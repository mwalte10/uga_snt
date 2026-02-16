library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

orderly::orderly_shared_resource(
  llin.xls = "raw_data/LLIN.xls"
)

orderly::orderly_shared_resource(
  llin_mdc.csv = "raw_data/2023_LLIN_mass_distribution_coverage_data.csv"
)
orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)

site <- readRDS("site.RDS")
llin <- readxl::read_xls("llin.xls") %>% data.table()

##country, iso3c, name_1, name_2, urban_rural, year, itn_use, usage_day_of_year
llin <- llin[,.(country = 'Uganda', iso3c = 'UGA', name_1 = orgunitlevel2,
                name_2 = gsub(orgunitlevel3, pattern = ' District', replacement = ''),
                year = periodname,
                itn_pop_distrib = `ITN - Number of population distributed`,
                itn_net_distrib = `ITN - Number of Net Distributed`,
                itn_hh_distrib = `ITN - Number of households distributed`)]
llin[,name_1 := gsub(pattern = ' Central', replacement = ' Buganda', name_1)]
llin <- unique(llin[,.(itn_pop_distrib = sum(itn_pop_distrib),
                       itn_net_distrib = sum(itn_net_distrib),
                       itn_hh_distrib = sum(itn_hh_distrib)), by = c('name_1', 'name_2', 'year', 'country', 'iso3c')])

site_population <- data.table(site$population$population_total)[,.(par_pf = sum(par_pf)), by = c('name_1', 'name_2', 'year')]

llin <- merge(llin, site_population, by = c('name_1', 'name_2', 'year'), all.x = T)
llin[,itn_coverage := itn_pop_distrib / par_pf]
llin[,net_pop_coverage := itn_net_distrib * 2]
llin[,low_level := 2]

llin_1 <- copy(llin[,.(name_1, name_2, year, country, iso3c, itn_pop_distrib, par_pf)])
llin_1[,name_2 := name_1]
llin_1 <- unique(llin_1[,.(itn_pop_distrib = sum(itn_pop_distrib, na.rm = T),
                           par_pf = sum(par_pf, na.rm = T)), by = c('name_1', 'name_2',
                                                                    'year', 'country', 'iso3c')])
llin_1[,itn_coverage := itn_pop_distrib / par_pf]
llin_1[,low_level := 1]



llin <- rbind(llin, llin_1, fill = T)
##right now nothing above here is used

####################################
##Site file ITN useage
####################################
site_itn_use <- data.table(site$interventions$itn$use)
site_pop <- data.table(site$population$population_total)

site_itn_use <- merge(site_itn_use[,.(name_1, name_2, urban_rural, year, itn_use)],
                      site_pop[,.(name_1, name_2, urban_rural, year, pop = par_pf)],
                      by = c('name_1', 'name_2', 'urban_rural', 'year'))
site_itn_use[,pop_use_itn := itn_use * pop]
site_itn_use <- unique(site_itn_use[,.(pop_use_itn = sum(pop_use_itn),
                                       pop = sum(pop)), by = c('name_1', 'year')])
site_itn_use[,cov := pop_use_itn / pop]

llin_mdc <- data.table(fread("llin_mdc.csv"))
llin_mdc <- llin_mdc[,.(name_2 = District_city,
                year = 2023,
                Validatedpop, Popserved)]
pop <- data.table(site$population$population_total)
pop <- pop[,.(par_pf = sum(par_pf)), by = c('year', 'name_2', 'name_1')]
llin_mdc <- merge(llin_mdc, pop[year == 2023], by = c('name_2', 'year'), all.y = T)
llin_mdc <- llin_mdc[,.(Validatedpop = sum(Validatedpop),
                Popserved = sum(Popserved),
                par_pf = sum(par_pf)), by = c('name_1', 'year')]
llin_mdc[,cov := netz::get_usage_rate('UGA') * Popserved / Validatedpop]

saveRDS(llin_mdc, './llin.RDS')



# llin <- data.table(readxl::read_xls("./shared/raw_data/LLIN.xls"))
# llin <- data.table(reshape2::melt(llin, id.vars = c('orgunitlevel1', 'orgunitlevel2', 'orgunitlevel3', 'orgunitlevel4',
#                                                     'organisationunitname', 'periodname')))
# llin <- llin[,.(value = sum(value)), by = c('orgunitlevel1', 'orgunitlevel2',
#                                             'periodname', 'variable')]
# llin[is.na(value),value := 0]
# llin[,total := sum(value, na.rm = T), by = c('orgunitlevel1', 'periodname', 'variable')]
# llin[,prop := value / total]
#
# ggplot(llin, aes(periodname, value, group = orgunitlevel2)) + geom_point() + facet_wrap(~variable, scales = 'free') +
#   geom_point(data = data.table(periodname = 2020, y = 0), aes(periodname, y, group = 0), col = NA)
#
#
# #dt<- data.table(readRDS("./archive/process_raw_data/20260121-124041-c42f7e75/formatted_disagg_data.RDS"))
#
# x <- dt[variable == 'EPI_LLINs' & low_level == 'adm_1']
# x <- x[,.(year, month, name_1, value)]
# x <- unique(x[,.(value = sum(value)), by = c('year', 'month', 'name_1')])
# x[,year_total := sum(value), by = c('year', 'name_1')]
# x = unique(x[,.(year,name_1, year_total)])
# x <- x[order(name_1),]
# ggplot(x, aes(year, year_total)) + geom_point() + facet_wrap(~name_1)
#
#
# #type <- data.table(fread("./shared/raw_data/ITN_2023_net_type.csv"))


