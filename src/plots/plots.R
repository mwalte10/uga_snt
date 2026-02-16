library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
#library(wesanderson)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)

orderly::orderly_dependency(
  name = "tx_cov",
  query = "latest()",
  files = c(
    tx_cov.RDS = "tx_cov.RDS"
  )
)

# orderly::orderly_dependency(
#   name = "smc",
#   query = "latest()",
#   files = c(
#     smc_cov.RDS = "smc.RDS"
#   )
# )

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

orderly::orderly_shared_resource(
  dhs_values.RDS = "dhs_values.RDS"
)
orderly::orderly_shared_resource(
  mis_2024.csv = "mis_2024.csv"
)

orderly::orderly_shared_resource(
  mc_smc_report.csv = "raw_data/mc_smc_report.csv"
)

##load in site file
#site = readRDS('Z:/Maggie/uga_snt/shared/raw_data/2026_01_19_UGA_SNT_sitefile.rds')
site <- readRDS("site.RDS")
##load in dhs values for comparison
#dhs_values = readRDS('Z:/Maggie/uga_snt/shared/dhs_values.RDS')
dhs_values <- readRDS("dhs_values.RDS")
data.table::setnames(dhs_values, c('adm_1_2025', 'adm_2_2025'), c('name_1', 'name_2'))
#mis_2024 = data.table::fread('Z:/Maggie/uga_snt/shared/mis_2024.csv')
mis_2024 <- fread("mis_2024.csv")
dhs_values <- rbind(dhs_values, mis_2024, fill = T)

#colors <- wesanderson::wes_palette("AsteroidCity1")
colors <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645")


################################################################################
##Prevalence
###going to weight by PAR to aggregate up to the
################################################################################
pop <- data.table(site$population$population_by_age)[,.(country, iso3c, name_1, name_2, urban_rural,
                                                        year, age_lower, age_upper, par_pf)]
##limit to <6
pop <- pop[age_upper < 6]
##include only half of the 0-1 population
pop[age_lower ==0, par_pf := 0.5 * par_pf]
pop <- pop[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural', 'year')]
pop[,par_pf_adm1 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'year')]
pop[,weight := par_pf  / par_pf_adm1]
pop <- pop[,.(country, iso3c, name_1, name_2, urban_rural, year, weight)]
prev <- data.table(site$prevalence)
prev <- merge(prev, pop, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'))
prev[,prev := pfpr * weight]
prev <- prev[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'year')]

##To aggregate across urban rural for admin 2
pop2 <- data.table(site$population$population_by_age)[,.(country, iso3c, name_1, name_2, urban_rural,
                                                        year, age_lower, age_upper, par_pf)]
##limit to <6
pop2 <- pop2[age_upper < 6]
##include only half of the 0-1 pop2ulation
pop2[age_lower ==0, par_pf := 0.5 * par_pf]
pop2 <- pop2[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural', 'year')]
pop2[,par_pf_adm2 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]
pop2[,weight := par_pf  / par_pf_adm2]
pop2 <- pop2[,.(country, iso3c, name_1, name_2, urban_rural, year, weight)]
prev2 <- data.table(site$prevalence)
prev2 <- merge(prev2, pop2, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'))
prev2[,prev := pfpr * weight]
prev2 <- prev2[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]

dhs_values[,prev_type := ifelse(prev_type == 'rdt', 'RDT', 'Microscopy')]

prev_plots <- list()
prev_plots[["admin_1"]] <- ggplot() + geom_line(data = prev, aes(year, prev)) +
  facet_wrap(~name_1) +
  geom_point(data = dhs_values[variable == 'prevalence' &
                                 admin_level == 1 &
                                 name_1 %in% prev$name_1], aes(as.integer(SurveyYear), mean, col = prev_type)) +
  geom_errorbar(data = dhs_values[variable == 'prevalence' &
                                    admin_level == 1 &
                                    name_1 %in% prev$name_1], aes(as.integer(SurveyYear), ymin = lower,
                                                                  ymax = upper, col = prev_type),
                width = 0) +
  scale_color_manual(values = c('RDT' = colors[1],
                                'Microscopy' = colors[2])) +
  xlim(2010,2025) + ylim(0,1) +
  theme_bw(base_size = 14) + theme(legend.position = 'bottom',
                                   legend.box = 'vertical') +
  labs(x = NULL, y = 'Prevalence among children 6m-5years',
       col = 'Prevalence test type')


for(name_1_in in unique(prev2$name_1)){
  prev_plots[[name_1_in]] <-   ggplot() +
    geom_line(data = prev2[name_1 == name_1_in], aes(year, prev)) +
    facet_wrap(~name_2) +
    geom_point(data = dhs_values[variable == 'prevalence' &
                                   admin_level == 2 &
                                   name_1 %in% name_1_in &
                                   name_2 %in% unique(prev2[name_1 == name_1_in, name_2])], aes(as.integer(SurveyYear), mean, col = prev_type)) +
    geom_errorbar(data = dhs_values[variable == 'prevalence' &
                                      admin_level == 2 &
                                      name_1 %in% name_1_in &
                                      name_2 %in% unique(prev2[name_1 == name_1_in, name_2])], aes(as.integer(SurveyYear), ymin = lower,
                                                                    ymax = upper, col = prev_type),
                  width = 0) +
    scale_color_manual(values = c('RDT' = colors[1],
                                  'Microscopy' = colors[2])) +
    xlim(2010,2025) + ylim(0,1) +
    theme_bw(base_size = 14) + theme(legend.position = 'bottom',
                                     legend.box = 'vertical') +
    labs(x = NULL, y = 'Prevalence among children 6m-5years',
         title = name_1_in,
         col = 'Prevalence test type',
         caption = 'Note: DHS/MIS values are not representative at the district level')
}

pdf("./prev.pdf", width = 8, height = 8)
prev_plots
dev.off()

################################################################################
##tx_cov
################################################################################
{
  #tx_cov <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
  tx_cov <- readRDS("tx_cov.RDS") %>% data.table()


  tx_cov_plots <- list()
  for(name_1_in in unique(tx_cov$name_1)){
    tx_cov_plots[[name_1_in]] <- ggplot() +
        geom_line(data = new[source == 'adj_map' & name_1 == name_1_in & year > 2009],
                  aes(year, value, group = urban_rural)) +
        geom_point(data = new[source == 'dhis2' & name_1 == name_1_in,],
                   aes(year, value)) +
        ylim(0,1) +
        facet_wrap(~name_2) + theme_bw() +
        labs(x = NULL, y = 'Treatment coverage', title = name_1_in)
  }


  pdf("./tx_cov.pdf", width = 8, height = 8)
  unique(tx_cov_plots)
  dev.off()
}
################################################################################
##smc_cov
################################################################################
#smc_cov <- readRDS(paste0(getwd(), '/archive/smc/', list.files(paste0(getwd(), '/archive/smc/'))[length(list.files(paste0(getwd(), '/archive/smc/')))], '/smc.RDS'))
#mc <- fread('./shared/raw_data/mc_smc_report.csv')
# smc_cov <- readRDS("smc_cov.RDS") %>% data.table()
# smc_cov <- data.table(reshape2::melt(smc_cov, id.vars = c('country', 'iso3c', 'name_1', 'name_2', 'smc_min_age',
#                                                           'smc_max_age', 'age', 'year', 'round', 'round_day_of_year', 'low_level')))
# smc_cov <- smc_cov[variable != 'smc_cov',]
# smc_cov[variable == 'smc_cov_no_split',variable :=
#           'Target population / WPP']
# smc_cov[variable == 'smc_cov_mc_report',variable :=
#           'Malaria Consortium survey coverage']
# smc_cov[variable == 'smc_cov_census',variable :=
#           'Target population / census']
#
#
# smc_cov_plots <- list()
# smc_cov_plots[["Uganda"]] <-  ggplot(smc_cov[low_level == 'adm_1',],
#                                      aes(x = year + round_day_of_year / 365,
#                                          y = value, col = age, group = age, pch = variable)) +
#   geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
#   scale_color_manual(values = c('<1' = colors[1],
#                                 '1-4' = colors[2],
#                                 '5-6' = colors[3],
#                                 '7-10' = colors[4],
#                                 '<5' = "midnightblue")) +
#   theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.5) +
#   geom_hline(yintercept = 1, col = 'grey') +
#   xlim(2020, (max(smc_cov$year) + 1)) +
#   labs(x = NULL, y = 'SMC Coverage', color = 'Age group', title = "Uganda",
#        shape = "SMC coverage source")
#
# for(name_1_in in unique(smc_cov$name_1)){
#   smc_cov_plots[[name_1_in]] <-  ggplot(smc_cov[name_1 == name_1_in & low_level == 'adm_2',],
#                                         aes(x = year + round_day_of_year / 365,
#                                             y = value, col = age, group = age, pch = variable)) +
#     geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
#     scale_color_manual(values = c('<1' = colors[1],
#                                   '1-4' = colors[2],
#                                   '5-6' = colors[3],
#                                   '7-10' = colors[4])) +
#     theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.5) +
#     geom_hline(yintercept = 1, col = 'grey') +
#     xlim(2020, (max(smc_cov$year) + 1)) +
#     labs(x = NULL, y = 'SMC Coverage', color = 'Age group', title = name_1_in)
# }
#
# pdf("./smc_cov.pdf", width = 8, height = 6)
# unique(smc_cov_plots)
# dev.off()

################################################################################
##r21_cov
################################################################################
r21_dt <- readRDS("r21_cov.RDS")
site_r21 <- data.table(site$interventions$vaccine$implementation)[,.(country, iso3c, name_1, name_2,
                                                                     urban_rural, year, r21_primary_cov,
                                                                     r21_booster1_cov)]
site_r21 <- data.table(reshape2::melt(site_r21, id.vars = c('country', 'iso3c',
                                                            'name_1', 'name_2',
                                                            'urban_rural',
                                                            'year')))
site_r21[,variable := paste0('Site file: ', variable)]
setnames(site_r21, 'value', 'r21_cov')
site_r21[,month := 6]
r21_dt <- rbind(r21_dt, site_r21, fill = T)
r21_dt[is.na(urban_rural), urban_rural := 'Not reported']
r21_dt[,variable := gsub(pattern = 'Malaria_vaccine_',
                         replacement = 'R21 dose: ',
                         variable)]

r21_cov_plots <- list()

r21_cov_plots[['Uganda']] <- ggplot() +
  theme_bw(base_size = 14) +
  geom_point(data = r21_dt[low_level == 'adm_1',],
             aes(x = year , y = r21_cov,
                 col = variable), size = 2) +
  geom_hline(data = dhs_values[admin_level == 1 & variable == 'dpt3_cov' & SurveyYear == 2016],
             aes(yintercept = mean)) +
  scale_color_manual(values = c('Site file: r21_primary_cov' = 'black',
                                'Site file: r21_booster1_cov' = 'grey',
                                'R21 dose: 1' = colors[1],
                                'R21 dose: 2' = colors[2],
                                'R21 dose: 3' = colors[3])) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2020,max(r21_dt$year)) +
  labs(x = NULL, y = 'R21 Coverage', color = 'R21 dose', title = 'Uganda',
       caption = 'Horizontal line represents 2016 DHS DPT3 coverage among 1 year olds') +
  guides(col=guide_legend(nrow=2)) +   facet_wrap(~name_1)

r21_cov_plots[['acholi_zoom']] <- ggplot() +
  theme_bw(base_size = 14) +
  geom_point(data = r21_dt[low_level == 'adm_1' & name_1 == 'Acholi',],
             aes(x = year , y = r21_cov,
                 col = variable), size = 2) +
  geom_hline(data = dhs_values[admin_level == 1 & variable == 'dpt3_cov' &
                                 SurveyYear == 2016 & name_1 == 'Acholi'],
             aes(yintercept = mean)) +
  scale_color_manual(values = c('Site file: r21_primary_cov' = 'black',
                                'Site file: r21_booster1_cov' = 'grey',
                                'R21 dose: 1' = colors[1],
                                'R21 dose: 2' = colors[2],
                                'R21 dose: 3' = colors[3])) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2020,max(r21_dt$year)) +
  labs(x = NULL, y = 'R21 Coverage', color = 'R21 dose',
       caption = 'Horizontal line represents 2016 DHS DPT3 coverage among 1 year olds') +
  guides(col=guide_legend(nrow=2)) +   facet_wrap(~name_1)



for(name_1_in in unique(r21_dt$name_1)){
  r21_cov_plots[[name_1_in]] <-  ggplot(r21_dt[name_1 == name_1_in & low_level == 'adm_2',],
                                        aes(x = year, y = r21_cov,
                                            col = variable)) +
    geom_point(size = 2) + facet_wrap(~name_2) +
    theme_bw(base_size = 14) +
    scale_color_manual(values = c('Site file: r21_primary_cov' = 'black',
                                  'Site file: r21_booster1_cov' = 'grey',
                                  'R21 dose: 1' = colors[1],
                                  'R21 dose: 2' = colors[2],
                                  'R21 dose: 3' = colors[3])) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
    xlim(2020,max(r21_dt$year)) +
    labs(x = NULL, y = 'R21 Coverage', color = 'R21 dose', title = name_1_in) +
    guides(col=guide_legend(nrow=2))
}

pdf("./r21_cov.pdf", width = 8, height = 8)
unique(r21_cov_plots)
dev.off()
#
# ################################################################################
# ##irs
# ################################################################################
#irs_dt <- readRDS(paste0(getwd(), '/archive/irs/', list.files(paste0(getwd(), '/archive/irs/'))[length(list.files(paste0(getwd(), '/archive/irs/')))], '/irs.RDS'))
#irs_dt <- readRDS(paste0(getwd(), '/draft/irs/', list.files(paste0(getwd(), '/draft/irs/'))[length(list.files(paste0(getwd(), '/draft/irs/')))], '/irs.RDS'))

irs_dt <- readRDS("irs_cov.RDS")



irs_plots <- list()
irs_plots[['Uganda']] <- ggplot() + facet_wrap(~name_1) +
  theme_bw() +
  geom_point(data = irs_dt[low_level == 'adm_1'],
            aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                col = variable), size = 1.1, alpha = 0.8) +
  geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 & SurveyYear > 2015],
             aes(as.integer(SurveyYear )+0.5, mean), col = "black",  size = 2) +
  scale_color_manual(values = c('Structure Coverage' = colors[1],
                                'Population Coverage' = colors[2],
                                'MAP population coverage' = 'grey')) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2010,2025) +
  geom_vline(xintercept = 2020) +
  labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = "Uganda",
       caption = 'Points represent MIS IRS coverage\nvalues pre-2020 are MAP and post-2020 are campaign data') +
  guides(col=guide_legend(nrow=2))

for(name_1_in in unique(irs_dt$name_1)){
  data <- irs_dt[name_1 == name_1_in & low_level == 'adm_2',]

  irs_plots[[name_1_in]] <-  ggplot() + facet_wrap(~name_2) +
    theme_bw() +
    geom_point(data = data,
              aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                  col = variable, lty = insecticide), size = 1.1, alpha = 0.8) +
    scale_color_manual(values = c('Structure Coverage' = colors[1],
                                  'Population Coverage' = colors[2],
                                  'MAP population coverage' = 'grey')) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
    xlim(2010,2025) +
    geom_vline(xintercept = 2020) +
    labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = name_1_in,
         caption = 'Values pre-2020 are MAP and post-2020 are campaign data',
         lty = 'Insecticide') +
    guides(col=guide_legend(nrow=2))
}

pdf("./irs.pdf", width = 8, height = 8)
unique(irs_plots)
dev.off()


# ################################################################################
# ##ITN
# ################################################################################
#llin<- readRDS(paste0(getwd(), '/archive/itn/', list.files(paste0(getwd(), '/archive/itn/'))[length(list.files(paste0(getwd(), '/archive/itn/')))], '/llin.RDS'))
llin <- readRDS("llin.RDS")

comp <- rbind(dhs_values[variable == 'net_usage' & SurveyYear %in% 2010:2025 & admin_level == 1 &
                           name_1 %in% unique(site_itn_use$name_1),.(type = 'DHS/MIS data', mean,
                                                                     name_1, year = SurveyYear,
                                                                     admin_level,
                                                                     lower, upper)],
              llin[,.(mean = cov, name_1, year = 2023, type = '2023 campaign data',
                      admin_level = 1)], fill = T)
site_itn_use$year <- as.numeric(site_itn_use$year)
comp$year <- as.numeric(comp$year)

pdf("./itn.pdf", width = 8, height = 8)
ggplot() +
  facet_wrap(~name_1) + ylim(0,1) + theme_bw(base_size = 14) +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_vline(xintercept = 2014, col = colors[3], lwd = 1.1, alpha = 0.1) +
  geom_vline(xintercept = 2017, col = colors[3], lwd = 1.1, alpha = 0.1) +
  geom_vline(xintercept = 2020, col = colors[3], lwd = 1.1, alpha = 0.1) +
  geom_vline(xintercept = 2023, col =colors[3], lwd = 1.1, alpha = 0.1) +
  geom_line(data  = site_itn_use[year %in% 2010:2025],
            aes(year, cov), lwd = 1.05) +
  geom_point(data = comp,
             aes(year,mean, col = type), size = 2.5) +
  geom_errorbar(data = comp,
                aes(x = year, ymin = lower, ymax = upper, col = type), width = 0) +
  scale_color_manual(values = c('2023 campaign data' = colors[1],
                                'DHS/MIS data' = colors[2])) +
  labs(x = NULL, y = 'ITN usage', color = 'Data source')
dev.off()
