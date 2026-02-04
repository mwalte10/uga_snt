library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
#library(wesanderson)

orderly::orderly_dependency(
  name = "site_file",
  query = "latest()",
  files = c(
    site.RDS = "site.RDS"
  )
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
    smc_cov.RDS = "smc.RDS"
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
site <- readRDS("site.RDS")
##load in dhs values for comparison
dhs_values <- readRDS("dhs_values.RDS")
setnames(dhs_values, c('adm_1_2025', 'adm_2_2025'), c('name_1', 'name_2'))
mis_2024 <- fread("mis_2024.csv")
dhs_values <- rbind(dhs_values, mis_2024, fill = T)

#colors <- wesanderson::wes_palette("AsteroidCity1")
colors <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645")


################################################################################
##Prevalence
###going to weight by PAR to aggregate up to the
################################################################################
pop <- data.table(site$population$population_by_age)[,.(country, iso3c, name_1, name_2, year, age_lower, age_upper, par_pf)]
##limit to <6
pop <- pop[age_upper < 6]
##include only half of the 0-1 population
pop[age_lower ==0, par_pf := 0.5 * par_pf]
pop <- pop[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'year')]
pop[,par_pf_adm1 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'year')]
pop[,weight := par_pf  / par_pf_adm1]
pop <- pop[,.(country, iso3c, name_1, name_2, year, weight)]

prev <- data.table(site$prevalence)
prev <- merge(prev, pop, by = c('country', 'iso3c', 'name_1', 'name_2', 'year'))
prev[,prev := pfpr * weight]
prev <- prev[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'year', 'urban_rural')]
dhs_values[,prev_type := ifelse(prev_type == 'rdt', 'RDT', 'Microscopy')]

prev_plots <- ggplot() + geom_line(data = prev, aes(year, prev, lty = urban_rural)) +
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
  labs(x = NULL, y = 'Prevalence among children 6m-5years', lty = 'Urbanicity', col = 'Prevalence test type')

pdf("./prev.pdf", width = 8, height = 8)
prev_plots
dev.off()

################################################################################
##tx_cov
################################################################################
{
  #tx_cov <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
  tx_cov <- readRDS("tx_cov.RDS") %>% data.table()
  tx_cov <- rbind(tx_cov,
                  data.table(site$interventions$treatment$implementation)[,.(country, iso3c, name_1,
                                                                             name_2, year, month = 1,
                                                                             urban_rural,
                                                                             value = tx_cov,
                                                                             low_level = 'adm_2',
                                                                             variable = 'tx_cov_sitefile')],
                  fill = T)
  tx_cov[is.na(urban_rural), urban_rural := 'Not reported']
  tx_cov[variable == 'tx_cov_baseline', variable := "Cases"]
  tx_cov[variable == 'tx_cov_fever', variable := "Fevers"]
  tx_cov[variable == 'tx_cov_N1', variable := "Cases (adj. testing)"]
  tx_cov[variable == 'tx_cov_N2', variable := "Cases (adj. reporting)"]
  tx_cov$variable <- factor(tx_cov$variable, levels = c('Cases',
                                                        "Cases (adj. testing)",
                                                        "Cases (adj. reporting)",
                                                        "Fevers"))
  
  tx_cov_plots <- list()
  ##plot with facet across admin_1
  tx_cov_adm_1 <- tx_cov[variable %in% c('Cases',
                                         "Cases (adj. testing)",
                                         "Cases (adj. reporting)",
                                         "Fevers") & low_level == 'adm_1',]
  
  tx_cov_plots[['Bukedi_strat_cases']] <- ggplot() +
    geom_line(data = tx_cov_adm_1[name_1 == 'Bukedi' & variable == 'Cases'],
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = colors[1],
                                  'Cases (adj. testing)' = colors[2],
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = colors[4],
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" &
                                   admin_level == 1 &
                                   as.integer(SurveyYear)>2015 &
                                   name_1 == 'Bukedi'],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" &
                                      admin_level == 1 &
                                      as.integer(SurveyYear)>2015 &
                                      name_1 == 'Bukedi'],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    geom_hline(yintercept = 0.9, col = 'grey', lty = 2) +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  tx_cov_plots[['Bukedi_strat_cases_n1']] <- ggplot() +
    geom_line(data = tx_cov_adm_1[name_1 == 'Bukedi' & variable %in% c('Cases', 'Cases (adj. testing)')],
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = colors[1],
                                  'Cases (adj. testing)' = colors[2],
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = colors[4],
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" &
                                   admin_level == 1 &
                                   as.integer(SurveyYear)>2015 &
                                   name_1 == 'Bukedi'],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" &
                                      admin_level == 1 &
                                      as.integer(SurveyYear)>2015 &
                                      name_1 == 'Bukedi'],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    geom_hline(yintercept = 0.9, col = 'grey', lty = 2) +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  tx_cov_plots[['Bukedi_strat_cases_n2']] <- ggplot() +
    geom_line(data = tx_cov_adm_1[name_1 == 'Bukedi' & variable %in% c('Cases', 'Cases (adj. testing)', 'Cases (adj. reporting)')],
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = colors[1],
                                  'Cases (adj. testing)' = colors[2],
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = colors[4],
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" &
                                   admin_level == 1 &
                                   as.integer(SurveyYear)>2015 &
                                   name_1 == 'Bukedi'],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" &
                                      admin_level == 1 &
                                      as.integer(SurveyYear)>2015 &
                                      name_1 == 'Bukedi'],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    geom_hline(yintercept = 0.9, col = 'grey', lty = 2) +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  tx_cov_plots[['Bukedi_strat_fevers']] <- ggplot() +
    geom_line(data = tx_cov_adm_1[name_1 == 'Bukedi' & variable %in% c('Cases',
                                                                       'Cases (adj. testing)',
                                                                       'Cases (adj. reporting)',
                                                                       "Fevers")],
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = colors[1],
                                  'Cases (adj. testing)' = colors[2],
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = colors[4],
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" &
                                   admin_level == 1 &
                                   as.integer(SurveyYear)>2015 &
                                   name_1 == 'Bukedi'],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" &
                                      admin_level == 1 &
                                      as.integer(SurveyYear)>2015 &
                                      name_1 == 'Bukedi'],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    geom_hline(yintercept = 0.9, col = 'grey', lty = 2) +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  
  
  tx_cov_plots[['Uganda']] <- ggplot() +
    geom_line(data = tx_cov_adm_1,
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = colors[1],
                                  'Cases (adj. testing)' = colors[2],
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = colors[4],
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" & admin_level == 1 & as.integer(SurveyYear)>2015 ],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" & admin_level == 1 & as.integer(SurveyYear)>2015 ],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  tx_cov_plots[['Uganda_adj']] <- ggplot() +
    geom_line(data = tx_cov_adm_1,
              aes(x = year + (month - 1) / 12, y = value,
                  col = variable),
              lwd = 1.1, alpha = 0.8) + facet_wrap(~name_1) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('Cases' = "grey",
                                  'Cases (adj. testing)' = "grey",
                                  'Cases (adj. reporting)' = colors[3],
                                  "Fevers" = "grey",
                                  'tx_cov_sitefile' = 'black')) +
    geom_point(data = dhs_values[variable == "trt_fever" & admin_level == 1 & as.integer(SurveyYear)>2015 ],
               aes(as.integer(SurveyYear), mean)) +
    geom_errorbar(data = dhs_values[variable == "trt_fever" & admin_level == 1 & as.integer(SurveyYear)>2015 ],
                  aes(x = as.integer(SurveyYear), ymin = lower, ymax = upper), width = 0) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.1) +
    xlim(2010, 2025) +
    geom_hline(yintercept = 1, col = 'grey') +
    labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = "Uganda",
         caption = "Points are DHS/MIS values of\npropotion of children with fever in the\nlast two weeks who sought treatment")
  
  
  # for(name_1_in in unique(tx_cov$name_1)){
  #   tx_cov_plots[[name_1_in]] <-  ggplot(tx_cov[name_1 == name_1_in & grepl('tx_cov', variable) &
  #                                                 low_level == 'adm_2',],
  #                                        aes(x = year + (month - 1) / 12, y = value, col = variable, lty = urban_rural)) +
  #     geom_line(lwd = 1.1, alpha = 0.8) + facet_wrap(~name_2) + theme_bw() +
  #     scale_color_manual(values = c('Cases' = colors[1],
  #                                   'Cases (adj. testing)' = colors[2],
  #                                   'Cases (adj. reporting)' = colors[3],
  #                                   "Fevers" = colors[4],
  #                                   'tx_cov_sitefile' = 'black')) +
  #
  #     theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.5) +
  #     xlim(2010, 2025) +
  #     geom_hline(yintercept = 1, col = 'grey') +
  #     labs(x = NULL, y = 'Treatment coverage', color = 'Denominator options', title = name_1_in,
  #          lty = 'Urbanicity')
  # }
  
  
  pdf("./tx_cov.pdf", width = 8, height = 8)
  unique(tx_cov_plots)
  dev.off()
}
################################################################################
##smc_cov
################################################################################
#smc_cov <- readRDS(paste0(getwd(), '/archive/smc/', list.files(paste0(getwd(), '/archive/smc/'))[length(list.files(paste0(getwd(), '/archive/smc/')))], '/smc.RDS'))
#mc <- fread('./shared/raw_data/mc_smc_report.csv')
smc_cov <- readRDS("smc_cov.RDS") %>% data.table()
smc_cov <- data.table(reshape2::melt(smc_cov, id.vars = c('country', 'iso3c', 'name_1', 'name_2', 'smc_min_age',
                                                          'smc_max_age', 'age', 'year', 'round', 'round_day_of_year', 'low_level')))
smc_cov <- smc_cov[variable != 'smc_cov',]
smc_cov[variable == 'smc_cov_no_split',variable :=
          'Target population / WPP']
smc_cov[variable == 'smc_cov_mc_report',variable :=
          'Malaria Consortium survey coverage']
smc_cov[variable == 'smc_cov_census',variable :=
          'Target population / census']


smc_cov_plots <- list()
smc_cov_plots[["Uganda"]] <-  ggplot(smc_cov[low_level == 'adm_1',],
                                     aes(x = year + round_day_of_year / 365,
                                         y = value, col = age, group = age, pch = variable)) +
  geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
  scale_color_manual(values = c('<1' = colors[1],
                                '1-4' = colors[2],
                                '5-6' = colors[3],
                                '7-10' = colors[4],
                                '<5' = "midnightblue")) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.5) +
  geom_hline(yintercept = 1, col = 'grey') +
  xlim(2020, (max(smc_cov$year) + 1)) +
  labs(x = NULL, y = 'SMC Coverage', color = 'Age group', title = "Uganda",
       shape = "SMC coverage source")

for(name_1_in in unique(smc_cov$name_1)){
  smc_cov_plots[[name_1_in]] <-  ggplot(smc_cov[name_1 == name_1_in & low_level == 'adm_2',],
                                        aes(x = year + round_day_of_year / 365,
                                            y = value, col = age, group = age, pch = variable)) +
    geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
    scale_color_manual(values = c('<1' = colors[1],
                                  '1-4' = colors[2],
                                  '5-6' = colors[3],
                                  '7-10' = colors[4])) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1.5) +
    geom_hline(yintercept = 1, col = 'grey') +
    xlim(2020, (max(smc_cov$year) + 1)) +
    labs(x = NULL, y = 'SMC Coverage', color = 'Age group', title = name_1_in)
}

pdf("./smc_cov.pdf", width = 8, height = 6)
unique(smc_cov_plots)
dev.off()

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
irs_dt <- readRDS("irs_cov.RDS")

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

irs_dt <- rbind(irs_dt, site_irs, fill = T)
irs_dt[variable == 'structure_coverage', variable := 'Structure Coverage']
irs_dt[variable == 'pop_coverage', variable := 'Population Coverage']
irs_dt$variable <- factor(irs_dt$variable, levels = c('Structure Coverage', 'Population Coverage', 'MAP population coverage'))
irs_dt_adm1 <- irs_dt[low_level == 'adm_1',irs_cov := sum(irs_cov), by = c('country', 'iso3c', 'name_1', 'name_2', 'low_level',
                                                                           'year', 'variable', 'peak_season', 'round', 'spray_day_of_year')]
irs_dt <- rbind(irs_dt[low_level != 'adm_1'],
                irs_dt_adm1)

irs_plots <- list()
irs_plots[['Uganda']] <- ggplot() + facet_wrap(~name_1) +
  theme_bw() +
  geom_line(data = irs_dt[low_level == 'adm_1'],
            aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                col = variable), lwd = 1.1, alpha = 0.8) +
  geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 & SurveyYear > 2015],
             aes(as.integer(SurveyYear )+0.5, mean), col = "black",  size = 2) +
  scale_color_manual(values = c('Structure Coverage' = colors[1],
                                'Population Coverage' = colors[2],
                                'MAP population coverage' = 'grey')) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2010,2025) +
  labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = "Uganda",
       caption = 'Points represent MIS IRS coverage') +
  guides(col=guide_legend(nrow=2))

irs_plots[['Teso_focus']] <- ggplot() + facet_wrap(~name_1) +
  theme_bw() +
  geom_line(data = irs_dt[low_level == 'adm_1' & name_1 == 'Teso'],
            aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                col = variable), lwd = 1.1, alpha = 0.8) +
  geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 & SurveyYear > 2015 & name_1 == 'Teso'],
             aes(as.integer(SurveyYear )+0.5, mean), col = "black",  size = 2) +
  scale_color_manual(values = c('Structure Coverage' = colors[1],
                                'Population Coverage' = colors[2],
                                'MAP population coverage' = 'grey')) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2010,2025) +
  labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type',
       caption = 'Points represent MIS IRS coverage') +
  guides(col=guide_legend(nrow=2))


for(name_1_in in unique(irs_dt$name_1)){
  data <- irs_dt[name_1 == name_1_in & low_level == 'adm_2',]
  
  irs_plots[[name_1_in]] <-  ggplot() + facet_wrap(~name_2) +
    theme_bw() +
    geom_line(data = data,
              aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                  col = variable, lty = insecticide), lwd = 1.1, alpha = 0.8) +
    scale_color_manual(values = c('Structure Coverage' = colors[1],
                                  'Population Coverage' = colors[2],
                                  'MAP population coverage' = 'grey')) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
    xlim(2010,2025) +
    labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = name_1_in,
         lty = 'Insecticide') +
    guides(col=guide_legend(nrow=2))
}

pdf("./irs.pdf", width = 8, height = 8)
unique(irs_plots)
dev.off()
