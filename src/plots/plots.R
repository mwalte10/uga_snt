library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
library(cowplot)
library(geofacet)
#library(wesanderson)

orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
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
    smc.RDS = "smc.RDS"
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

orderly::orderly_dependency(
  name = "itn",
  query = "latest()",
  files = c(
    llin.RDS = "llin.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_calibration",
  query = "latest()",
  files = c(
    calibrated_results.RDS = "calibrated_results.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_calibration",
  query = "latest()",
  files = c(
    aggregated_results.RDS = "aggregated_results.RDS"
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
#site = readRDS('Z:/Maggie/uga_snt/shared/raw_data/2026_02_18_UGA_SNT_sitefile.rds')
site <- readRDS("site.RDS")
##load in dhs values for comparison
#dhs_values = readRDS('Z:/Maggie/uga_snt/shared/dhs_values.RDS')
dhs_values <- readRDS("dhs_values.RDS")
data.table::setnames(dhs_values, c('adm_1_2025', 'adm_2_2025'), c('name_1', 'name_2'))
#mis_2024 = data.table::fread('Z:/Maggie/uga_snt/shared/mis_2024.csv')
mis_2024 <- fread("mis_2024.csv")
dhs_values <- rbind(dhs_values, mis_2024, fill = T)
dhs_values[name_2 == 'Sembabule', name_2 := 'Ssembabule']
dhs_values[name_2 == 'Buvuma', name_1 := 'North Buganda']

#colors <- wesanderson::wes_palette("AsteroidCity1")
colors <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645")

districts <- unique(site$shape$level_2$name_2)
districts <- c(setdiff(districts, 'Sembabule'), 'Ssembabule')

################################################################################
##Prevalence
###going to weight by PAR to aggregate up to the
################################################################################
pop <- data.table(site$population$population_by_age)[,.(country, iso3c, name_1, name_2, urban_rural,
                                                        year, age_lower, age_upper, par_pf)]
##limit to 2-10 years
pop <- pop[age_lower %in% 2:10]
pop <- pop[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural', 'year')]
pop[,par_pf_adm1 := sum(par_pf), by = c('country', 'iso3c', 'name_1', 'year')]
pop[,weight := par_pf  / par_pf_adm1]
pop <- pop[,.(country, iso3c, name_1, name_2, urban_rural, year, weight)]
prev <- data.table(site$prevalence)
prev <- merge(prev, pop, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'))
prev[,prev := pfpr * weight]
prev <- prev[,.(prev= sum(prev)), by = c('country', 'iso3c', 'name_1', 'year')]

##To aggregate across urban rural for admin 2
prev <- data.table(site$prevalence)
pop2 <- data.table(site$population$population_by_age)[,.(country, iso3c, name_1, name_2, urban_rural,
                                                         year, age_lower, age_upper, par_pf)]
##limit to 2-10 years
pop2 <- pop2[age_lower %in% 2:9]
pop2 <- pop2[,.(par_pf = sum(par_pf)), by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural', 'year')]
prev <- merge(prev, pop2, by = c('country', 'iso3c', 'name_1', 'name_2', 'urban_rural','year'),
              all.x = T)
prev[,pf_prev := par_pf * pfpr]
prev <- prev[,.(pf_prev = sum(pf_prev), par_pf = sum(par_pf)), by = c('name_1', 'year')]
prev[,prev := pf_prev / par_pf]

dhs_values[,prev_type := ifelse(prev_type == 'rdt', 'RDT', 'Microscopy')]
##remove 2015/2016 microscopy, not collected in that survey so those zeroes are false
dhs_values <- dhs_values[!(prev_type == 'Microscopy' & SurveyYear == 2016),]
dhs_values <- dhs_values[!(SurveyYear == 2011),] ##only Karamoja
# dhs_values <- rbind(dhs_values,
#                     data.table(name_1 = 'Acholi', name_2 = 'Acholi',  variable = 'prevalence',
#                                  admin_level = 1, SurveyYear = 2020, prev_type = 'PCR', mean = NA),
#                     fill = T)


#agg_results <- readRDS(paste0(getwd(), '/archive/process_calibration/', list.files(paste0(getwd(), '/archive/process_calibration/'))[length(list.files(paste0(getwd(), '/archive/process_calibration/')))], '/aggregated_results.RDS'))
agg_results <- readRDS("aggregated_results.RDS")

prev_plots <- list()
prev_plots[["Uganda"]] <- ggplot() +
  geom_line(data = prev, aes(year, prev)) +
  # geom_line(data = prev[year < 2019], aes(year, prev)) +
 # geom_line(data = prev[year >= 2019], aes(year, prev), col = 'grey') +
  facet_wrap(~name_1) +
  geom_point(data = dhs_values[variable == 'prevalence' &
                                 admin_level == 1 &
                                 name_1 %in% prev$name_1], aes(as.integer(SurveyYear), mean, col = prev_type)) +
  geom_errorbar(data = dhs_values[variable == 'prevalence' &
                                    admin_level == 1 &
                                    name_1 %in% prev$name_1], aes(as.integer(SurveyYear), ymin = lower,
                                                                  ymax = upper, col = prev_type),
                width = 0) +
  # geom_line(data = agg_results[low_level == 'adm_1' & variable == 'detect_pcr' & year > 2009,],
  #           aes(year + day_of_year / 365, prev), col = colors[3]) +
  geom_line(data = agg_results[low_level == 'adm_1' & variable == 'detect_lm' & year > 2009,],
            aes(year + day_of_year / 365, prev), col = colors[2]) +
  scale_color_manual(values = c('RDT' = colors[1],
                                'Microscopy' = colors[2])) +
  xlim(2010,2026) + ylim(0,1) +
  theme_bw(base_size = 14) + theme(legend.position = 'bottom',
                                   legend.box = 'vertical') +
  labs(x = NULL, y = 'Prevalence among children 2-10 years',
       title = 'Uganda',
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
    labs(x = NULL, y = 'Prevalence among children 2-10 years',
         title = name_1_in,
         col = 'Prevalence test type',
         caption = 'Note: DHS/MIS values are not representative at the district level')
}

pdf("./prev.pdf", width = 8, height = 8)
prev_plots
dev.off()

#calib <- readRDS(paste0(getwd(), '/archive/process_calibration/', list.files(paste0(getwd(), '/archive/process_calibration/'))[length(list.files(paste0(getwd(), '/archive/process_calibration/')))], '/calibrated_results.RDS'))
calib <- readRDS("calibrated_results.RDS")


prev_plots_admin2 <- list()
for(name_2_in in districts){
  if(name_2_in %in% unique(calib$name_2)){
    x <- calib[name_2 == name_2_in,]
    fitted_prev <- data.table(postie::get_prevalence(x))
  }else{
    fitted_prev  <- data.table::data.table(
      time = NA,
      lm_prevalence_2_10 = NA,
      name_2 = name_2_in
    )
  }


  prev_plots_admin2[[name_2_in]] <-   ggplot() +
    geom_point(data = prev2[name_2 == name_2_in], aes(year + 0.5, prev),
               col = colors[2], size = 3) +
    geom_line(data = fitted_prev, aes(time, lm_prevalence_2_10)) +
    facet_wrap(~name_2) +
    geom_point(data = dhs_values[variable == 'prevalence' &
                                   admin_level == 2 &
                                   name_2 %in% name_2_in &
                                   prev_type == 'Microscopy'],
               aes(as.integer(SurveyYear), mean, col = prev_type),
               shape = 18, alpha = 0.5, col = 'black', size = 3) +
    scale_color_manual(values = c('Microscopy' = colors[3])) +
    xlim(2010,2026) + ylim(0,1) +
    theme_bw() + theme(legend.position = 'bottom',
                                     legend.box = 'vertical') +
    labs(x = NULL, y = expression(PfPR[2*y - 10*y]))
}


################################################################################
# CCM ---------------------------------------------------------------------
################################################################################
{
  #tx_cov <- readRDS(paste0(getwd(), '/archive/tx_cov/', list.files(paste0(getwd(), '/archive/tx_cov/'))[length(list.files(paste0(getwd(), '/archive/tx_cov/')))], '/tx_cov.RDS'))
  tx_cov <- readRDS("tx_cov.RDS") %>% data.table()
  tx_cov[source == 'dhis2', source := 'NMED']
  tx_cov[name_2 == 'Sembabule', name_2 := 'Ssembabule']

  ##Unadjusted MAP estimates
  map <- data.table(site$interventions$treatment$implementation)

  tx_cov_plots <- list()
  for(name_1_in in unique(tx_cov$name_1)){
    tx_cov_plots[[name_1_in]] <-ggplot() +
        geom_line(data = tx_cov[source == 'adj_map' & name_1 == name_1_in & year > 2009],
                  aes(year, value, group = urban_rural)) +
      geom_line(data = map[name_1 == name_1_in & year > 2009],
                aes(year, tx_cov, group = urban_rural), col = 'grey') +
      geom_line(data = map[name_1 == name_1_in & year > 2009],
                aes(year, tx_cov*prop_act, group = urban_rural), col = 'grey', lty = 2) +
      facet_wrap(~name_2) +
      geom_point(data = tx_cov[source == 'NMED' &
                                     name_1 == name_1_in &
                                 low_level == 'adm_2'] ,
                 aes(year, value, col = source),
                 size = 2) +
      scale_color_manual(values = c('NMED' = colors[1])) +
      geom_point(data = dhs_values[variable == 'trt_fever' & SurveyYear > 2015 &
                                   name_1 == name_1_in & name_2 %in% unique(tx_cov[name_1 == name_1_in, name_2])],
                 aes(as.integer(SurveyYear )+0.5, mean), col = "black",
                 size = 2, shape = 18, alpha = 0.5) +
        ylim(0,1) +
        theme_bw() + theme(legend.position = 'bottom') +
        labs(x = NULL, y = 'Treatment coverage', title = name_1_in, col = 'Data source')
  }

  pdf("./tx_cov.pdf", width = 8, height = 8)
  tx_cov_plots
  dev.off()

  tx_cov_plots_admin2 <- list()
  for(name_2_in in districts){
    tx_cov_plots_admin2[[name_2_in]] <- ggplot() +
      geom_line(data = tx_cov[source == 'adj_map' & name_2 == name_2_in & year > 2009],
                aes(year, value, group = urban_rural)) +
      geom_line(data = map[name_2 == name_2_in & year > 2009],
                aes(year, tx_cov, group = urban_rural), col = 'grey') +
      geom_line(data = map[name_2 == name_2_in & year > 2009],
                aes(year, tx_cov*prop_act, group = urban_rural), col = 'grey', lty = 2) +
      facet_wrap(~name_2) +
      geom_point(data = tx_cov[source == 'NMED' &
                                 name_2 == name_2_in &
                                 low_level == 'adm_2'] ,
                 aes(year, value, col = source),
                 size = 2) +
      scale_color_manual(values = c('NMED' = colors[1])) +
      ##TODO: pull out cluster level treatment for fever
      geom_point(data = dhs_values[variable == 'trt_fever' & SurveyYear > 2015 &
                                     name_2 == name_2_in ],
                 aes(as.integer(SurveyYear )+0.5, mean), col = "black",
                 size = 3, shape = 18, alpha = 0.5) +
      ylim(0,1) +
      theme_bw() + theme(legend.position = "none") +
      labs(x = NULL, y = 'Treatment coverage', col = 'Data source')
  }


}

################################################################################
# SMC ---------------------------------------------------------------------
################################################################################
#smc <- readRDS(paste0(getwd(), '/archive/smc/', list.files(paste0(getwd(), '/archive/smc/'))[length(list.files(paste0(getwd(), '/archive/smc/')))], '/smc.RDS'))
smc <- readRDS("smc.RDS") %>% data.table()
smc[,age_label := paste0(smc_min_age / 365, '-', smc_max_age/365)]
smc$age_label <- factor(smc$age_label, levels = c('0-4', '5-10', '0-10'))

smc_cov_plots <- list()
smc_cov_plots[["Uganda"]] <-  ggplot(smc[low_level == 'adm_1',],
                                     aes(x = year + round_day_of_year / 365,
                                         y = smc_cov, col = age_label)) +
  geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
  theme(legend.position = 'bottom', legend.box = 'vertical') +
  geom_hline(yintercept = 1, col = 'grey') +
  xlim(2020, (max(smc$year) + 1)) +
  geom_point(data = data.table(name_1 = unique(smc$name_1), year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
  labs(x = NULL, y = 'SMC coverage', title = "Uganda", col = 'SMC eligibility') +
    scale_y_continuous(       breaks = seq(0, 1, by = 0.25),       limits = c(0, 1)     )


for(name_1_in in unique(smc$name_1)){
  smc_cov_plots[[name_1_in]] <-  ggplot(smc[low_level == 'adm_2' & name_1 == name_1,],
                                        aes(x = year + round_day_of_year / 365,
                                            y = smc_cov, col = age_label)) +
    geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
    theme(legend.position = 'bottom', legend.box = 'vertical') +
    geom_hline(yintercept = 1, col = 'grey') +
    xlim(2020, (max(smc$year) + 1)) +
    geom_point(data = data.table(name_1 = unique(smc$name_1), year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
    labs(x = NULL, y = 'SMC coverage', title = "Uganda", col = 'SMC eligibility')+
      scale_y_continuous(       breaks = seq(0, 1, by = 0.25),       limits = c(0, 1)     )

}

pdf("./smc_cov.pdf", width = 8, height = 6)
unique(smc_cov_plots)
dev.off()

smc_plots_admin2 <- list()
for(name_2_in in districts){
  data <- smc[name_2 == name_2_in & low_level == 'adm_2',]
  data <- unique(data)
  if(nrow(data) == 0){
    data <- data.table(year = 2010:2026, round_day_of_year = 0, smc_cov = 0, age_label = NA,
                       name_2 = name_2_in)
  }

  smc_plots_admin2[[name_2_in]] <- ggplot(data,
                                          aes(x = year + round_day_of_year / 365,
                                              y = smc_cov, col = age_label)) +
    geom_point(size = 2) + facet_wrap(~name_2) + theme_bw() +
    theme(legend.position = 'bottom', legend.box = 'vertical') +
    geom_hline(yintercept = 1, col = 'grey') +
    xlim(2010, (max(smc$year) + 1)) +
    geom_point(data = data.table(name_2 = unique(data$name_2), year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
    labs(x = NULL, y = 'SMC coverage', col = 'SMC eligibility') +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.05))
}


################################################################################
## R21 ---------------------------------------------------------------------
################################################################################
#r21_dt <- readRDS(paste0(getwd(), '/archive/r21/', list.files(paste0(getwd(), '/archive/r21/'))[length(list.files(paste0(getwd(), '/archive/r21/')))], '/r21.RDS'))
r21_dt <- readRDS("r21_cov.RDS")
r21_dt[,r21_vaccine_dose := gsub(pattern = 'Malaria_vaccine_',
                         replacement = 'R21 dose: ',
                         r21_vaccine_dose)]
r21_dt <- r21_dt[r21_vaccine_dose == 'R21 dose: 3',.(country, iso3c, year,
                                                     name_1, name_2, low_level,
                                                     r21_primary_cov, r21_booster1_cov)]
r21_dt <- data.table(reshape2::melt(r21_dt, id.vars = c('country', 'iso3c', 'year',
                                                        'name_1', 'name_2', 'low_level')))
r21_dt[,variable := ifelse(variable == 'r21_primary_cov', '3-dose series coverage', 'Booster coverage')]


r21_cov_plots <- list()
r21_cov_plots[['Uganda']] <- ggplot() +
  theme_bw(base_size = 14) +
  geom_point(data = r21_dt[low_level == 'adm_1',],
             aes(x = year , y = value,
                 col = variable), size = 2) +
  geom_line(data  = r21_dt[low_level == 'adm_1',],
            aes(x = year , y = value, group = variable)) +
  geom_hline(data = dhs_values[admin_level == 1 & variable == 'dpt3_cov' & SurveyYear == 2016],
             aes(yintercept = mean)) +
  scale_color_manual(values = c('3-dose series coverage' = colors[3],
                                'Booster coverage' = colors[4])) +
  theme(legend.position = 'bottom') +
  xlim(2010,max(r21_dt$year)) +
  labs(x = NULL, y = 'R21 coverage', color = 'R21 dose', title = 'Uganda',
       caption = 'Horizontal line represents 2016 DHS DPT3 coverage among 1 year olds\nBooster coverage is assumed.') +
  guides(col=guide_legend(nrow=2)) +   facet_wrap(~name_1) +
    scale_y_continuous(       breaks = seq(0, 1, by = 0.25),       limits = c(0, 1)     )


for(name_1_in in unique(r21_dt$name_1)){
  r21_cov_plots[[name_1_in]] <-  ggplot() +
    theme_bw(base_size = 14) +
    geom_point(data = r21_dt[low_level == 'adm_2' & name_1 == name_1_in,],
               aes(x = year , y = value,
                   col = variable), size = 2) +
    geom_line(data  = r21_dt[low_level == 'adm_2' & name_1 == name_1_in,],
              aes(x = year , y = value, group = variable)) +
    geom_hline(data = dhs_values[admin_level == 2 & variable == 'dpt3_cov' & SurveyYear == 2016 &
                                 name_1 == name_1_in],
               aes(yintercept = mean)) +
    scale_color_manual(values = c('3-dose series coverage' = colors[3],
                                  'Booster coverage' = colors[4])) +
    theme(legend.position = 'bottom') +
    xlim(2010,max(r21_dt$year)) +
    labs(x = NULL, y = 'R21 coverage', color = 'R21 dose', title = name_1_in) +
    facet_wrap(~name_2) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1)
    )
}

pdf("./r21_cov.pdf", width = 8, height = 8)
unique(r21_cov_plots)
dev.off()


r21_plots_admin2 <- list()
for(name_2_in in districts){
  data <- r21_dt[low_level == 'adm_2' & name_2 == name_2_in,]
  if(nrow(data) == 0){
    data <- data.table(year = 2010:2026,
                       value = 0,
                       variable = NA,
                       name_2 = name_2_in)
  }

  r21_plots_admin2[[name_2_in]] <-  ggplot() +
    theme_bw() +
    geom_point(data = data,
               aes(x = year , y = value,
                   col = variable), size = 2) +
    geom_line(data  = data,
              aes(x = year , y = value, group = variable)) +
    geom_point(data = dhs_values[admin_level == 2 & variable == 'dpt3_cov' &
                                   name_2 == name_2_in],
               aes(x = as.integer(SurveyYear), y = mean), col = 'black', alpha = 0.5, size = 3, shape = 18) +
    scale_color_manual(values = c('3-dose series coverage' = colors[3],
                                  'Booster coverage' = colors[4])) +
    theme(legend.position = 'bottom') +
    xlim(2010,max(r21_dt$year)) +
    labs(x = NULL, y = 'R21 coverage', color = 'R21 dose') +
    facet_wrap(~name_2) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1)
    )

}
# ################################################################################
# IRS ---------------------------------------------------------------------
# ################################################################################
#irs_dt <- readRDS(paste0(getwd(), '/archive/irs/', list.files(paste0(getwd(), '/archive/irs/'))[length(list.files(paste0(getwd(), '/archive/irs/')))], '/irs.RDS'))
#irs_dt <- readRDS(paste0(getwd(), '/draft/irs/', list.files(paste0(getwd(), '/draft/irs/'))[length(list.files(paste0(getwd(), '/draft/irs/')))], '/irs.RDS'))

irs_dt <- readRDS("irs_cov.RDS")
irs_dt[source == 'campaign_data', source := 'NMED']
irs_dt[source == 'MAP estimates', source := 'MAP']

irs_plots <- list()
irs_plots[['Uganda']] <- ggplot() + facet_wrap(~name_1) +
  theme_bw() +
  geom_line(data = irs_dt[low_level == 'adm_1' & source == 'hybrid_input'],
            aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                col = source), size = 1.1, alpha = 0.8, col = 'black') +
  geom_point(data = irs_dt[low_level == 'adm_1' & source != 'hybrid_input'],
            aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                col = source), size = 1.1, alpha = 0.8) +
  geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 & SurveyYear > 2015],
             aes(as.integer(SurveyYear )+0.5, mean), col = "black",
             size = 2, shape = 18) +
  scale_color_manual(values = c('NMED' = colors[1],
                                'MAP' = colors[2])) +
  theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
  xlim(2010,2025) +
  geom_vline(xintercept = 2020) +
  labs(x = NULL, y = 'IRS Coverage', color = 'Comparison data', title = "Uganda",
       caption = 'Diamonds represent MIS IRS coverage')

for(name_1_in in unique(irs_dt$name_1)){
  data <- irs_dt[name_1 == name_1_in & low_level == 'adm_2',]

  irs_plots[[name_1_in]] <-  ggplot() + facet_wrap(~name_2) +
    theme_bw() +
    geom_line(data = data[source == 'hybrid_input'],
              aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                  col = source), size = 1.1, alpha = 0.8, col = 'black') +
    geom_point(data = data[source != 'hybrid_input'],
               aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                   col = source), size = 1.1, alpha = 0.8) +
    geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 2 & SurveyYear > 2015 &
                                   name_1 == name_1_in & !is.na(name_2)],
               aes(as.integer(SurveyYear )+0.5, mean), col = "black",  size = 2, alpha = 0.5,
               shape = 18) +
    scale_color_manual(values = c('NMED' = colors[1],
                                  'MAP' = colors[2])) +
    theme(legend.position = 'bottom', legend.box = 'vertical') + ylim(0,1) +
    xlim(2010,2026) +
    geom_vline(xintercept = 2020) +
    labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = name_1_in,
         caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative')
}

pdf("./irs.pdf", width = 8, height = 8)
unique(irs_plots)
dev.off()

irs_plots_admin2 <- list()
for(name_2_in in districts){
  data <- irs_dt[name_2 == name_2_in & low_level == 'adm_2',]
  data <- unique(data)

  irs_plots_admin2[[name_2_in]] <-  ggplot() + facet_wrap(~name_2) +
    theme_bw() +
    geom_line(data = data[source == 'hybrid_input'],
              aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov),  alpha = 0.8,
              col = 'black') +
    geom_point(data = data[source != 'hybrid_input'],
               aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
                   col = source), size = 2, alpha = 0.8) +
    geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 2 & SurveyYear > 2015 &
                                   name_2 == name_2_in & !is.na(name_2)],
               aes(as.integer(SurveyYear )+0.5, mean), col = "black",  size = 3, alpha = 0.5,
               shape = 18) +
    scale_color_manual(values = c('NMED' = colors[1],
                                  'MAP' = colors[2])) +
    theme(legend.position = 'bottom', legend.box = 'vertical') +
    xlim(2010,2026) +
    geom_vline(xintercept = 2020) +
    labs(x = NULL, y = 'IRS coverage', color = 'Coverage type',
         #caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative',
         lty = 'Insecticide') +
    geom_line(data = data[source == 'hybrid_input' ], aes(x = year, y = 0, lty = insecticide)) +
      scale_y_continuous(       breaks = seq(0, 1, by = 0.25),       limits = c(0, 1)     )

}

# ################################################################################
# ITN ---------------------------------------------------------------------
# ################################################################################
#llin<- readRDS(paste0(getwd(), '/archive/itn/', list.files(paste0(getwd(), '/archive/itn/'))[length(list.files(paste0(getwd(), '/archive/itn/')))], '/llin.RDS'))
llin <- readRDS("llin.RDS")
llin[source == "Programmatic distribution coverage", source := 'NMED']

llin_plot_list <- list()
llin_plot_list[['Uganda']] <- ggplot() +
  geom_hline(yintercept = 1, alpha = 0.8) +
  geom_line(data = llin[level == 'adm_1' & grepl('Modelled usage', source) & year > 2009],
            aes(x = year + (usage_day_of_year - 1)/ 365, y = itn_use)) +
  geom_point(data = dhs_values[variable == 'net_usage'  & SurveyYear %in% 2010:2025 & admin_level == 1 &
                               name_1 %in% unique(llin[level== 'adm_1',name_1])],
             aes(as.integer(SurveyYear), mean), size = 2, alpha = 0.5, shape = 18) +
  geom_point(data = llin[level == 'adm_1' & source == "NMED" & year > 2009],
             aes(year, itn_use), col = colors[1]) +
  facet_wrap(~name_1) +
  labs(x = NULL, y = 'ITN usage',
       caption = 'Diamonds represent DHS/MIS coverage') +
  theme_bw(base_size = 14)

for(name_1_in in unique(llin$name_1)){
  y_max <- max(llin[level == 'adm_2' & name_1 == name_1_in & year > 2009,itn_use])
  map <- data.table(site$interventions$itn$use)
  map <- map[name_1 == name_1_in,.(itn_use = mean(itn_use)), by = c('name_2', 'year')]


  llin_plot_list[[name_1_in]] <- ggplot() +
    geom_hline(yintercept = 1, alpha = 0.8) +
    geom_line(data = llin[level == 'adm_2' & grepl('Modelled usage', source) &
                            name_1 == name_1_in & year > 2009],
              aes(x = year + (usage_day_of_year - 1)/ 365, y = itn_use)) +
    geom_point(data = dhs_values[variable == 'net_usage'  & SurveyYear %in% 2010:2025 & admin_level == 2 &
                                   name_2 %in% unique(llin$name_2) &
                                   name_1 == name_1_in],
               aes(as.integer(SurveyYear), mean), size = 2, alpha = 0.5, shape = 18) +
    geom_point(data = llin[level == 'adm_2' & source == "NMED" & name_1 == name_1_in & year > 2009],
               aes(year + (usage_day_of_year - 1)/ 365, itn_use), col = colors[1]) +
    geom_point(data = map[year > 2009],
               aes(year + 0.5, itn_use), col = colors[2], size = 2, alpha = 0.8) +
    geom_bar(data = llin[level == 'adm_2' &
                           grepl('Modelled usage', source) &
                           year > 2009 &
                           name_1 == name_1_in &
                           !is.na(model_distribution)],
             aes(x = year + (distribution_day_of_year - 1)/365,
                 y = model_distribution, col = distribution_type), stat = 'identity',
             show.legend = F) +
    facet_wrap(~name_2) +
    ylim(0, y_max) +
    labs(x = NULL, y = 'ITN usage',
         title = name_1_in,
         caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative') +
    theme_bw(base_size = 14)
}

pdf("./itn.pdf", width = 8, height = 8)
unique( llin_plot_list)
dev.off()

##TODO: Add back on the original MAP points
itn_plots_admin2 <- list()
for(name_2_in in districts){
  y_max <- max(llin[level == 'adm_2' & name_2 == name_2_in & year > 2009,itn_use])
  map <- data.table(site$interventions$itn$use)
  map <- map[name_2 == name_2_in,.(itn_use = mean(itn_use)), by = c('name_2', 'year')]

  itn_plots_admin2[[name_2_in]] <- ggplot() +
    geom_line(data = llin[level == 'adm_2' & grepl('Modelled usage', source) & name_2 == name_2_in & year > 2009],
              aes(x = year + (usage_day_of_year - 1)/ 365, y = itn_use)) +
    geom_point(data = dhs_values[variable == 'net_usage'  & SurveyYear %in% 2010:2025 & admin_level == 2 &
                                   name_2 == name_2_in],
               aes(as.integer(SurveyYear), mean), size = 3, alpha = 0.5, shape = 18) +
    geom_point(data = llin[level == 'adm_2' & source == "NMED" &
                             name_2 == name_2_in & year > 2009 & itn_use < 1],
               aes(year + (usage_day_of_year - 1)/ 365, itn_use), col = colors[1], size = 2, alpha = 0.8) +
    geom_point(data = llin[level == 'adm_2' & source == "NMED" &
                             name_2 == name_2_in & year > 2009 & itn_use > 1],
               aes(year + (usage_day_of_year - 1)/ 365, y = 1), col = colors[1], size = 2, alpha = 0.8,
               shape = 8) +
    geom_point(data = map[year > 2009],
               aes(year + 0.5, itn_use), col = colors[2], size = 2, alpha = 0.8) +
    geom_bar(data = llin[level == 'adm_2' & grepl('Modelled usage', source) & name_2 == name_2_in & year > 2009 &
                          !is.na(model_distribution)],
             aes(x = year + (distribution_day_of_year - 1)/365,
                 y = model_distribution, col = distribution_type), stat = 'identity',
             show.legend = F) +
    ggrepel::geom_text_repel(data = llin[level == 'adm_2' & source == "NMED" &
                             name_2 == name_2_in & year > 2009 & itn_use > 1],
               aes(year + (usage_day_of_year - 1)/ 365, y = 1, label = paste0(itn_use*100,'%')), col = colors[1], size = 2.5, alpha = 0.8) +
    facet_wrap(~name_2) +
    geom_point(data = data.table(name_2 = name_2_in, year = 2015, itn_use = 0), aes(year, itn_use), col = NA) +
    labs(x = NULL, y = 'ITN usage',
         #caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative\nRed circles represent reported distribution coverage'
         ) +
    theme_bw()+
      scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                         limits = c(0, 1))

}


# District specific plots -------------------------------------------------
get_dist_plots <- function(name_2_in){
  print(name_2_in)

  ## Interventions
  itn <- itn_plots_admin2[[name_2_in]]

  irs <- irs_plots_admin2[[name_2_in]]
  irs <- irs + theme(legend.position = 'none')

  smc <- smc_plots_admin2[[name_2_in]]
  smc <- smc + theme(legend.position = 'none')

  r21 <- r21_plots_admin2[[name_2_in]]
  r21 <- r21 + theme(legend.position = 'none')

  tx_cov <- tx_cov_plots_admin2[[name_2_in]]

  ## Burden
  prev <- prev_plots_admin2[[name_2_in]]

  gridExtra::grid.arrange(
    grobs = c(itn, irs, smc, r21, tx_cov, prev),
    nrow = 5,
    ncol = 2,
    as.table = FALSE
  )
}

pdf("./district_plots.pdf", width = 8, height = 12)

invisible(lapply(districts, get_dist_plots))

dev.off()




