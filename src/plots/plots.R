#setwd('Z:/Maggie/uga_snt/src/plots/')
library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
library(cowplot)
library(geofacet)
#library(wesanderson)

orderly::orderly_shared_resource(
  old_site.RDS = "raw_data/2026_02_18_UGA_SNT_sitefile.rds"
)

orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_calibration",
  query = "latest()",
  files = c(
    aggregated_results.RDS = "aggregated_results.RDS"
  )
)

orderly::orderly_dependency(
  name = "process_raw_data",
  query = "latest()",
  files = c(
    inc_adj_dt.RDS = "formatted_yearly_inc_adj.RDS"
  )
)

orderly::orderly_dependency(
  name = "format_comparison_data",
  query = "latest()",
  files = c(
    comp_data.RDS = "comp_data.RDS"
  )
)

orderly::orderly_dependency(
  name = "resistance",
  query = "latest()",
  files = c(
    insect_resist.RDS = "insect_resist.RDS"
  )
)

##load in data sources
old_site <- readRDS("old_site.RDS")
site <- readRDS("new_site.RDS")
comp_data <- readRDS("comp_data.RDS")
agg_results <- readRDS("aggregated_results.RDS")
yearly_inc_adj <- readRDS("inc_adj_dt.RDS")

#colors <- wesanderson::wes_palette("AsteroidCity1")
colors <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645")

districts <- unique(site$shape$level_2$name_2)
year_min = 2008
loc_map <- unique(data.table(site$sites)[,.(name_1, name_2)])

##Draft args
##site file
##include map prev
include_map_prev <- T
##include calibrated results
##year_min, year_max
year_min = 2008
year_max = 2026
##DHS color representative
dhs_col_rep = colors[3]
##DHS color cluster
dhs_col_cluster = colors[2]
##NMED data
nmed_col = colors[1]
##MAP color
map_col = 'grey'

malaria_plot_theme <- function(name_1, y_lab, caption_in = 'Note: DHS/MIS values are not representative at the district level') {
  list(
    scale_color_manual(values = c('DHS' = dhs_col_rep,
                                  'MAP' = map_col,
                                  'DHS cluster' = dhs_col_cluster,
                                  'NMED' = nmed_col,
                                  'routine' = colors[4],
                                  'mass' = colors[1])),
    scale_shape_manual(values = c('DHS' = 16,
                                  'DHS cluster' = 18,
                                  'NMED' = 16)),
    scale_alpha_manual(values = c('DHS' = 1,
                                  'DHS cluster' = 0.5,
                                  'NMED' = 1)),
    xlim((year_min + 1), year_max),
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)),
    theme_bw(base_size = 14),
    theme(legend.position = 'bottom',
          legend.box = 'vertical'),
    guides(shape = 'none', alpha = 'none'),
    labs(x = NULL, y = y_lab,
         title = name_1,
         col = 'Prevalence test type',
         caption = caption_in)
  )
}

################################################################################
# Prevalence --------------------------------------------------------------
################################################################################
comp_prev <- comp_data$prev
if(!include_map_prev){
  comp_prev <- comp_prev[source != 'MAP']
}

prev_plots <- list()
prev_plots[["Uganda"]] <- ggplot() +
  geom_line(data = comp_prev[low_level == 'adm_1' & source == 'MAP'], aes(ts, mean, col = source)) +
  facet_wrap(~name_1, nrow = 3) +
  geom_line(data = agg_results[low_level == 'adm_1' & variable == 'detect_lm' & year > year_min,],
            aes(year + day_of_year / 365, prev), col = "black") +
  geom_point(data = comp_prev[low_level == 'adm_1' & source == 'DHS'], aes(ts, mean, col = source),
             size = 3) +
  geom_errorbar(data = comp_prev[low_level == 'adm_1' & source == 'DHS'],
                aes(ts, ymin = lower,
                    ymax = upper, col = source), width = 0) +
  scale_color_manual(values = c('DHS' = dhs_col_rep,
                                'MAP' = map_col)) +
  xlim((year_min + 1),year_max) + ylim(0,1) +
  theme_bw(base_size = 14) + theme(legend.position = 'bottom',
                                   legend.box = 'vertical') +
  labs(x = NULL, y = 'Prevalence among children 0-5 years',
       title = 'Uganda',
       col = 'Prevalence test type')

for(name_1_in in unique(loc_map$name_1)){
  fitted_prev <- agg_results[low_level == 'adm_2' & variable == 'detect_lm' & name_1 == name_1_in]

  prev_plots[[name_1_in]] <-   ggplot() +
    geom_line(data = comp_prev[name_1 == name_1_in & source == 'MAP' & low_level == 'adm_2'],
              aes(ts, mean, col = source)) +
    geom_line(data = fitted_prev, aes(year + (day_of_year - 1) /365, prev), col = 'black') +
    facet_wrap(~name_2) +
    geom_point(data = comp_prev[low_level %in% c('adm_1', 'adm_2') & grepl('DHS', source) &
                                  name_1 %in% name_1_in &
                                  name_2 %in% loc_map[name_1 == name_1_in, name_2],],
               aes(ts, mean, col = source, shape = source, alpha = source)) +
    geom_errorbar(data = comp_prev[low_level == 'adm_1' & source == 'DHS' &
                                     name_1 %in% name_1_in &
                                     name_2 %in% loc_map[name_1 == name_1_in, name_2],],
                  aes(ts, ymin = lower,
                      ymax = upper, col = source),
                  width = 0) +
    malaria_plot_theme(name_1 = name_1_in, y_lab = 'Prevalence among children 0-5 years')
}

pdf("prev.pdf", width = 10, height = 8)
prev_plots
dev.off()

prev_plots_admin2 <- list()
for(name_2_in in districts){
  fitted_prev  <- unique(agg_results[name_2 == name_2_in & age_lower == 0 & age_upper == 5 & low_level == 'adm_2' &
                                       variable == 'detect_lm',.(
                                         ts = year + (day_of_year - 1) / 365, name_2, mean = prev,
                                         title = "Prevalence, 0-5y")])

  prev_plots_admin2[[name_2_in]] <- ggplot() +
    geom_line(data = fitted_prev,
              aes(ts, mean)) +
    geom_line(data = comp_prev[source == 'MAP' & name_2 == name_2_in],
              aes(year + 0.5, mean, col = source)) +
    facet_wrap(~title) +
    geom_point(data = comp_prev[grepl('DHS', source) & name_2 == name_2_in],
               aes(year, mean, col = source, alpha = source)) +
    geom_errorbar(data = comp_prev[grepl('DHS', source) & name_2 == name_2_in],
                  aes(year, ymin = lower,
                      ymax = upper,
                      col = source),
                  width = 0) +
    malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption_in = NULL)


}


################################################################################
# CCM ---------------------------------------------------------------------
################################################################################
{
  comp_tx <- comp_data$ccm
  site_tx <- data.table::data.table(site$interventions$treatment$implementation)
  site_tx[,title := 'Treatment coverage']

  # tx_cov_plots <- list()
  # for(name_1_in in unique(tx_cov$name_1)){
  #   tx_cov_plots[[name_1_in]] <- ggplot() +
  #     geom_line(data = comp_tx[source == 'MAP' & name_1 == name_1_in &
  #                                low_level == 'adm_2' &
  #                                year %in% year_min:year_max &
  #                                name_2 %in% loc_map[name_1 == name_1_in, name_2]],
  #               aes(ts, mean, col = source)) +
  #     geom_line(data = comp_tx[source == 'MAP' & name_1 == name_1_in &
  #                                low_level == 'adm_2' &
  #                                year %in% year_min:year_max&
  #                                name_2 %in% loc_map[name_1 == name_1_in, name_2]],
  #               aes(ts, mean*prop_act, col = source), lty = 2) +
  #     geom_line(data = site_tx[name_1 == name_1_in &
  #                                year %in% year_min:year_max &
  #                                name_2 %in% loc_map[name_1 == name_1_in, name_2]],
  #               aes(year, tx_cov), col = 'black') +
  #     facet_wrap(~name_2) +
  #     geom_point(data = comp_tx[source == 'NMED' & name_1 == name_1_in &
  #                                 low_level == 'adm_2' &
  #                                 year %in% year_min:year_max],
  #                aes(ts, mean, col = source), size = 2) +
  #     geom_point(data = comp_tx[grepl('DHS', source) & name_1 == name_1_in &
  #                                 name_2 %in% loc_map[name_1 == name_1_in, name_2]],
  #                aes(ts+0.5, mean, col = source, alpha = source, shape = source)) +
  #     malaria_plot_theme(name_1 = name_1_in, y_lab = 'Treatment coverage')
  #
  # }
  #
  # pdf("tx_cov.pdf", width = 8, height = 8)
  # tx_cov_plots
  # dev.off()

  tx_cov_plots_admin2 <- list()
  for(name_2_in in districts){
    tx_cov_plots_admin2[[name_2_in]] <- ggplot() +
      ##MAP values
      geom_line(data = comp_tx[source == 'MAP' & name_2 == name_2_in],
                aes(year,  mean, col = source)) +
      geom_line(data = comp_tx[source == 'MAP' & name_2 == name_2_in],
                aes(year, mean*prop_act, col = source), lty = 2) +
      ##Site file
      geom_line(data = site_tx[name_2 == name_2_in & year %in% year_min:year_max],
                aes(year, tx_cov), col = 'black') +
      facet_wrap(~title) +
      geom_point(data = comp_tx[source == 'NMED' & name_2 == name_2_in & year %in% year_min:year_max] ,
                 aes(ts, mean, col = source, shape = source, alpha = source),
                 size = 2) +
      malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)
  }


}

################################################################################
# SMC ---------------------------------------------------------------------
################################################################################
comp_smc <- comp_data$smc

site_smc <- data.table::data.table(site$interventions$smc$implementation)
site_smc[,ts := year + (round_day_of_year - 1) / 365]
site_smc[,age_label := paste0(smc_min_age / 365, '-', smc_max_age/365)]
site_smc$age_label <- factor(site_smc$age_label, levels = c('0-4', '5-10', '0-10'))

# smc_cov_plots <- list()
# smc_cov_plots[["Uganda"]] <-  ggplot(site_smc[low_level == 'adm_1',],
#                                      aes(x = ts,
#                                          y = smc_cov,
#                                          col = age_label)) +
#   geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
#   theme(legend.position = 'bottom', legend.box = 'vertical') +
#   geom_hline(yintercept = 1, col = 'grey') +
#   xlim(2020, year_max) +
#   geom_point(data = data.table(name_1 = unique(smc$name_1),
#                                year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
#   labs(x = NULL, y = 'SMC coverage', title = "Uganda", col = 'SMC eligibility') +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))
#
#
# for(name_1_in in unique(site_smc$name_1)){
#   smc_cov_plots[[name_1_in]] <-  ggplot(site_smc[low_level == 'adm_2' & name_1 == name_1_in,],
#                                         aes(x = ts,
#                                             y = smc_cov,
#                                             col = age_label)) +
#     geom_point(size = 2) + facet_wrap(~name_2) + theme_bw(base_size = 14) +
#     theme(legend.position = 'bottom', legend.box = 'vertical') +
#     geom_hline(yintercept = 1, col = 'grey') +
#     xlim(2020, year_max) +
#     geom_point(data = data.table(name_1 = unique(smc$name_1), year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
#     labs(x = NULL, y = 'SMC coverage', title = "Uganda", col = 'SMC eligibility')+
#     scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))
#
# }
#
# pdf("smc_cov.pdf", width = 8, height = 6)
# unique(smc_cov_plots)
# dev.off()

smc_plots_admin2 <- list()
for(name_2_in in districts){
  if(nrow(comp_smc[name_2 == name_2_in,]) == 0){
    data <- data.table(year = (year_min + 1):2026, round_day_of_year = 0, smc_cov = 0, age_label = NA,
                       name_2 = name_2_in, title = 'SMC coverage')
    smc_plots_admin2[[name_2_in]] <- ggplot(data,
                                            aes(x = year + round_day_of_year / 365,
                                                y = smc_cov), col = colors[1]) +
      geom_point(size = 2) + facet_wrap(~title) + theme_bw() +
      theme(legend.position = 'bottom', legend.box = 'vertical') +
      geom_hline(yintercept = 1, col = 'grey') +
      xlim((year_min + 1), 2026) +
      geom_point(data = data.table(name_2 = unique(data$name_2), year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.05))
  }else if(length(unique(comp_smc[name_2 == name_2_in,age_label]) == 1)){
    smc_plots_admin2[[name_2_in]] <- ggplot() +
      geom_line(data = site_smc[name_2 == name_2_in & low_level == 'adm_2'],
                aes(x = ts,
                    y = smc_cov), col = 'black') +
      geom_point(data = comp_smc[name_2 == name_2_in], aes(ts, mean, col = source)) +
      facet_wrap(~title) + theme_bw() +
      theme(legend.position = 'bottom', legend.box = 'vertical') +
      geom_hline(yintercept = 1, col = 'grey') +
      geom_text(data = data.table::data.table(year = 2020),
                aes(year, y = 0), hjust = -1, label = '0-4') +
      malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)
  }else{
    labels = unique(site_smc[name_2 == name_2_in,.(year, age_label)])
    labels[,min_year := min(year), by = c('age_label')]
    labels[,title := 'SMC coverage']


    smc_plots_admin2[[name_2_in]] <- ggplot() +
      geom_line(data = site_smc[name_2 == name_2_in,],
                aes(x = ts,
                    y = smc_cov), col = "black") +
      geom_point(data = comp_smc[name_2 == name_2_in,],
                 aes(x = ts,
                     y = mean, col = source, shape = source)) +
      facet_wrap(~title) +
      geom_hline(yintercept = 1, col = 'grey') +
      geom_vline(data = labels[min_year == max(min_year)], aes(xintercept = year)) +
      geom_text(data = labels[year == min_year,.(year,age_label)],
                aes(year, y = 0, label = age_label), hjust = -1) +
      geom_point(data = data.table(name_2 = name_2_in, year = 2020, smc_cov = 0), aes(year, smc_cov), col = NA) +
      malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)
  }

}


################################################################################
## R21 ---------------------------------------------------------------------
################################################################################
comp_r21 <- comp_data$r21
site_r21 <- data.table::data.table(site$interventions$vaccine$implementation)
site_r21 <- site_r21[r21_vaccine_dose == 'Malaria_vaccine_3',.(country, iso3c, year, name_1, name_2, low_level, r21_primary_cov, r21_booster1_cov)]
site_r21 <- data.table::data.table(reshape2::melt(site_r21, id.vars = c('country', 'iso3c', 'year', 'name_1', 'name_2', 'low_level')))
site_r21[,title := 'R21 coverage']

# r21_cov_plots <- list()
# r21_cov_plots[['Uganda']] <- ggplot() +
#   theme_bw(base_size = 14) +
#   geom_point(data = site_r21[low_level == 'adm_1'],
#              aes(x = year , y = value,
#                  col = variable), size = 2) +
#   geom_line(data  = site_r21[low_level == 'adm_1'],
#             aes(x = year , y = value, group = variable)) +
#   geom_hline(data = comp_r21[source == 'DHS' & year == 2016 & low_level == 'adm_1' & name_1 == name_2],
#              aes(yintercept = mean)) + facet_wrap(~name_1) +
#   malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL) +
#   scale_color_manual(values = c("r21_primary_cov" = colors[1],
#                                 "r21_booster1_cov" = colors[3]))
#
#
# for(name_1_in in unique(r21_dt$name_1)){
#   r21_cov_plots[[name_1_in]] <-  ggplot() +
#     theme_bw(base_size = 14) +
#     geom_point(data = site_r21[low_level == 'adm_2' & name_1 == name_1_in,],
#                aes(x = year , y = value,
#                    col = variable), size = 2) +
#     geom_line(data  = site_r21[low_level == 'adm_2' & name_1 == name_1_in,],
#               aes(x = year , y = value, group = variable)) +
#     # geom_hline(data =  comp_r21[source == 'DHS' & year == 2016 & low_level == 'adm_2' &
#     #                             name_1 == name_2],
#     #            aes(yintercept = mean)) +
#     malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL) +
#     scale_color_manual(values = c("r21_primary_cov" = colors[1],
#                                   "r21_booster1_cov" = colors[3])) +
#     facet_wrap(~name_2)
# }
#
# pdf("r21_cov.pdf", width = 8, height = 8)
# unique(r21_cov_plots)
# dev.off()


r21_plots_admin2 <- list()
for(name_2_in in districts){
  if(nrow(site_r21[name_2 == name_2_in,]) == 0){
    data <- data.table(year = (year_min + 1),
                       value = 0,
                       variable = NA,
                       name_2 = name_2_in,
                       title = 'R21 coverage')
  }

  r21_plots_admin2[[name_2_in]] <-  ggplot() +
    theme_bw() +
    ##site file
    geom_line(data = site_r21[name_2 == name_2_in,],
              aes(x = year , y = value, group = variable)) +
    ##NMED data
    geom_point(data  = comp_r21[name_2 == name_2_in & low_level != 'adm_1' & source == 'NMED'],
               aes(x = year, y = mean,  color = source, shape = source)) +
    geom_text(data = site_r21[name_2 == name_2_in & year == 2025,],
              aes(year - 0.5, y = value, label = variable), hjust = 1) +
    # geom_point(data = dhs,
    #            aes(x = as.integer(SurveyYear), y = mean),
    #            alpha = 0.5, size = 3, shape = 18, color = colors[2]) +
    malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)


}
# ################################################################################
# IRS ---------------------------------------------------------------------
# ################################################################################
comp_irs <- comp_data$irs
site_irs <- data.table::data.table(site$interventions$irs$implementation)
site_irs <- site_irs[,.(country, iso3c, name_1, name_2, year, ts = year + (spray_day_of_year - 1) / 365,
                        mean = irs_cov, title = 'IRS coverage')]

# irs_plots <- list()
# irs_plots[['Uganda']] <- ggplot() + facet_wrap(~name_1) +
#   theme_bw() +
#   geom_line(data = irs_dt[low_level == 'adm_1' & source == 'hybrid_input'],
#             aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
#                 col = source), size = 1.1, alpha = 0.8, col = 'black') +
#   geom_point(data = irs_dt[low_level == 'adm_1' & source != 'hybrid_input'],
#             aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
#                 col = source), size = 1.1, alpha = 0.8) +
#   geom_point(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 &
#                               name_1 %in% loc_map$name_1],
#              aes(as.integer(SurveyYear )+0.5, mean), col = colors[3],
#              alpha = 1)  +
#   geom_errorbar(data = dhs_values[variable == 'irs_coverage' & admin_level == 1 &
#                                     name_1 %in% loc_map$name_1],
#                 aes(as.integer(SurveyYear )+0.5, ymin = lower, ymax = upper),
#                 col = colors[3],
#                 width = 0, alpha = 1) +
#   scale_color_manual(values = c('NMED' = colors[1],
#                                 'MAP' = colors[2],
#                                 'DHS/MIS' = colors[3])) +
#   theme(legend.position = 'bottom', legend.box = 'vertical') + #ylim(0,1) +
#   geom_hline(yintercept = 1, col = 'grey') +
#   xlim((year_min + 1),2025) +
#   geom_vline(xintercept = 2020) +
#   labs(x = NULL, y = 'IRS Coverage', color = 'Comparison data', title = "Uganda")
#
# dhs_irs <- dhs_values[variable == 'irs_coverage' & admin_level == 1,]
# dhs_irs_adm2 <- dhs_values[variable == 'irs_coverage' & admin_level == 2,]
#
# for(name_1_in in unique(irs_dt$name_1)){
#   data <- irs_dt[name_1 == name_1_in & low_level == 'adm_2',]
#
#   irs_plots[[name_1_in]] <-  ggplot() + facet_wrap(~name_2) +
#     theme_bw() +
#     geom_line(data = data[source == 'hybrid_input'],
#               aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
#                   col = source), size = 1.1, alpha = 0.8, col = 'black') +
#     geom_point(data = data[source != 'hybrid_input'],
#                aes(x = year + (spray_day_of_year - 1)/ 365, y = irs_cov,
#                    col = source), size = 1.1, alpha = 0.8) +
#     geom_point(data = dhs_irs_adm2[name_1 == name_1_in & !is.na(name_2) &
#                                    name_2 %in% loc_map[name_1 == name_1_in, name_2]],
#                aes(as.integer(SurveyYear )+0.5, mean),   size = 2, alpha = 0.5,
#                shape = 18, color = "black") +
#     geom_point(data = dhs_irs[name_1 == name_1_in],
#                aes(as.integer(SurveyYear )+0.5, mean), col = colors[3],
#                alpha = 1)  +
#     geom_errorbar(data =  dhs_irs[name_1 == name_1_in],
#                   aes(as.integer(SurveyYear )+0.5, ymin = lower, ymax = upper),
#                   col = colors[3],
#                   width = 0, alpha = 1) +
#     scale_color_manual(values = c('NMED' = colors[1],
#                                   'MAP' = colors[2])) +
#     theme(legend.position = 'bottom', legend.box = 'vertical') + #ylim(0,1) +
#     xlim((year_min + 1),2026) +
#     geom_vline(xintercept = 2020) +
#     labs(x = NULL, y = 'IRS Coverage', color = 'Coverage type', title = name_1_in,
#          caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative')
# }
#
# pdf("irs.pdf", width = 8, height = 8)
# unique(irs_plots)
# dev.off()

irs_plots_admin2 <- list()
for(name_2_in in districts){
  irs_plots_admin2[[name_2_in]] <-  ggplot() +
    facet_wrap(~title) +
    theme_bw() +
    geom_line(data = site_irs[name_2 == name_2_in],
              aes(x = ts, y = mean),  alpha = 0.8,
              col = 'black') +
    geom_point(data = comp_irs[name_2 == name_2_in],
               aes(x = ts, y = mean, alpha = source, col = source, shape = source),
               size = 2) +
    geom_errorbar(data = comp_irs[source == 'DHS' & name_2 == name_2_in],
                  aes(ts, ymin = lower, ymax = upper, col = source, ),
                  width = 0, alpha = 1) +
    malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)

}

#################################################################################
# ITN ---------------------------------------------------------------------
#################################################################################
# llin_plot_list <- list()
# llin_plot_list[['Uganda']] <- ggplot() +
#   geom_hline(yintercept = 1, alpha = 0.8) +
#   geom_line(data = llin[level == 'adm_1' & grepl('Modelled usage', source) & year > year_min],
#             aes(x = year + (usage_day_of_year - 1)/ 365, y = itn_use)) +
#   geom_point(data = dhs_values[variable == 'net_usage'  & SurveyYear %in% (year_min + 1):2025 & admin_level == 1 &
#                                name_1 %in% unique(llin[level== 'adm_1',name_1])],
#              aes(as.integer(SurveyYear), mean), size = 2, alpha = 0.5, shape = 18, color = colors[2]) +
#   geom_point(data = llin[level == 'adm_1' & source == "NMED" & year > year_min],
#              aes(year, itn_use), col = colors[1]) +
#   facet_wrap(~name_1) +
#   labs(x = NULL, y = 'ITN usage',
#        caption = 'Diamonds represent DHS/MIS coverage') +
#   theme_bw(base_size = 14)
#
# for(name_1_in in unique(llin$name_1)){
#   y_max <- max(llin[level == 'adm_2' & name_1 == name_1_in & year > year_min,itn_use])
#   map <- data.table(site$interventions$itn$use)
#   map <- map[name_1 == name_1_in,.(itn_use = mean(itn_use)), by = c('name_2', 'year')]
#
#   llin_plot_list[[name_1_in]] <- ggplot() +
#     geom_hline(yintercept = 1, alpha = 0.8) +
#     geom_line(data = llin[level == 'adm_2' & grepl('Modelled usage', source) &
#                             name_1 == name_1_in & year > year_min],
#               aes(x = year + (usage_day_of_year - 1)/ 365, y = itn_use)) +
#     geom_point(data = dhs_values[variable == 'net_usage'  & SurveyYear %in% year_min:2025 & admin_level == 2 &
#                                    name_2 %in% unique(map$name_2)],
#                aes(as.integer(SurveyYear), mean), size = 2, alpha = 0.5, shape = 18, color = colors[2]) +
#     geom_point(data = llin[level == 'adm_2' & source == "NMED" &
#                              name_1 == name_1_in & year > year_min & itn_use < 1],
#                aes(year + (usage_day_of_year - 1)/ 365, itn_use), col = colors[1], size = 2, alpha = 0.8) +
#     geom_point(data = llin[level == 'adm_2' & source == "NMED" &
#                              name_1 == name_1_in & year > year_min & itn_use > 1],
#                aes(year + (usage_day_of_year - 1)/ 365, y = 1), col = colors[1], size = 2, alpha = 0.8,
#                shape = 8) +
#     ggrepel::geom_text_repel(data = llin[level == 'adm_2' & source == "NMED" &
#                                            name_1 == name_1_in & year > year_min & itn_use > 1],
#                              aes(year + (usage_day_of_year - 1)/ 365, y = 1,
#                                  label = paste0(itn_use*100,'%')), col = colors[1], size = 2.5, alpha = 0.8) +
#     geom_point(data = map[year > year_min],
#                aes(year + 0.5, itn_use), col = colors[2], size = 2, alpha = 0.8) +
#     geom_bar(data = llin[level == 'adm_2' &
#                            grepl('Modelled usage', source) &
#                            year > year_min &
#                            name_1 == name_1_in &
#                            !is.na(model_distribution)],
#              aes(x = year + (distribution_day_of_year - 1)/365,
#                  y = model_distribution, col = distribution_type), stat = 'identity',
#              show.legend = F) +
#     scale_color_manual(values = c('rt' = colors[3],
#                                   'mda' = colors[1])) +
#     facet_wrap(~name_2) +
#     labs(x = NULL, y = 'ITN usage',
#          title = name_1_in,
#          caption = 'Diamonds represent cluster-level DHS/MIS coverage and are NOT representative') +
#     theme_bw(base_size = 14)+
#     scale_y_continuous(breaks = seq(0, 1, by = 0.25),
#                        limits = c(0, 1))
# }
#
# pdf("itn.pdf", width = 8, height = 8)
# unique( llin_plot_list)
# dev.off()

site_llin_dist <- data.table::data.table(site$interventions$itn$implementation)
site_llin_dist[,ts := year + (distribution_day_of_year - 1) / 365]
site_llin_dist[,title := 'ITN usage']

site_llin_use <- data.table::data.table(site$interventions$itn$use)
site_llin_use[,ts := year + (usage_day_of_year - 1) / 365]
site_llin_use[,title := 'ITN usage']

comp_llin <- comp_data$itn

itn_plots_admin2 <- list()
for(name_2_in in districts){
  y_max <- max(site_llin_use[name_2 == name_2_in & year > year_min,itn_use])

  itn_plots_admin2[[name_2_in]] <- ggplot() +
    geom_line(data = site_llin_use[name_2 == name_2_in,],
              aes(x = ts, y = itn_use)) +
    facet_wrap(~title) +
    geom_point(data = comp_llin[name_2 == name_2_in & mean < 1 & !grepl('MAP', source),],
               aes(ts, mean, alpha = source, col = source, shape = source)) +
    geom_errorbar(data = comp_llin[name_2 == name_2_in  & low_level == 'adm_1' & mean < 1 & grepl('DHS', source),],
                  aes(ts, ymin = lower, ymax = upper, col = source), width = 0) +
    geom_point(data = comp_llin[name_2 == name_2_in & mean > 1 & !grepl('MAP', source)],
               aes(ts, y = 1, col = source), size = 2, alpha = 0.8, shape = 8) +
    ggrepel::geom_text_repel(data = comp_llin[name_2 == name_2_in & mean > 1 & !grepl('MAP', source)],
                             aes(ts, y = 1, label = paste0(mean*100,'%'), col = source), size = 2.5) +
    geom_line(data = comp_llin[source == 'MAP' & name_2 == name_2_in],
              aes(ts, mean, col = source)) +
    geom_bar(data = site_llin_dist[name_2 == name_2_in,],
             aes(x = ts,
                 y = itn_input_dist, col = distribution_type), stat = 'identity',
             show.legend = F) +
    malaria_plot_theme(name_1 = NULL, y_lab = NULL, caption = NULL)


}

#################################################################################
# Insecticide resistance --------------------------------------------------
#################################################################################
resist <- readRDS("insect_resist.RDS")
resist_plots <- list()
resist_plots[['Uganda']] <- ggplot(data = resist[source == 'New fitted curves'],
                                   aes(year, pyrethroid_resistance)) +
  geom_line(aes(lty = source), show.legend = F) +
  geom_point(data = resist[source == 'NMED'],
             aes(year, y = pyrethroid_resistance), col = colors[1], alpha = 0.4) +
  facet_wrap(~name_1) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = 'Pyrethroid resistance')

resist[,title := 'Pyrethroid resistance']
resist_plots_adm2 <- list()
for(name_2_in in unique(resist$name_2)){
  years = (year_min+1):2026
  resist_plots_adm2[[name_2_in]] <- ggplot(data = resist[source == 'New fitted curves' &
                                                           name_2 == name_2_in &
                                                           year %in% years],
                                           aes(year, pyrethroid_resistance)) +
    geom_line(aes(lty = source), show.legend = F) +
    geom_point(data = resist[source == 'NMED' & name_1 == loc_map[name_2 == name_2_in, name_1] &
                               year %in% years],
               aes(year, y = pyrethroid_resistance), col = colors[1], alpha = 0.4) +
    facet_wrap(~title) +
    theme_bw() +
    labs(x = NULL, y = NULL)+
    scale_y_continuous( breaks = seq(0, 1, by = 0.25),       limits = c(0, 1)     )
}

#################################################################################
# Cases -------------------------------------------------------------------
#################################################################################
temp_agg <- agg_results[variable == 'inc_clinical' & low_level == 'adm_2',]
temp_agg <- temp_agg[,.(value = sum(value)), by = c('name_1', 'name_2', 'year')]
yearly_inc_adj <- yearly_inc_adj[variable == 'N2' & low_level == 'adm_2',]
yearly_inc_adj <- yearly_inc_adj[,.(N2 = sum(value)), by = c('country', 'iso3c', 'name_1',
                                                             'name_2', 'year')]

cases <- merge(temp_agg, yearly_inc_adj, by = c('name_1', 'name_2', 'year'), all.x = T)
cases <- data.table(reshape2::melt(cases, id.vars = c('name_1', 'name_2', 'year', 'country', 'iso3c')))

list_plots <- list()
for(i in 1:length(unique(cases$name_1))){
  list_plots[[i]] <- ggplot() + geom_line(data = cases[name_1 == unique(cases$name_1)[i]],
                                          aes(year, value, col = variable)) +
    facet_wrap(~name_2, scales = 'free') + labs(title = unique(cases$name_1)[i])
}
list_plots

cases_plots_adm2 <- list()
cases[,title := 'Clinical cases']
for(name_2_in in unique(cases$name_2)){
  cases_plots_adm2[[name_2_in]] <- ggplot() +
    geom_line(data = cases[name_2 == name_2_in & variable == 'value'],
              aes(year, value),
              col = 'black',
              show.legend = F) +
    geom_point(data = cases[name_2 == name_2_in & variable == 'N2'],
               aes(year, value),
               color = colors[1],
               show.legend = F, size  = 2) +
    geom_point(data= data.table(year = 2000, value =0), aes(year, value), col = NA) +
    facet_wrap(~title, scales = 'free') + theme_bw() + labs(x =NULL, y = NULL)
}

#################################################################################
# Get plot extras ---------------------------------------------------------
#################################################################################
data <- data.table(expand.grid(list(name_2 = unique(loc_map$name_2),
                                    info = c('Malariasimulation',
                                             'MAP',
                                             'NMED',
                                             'DHS/MIS: cluster-level',
                                             'DHS/MIS: sub-regional'))))
data <- merge(data, loc_map, by = 'name_2')

plot_extra_adm2 <- list()
for(name_2_in in unique(loc_map$name_2)){
  dt <- data[name_2 == name_2_in]

  plot  <- ggplot() + geom_point(data = dt, aes(col = info, x = 0, y = 0)) +
    geom_line(data = dt, aes(col = info, x = 0, y = 0)) +
    scale_color_manual(values = c('Malariasimulation' = 'black',
                                  'MAP' = 'grey',
                                  'NMED' = colors[1],
                                  'DHS/MIS: cluster-level' = colors[2],
                                  'DHS/MIS: sub-regional' = colors[3]))  +
    theme_bw() + labs(title = paste0(dt$name_2, ' district, ', dt$name_1, ' sub-region'),
                      color = NULL) +
    theme(legend.position = 'bottom')
  title_text <- plot$labels$title
  title_grob <- grid::textGrob(
    title_text,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
  plot_extra_adm2[[name_2_in]] <- title_grob
}

dt <- data[name_2 == 'Abim']
plot  <- ggplot() + geom_point(data = dt, aes(col = info, x = 0, y = 0)) +
  geom_line(data = dt, aes(col = info, x = 0, y = 0, group = info)) +
  scale_color_manual(values = c('Malariasimulation' = 'black',
                                'MAP' = 'grey',
                                'NMED' = colors[1],
                                'DHS/MIS: cluster-level' = colors[2],
                                'DHS/MIS: sub-regional' = colors[3]),
                     drop = FALSE   )  +
  theme_bw() + labs(title = paste0(dt$name_2, ' district, ', dt$name_1, ' sub-region'),
                    color = NULL) +
  theme(legend.position = 'bottom')
legend <- ggpubr::get_legend(plot)

#################################################################################
# District specific plots -------------------------------------------------
#################################################################################
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

  resist = resist_plots_adm2[[name_2_in]]


  int1 <-   gridExtra::arrangeGrob(
    grobs = list(itn, irs, resist),
    nrow = 1,
    ncol = 3,
    as.table = FALSE
  )

  int2 <-   gridExtra::arrangeGrob(
    grobs = list(smc, r21, tx_cov),
    nrow = 1,
    ncol = 3,
    as.table = FALSE
  )

  ## Burden
  prev <- prev_plots_admin2[[name_2_in]]
  prev <- prev + theme(legend.position = 'none')

  cases = cases_plots_adm2[[name_2_in]]


  outcomes <- gridExtra::arrangeGrob(
    grobs = list(prev, cases),
    nrow = 1,
    ncol = 2,
    as.table = FALSE
  )

  # Extract title grob
  title_grob = plot_extra_adm2[[name_2_in]]

  gridExtra::grid.arrange(
    grobs = list(title_grob, int1, int2, outcomes, legend),
    nrow = 5,
    ncol = 1,
    heights = unit(c(0.2, 1, 1, 2, 0.2), "null"),  # equal height rows
    as.table = FALSE
  )
}

dist_order <- loc_map[order(name_1),name_2]
pdf("./district_plots.pdf", width = 11, height = 10)
invisible(lapply(dist_order, get_dist_plots))
dev.off()








