#setwd('Z:/Maggie/uga_snt/src/plot_scenario_inputs/')
library(ggplot2)
library(tidyverse)
library(data.table)
library(gridExtra)
library(cowplot)

orderly2::orderly_shared_resource(
  plot_utils.R = "utils/plot_utils.R"
)

orderly::orderly_dependency(
  name = "gen_scenario_site_files",
  query = "latest()",
  files = c(
    site_no.RDS     = 'site_no.RDS',
    site_nomass.RDS = 'site_nomass.RDS',
    site_bau.RDS    = 'site_bau.RDS',
    site_high.RDS   = 'site_high.RDS'
  )
)
orderly::orderly_dependency(
  name = "format_comparison_data",
  query = "latest()",
  files = c(
    comp_data.RDS = "comp_data.RDS"
  )
)

site_list <- list(
  low    = readRDS("site_no.RDS"),
  nomass = readRDS("site_nomass.RDS"),
  bau    = readRDS("site_bau.RDS"),
  high   = readRDS("site_high.RDS")
)
comp_data <- readRDS("comp_data.RDS")
source("plot_utils.R")

# ── Helper: extract and stack an intervention table across site files ──────────
stack_sites <- function(site_list, site_names, extract_fn) {
  rbindlist(
    mapply(function(site, nm) {
      dt <- extract_fn(site)
      dt[, site_file := nm]
      dt
    }, site_list, site_names, SIMPLIFY = FALSE),
    fill = TRUE
  )
}

# ── Global aesthetics ──────────────────────────────────────────────────────────
colors <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645")

year_min        <- 2008
year_max        <- 2035
dhs_col_rep     <- colors[3]
dhs_col_cluster <- colors[2]
nmed_col        <- colors[1]
map_col         <- "grey"

malaria_plot_theme <- function(name_1 = NULL, y_lab = NULL, caption_in = NULL) {
  list(
    scale_color_manual(values = c(
      "DHS"         = dhs_col_rep,
      "MAP"         = map_col,
      "DHS cluster" = dhs_col_cluster,
      "NMED"        = nmed_col,
      "routine"     = colors[4],
      "mass"        = colors[1]
    )),
    scale_shape_manual(values = c("DHS" = 16, "DHS cluster" = 18, "NMED" = 16)),
    scale_alpha_manual(values = c("DHS" = 1, "DHS cluster" = 0.5, "NMED" = 1)),
    xlim((year_min + 1), year_max),
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)),
    theme_bw(base_size = 12),
    theme(legend.position = "none"),
    guides(shape = "none", alpha = "none"),
    labs(x = NULL, y = y_lab, title = name_1, caption = caption_in)
  )
}



# ── ITN plot — faceted by site_file so bars are separated per scenario ─────────
make_itn_plot <- function(name_2_in, site_list, site_names, comp_itn) {
  site_itn_use <- stack_sites(site_list, site_names, function(s) {
    dt <- data.table(s$itn$use)
    dt[, ts    := year + (usage_day_of_year - 1) / 365]
    dt[, title := "ITN usage"]
    dt
  })
  site_itn_use[, site_file := factor(site_file, levels = site_names)]

  site_itn_dist <- stack_sites(site_list, site_names, function(s) {
    dt <- data.table(s$itn$implementation)
    dt[, ts    := year + (distribution_day_of_year - 1) / 365]
    dt[, title := "ITN usage"]
    dt
  })
  site_itn_dist[, site_file := factor(site_file, levels = site_names)]

  # Linetype values to match the other intervention plots
  lty_values <- c("bau" = "solid", "low" = "dashed", "nomass" = "longdash", "high" = "dotted")

  ggplot() +
    # Historical usage (up to 2026) — solid black, same across scenarios
    geom_line(data = site_itn_use[name_2 == name_2_in & year <= 2026],
              aes(ts, itn_use), col = "black") +
    # Projected usage (2026 onward) — linetype varies by scenario
    geom_line(data = site_itn_use[name_2 == name_2_in & year >= 2026],
              aes(ts, itn_use, lty = site_file, group = site_file), col = "black") +
    scale_linetype_manual(values = lty_values) +
    # Distribution bars — filled by type, separated per scenario facet
    geom_bar(data = site_itn_dist[name_2 == name_2_in],
             aes(ts, itn_input_dist, fill = distribution_type),
             stat = "identity", alpha = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = c("routine" = colors[4], "mass" = colors[1])) +
    # MAP comparison line (repeated across facets for reference)
    geom_line(data = comp_itn[source == "MAP" & name_2 == name_2_in],
              aes(ts, mean, col = source)) +
    # Observed points
    geom_point(data = comp_itn[name_2 == name_2_in & mean < 1 & !grepl("MAP", source)],
               aes(ts, mean, col = source, shape = source, alpha = source)) +
    geom_errorbar(data = comp_itn[name_2 == name_2_in & low_level == "adm_1" &
                                    mean < 1 & grepl("DHS", source)],
                  aes(ts, ymin = lower, ymax = upper, col = source), width = 0) +
    geom_point(data = comp_itn[name_2 == name_2_in & mean > 1 & !grepl("MAP", source)],
               aes(ts, y = 1, col = source), size = 2, alpha = 0.8, shape = 8) +
    ggrepel::geom_text_repel(
      data = comp_itn[name_2 == name_2_in & mean > 1 & !grepl("MAP", source)],
      aes(ts, y = 1, label = paste0(mean * 100, "%"), col = source), size = 2.5) +
    # Facet by scenario
    facet_wrap(~site_file, nrow = 1) +
    malaria_plot_theme(y_lab = "ITN usage")
}

# ── Treatment plot ─────────────────────────────────────────────────────────────
make_tx_plot <- function(name_2_in, site_list, site_names, comp_tx) {
  site_tx <- stack_sites(site_list, site_names, function(s) {
    data.table(s$treatment$implementation)
  })
  site_tx[, title := "Treatment coverage"]

  ggplot() +
    geom_line(data = comp_tx[source == "MAP" & name_2 == name_2_in],
              aes(year, mean, col = source)) +
    geom_line(data = comp_tx[source == "MAP" & name_2 == name_2_in],
              aes(year, mean * prop_act, col = source), lty = 2) +
    geom_line(data = site_tx[name_2 == name_2_in & year %in% year_min:year_max],
              aes(year, tx_cov, lty = site_file, group = site_file), col = "black") +
    geom_point(data = comp_tx[source == "NMED" & name_2 == name_2_in & year %in% year_min:year_max],
               aes(ts, mean, col = source, shape = source, alpha = source), size = 2) +
    facet_wrap(~title) +
    malaria_plot_theme()
}

# ── SMC plot ───────────────────────────────────────────────────────────────────
make_smc_plot <- function(name_2_in, site_list, site_names, comp_smc) {
  site_smc <- stack_sites(site_list, site_names, function(s) {
    data.table(s$smc$implementation)
  })
  site_smc[, ts        := year + (round_day_of_year - 1) / 365]
  site_smc[, age_label := paste0(smc_min_age / 365, "-", smc_max_age / 365)]
  site_smc[, title     := "SMC coverage"]
  site_smc$age_label <- factor(site_smc$age_label, levels = c("0-4", "5-10", "0-10"))

  dist_smc <- comp_smc[name_2 == name_2_in]

  if (length(unique(site_smc$age_label)) == 1) {
    ggplot() +
      geom_line(data = site_smc[name_2 == name_2_in & low_level == "adm_2"],
                aes(ts, smc_cov, lty = site_file, group = site_file), col = "black") +
      geom_point(data = dist_smc, aes(ts, mean, col = source)) +
      facet_wrap(~title) +
      geom_text(data = data.table(year = year_min + 1),
                aes(year, y = 0), hjust = -1, label = "0-4") +
      malaria_plot_theme()
  } else {
    labels <- unique(site_smc[name_2 == name_2_in, .(year, age_label)])
    labels[, min_year := min(year), by = "age_label"]
    labels[, title    := "SMC coverage"]

    ggplot() +
      geom_line(data = site_smc[name_2 == name_2_in],
                aes(ts, smc_cov, lty = site_file, group = site_file), col = "black") +
      geom_point(data = dist_smc, aes(ts, mean, col = source, shape = source)) +
      facet_wrap(~title) +
      geom_vline(data = labels[min_year == max(min_year)][year == max(year)], aes(xintercept = year)) +
      geom_text(data = labels[year == min_year, .(year, age_label)],
                aes(year, y = 0, label = age_label), hjust = -1) +
      malaria_plot_theme()
  }
}

# ── R21 plot ───────────────────────────────────────────────────────────────────
make_r21_plot <- function(name_2_in, site_list, site_names, comp_r21) {
  site_r21 <- stack_sites(site_list, site_names, function(s) {
    dt <- data.table(s$vaccine$implementation)
    if (!any(colnames(dt) == "low_level")) dt[, low_level := "adm_2"]
    dt <- dt[r21_vaccine_dose == "Malaria_vaccine_3",
             .(country, iso3c, year, name_1, name_2, low_level,
               r21_primary_cov, r21_booster1_cov)]
    data.table(reshape2::melt(dt,
                              id.vars = c("country", "iso3c", "year", "name_1", "name_2", "low_level")))
  })
  site_r21[, title := "R21 coverage"]

  if (nrow(site_r21[name_2 == name_2_in]) == 0) {
    site_r21 <- data.table(year = year_min + 1, value = 0, variable = NA,
                           name_2 = name_2_in, title = "R21 coverage",
                           site_file = site_names[1])
  }

  ggplot() +
    geom_line(data = site_r21[name_2 == name_2_in],
              aes(year, value, group = interaction(variable, site_file), lty = site_file)) +
    geom_point(data = comp_r21[name_2 == name_2_in & low_level != "adm_1" & source == "NMED"],
               aes(year, mean, col = source, shape = source)) +
    geom_text(data = site_r21[name_2 == name_2_in,
                              .SD[year == min(year)], by = .(variable, site_file)],
              aes(year - 0.3, value, label = variable), hjust = 1, size = 3) +
    facet_wrap(~title) +
    malaria_plot_theme()
}

# ── IRS plot ───────────────────────────────────────────────────────────────────
make_irs_plot <- function(name_2_in, site_list, site_names, comp_irs) {
  site_irs <- stack_sites(site_list, site_names, function(s) {
    dt <- data.table(s$irs$implementation)
    dt[, .(country, iso3c, name_1, name_2, year,
           ts    = year + (spray_day_of_year - 1) / 365,
           mean  = irs_cov,
           title = "IRS coverage")]
  })

  ggplot() +
    facet_wrap(~title) +
    theme_bw(base_size = 12) +
    geom_line(data = site_irs[name_2 == name_2_in],
              aes(ts, mean, lty = site_file, group = site_file), col = "black", alpha = 0.8) +
    geom_point(data = comp_irs[name_2 == name_2_in],
               aes(ts, mean, alpha = source, col = source, shape = source), size = 2) +
    geom_errorbar(data = comp_irs[source == "DHS" & name_2 == name_2_in],
                  aes(ts, ymin = lower, ymax = upper, col = source), width = 0) +
    malaria_plot_theme()
}

# ── Shared legend: colour (data sources) + linetype (scenario) ────────────────
make_shared_legend <- function(site_names) {

  # Colour legend — data sources
  col_data <- data.table(
    source = c("Malariasimulation", "MAP", "NMED", "DHS/MIS: cluster-level", "DHS/MIS: sub-regional"),
    x = 0, y = 0
  )
  col_plot <- ggplot(col_data) +
    geom_line(aes(x, y, col = source, group = source)) +
    geom_point(aes(x, y, col = source)) +
    scale_color_manual(
      values = c(
        "Malariasimulation"      = "black",
        "MAP"                    = "grey",
        "NMED"                   = colors[1],
        "DHS/MIS: cluster-level" = colors[2],
        "DHS/MIS: sub-regional"  = colors[3]
      ),
      drop = FALSE,
      name = "Data source"
    ) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text  = element_text(size = 10))

  # Linetype legend — one entry per scenario, in the order supplied
  lty_values <- c("bau" = "solid", "low" = "dashed", "nomass" = "longdash", "high" = "dotted")
  lty_data <- data.table(
    scenario = factor(site_names, levels = site_names),
    x = 0, y = 0
  )
  lty_plot <- ggplot(lty_data) +
    geom_line(aes(x, y, lty = scenario, group = scenario)) +
    scale_linetype_manual(values = lty_values, name = "Scenario") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10, face = "bold"),
          legend.text  = element_text(size = 10))

  col_grob <- ggpubr::get_legend(col_plot)
  lty_grob <- ggpubr::get_legend(lty_plot)

  gridExtra::arrangeGrob(col_grob, lty_grob, ncol = 1)
}

# ── Assemble one district page ────────────────────────────────────────────────
#
# Layout (4 rows):
#   Row 1 — title
#   Row 2 — ITN faceted by scenario (full width, n_scenario panels)
#   Row 3 — IRS | SMC | R21 | Treatment (single panel each)
#   Row 4 — shared colour + linetype legend

plot_adm2_inputs <- function(name_2_in,
                             site_list,
                             site_names = names(site_list),
                             comp_data,
                             loc_map,
                             legend) {
  message("Plotting: ", name_2_in)

  strip <- function(p) p + theme(legend.position = "none")

  # nomass is an ITN-only scenario — exclude from all other intervention plots
  itn_names  <- site_names
  other_names <- site_names[site_names != "nomass"]
  other_sites <- site_list[other_names]

  itn_p <- make_itn_plot(name_2_in, site_list,   itn_names,   comp_data$itn) |> strip()
  irs_p <- make_irs_plot(name_2_in, other_sites, other_names, comp_data$irs) |> strip()
  smc_p <- make_smc_plot(name_2_in, other_sites, other_names, comp_data$smc) |> strip()
  r21_p <- make_r21_plot(name_2_in, other_sites, other_names, comp_data$r21) |> strip()
  tx_p  <- make_tx_plot( name_2_in, other_sites, other_names, comp_data$ccm) |> strip()

  itn_row <- gridExtra::arrangeGrob(itn_p, nrow = 1)
  int_row <- gridExtra::arrangeGrob(irs_p, smc_p, r21_p, tx_p, nrow = 1)

  name_1_in  <- loc_map[name_2 == name_2_in, name_1]
  title_text <- paste0(name_2_in, " district, ", name_1_in, " sub-region")
  title_grob <- grid::textGrob(title_text,
                               gp = grid::gpar(fontsize = 14, fontface = "bold"))

  gridExtra::grid.arrange(
    grobs   = list(title_grob, itn_row, int_row, legend),
    nrow    = 4,
    heights = unit(c(0.15, 1, 1, 0.25), "null")
  )
}

# ── Top-level runner ───────────────────────────────────────────────────────────
run_district_plots <- function(site_list,
                               comp_data,
                               output_path = "district_plots.pdf",
                               site_names  = names(site_list),
                               districts   = NULL) {

  loc_map <- unique(data.table(site_list[[1]]$treatment$implementation)[, .(name_1, name_2)])

  if (is.null(districts)) {
    districts <- loc_map[order(name_1), name_2]
  }

  legend <- make_shared_legend(site_names)

  pdf(output_path, width = 14, height = 10)
  on.exit(dev.off())
  invisible(lapply(districts, function(d) {
    tryCatch({
     # grid::grid.newpage()
      plot_adm2_inputs(
        name_2_in  = d,
        site_list  = site_list,
        site_names = site_names,
        comp_data  = comp_data,
        loc_map    = loc_map,
        legend     = legend
      )
    }, error = function(e) {
      # Draw a blank error page so the PDF device stays valid
      grid::grid.newpage()
      grid::grid.text(paste0("Error in district: ", d, "\n", conditionMessage(e)),
                      gp = grid::gpar(fontsize = 12, col = "red"))
      message("Error in district ", d, ": ", conditionMessage(e))
    })
  }))

  message("Saved: ", output_path)
}

run_district_plots(site_list, comp_data, output_path = paste0(getwd(), "/district_plots.pdf"))
