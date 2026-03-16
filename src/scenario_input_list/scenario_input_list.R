#setwd('Z:/Maggie/uga_snt/src/scenario_input_list/')
library(tidyverse)
library(ggplot2)
library(data.table)

orderly2::orderly_resource(
  files = "scenario_input_list_utils.R"
)

orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)
site <- readRDS('new_site.RDS')

intervention_list <- list()
for(name in c('treatment', 'itn', 'irs', 'smc', 'vaccine')){
  dt <- site$interventions[[name]]$implementation %>%data.table::data.table()
  dt[,intervention := name]
  intervention_list[[name]] <- dt
}

loc_map <- unique(data.table(site$sites)[,.(name_1, name_2)])

# Helper: compute ITN usage from a distribution table for a single district
net_input_dist <- function(adm2, dist_list){
  dist_list <- data.table::data.table(dist_list)
  dist_dt.adm2 <- dist_list[name_2 == adm2,]
  dist_dt.adm2[, distribution_timesteps := (year - 2000) * 365 + distribution_day_of_year]

  model_usage <- netz::model_distribution_to_usage(
    usage_timesteps      = 1:(365 * length(2000:2037)),
    distribution         = dist_dt.adm2$itn_input_dist,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life            = netz::get_halflife('UGA'),
    net_loss_function    = netz::net_loss_map
  )

  use_dt.adm2 <-data.table::data.table(country = 'Uganda', iso3c = 'UGA',
                            name_1 = loc_map[name_2 == adm2, name_1], name_2 = adm2,
                            year = rep(2000:2037, each = 365),
                            usage_day_of_year = rep(1:365, length(2000:2037)),
                            usage_timesteps =  1:(365 * length(2000:2037)),
                            itn_use = model_usage)

  model_distribution <- netz::usage_to_model_distribution(
    usage                  = use_dt.adm2$itn_use,
    usage_timesteps        = use_dt.adm2$usage_timesteps,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life              = netz::get_halflife('UGA'),
    distribution_lower     = dist_dt.adm2$distribution_lower,
    distribution_upper     = dist_dt.adm2$distribution_upper,
    net_loss_function      = netz::net_loss_map
  )
  dist_dt.adm2[, modelled_dist := model_distribution]



  print(adm2)
  return(list(dist = dist_dt.adm2, use = use_dt.adm2))

}

# ITN high: infer distribution required to achieve 80% usage via netz --------
get_dist_and_usage <- function(adm2){
  use_dt.adm2  <- use_dt[name_2 == adm2, ]
  dist_dt.adm2 <- dist_dt[name_2 == adm2 &
                            distribution_timesteps < max(use_dt.adm2$usage_timesteps), ]

  model_distribution <- netz::usage_to_model_distribution(
    usage                  = use_dt.adm2$itn_use,
    usage_timesteps        = use_dt.adm2$usage_timesteps,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life              = netz::get_halflife('UGA'),
    distribution_lower     = dist_dt.adm2$distribution_lower,
    distribution_upper     = dist_dt.adm2$distribution_upper,
    net_loss_function      = netz::net_loss_map
  )
  dist_dt.adm2[, modelled_dist := model_distribution]

  model_usage <- netz::model_distribution_to_usage(
    usage_timesteps      = 1:(365 * length(2000:2035)),
    distribution         = dist_dt.adm2$modelled_dist,
    distribution_timesteps = dist_dt.adm2$distribution_timesteps,
    half_life            = netz::get_halflife('UGA'),
    net_loss_function    = netz::net_loss_map
  )

  use_dt.adm2 <-data.table::data.table(country = 'Uganda', iso3c = 'UGA',
                            name_1 = loc_map[name_2 == adm2, name_1], name_2 = adm2,
                            year = rep(2000:2035, each = 365),
                            usage_day_of_year = rep(1:365, length(2000:2035)),
                            modelled_usage = model_usage)
  print(adm2)
  return(list(dist = dist_dt.adm2, use = use_dt.adm2))
}

# Business as usual -------------------------------------------------------
# Future years (2026-2035) repeat the 2023-2025 cycle.
# SMC: 2025 average coverage carried forward.
# ITN: mass campaigns replicated on the same 3-year cadence.

copy_coverage <- function(int, years = 2026:2035){
  int <-data.table::data.table(ensure_all_years(int, loc_map))
  int <- unique(int)

  int_columns <- intersect(colnames(int),
                           c("tx_cov", "prop_act", "itn_input_dist", "distribution_upper",
                             "distribution_lower", "irs_cov", "smc_cov",
                             "pmc_cov", "r21_primary_cov", "r21_booster_cov",
                             "r21_booster1_cov", "lsm_cov"))

  # ── ITN: replicate 2023-2025 distributions in 3-year blocks ────────────────
  if ("itn_input_dist" %in% int_columns) {
    int <- int[year < 2026, ]
    copy <- int[year %in% 2023:2025, ]
    for (new_year in c(3, 6, 9, 12)) {
      x <- copy(copy)
      x[, year := year + new_year]
      int <- rbind(int, x)
    }
    int <- int[order(name_2, year, distribution_day_of_year), ]
    # int[year > 2025 & distribution_type == 'routine',
    #     distribution_upper := distribution_upper / 3]
    return(data.frame(int))
  }

  # ── SMC: carry forward average 2025 coverage ───────────────────────────────
  if ('smc_cov' %in% int_columns) {
    val_2025 <- int[year == 2025, .(smc_cov = mean(smc_cov, na.rm = TRUE)),
                    by = .(name_2, smc_min_age, smc_max_age)]

    future <- int[year > 2025, .(name_1, name_2, year, round, round_day_of_year,
                                 low_level, intervention)]
    future <- merge(future, val_2025, by = 'name_2', allow.cartesian = TRUE)
    future[, country    := 'Uganda']
    future[, iso3c      := 'UGA']
    future[, urban_rural := NA]
    future[, peak_season := NA]

    int <- rbind(int[year <= 2025, ], future, fill = TRUE)
    int[is.na(smc_cov), smc_cov := 0]
    return(data.frame(int))
  }

  if("r21_primary_cov" %in% int_columns){
    need <- do.call(rbind, lapply(years, function(yr) {
      y <- unique(int[, .(country, iso3c, name_1, name_2)])
      y[, year := yr]
      y
    }))

    val_2025 <- int[year == 2025, ]
    val_2026 <- int[year == 2026, ]
    val_2025[, year := NULL]
    val_2026[, year := NULL]
    val_2025[,r21_booster1_cov := NULL]
    val <- merge(val_2025, unique(val_2026[,.(name_2, low_level, r21_booster1_cov)]), by = c('name_2', 'low_level'))

    need <- merge(val, need,
                  by = intersect(colnames(val_2025), colnames(need)),
                  all = TRUE, allow.cartesian = TRUE)

    int <- rbind(int[year < 2026], need)
    return(data.frame(int))
  }


  if("irs_cov" %in% int_columns){
    need <- do.call(rbind, lapply(years, function(yr) {
      y <- unique(int[, .(country, iso3c, name_1, name_2)])
      y[, year := yr]
      y
    }))

    ##Average over recent years of spraying, unless there was >0 coverage
    ##in 2025 that is greater than the average
    val_avg <- int[year %in% 2023:2025, ]
    val_avg <- val_avg[,irs_cov := mean(irs_cov), by = c('country', 'iso3c',
                                                            'name_1','name_2','peak_season',
                                                            'insecticide')]
    val_avg[, year := NULL]
    val_avg <- unique(val_avg)

    val_2025 <- int[year == 2025, ]
    val_2025[, year := NULL]
    val_2025 <- unique(val_2025)

    val_2025 <- merge(val_avg, val_2025, by =  c('country', 'iso3c',
                                                 'name_1','name_2','peak_season',
                                                 'insecticide', 'intervention'))
    val_2025 <- val_2025[,irs_cov := ifelse(irs_cov.x < irs_cov.y, irs_cov.y, irs_cov.x)]
    val_2025[,irs_cov.y := NULL]; val_2025[,irs_cov.x := NULL]
    val_2025[,spray_day_of_year := spray_day_of_year.x]
    val_2025[,spray_day_of_year.y := NULL]; val_2025[,spray_day_of_year.x := NULL]

    need <- merge(val_2025, need,
                  by = intersect(colnames(val_2025), colnames(need)),
                  all = TRUE, allow.cartesian = TRUE)
    ##TODO: change to vectron5000
    ##this is just to remind me to change it in the future
    need[year > 2026, insecticide := 'bendiocarb']
    int <- rbind(int[year < 2026], need)
    return(data.frame(int))
  }


  # ── All other interventions: carry 2025 values forward ─────────────────────
  need <- do.call(rbind, lapply(years, function(yr) {
    y <- unique(int[, .(country, iso3c, name_1, name_2)])
    y[, year := yr]
    y
  }))

  val_2025 <- int[year == 2025, ]
  val_2025[, year := NULL]
  need <- merge(val_2025, need,
                by = intersect(colnames(val_2025), colnames(need)),
                all = TRUE, allow.cartesian = TRUE)
  int <- rbind(int[year < 2026], need)
  return(data.frame(int))
}

bau_list <- lapply(intervention_list, copy_coverage)

# ITN usage from BAU distributions
bau_itn <- lapply(unique(loc_map$name_2), net_input_dist,
                                dist_list = bau_list[['itn']])
bau_itn_use <- rbindlist(lapply(bau_itn, '[[', 2))
bau_itn_dist <- rbindlist(lapply(bau_itn, '[[', 1))
bau_list[['itn_dist']] <- data.frame(bau_itn_dist)
bau_list[['itn_use']]  <- data.frame(bau_itn_use)
bau_list[['itn']]      <- NULL


# No / low coverage -------------------------------------------------------
# Future years (2026-2035): all coverage columns set to 0.
# ITN: mass campaigns zeroed out; routine distributions kept at low level.

remove_coverage <- function(int, years = 2026:2035){
  int <-data.table::data.table(ensure_all_years(int, loc_map))
  int <- unique(int)

  int_columns <- intersect(colnames(int),
                           c("tx_cov", "prop_act", "itn_input_dist", "distribution_upper",
                             "distribution_lower", "irs_cov", "smc_cov",
                             "pmc_cov", "r21_primary_cov", "r21_booster_cov",
                             "r21_booster1_cov", "lsm_cov"))

  for (col in int_columns){
    int[year > 2025, (col) := 0]
    if(col == 'prop_act'){
      int[year > 2025, (col) := 1]
    }
  }

  if ('itn_input_dist' %in% int_columns) {
    copy <- int[year %in% 2023:2025]
    for (new_year in c(3, 6, 9, 12)) {
      x <- copy(copy)
      x[, year := year + new_year]
      x[, itn_input_dist    := 0]
      x[, distribution_lower := 0]
      x[, distribution_upper := 0]
      int <- rbind(int, x)
    }
    int <-data.table::data.table(int)[order(name_2, year, distribution_day_of_year)]
  }

  return(data.frame(int))
}

nolow_list <- lapply(intervention_list, remove_coverage)

# ITN special case: zero mass campaigns, keep routine at low level
itn <-data.table::data.table(intervention_list[["itn"]])
copy <- itn[year %in% 2023:2025]
for (new_year in c(3, 6, 9, 12)) {
  x <- copy(copy)
  x[, year := year + new_year]
  x[distribution_type == 'mass', itn_input_dist    := 0]
  x[distribution_type == 'mass', distribution_lower := 0]
  x[distribution_type == 'mass', distribution_upper := 0]
  itn <- rbind(itn, x)
}
# itn[year > 2025 & distribution_type == 'routine',
#     distribution_upper := 0.1 / 3]
nolow_list[["itn_routine_dist"]] <- data.frame(itn)
nolow_list[["itn_dist"]]         <- nolow_list[["itn"]]
nolow_list[["itn"]]              <- NULL

itn_use_no_mass <- lapply(unique(loc_map$name_2), net_input_dist,
                                    dist_list = nolow_list[['itn_routine_dist']])
use_no_mass  <- rbindlist(lapply(itn_use_no_mass, '[[', 2))
dist_no_mass <- rbindlist(lapply(itn_use_no_mass, '[[', 1))

itn_use_no_nets <- lapply(unique(loc_map$name_2), net_input_dist,
                                    dist_list = nolow_list[['itn_dist']])
use_no_nets  <- rbindlist(lapply(itn_use_no_nets, '[[', 2))
dist_no_nets <- rbindlist(lapply(itn_use_no_nets, '[[', 1))

nolow_list[["itn_routine_use"]] <- data.frame(use_no_mass )
nolow_list[["itn_use"]]         <- data.frame(use_no_nets)
nolow_list[["itn_routine_dist"]] <- data.frame(dist_no_mass)
nolow_list[["itn_dist"]]         <-  data.frame(dist_no_nets)

# High coverage -----------------------------------------------------------
# Future years (2026-2035): coverage raised to 0.9 (or current value if higher).
# SMC Karamoja region hardcoded to 0.994.
# ITN: infer the distribution needed to achieve 80% usage via netz.

high_coverage <- function(int, years = 2026:2035){
  int <-data.table::data.table(ensure_all_years(int, loc_map))

  int_columns <- intersect(colnames(int),
                           c("tx_cov", "itn_input_dist", "irs_cov", "smc_cov",
                             "pmc_cov", "r21_primary_cov", "r21_booster_cov",
                             "r21_booster1_cov", "lsm_cov"))

  # ── ITN: replicate 2023-2025 as the baseline; usage-to-distribution done later
  if ("itn_input_dist" %in% int_columns) {
    copy <- int[year %in% 2023:2025]
    for (new_year in c(3, 6, 9, 12)) {
      x <- copy(copy)
      x[, year := year + new_year]
      int <- rbind(int, x)
    }
    # int[year > 2025 & distribution_type == 'routine',
    #     distribution_upper := distribution_upper / 3]
    int <-data.table::data.table(int)[order(name_2, year, distribution_day_of_year)]
    return(data.frame(int))
  }

  # ── SMC: set future coverage to 0.9, with Karamoja override ────────────────
  if ('smc_cov' %in% int_columns) {
    int[year < 2026 & is.na(smc_cov), smc_cov := 0]
    int[year >= 2026, smc_cov := 0.9]
    int[year >= 2026 & name_1 %in% 'Karamoja', smc_cov := 0.994]
    return(data.frame(int))
  }

  # ── IRS: set future coverage to 0.9, unless that is lower than BAU, ensure that dists w/o
  # spraying use actellic
  # make it so all spraying occurs before 4 months peak season (Best case scenario) ────────────────
  if ('irs_cov' %in% int_columns) {
    int <- data.table::data.table(bau_list[['irs']])
    int[name_2 %in% int[irs_cov == 0 & year == 2030, name_2] & year > 2025, insecticide := "actellic"]
    int[year > 2025,irs_cov := ifelse(irs_cov > 0.9, irs_cov, 0.9)]
    ##spray 4 months before the peak season
    int[year > 2025, spray_day_of_year := peak_season - 28 * 4]
    return(data.frame(int))
  }

  # ── All other interventions: raise to 0.9 (or keep if already higher) ──────
  for (col in int_columns) {
    if (col == "lsm_cov") {
      int[, (col) := 0]
    } else {
      int[year >= 2026, (col) := pmax(get(col), 0.9, na.rm = TRUE)]
    }
  }

 # if ("low_level" %in% colnames(int)) int <- int[low_level != 'adm_1']

  return(data.frame(int))
}

high_list <- lapply(intervention_list, high_coverage)


#Vaccine, cap booster in 2026 as BAU booster
vac <-data.table::data.table(high_list[['vaccine']])
vac_2026 <- vac[year == 2026]
vac_2026[,r21_booster1_cov := NULL]
vac_2026 <- merge(vac_2026,data.table::data.table(bau_list[['vaccine']])[,.(name_2, low_level, r21_booster1_cov)], by = c('name_2', 'low_level'), allow.cartesian = T)
vac <- rbind(vac[year!= 2026], vac_2026)
high_list[['vaccine']] <- data.frame(vac)

# Target: 80% ITN usage across 1.5 years after each MDC
use_dt <- rbindlist(lapply(c(2026,2029,2032,2035), function(x) {
  base_int <-data.table::data.table(site$interventions$itn$use)
  y <- unique(base_int[, .(country, iso3c, name_1, name_2,
                           usage_day_of_year = rep(222-28*1.5 + 28*6,
                                                   length(unique(base_int$name_2))))])
  y[, year    := x+1]
  y[, itn_use := 0.8]
  y
}))
use_dt <- rbind(use_dt,data.table::data.table(site$interventions$itn$use))
use_dt[, usage_timesteps := (year - 2000) * 365 + usage_day_of_year]

dist_dt <-data.table::data.table(high_list[["itn"]])[order(year)]
dist_dt <- dist_dt[year < 2036]
##Assume for high
dist_dt[year > 2025, net_type := "pyrethroid_pbo"]
##Mass distributions occur one and a half months before the peak season
dist_dt[distribution_type == 'mass' & year > 2025, distribution_day_of_year := 222-28*1.5]
dist_dt[, distribution_timesteps := (year - 2000) * 365 + distribution_day_of_year]

test <- lapply(unique(loc_map$name_2), get_dist_and_usage)
use  <- rbindlist(lapply(test, '[[', 2))
dist <- rbindlist(lapply(test, '[[', 1))

high_list[['itn']]      <- NULL
high_list[['itn_dist']] <- dist[, .(country, iso3c, name_1, name_2, year,
                                    distribution_day_of_year,
                                    itn_input_dist = modelled_dist,
                                    net_type, distribution_type,
                                    distribution_lower, distribution_upper,
                                    distribution_timesteps)]
high_list[['itn_use']]  <- use[, .(country, iso3c, name_1, name_2, year,
                                   usage_day_of_year,
                                   itn_use = modelled_usage)]


# Format for runs ---------------------------------------------------------
# ensure_all_years() applied to every final table to catch any remaining gaps
# across the full 2000:2030 strata before saving.

treatment <- list(
  no   = nolow_list[["treatment"]],
  bau  = bau_list[["treatment"]],
  high = high_list[["treatment"]]
)

itn_use <- list(
  no      = nolow_list[["itn_use"]],
  no_mass = nolow_list[["itn_routine_use"]],
  bau     = bau_list[["itn_use"]],
  high    = high_list[["itn_use"]]
)

itn_dist <- list(
  no      = nolow_list[["itn_dist"]],
  no_mass = nolow_list[["itn_routine_dist"]],
  bau     = bau_list[["itn_dist"]],
  high    = high_list[["itn_dist"]]
)

irs <- list(
  no   = nolow_list[["irs"]],
  bau  = bau_list[["irs"]],
  high = high_list[["irs"]]
)

smc <- list(
  no   = nolow_list[["smc"]],
  bau  = bau_list[["smc"]],
  high = high_list[["smc"]]
)

vaccine <- list(
  no   = nolow_list[["vaccine"]],
  bau  = bau_list[["vaccine"]],
  high = high_list[["vaccine"]]
)

input_strat <- list(
  treatment = treatment,
  itn_use   = itn_use,
  itn_dist  = itn_dist,
  irs       = irs,
  smc       = smc,
  vaccine   = vaccine
)

saveRDS(input_strat, 'input_strat.RDS')
