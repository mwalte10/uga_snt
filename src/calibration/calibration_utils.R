#site <- readRDS(paste0(getwd(), '/archive/gen_new_site/', list.files(paste0(getwd(), '/archive/gen_new_site/'))[length(list.files(paste0(getwd(), '/archive/gen_new_site/')))], '/new_site.RDS'))

cali_function <- function(name_2_in, site, dir){

  ##Replace broken site file characteristics
  ##TODO: move this into a more sensible spot
# Vaccine -----------------------------------------------------------------
  vx <- data.table::data.table(site$interventions$vaccine$implementation)[r21_primary_cov > 1, r21_primary_cov := 1]
  vx[r21_booster1_cov > 1, r21_booster1_cov := 1]
  ##issue here is with bukwo, make sure I'm doing the booster coverage correctly...
  vx[r21_booster1_cov > r21_primary_cov, r21_primary_cov := r21_booster1_cov]
  site$interventions$vaccine$implementation <- data.frame(vx)

# Treatment ---------------------------------------------------------------
  tx <- data.table::data.table(site$interventions$treatment$implementation)[tx_cov > 1, tx_cov := 1]
  site$interventions$treatment$implementation <- data.frame(tx)


# usage day of year -------------------------------------------------------
 ##Example Mbarara
  itn_use <- data.table::data.table(site$interventions$itn$use)
  itn_use[usage_day_of_year > 365, usage_day_of_year := 365]
  itn_imp <- data.table::data.table(site$interventions$itn$implementation)
  itn_imp[distribution_day_of_year > 365, distribution_day_of_year := 365]
  site$interventions$itn$use <- data.frame(itn_use)
  site$interventions$itn$implementation <- data.frame(itn_imp)

  urbanicity <- unique(data.table::data.table(site$sites)[name_2 == name_2_in, urban_rural])
  if(length(urbanicity) > 1){
    urbanicity = 'rural'
  }

  site_adm2 <- site::subset_site(
    site = site,
    site_filter = data.frame(
      country = 'Uganda',
      iso3c = 'UGA',
      name_1 = unique(data.table::data.table(site$sites)[name_2 == name_2_in, name_1]),
      name_2 = name_2_in,
      urban_rural = urbanicity
    )
  )
  site_adm2$interventions$treatment$implementation$day_of_year = 1

  site_par_adm2 <- site::site_parameters(
    interventions = site_adm2$interventions,
    demography = site_adm2$demography,
    vectors = site_adm2$vectors,
    seasonality = site_adm2$seasonality,
    eir = 20,
    overrides = list(
      human_population = 5000
    ),
    start_year = 2000,
    end_year = 2025,
    irs_adjust = 0.6
  )

  set.seed(925)

  cali_years = c(2000,2006,2009,2011,2014,2016,2018)

  get_annual_prev <- function(x){
    prev <- data.table::data.table(postie::get_prevalence(x))
    prev <- prev[,.(prev = mean(lm_prevalence_2_10)), by = c('year')]
    prev <- prev[year %in% cali_years]
    return(prev$prev)
  }

  out <- cali::calibrate(
    parameters = site_par_adm2,
    target = data.table::data.table(site_adm2$prevalence)[year %in% cali_years,pfpr],
    summary_function = get_annual_prev,
    eq_prevalence = data.table::data.table(site_adm2$prevalence)[year %in% c(2000),pfpr]
  )

  site_par_adm2$human_population <- 5000
  parameters <- malariasimulation::set_equilibrium(site_par_adm2, init_EIR = out)
  raw <- malariasimulation::run_simulation(parameters$timesteps + 100, parameters = parameters)

  ##consider putting the cluster task in here so that the same archive folder is used for all locations
  saveRDS(list(parameters = parameters,
               sim_out = raw),
          file = paste0(dir, '/', name_2_in, '.RDS'))
}

