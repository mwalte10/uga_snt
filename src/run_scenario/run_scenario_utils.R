built_site_file <- function(site, name_2_in, dir_in,  input_strat, combos, input){
  x <- combos[input,]
  ##Replace intervention coverages
  treatment <- input_strat[["treatment"]][x[['treatment']]][[1]]
  itn_use <- input_strat[["itn_use"]][x[['itn']]][[1]]
  itn_dist <- input_strat[["itn_dist"]][x[['itn']]][[1]]
  irs <- input_strat[["irs"]][x[['irs']]][[1]]
  smc <- input_strat[["smc"]][x[['smc']]][[1]]
  vaccine <- input_strat[["vaccine"]][x[['vaccine']]][[1]]
  index <- x[['index']][[1]]


  site$interventions$treatment$implementation <- treatment
  site$interventions$itn$use <- itn_use
  site$interventions$itn$implementation <- itn_dist
  site$interventions$irs$implementation <- irs
  site$interventions$smc$implementation <- smc
  site$interventions$vaccine$implementation <- vaccine

  urbanicity <- unique(data.table::data.table(site$sites)[name_2 == name_2_in, urban_rural])
  for(urbanicity_in in urbanicity){
    site_adm2 <- site::subset_site(
      site = site,
      site_filter = data.frame(
        country = 'Uganda',
        iso3c = 'UGA',
        name_1 = unique(data.table::data.table(site$sites)[name_2 == name_2_in, name_1]),
        name_2 = name_2_in,
        urban_rural = urbanicity_in
      )
    )
    site_adm2$interventions$treatment$implementation$day_of_year = 1


    site_par_adm2 <- site::site_parameters(
      interventions = site_adm2$interventions,
      demography = site_adm2$demography,
      vectors = site_adm2$vectors,
      seasonality = site_adm2$seasonality,
      eir = site_adm2$eir$eir,
      overrides = list(
        human_population = 5000
      ),
      start_year = 2000,
      end_year = 2035,
      irs_adjust = 0.6
    )
    site_par_adm2$prevalence_rendering_min_ages <- 0
    site_par_adm2$prevalence_rendering_max_ages <- 1824

    ####Pull in init_EIR
    parameters <- malariasimulation::set_equilibrium(site_par_adm2, init_EIR = site_adm2$eir$eir)

    ##about 9 minutes
    #start <- Sys.time()
    projection <- malariasimulation::run_simulation(length(2000:2034) * 365, parameters = parameters)
    # end <- Sys.time()
    projection <- data.table::data.table(projection)
    projection[,name_2 := name_2_in]
    projection[,urban_rural := urbanicity_in]
    projection[,scenario := index]

    saveRDS(projection, paste0(dir_in, '/', name_2_in, '_', urbanicity_in, '_', index, '.RDS'))
  }
}


