#setwd('Z:/Maggie/uga_snt/src/run_scenario/')
library(tidyverse)
library(ggplot2)
library(data.table)


orderly::orderly_dependency(
  name = "insert_calib_eir",
  query = "latest()",
  files = c(
    site.RDS = "calib_site.RDS"
  )
)

orderly2::orderly_resource(
  files = "run_scenario_utils.R"
)

site <- readRDS('site.RDS')


var_vec = c('treatment', 'itn', 'irs', 'smc', 'vaccine')

get_combos <- function(var_vec = c('treatment', 'itn', 'irs', 'smc', 'vaccine')){

  ## Create index mat
  input = vector('list', length = length(var_vec))
  names(input) <- var_vec
  for(i in 1:length(var_vec)){
    if(names(input)[i] == 'itn'){
      input[[i]] <- c('no', 'no_mass', 'bau', 'high')
    }else{
      input[[i]] <- c('no', 'bau', 'high')
    }
  }
  input <- expand.grid(input)
  input <- data.table::data.table(input)
  input[,index := 1:nrow(input)]
  assertthat::assert_that(nrow(input) == 4 * 3^((length(var_vec)-1)), msg = 'Expected number of scenarios are created')


  return(input)

}

combos <- get_combos(var_vec = c('treatment', 'itn', 'irs', 'smc', 'vaccine'))

##will parallelize across scenarios, with one job submitted per location
built_site_file <- function(x){
  ##Replace intervention coverages
  treatment <- input_strat[["treatment"]][x[['treatment']]][[1]]
  itn_use <- input_strat[["itn_use"]][x[['itn']]][[1]]
  itn_dist <- input_strat[["itn_dist"]][x[['itn']]][[1]]
  irs <- input_strat[["irs"]][x[['irs']]][[1]]
  smc <- input_strat[["smc"]][x[['smc']]][[1]]
  vaccine <- input_strat[["vaccine"]][x[['vaccine']]][[1]]

  site$interventions$treatment$implementation <- treatment
  site$interventions$itn$use <- itn_use
  site$interventions$itn$implementation <- itn_dist
  site$interventions$irs$implementation <- irs
  site$interventions$smc$implementation <- smc
  site$interventions$vaccine$implementation <- vaccine

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
    eir = 20,
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
  parameters <- malariasimulation::set_equilibrium(site_par_adm2, init_EIR = out)

  ##about 9 minutes
  #start <- Sys.time()
  projection <- malariasimulation::run_simulation(length(2000:2034) * 365, parameters = parameters)
  # end <- Sys.time()

  x  = data.table(postie::get_prevalence(projection))


}
