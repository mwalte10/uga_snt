#setwd('Z:/Maggie/uga_snt/src/run_scenario/')
library(tidyverse)
library(ggplot2)
library(data.table)

orderlyparams <- orderly::orderly_parameters(
  name_2 = NULL,
  index = NULL
  )
name_2 = orderlyparams$name_2
index = orderlyparams$index

orderly::orderly_dependency(
  name = "insert_calib_eir",
  query = "latest()",
  files = c(
    site.RDS = "calib_site.RDS"
  )
)

orderly::orderly_dependency(
  name = "scenario_input_list",
  query = "latest()",
  files = c(
    input_strat.RDS = 'input_strat.RDS'
  )
)


orderly::orderly_resource(
  "run_scenario_utils.R"
)

source("run_scenario_utils.R")
site <- readRDS('site.RDS')
input_strat <- readRDS('input_strat.RDS')

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

dir_in = 'Z:/Maggie/uga_snt/scenario_analysis/'
built_site_file(site = site,
                name_2_in = name_2,
                input = index,
                dir = dir_in,
                input_strat = input_strat,
                combos = combos)
