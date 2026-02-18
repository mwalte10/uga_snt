library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
library(postie)
library(cali)
# remotes::install_git(url = "https://github.com/mrc-ide/site/",
#                      branch = "site-2601")
# remotes::install_git(url = "https://github.com/mrc-ide/postie/",
#                      branch = "main")

##Args
name_2_in = "Koboko"

##orderly resources
orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

#site <- readRDS(paste0(getwd(), '/archive/gen_new_site/', list.files(paste0(getwd(), '/archive/gen_new_site/'))[length(list.files(paste0(getwd(), '/archive/gen_new_site/')))], '/new_site.RDS'))

site_adm2 <- site::subset_site(
  site = site,
  site_filter = data.frame(
    country = 'Uganda',
    iso3c = 'UGA',
    name_1 = unique(data.table(site$sites)[name_2 == name_2_in, name_1]),
    name_2 = name_2_in,
    urban_rural = 'rural'
  )
)
site_adm2$interventions$treatment$implementation$day_of_year = 1

site_par_adm2 <- site_parameters(
  interventions = site_adm2$interventions,
  demography = site_adm2$demography,
  vectors = site_adm2$vectors,
  seasonality = site_adm2$seasonality,
  eir = 20,
  overrides = list(
    human_population = 5000
  ),
  start_year = 2000,
  end_year = 2025
)

set.seed(925)

get_annual_prev <- function(x){
  prev <- data.table(postie::get_prevalence(x))
  prev <- prev[,.(prev = mean(lm_prevalence_2_10)), by = c('year')]
  return(prev$prev)
}

out <- calibrate(
  parameters = site_par_adm2,
  target = site_adm2$prevalence$pfpr[1:26],
  summary_function = get_annual_prev,
  eq_prevalence = site_adm2$prevalence$pfpr[1]
)

site_par_adm2$human_population <- 5000
parameters <- set_equilibrium(site_par_adm2, init_EIR = out)
raw <- run_simulation(parameters$timesteps + 100, parameters = parameters)

prev <- postie::get_prevalence(raw) %>% data.table()

ggplot(prev, aes(time, lm_prevalence_2_10)) + geom_line() +
  geom_point(data = site_adm2$prevalence, aes(year, pfpr), col = 'red') +
  ylim(0,1)


