#setwd("Z:/Maggie/uga_snt")
library(orderly)
library(hipercow)


# Hipercow setup ----------------------------------------------------------
hipercow::hipercow_init(driver = "dide-windows")
#hipercow::hipercow_provision(driver = "dide-windows")
resources_in <- hipercow_resources(cores = 2)
options(hipercow.max_size_local = Inf)
source_files <- c('./src/calibration/calibration_utils.R')
# hipercow::hipercow_environment_create(sources = source_files)
##set up for districts
site <- readRDS('./shared/raw_data/2026_02_18_UGA_SNT_sitefile.rds')


# Orderly set up, does not need to be run again  --------------------------
#orderly_init(use_file_store = T)
# orderly::orderly_new("process_raw_data",template = F)
# orderly::orderly_new("site_file",template = F)
# orderly::orderly_new("tx_cov",template = F)
# orderly::orderly_new("smc",template = F)
# orderly::orderly_new("r21",template = F)
# orderly::orderly_new("irs",template = F)
# orderly::orderly_new("resistance",template = F)
# orderly::orderly_new("gen_new_site",template = F)
# orderly::orderly_new("format_comparison_data",template = F)
# orderly::orderly_new("calibration",template = F)
# orderly::orderly_new("process_calibration",template = F)
# orderly::orderly_new("plots",template = F)
# orderly::orderly_new("gen_scenario_site_files",template = F)




# Process data ------------------------------------------------------------
##Process raw data
# process_raw_data <- task_create_expr(orderly::orderly_run(name = "process_raw_data"),
#                                      resources = resources_in)
# while(task_status(process_raw_data) %in% c('running', 'submitted')){
#   print('Waiting for prep inputs to finish')
#   Sys.sleep(5)
# }
# task_result(process_raw_data)
orderly::orderly_run(name = "process_raw_data")

# Prep historical interventions -------------------------------------------
##Treatment coverage
# tx_cov <- task_create_expr(orderly::orderly_run(name = "tx_cov"),
#                                      resources = resources_in)
# while(task_status(tx_cov) %in% c('running', 'submitted')){
#   print('Waiting for treatment coverage to finish')
#   Sys.sleep(5)
# }
# task_result(tx_cov)
orderly::orderly_run(name = "tx_cov")

##SMC
# smc <- task_create_expr(orderly::orderly_run(name = "smc"),
#                            resources = resources_in)
# while(task_status(smc) %in% c('running', 'submitted')){
#   print('Waiting for SMC coverage to finish')
#   Sys.sleep(5)
# }
# task_result(smc)
orderly::orderly_run(name = "smc")

##R21
# r21 <- task_create_expr(orderly::orderly_run(name = "r21"),
#                         resources = resources_in)
# while(task_status(r21) %in% c('running', 'submitted')){
#   print('Waiting for R21 coverage to finish')
#   Sys.sleep(5)
# }
# task_result(r21)
orderly::orderly_run(name = "r21")

##IRS
# irs <- task_create_expr(orderly::orderly_run(name = "irs"),
#                         resources = resources_in)
# while(task_status(irs) %in% c('running', 'submitted')){
#   print('Waiting for IRS coverage to finish')
#   Sys.sleep(5)
# }
# task_result(irs)
orderly::orderly_run(name = "irs")

##itn
# itn <- task_create_expr(orderly::orderly_run(name = "itn"),
#                         resources = resources_in)
# while(task_status(itn) %in% c('running', 'submitted')){
#   print('Waiting for ITN coverage to finish')
#   Sys.sleep(5)
# }
# task_result(itn)
orderly::orderly_run(name = "itn")

##Insecticide resistnace
# resistance <- task_create_expr(orderly::orderly_run(name = "resistance"),
#                         resources = resources_in)
# while(task_status(itn) %in% c('running', 'submitted')){
#   print('Waiting for resistance to finish')
#   Sys.sleep(5)
# }
# task_result(resistance)
orderly::orderly_run(name = "resistance")

# Update site file with intervention modifications ------------------------
##new site
# new_site <- task_create_expr(orderly::orderly_run(name = "gen_new_site"),
#                         resources = resources_in)
# while(task_status(new_site) %in% c('running', 'submitted')){
#   print('Waiting for new site generation to finish')
#   Sys.sleep(5)
# }
# task_result(new_site)
orderly::orderly_run(name = "gen_new_site")
orderly::orderly_run(name = "format_comparison_data")

# Calibrate EIR -----------------------------------------------------------
cali <- task_create_expr(orderly::orderly_run(name = "calibration"),
                         resources = hipercow_resources(cores = 20),
                         parallel = hipercow_parallel("parallel"))

while(task_status(cali) %in% c('running', 'submitted')){
  print('Waiting for calibration to finish')
  Sys.sleep(5)
}
task_result(cali)


# Combine calibrated results ----------------------------------------------
combine <- task_create_expr(orderly::orderly_run(name = "process_calibration"),
                             resources = resources_in)
while(task_status(combine) %in% c('running', 'submitted')){
  print('Waiting for combined results to finish')
  Sys.sleep(5)
}
task_result(combine)

# Plot outputs for vetting ------------------------------------------------
##TODO: figure out why tx_cov isn't working??
plots <- task_create_expr(orderly::orderly_run(name = "plots"),
                        resources = resources_in)
while(task_status(plots) %in% c('running', 'submitted')){
  print('Waiting for ploting to finish')
  Sys.sleep(5)
}
task_result(plots)
orderly::orderly_run(name = "plots")



# Generate scenario inputs ------------------------------------------------
orderly::orderly_run(name = "scenario_input_list")
##put int site files (just to make plotting easier)

