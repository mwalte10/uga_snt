library(orderly)
library(hipercow)

################################################################################
##Hipercow set up
################################################################################
hipercow::hipercow_init(driver = "dide-windows")
#hipercow::hipercow_provision(driver = "dide-windows")
resources_in <- hipercow_resources(cores = 2)
options(hipercow.max_size_local = Inf)
# source_files <- list.files("./src/", recursive = T, full.names = T)
# hipercow::hipercow_environment_create(sources = source_files)

################################################################################
##Orderly set up, does not need to be run again
################################################################################
# orderly::orderly_new("process_raw_data",template = F)
# orderly::orderly_new("site_file",template = F)
# orderly::orderly_new("tx_cov",template = F)
# orderly::orderly_new("smc",template = F)
# orderly::orderly_new("r21",template = F)
# orderly::orderly_new("irs",template = F)
# orderly::orderly_new("plots",template = F)


################################################################################
##Run historical intervention input options
###NOTE: this could be run interactively, but R is so slow on the shared drive
################################################################################
#orderly::orderly_run("site_file")
##Process raw data
process_raw_data <- task_create_expr(orderly::orderly_run(name = "process_raw_data"),
                                     resources = resources_in)
while(task_status(process_raw_data) %in% c('running', 'submitted')){
  print('Waiting for prep inputs to finish')
  Sys.sleep(5)
}
task_result(process_raw_data)

##Treatment coverage
tx_cov <- task_create_expr(orderly::orderly_run(name = "tx_cov"),
                                     resources = resources_in)
while(task_status(tx_cov) %in% c('running', 'submitted')){
  print('Waiting for treatment coverage to finish')
  Sys.sleep(5)
}
task_result(tx_cov)

##SMC
smc <- task_create_expr(orderly::orderly_run(name = "smc"),
                           resources = resources_in)
while(task_status(smc) %in% c('running', 'submitted')){
  print('Waiting for SMC coverage to finish')
  Sys.sleep(5)
}
task_result(smc)

##R21
r21 <- task_create_expr(orderly::orderly_run(name = "r21"),
                        resources = resources_in)
while(task_status(r21) %in% c('running', 'submitted')){
  print('Waiting for R21 coverage to finish')
  Sys.sleep(5)
}
task_result(r21)

##IRS
irs <- task_create_expr(orderly::orderly_run(name = "irs"),
                        resources = resources_in)
while(task_status(irs) %in% c('running', 'submitted')){
  print('Waiting for IRS coverage to finish')
  Sys.sleep(5)
}
task_result(irs)

##itn
itn <- task_create_expr(orderly::orderly_run(name = "itn"),
                        resources = resources_in)
while(task_status(itn) %in% c('running', 'submitted')){
  print('Waiting for ITN coverage to finish')
  Sys.sleep(5)
}
task_result(itn)

##Plots, this didn't work
plots <- task_create_expr(orderly::orderly_run(name = "plots"),
                        resources = resources_in)
while(task_status(plots) %in% c('running', 'submitted')){
  print('Waiting for ploting to finish')
  Sys.sleep(5)
}
task_result(plots)
