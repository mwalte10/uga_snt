library(orderly)
library(hipercow)

################################################################################
##Hipercow set up
################################################################################
working_dir <- getwd()
root <- getwd()
hipercow::hipercow_init(driver = "dide-windows")
#hipercow::hipercow_provision(driver = "dide-windows")
resources_in <- hipercow_resources(cores = 2)
options(hipercow.max_size_local = Inf)
source_files <- list.files("./src/", recursive = T, full.names = T)
hipercow::hipercow_environment_create(sources = source_files)

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
task_log_show(process_raw_data)


orderly::orderly_run("tx_cov")
orderly::orderly_run("smc")
orderly::orderly_run("r21")
orderly::orderly_run("irs")
orderly::orderly_run("plots")


x <- data.table(site$interventions$irs$implementation)
ggplot(x[name_2 == "Namutumba"], aes(year, irs_cov)) + geom_line()
