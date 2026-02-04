library("site")
library("tidyverse")
library("ggplot2")
orderly::orderly_description(display = "Pull in base site file")

##For right now, going to read in the current (but soon to be updated) site file as a place holder
#uga <- site::fetch_site(iso3c = 'UGA')

##From Pete's directory
orderly::orderly_shared_resource(
  site.RDS = "raw_data/2026_01_19_UGA_SNT_sitefile.rds"
)

uga <- readRDS('site.RDS')

saveRDS(uga, 'site.RDS')

##Not running this for right now as don't want to clog up history
##This declares that this file cannot be modified
#orderly2::orderly_resource(uga)