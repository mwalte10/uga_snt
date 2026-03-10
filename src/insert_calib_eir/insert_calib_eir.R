#setwd('Z:/Maggie/uga_snt/src/insert_calib_eir/')
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

orderly::orderly_dependency(
  name = "process_calibration",
  query = "latest()",
  files = c(
    eir.RDS = "calibrated_eir.RDS"
  )
)
orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

site <- readRDS('new_site.RDS')
eir <- readRDS("eir.RDS")

site_eir <- data.table(site$eir)
site_eir[,eir := NULL]
site_eir <- merge(site_eir, eir, by = c('name_2', 'urban_rural'), all.x = T, all.y = T)

saveRDS(site_eir, 'calib_site.RDS')
