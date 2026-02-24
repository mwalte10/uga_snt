##orderly resources
orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

orderly2::orderly_resource(
  files = "calibration_utils.R"
)

site_in <- readRDS("new_site.RDS")

dists <- unique(site_in$sites$name_2)
dists = c("Apac", 'Dokolo', 'Amolatar', 'Kalaki', 'Kaberamaido')
dir_in = getwd()
parallel::clusterApply(cl = NULL, dists, cali_function, site = site_in,
                       dir = dir_in)



