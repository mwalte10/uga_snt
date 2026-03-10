#setwd('src/process_calibration')
library(data.table)
library(orderly)

orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)
site <- readRDS('new_site.RDS')
dists <- unique(site$sites$name_2)
srs <- unique(site$sites$name_1)
loc_map <- unique(data.table(site$sites)[,.(name_1, name_2, urban_rural)])
name2_ur <- paste0(loc_map$name_2, '_', loc_map$urban_rural)

orderly::orderly_dependency(
  name = "calibration",
 query = "latest()",
  files = c(here = './')
)

dists <- list.files('./here/', pattern = '.RDS')
dists <- setdiff(dists, 'new_site.RDS')
dists <- unlist(lapply(dists, gsub, pattern = '.RDS', replacement = ''))

dt <- list()
for(name2 in dists){
  x <- readRDS(paste0('./here/', name2, '.RDS'))
  x <- data.table::data.table(x$sim_out)
  name_2_in <- strsplit(name2, split = '_')[[1]][1]
  urbanicity <- strsplit(name2, split = '_')[[1]][2]
  x[,name_2 := name_2_in]
  x[,urban_rural := urbanicity]
  dt[[name2]] <- x
  print(name2)
}

dt <- rbindlist(dt, fill = T)
saveRDS(dt, 'calibrated_results.RDS')

eir <- list()
for(name2 in dists){
  x <- readRDS(paste0('./here/', name2, '.RDS'))
  x <- data.table::data.table(eir = x$parameters$init_EIR)
  name_2_in <- strsplit(name2, split = '_')[[1]][1]
  urbanicity <- strsplit(name2, split = '_')[[1]][2]
  x[,name_2 := name_2_in]
  x[,urban_rural := urbanicity]
  eir[[name2]] <- x
  print(name2)
}

eir <- rbindlist(eir, fill = T)
saveRDS(eir, 'calibrated_eir.RDS')

##Only run this if we have all districts
if(all(sort(dists) == sort(name2_ur))){
  pop_age <- data.table(site$population$population_by_age)
  test_ages <- pop_age[age_lower %in% 0:5,]
  test_ages <- test_ages[,.(pop = sum(pop),
                            par = sum(par),
                            par_pf = sum(par_pf),
                            par_pv = sum(par_pv)), by = c('country','iso3c','name_1','name_2', 'urban_rural','year')]
  test_ages <- test_ages[,.(country,iso3c,name_1,name_2,urban_rural,year,age_lower=0,age_upper=5,pop,par,par_pf,par_pv)]

  pop_age[age_upper<=5,age_band:='0_5']
  pop_age[age_lower%in%5:14,age_band :='5_15']
  pop_age[age_lower%in%15:99,age_band:='15_100']
  pop_age <- pop_age[,.(pop =sum(pop),par=sum(par),par_pf=sum(par_pf),par_pv=sum(par_pv)), by = c('country', 'iso3c',
                                                                                                  'name_1', 'name_2',
                                                                                                  'urban_rural',
                                                                                                  'year', 'age_band')]
  pop_age <- pop_age[!is.na(age_band)]
  pop_age[,age_lower := as.integer(unlist(lapply(unlist(lapply(age_band, strsplit, split = '_'), recursive = F), '[[', 1)))]
  pop_age[,age_upper := as.integer(unlist(lapply(unlist(lapply(age_band, strsplit, split = '_'), recursive = F), '[[', 2)))]
  pop_age <- pop_age[,.(country,iso3c,name_1,name_2,urban_rural,year,age_lower,age_upper,pop,par,par_pf,par_pv)]
  pop_age <- unique(rbind(test_ages,pop_age, fill = T))

  adm2_prev <- list()
  for(name_2_in in unique(pop_age$name_2)){
    dt <- data.table()
    urban_regions <- loc_map[name_2 == name_2_in, urban_rural]
    for(urbanicity in urban_regions){
      x <- readRDS(paste0('./here/', paste0(name_2_in, '_', urbanicity), '.RDS'))
      x <- data.table::data.table(x$sim_out)
      x[,name_2 := name_2_in]
      x[,urban_rural := urbanicity]
      dt <- rbind(dt, x,fill=T)
    }
    age_bands <- c('0_1824', '730_3649', '1825_5474', '5475_36499')
    select_colnames <- colnames(dt)[unlist(lapply(age_bands, function(y) grep(pattern = y, colnames(dt))))]
    ##Just want n's (numbers rather than summed probabilities)
    select_colnames <- c("timestep", "name_2", 'urban_rural', select_colnames[grepl('n_', select_colnames)])
    dt <- dt[,..select_colnames]
    dt <- data.table(reshape2::melt(dt, id.vars = c('timestep', 'name_2', 'urban_rural')))
    dt[,variable := gsub(pattern = 'n_', replacement = '', variable)]

    ##Pull out ages
    for(age_in in age_bands){
      dt[grepl(age_in,variable), age := age_in]
    }
    dt[, variable := mapply(
      function(var, a) gsub(paste0("_", a), "", var),
      variable,
      age
    )]
    setnames(dt, 'age', 'age_band')

    dt <- dcast(dt, timestep + name_2 + urban_rural + age_band ~ variable, value.var = 'value')
    dt[,age_lower := as.integer(unlist(lapply(strsplit(age_band, split = '_'), '[[', 1))) / 365]
    dt[,age_upper := round(as.integer(unlist(lapply(strsplit(age_band, split = '_'), '[[', 2))) / 365)]
    dt <- dt[,.(name_2, urban_rural, timestep, age_lower, age_upper,
                pop = age, detect_lm, detect_pcr, inc_clinical, inc_severe)]
    dt <- data.table(reshape2::melt(dt, id.vars = c('name_2', 'urban_rural', 'timestep',
                                                    'age_lower', 'age_upper', 'pop')))
    dt <- dt[!is.na(value)]
    dt[,prev := value / pop]
    dt[,year := floor((timestep-1)/365) + 2000]
    dt[,day_of_year := (timestep) - (year - 2000) * 365]
    dt <- dt[,.(name_2, urban_rural,year,day_of_year,age_lower,age_upper,pop,variable,value,prev)]
    adm2_prev <- rbind(adm2_prev, dt, fill = T)
    print(name_2_in)
  }
  save <- copy(adm2_prev)

  adm2_prev <- merge(adm2_prev, pop_age[,.(name_1, name_2,
                                           urban_rural,
                                           year, age_lower, age_upper,
                                           par_pf)], by = c('name_2', 'urban_rural',
                                                          'year', 'age_lower', 'age_upper'),
                    allow.cartesian = T, all.x = T)
  adm2_prev[,value := prev * par_pf]
  adm2_prev <- adm2_prev[,.(value = sum(value), pop = sum(par_pf)), by = c('name_1', 'name_2', 'year',
                                                                           'day_of_year', 'age_lower',
                                                                           'age_upper', 'variable')]
  adm2_prev[,prev := value / pop]


  # Admin 2 aggregation -----------------------------------------------------
  adm1 <- copy(adm2_prev)
  adm1[,name_2 := name_1]
  adm1 <- adm1[,.(value = sum(value), pop = sum(pop)), by = c('name_1', 'name_2',
                                                              'year', 'day_of_year',
                                                              'age_lower',
                                                              'age_upper', 'variable')]
  adm1[,prev := value / pop]

  adm0 <- copy(adm1)
  adm0[,name_1 := 'Uganda']
  adm0[,name_2 := 'Uganda']
  adm0 <- adm0[,.(value = sum(value), pop = sum(pop)), by = c('name_1', 'name_2',
                                                              'year', 'day_of_year',
                                                              'age_lower',
                                                              'age_upper', 'variable')]
  adm0[,prev := value / pop]

  dt <- rbind(adm2_prev[,low_level := 'adm_2'],
              adm1[,low_level := 'adm_1'],
              adm0[,low_level := 'adm_0'], fill = T)
  saveRDS(dt, 'aggregated_results.RDS')
}















