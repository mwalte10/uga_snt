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
loc_map <- unique(data.table(site$sites)[,.(name_1, name_2)])

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
  x[,name_2 := name2]
  dt[[name2]] <- x
}

dt <- rbindlist(dt, fill = T)
saveRDS(dt, 'calibrated_results.RDS')

##Only run this if we have all districts
if(all(sort(dists) == sort(unique(site$sites$name_2)))){
  pop_age <- data.table(site$population$population_by_age)
  test_ages <- pop_age[age_lower %in% 2:9,]
  test_ages <- test_ages[,.(pop = sum(pop),
                            par = sum(par),
                            par_pf = sum(par_pf),
                            par_pv = sum(par_pv)), by = c('country','iso3c','name_1','name_2','year')]
  test_ages <- test_ages[,.(country,iso3c,name_1,name_2,year,age_lower=2,age_upper=10,pop,par,par_pf,par_pv)]

  pop_age[age_upper<=5,age_band:='0_5']
  pop_age[age_lower%in%5:14,age_band :='5_15']
  pop_age[age_lower%in%15:99,age_band:='15_100']
  pop_age <- pop_age[,.(pop =sum(pop),par=sum(par),par_pf=sum(par_pf),par_pv=sum(par_pv)), by = c('country', 'iso3c',
                                                                                                  'name_1', 'name_2',
                                                                                                  'year', 'age_band')]
  pop_age <- pop_age[!is.na(age_band)]
  pop_age[,age_lower := as.integer(unlist(lapply(unlist(lapply(age_band, strsplit, split = '_'), recursive = F), '[[', 1)))]
  pop_age[,age_upper := as.integer(unlist(lapply(unlist(lapply(age_band, strsplit, split = '_'), recursive = F), '[[', 2)))]
  pop_age <- pop_age[,.(country,iso3c,name_1,name_2,year,age_lower,age_upper,pop,par,par_pf,par_pv)]
  pop_age <- rbind(test_ages,pop_age, fill = T)

  sts_prev <- list()
  for(sr in srs[1:length(srs)]){
    dt <- data.table()
    dists <- loc_map[name_1 == sr, name_2]
    for(name2 in dists){
      x <- readRDS(paste0('./here/', name2, '.RDS'))
      x <- data.table::data.table(x$sim_out)
      x[,dist := name2]
      dt <- rbind(dt, x,fill=T)
    }
    age_bands <- c('0_1824', '730_3649', '1825_5474', '5475_36499')
    select_colnames <- colnames(dt)[unlist(lapply(age_bands, function(y) grep(pattern = y, colnames(dt))))]
    ##Just want n's (numbers rather than summed probabilities)
    select_colnames <- c("timestep", "dist", select_colnames[grepl('n_', select_colnames)])
    dt <- dt[,..select_colnames]
    dt <- data.table(reshape2::melt(dt, id.vars = c('timestep', 'dist')))
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

    dt <- dcast(dt, timestep + dist + age_band ~ variable, value.var = 'value')
    dt[,age_lower := as.integer(unlist(lapply(strsplit(age_band, split = '_'), '[[', 1))) / 365]
    dt[,age_upper := round(as.integer(unlist(lapply(strsplit(age_band, split = '_'), '[[', 2))) / 365)]
    dt <- dt[,.(dist, timestep, age_lower, age_upper,
                pop = age, detect_lm, detect_pcr, inc_clinical, inc_severe)]
    dt <- data.table(reshape2::melt(dt, id.vars = c('dist', 'timestep', 'age_lower', 'age_upper', 'pop')))
    dt <- dt[!is.na(value)]
    dt[,prev := value / pop]
    dt[,year := floor((timestep-1)/365) + 2000]
    dt[,day_of_year := (timestep) - (year - 2000) * 365]
    dt <- dt[,.(name_2 = dist,year,day_of_year,age_lower,age_upper,pop,variable,value,prev)]
    sts_prev <- rbind(sts_prev, dt, fill = T)
    print(sr)
  }
  save <- copy(sts_prev)

  sts_prev <- merge(sts_prev, pop_age[,.(name_1, name_2,
                                         year, age_lower, age_upper,
                                         par_pf)], by = c('name_2',
                                                          'year', 'age_lower', 'age_upper'),
                    allow.cartesian = T, all.x = T)
  sts_prev[,value := prev * par_pf]
  sts_prev <- sts_prev[,.(name_1, name_2, year, day_of_year,
                          age_lower, age_upper,
                          variable, value, pop = par_pf)]
  sts_prev[,prev := value / pop]


  # Admin 1 aggregation -----------------------------------------------------
  adm1 <- copy(sts_prev)
  adm1[,name_2 := name_1]
  adm1 <- adm1[,.(value = sum(value), pop = sum(pop)), by = c('name_1', 'name_2', 'year', 'day_of_year',
                                                              'age_lower',
                                                              'age_upper', 'variable')]
  adm1[,prev := value / pop]
  dt <- rbind(sts_prev[,low_level := 'adm_2'],
              adm1[,low_level := 'adm_1'], fill = T)
  saveRDS(dt, 'aggregated_results.RDS')
}















