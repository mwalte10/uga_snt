library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

orderly::orderly_shared_resource(
  ir_data.xlsx = "Uganda collated IR data.xlsx"
)

# Insecticide resistance data ---------------------------------------------
ir <- readxl::read_xlsx('ir_data.xlsx', sheet = 'Analysis') %>% data.table()
ir <- ir[,.(name_2 = `District                       (2nd admin level)`,
            year = `Year of test`,
            insecticide = `Insecticide Tested`,
            mult_dc = `Multiple of DC`,
            synergist_type = `Synergist type`,
            time_reading = `Time at which Mortality Reading was taken`,
            adj_mort = `ADJUSTED MORTALITY %`)]
ir <- ir[insecticide %in% c('Permethrin',
                            'Alphacypermethrin',
                            'Deltamethrin') &
           mult_dc == 'x1' &
           synergist_type == 'No synergist' &
           time_reading == '24hrs']
fix_year <- round(mean(ir$year))
ir <- merge(ir, loc_map, by = 'name_2')
save_ir = copy(ir)
ir <- ir[,.(adj_mort = mean(adj_mort),
            adj_mort_sd = sd(adj_mort),
            mean_year = mean(year)), by = c('name_1')]
##Don't have data on Bugisu, Ankole, Kigezi, or Kampala so will use the following as proxies
###Bugisu: Karamoja, Teso, Bukedi
ir <- rbind(ir,
            copy(ir)[name_1 %in% c('Karamoja', 'Teso', 'Bukedi'),.(name_1 = 'Bugisu',
                                                                   adj_mort = mean(adj_mort),
                                                                   adj_mort_sd = NA,
                                                                   mean_year = mean(mean_year))])
###Ankole:  Tooro, South Buganda
ir <- rbind(ir,
            copy(ir)[name_1 %in% c('Tooro', 'South Buganda'),.(name_1 = 'Ankole',
                                                               adj_mort = mean(adj_mort),
                                                               adj_mort_sd = NA,
                                                               mean_year = mean(mean_year))])
###Kigezi: Tooro & South Buganda
ir <- rbind(ir,
            copy(ir)[name_1 %in% c('Tooro', 'South Buganda'),.(name_1 = 'Kigezi',
                                                               adj_mort = mean(adj_mort),
                                                               adj_mort_sd = NA,
                                                               mean_year = mean(mean_year))])
##Kampala: South Buganda
ir <- rbind(ir,
            copy(ir)[name_1 %in% c('South Buganda'),.(name_1 = 'Kampala',
                                                                       adj_mort = mean(adj_mort),
                                                                       adj_mort_sd = NA,
                                                      mean_year = mean(mean_year))])
ir[,pyrethroid_resistance := 1 - adj_mort / 100]
ir[,mean_year := round(mean_year)]

# plot_ir <- merge(site$shape$level_1, ir, by = 'name_1')
# ggplot() + geom_sf(data = plot_ir, aes(fill = pyrethroid_resistance)) + scale_fill_continuous(limits = c(0,1))

site_resist <- unique(resist$pyrethroid_resistance)
##Modify to each sub-region
update_resistance_fit <- function(year = c(2000:2050),
                                  resistance,
                                  fix_year = 2025){
  A <- min(resistance[year <= fix_year]) # fixed lower asymptote
  K <- resistance[year == fix_year] # fixed upper asymptote
  fit <- minpack.lm::nlsLM(
    resistance ~ A + (K - A) / (1 + exp(-k * (year - t0))),
    start = list(k = 0.1, t0 = median(year)),
    lower = c(k = 0,   t0 = min(year)),
    upper = c(k = Inf, t0 = max(year)),
    control = minpack.lm::nls.lm.control(maxiter = 200)
  )
  predict(fit)
}

new_ir_curves <- data.table()
for(name_1_in in unique(ir$name_1)){
  year_test = ir[name_1 == name_1_in,mean_year]
  test_resist = ir[name_1 == name_1_in,pyrethroid_resistance]

  resistance <- seq(site_resist[1], ir[name_1 == name_1_in,pyrethroid_resistance], length.out = length(2000:year_test))
  resistance <- c(resistance, rep(ir[name_1 == name_1_in,pyrethroid_resistance], length((year_test + 1):2050)))
  names(resistance) <- 2000:2050

  new <- update_resistance_fit(resistance = resistance, fix_year = fix_year)
  new <- data.table(name_1 = name_1_in,
                    year = 2000:2050,
                    pyrethroid_resistance = new)
  new_ir_curves <- rbind(new_ir_curves, new)

}

# plot_dt <- rbind(new_ir_curves[,source := 'New values'],
#                  unique(data.table(resist)[,.(name_1, year, pyrethroid_resistance, source = 'Site file')]))
# ggplot(data = plot_dt, aes(year, pyrethroid_resistance)) +
#   geom_line(aes(lty = source)) +
#   geom_point(data = save_ir, aes(year, y = 1 - adj_mort / 100, col = insecticide)) +
#   facet_wrap(~name_1)

new_ir_curves <- merge(new_ir_curves, loc_map, by = 'name_1', allow.cartesian = T)
new_ir_curves <- new_ir_curves[,.(iso3c = 'UGA',
                                  country = 'Uganda',
                                  name_1, name_2, year, pyrethroid_resistance, source = 'New fitted curves')]
new_ir_curves <- rbind(new_ir_curves,
                       save_ir[,.(name_1, name_2, year, insecticide, pyrethroid_resistance = 1 - adj_mort/ 100,
                                  source = 'NMED')], fill = T)
saveRDS(new_ir_curves, 'insect_resist.RDS')
