#setwd("Z:/Maggie/uga_snt/src/gen_scenario_site_files")
orderly::orderly_dependency(
  name = "scenario_input_list",
  query = "latest()",
  files = c(
    input_strat.RDS = 'input_strat.RDS'
  )
)
orderly::orderly_dependency(
  name = "gen_new_site",
  query = "latest()",
  files = c(
    new_site.RDS = "new_site.RDS"
  )
)

input_strat <- readRDS("input_strat.RDS")
site <- readRDS('new_site.RDS')

bau <- copy(site$interventions)
high <- copy(site$interventions)

no <- copy(site$interventions)
no$treatment$implementation <- input_strat$treatment$no
no$itn$use <- input_strat$itn_use$no
no$itn$implementation <- input_strat$itn_dist$no
no$irs$implementation <- input_strat$irs$no
no$smc$implementation <- input_strat$smc$no
no$vaccine$implementation <- input_strat$vaccine$no
saveRDS(no, 'site_no.RDS')
rm("no")

no <- copy(site$interventions)
no$treatment$implementation <- input_strat$treatment$no
no$itn$use <- input_strat$itn_use$no
no$itn$implementation <- input_strat$itn_dist$no
no$irs$implementation <- input_strat$irs$no
no$smc$implementation <- input_strat$smc$no
no$vaccine$implementation <- input_strat$vaccine$no
saveRDS(no, 'site_no.RDS')
rm("no")

bau <- copy(site$interventions)
bau$treatment$implementation <- input_strat$treatment$bau
bau$itn$use <- input_strat$itn_use$bau
bau$itn$implementation <- input_strat$itn_dist$bau
bau$irs$implementation <- input_strat$irs$bau
bau$smc$implementation <- input_strat$smc$bau
bau$vaccine$implementation <- input_strat$vaccine$bau
saveRDS(bau, 'site_bau.RDS')
rm("bau")
