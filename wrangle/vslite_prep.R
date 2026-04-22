source("~/GitHub/runvslite/runvslite.R")
library(dplR)
library(tidyverse)

beech_c <- chron(detrend(read.rwl("data/bausenberg_beech.rwl"), method = "Spline", nyrs = 32))
oak_c <- chron(detrend(read.rwl("data/bausenberg_oak.rwl"), method = "Spline", nyrs = 32))
climate <- read.csv2("data/climate_bausenberg.csv") %>% 
  select(year, month, temp = tmean, prec)
lfu <- read_csv2("data/lfu_cordex_ensemble_monthly.csv")

input_historic_beech <- make_vsinput_historic(beech_c, climate)
input_historic_oak <- make_vsinput_historic(oak_c, climate)

params_beech <- vs_params(input_historic_beech$trw,
                          input_historic_beech$temp,
                          input_historic_beech$prec,
                          input_historic_beech$syear,
                          input_historic_beech$eyear,
                          50.28)

params_oak <- vs_params(input_historic_oak$trw,
                        input_historic_oak$temp,
                        input_historic_oak$prec,
                        input_historic_oak$syear,
                        input_historic_oak$eyear,
                        50.28)

rcp26 <- lfu %>% 
  filter(rcp == "RCP_26", year > 2025) %>% 
  group_by(year, month) %>% 
  summarise(temp = mean(temp),
            prec = mean(prec)) %>% 
  ungroup()

rcp45 <- lfu %>% 
  filter(rcp == "RCP_45", year > 2025) %>% 
  group_by(year, month) %>% 
  summarise(temp = mean(temp),
            prec = mean(prec)) %>% 
  ungroup()

rcp85 <- lfu %>% 
  filter(rcp == "RCP_85", year > 2025) %>% 
  group_by(year, month) %>% 
  summarise(temp = mean(temp),
            prec = mean(prec)) %>% 
  ungroup()

input_rcp26 <- make_vsinput_transient(rcp26)
input_rcp45 <- make_vsinput_transient(rcp45)
input_rcp85 <- make_vsinput_transient(rcp85)

save(input_rcp26, input_rcp45, input_rcp85, file = "data/input_transient.rda")
