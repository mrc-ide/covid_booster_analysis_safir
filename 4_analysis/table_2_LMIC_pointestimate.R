cost1 <- 2
cost2 <- 20
cost3 <- 50
### main analysis for the LMIC setting ###

rq2 <- readRDS("processed_outputs/df_summarise_totals_rq2_lmic.rds") %>%
  select(name, strategy, strategy_name, target_pop, deaths_med,  hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = value / target_pop * 1e6) %>%
  pivot_wider(names_from = compartment, values_from = value)

# table of total events
totals_table_rq2 <- rq2 %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  arrange(name, total_doses_med)

write_csv(totals_table_rq2, "tables/totals_table_rq2.csv")

# table of events averted
rq2_counter <- rq2 %>%
  filter(strategy_name == "No additional doses") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name, -strategy)

rq2_averted <- rq2 %>%
  filter(strategy_name != "No additional doses") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq2_counter) %>%
  mutate(events_averted = value_counter - value) %>%
  select(name, strategy_name, compartment, events_averted) %>%
  pivot_wider(names_from = compartment, values_from = events_averted) %>%
  mutate(total_doses_med = -total_doses_med) %>%
  mutate(cost_per_hosp1 = round(cost1 * total_doses_med / hosp_med,0),
         cost_per_hosp2 = round(cost2 * total_doses_med / hosp_med,0),
         cost_per_hosp3 = round(cost3 * total_doses_med / hosp_med,0),
         cost_per_death1 = round(cost1 * total_doses_med / deaths_med,0),
         cost_per_death2 = round(cost2 * total_doses_med / deaths_med,0),
         cost_per_death3 = round(cost3 * total_doses_med / deaths_med,0)) %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths, cost_per_hosp1, cost_per_hosp2, cost_per_hosp3, cost_per_death1, cost_per_death2, cost_per_death3)%>%
  arrange(name, total_doses_med)

write_csv(rq2_averted, "tables/rq2_averted.csv")



rq2_newvar <- readRDS("processed_outputs/df_summarise_totals_rq2_lmic_newvariant.rds") %>%
  mutate(variant_scenario = "undefined") %>%
  mutate(
    variant_scenario = case_when(omicron_vaccine == 0 & vfr2 == vfr & hosp_scal_vfr2 == hosp_scal_vfr ~ "no new variant",
                                 omicron_vaccine == 0 & vfr2 == vfr & hosp_scal_vfr2 == 1 ~ "increased severity",
                                 omicron_vaccine == 0 & vfr2 == 10 & hosp_scal_vfr2 == hosp_scal_vfr ~ "additional immune escape",
                                 omicron_vaccine == 1 ~ "omicron-targeted vaccine",
                                 omicron_vaccine == 0 & vfr2 ==  10 & hosp_scal_vfr2 == 1 ~ "increased severity and immune escape",
                                 TRUE ~ variant_scenario)) %>%
  mutate(variant_scenario = factor(variant_scenario, levels = c("increased severity", "additional immune escape", "increased severity and immune escape", "omicron-targeted vaccine", "no new variant")))%>%
  select(name, strategy, strategy_name, variant_scenario, target_pop, deaths_med,  hosp_med, inc_med,  total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = value / target_pop * 1e6) %>%
  pivot_wider(names_from = compartment, values_from = value)

# table of total events
totals_table_rq2_newvar <- rq2_newvar %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths)%>%
  arrange(name, total_doses_med) %>%
  select(-name)

write_csv(totals_table_rq2_newvar, "tables/totals_table_rq2_newvar.csv")

# table of events averted
rq2_counter_newvar <- rq2_newvar %>%
  filter(strategy_name == "No additional doses", variant_scenario != "no new variant", variant_scenario != "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name, -strategy)

rq2_newvar_averted <- rq2_newvar %>%
  filter(strategy_name != "No additional doses", variant_scenario != "no new variant", variant_scenario != "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq2_counter_newvar) %>%
  mutate(events_averted = value_counter - value) %>%
  select(name, strategy_name, variant_scenario, compartment, events_averted) %>%
  pivot_wider(names_from = compartment, values_from = events_averted) %>%
  mutate(total_doses_med = -total_doses_med) %>%
  mutate(cost_per_hosp1 = round(cost1 * total_doses_med / hosp_med,0),
         cost_per_hosp2 = round(cost2 * total_doses_med / hosp_med,0),
         cost_per_hosp3 = round(cost3 * total_doses_med / hosp_med,0),
         cost_per_death1 = round(cost1 * total_doses_med / deaths_med,0),
         cost_per_death2 = round(cost2 * total_doses_med / deaths_med,0),
         cost_per_death3 = round(cost3 * total_doses_med / deaths_med,0)) %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths, cost_per_hosp1, cost_per_hosp2, cost_per_hosp3, cost_per_death1, cost_per_death2, cost_per_death3)%>%
  arrange(name, total_doses_med)

write_csv(rq2_newvar_averted, "tables/rq2_newvar_averted.csv")

# table of events averted - omicron vaccine
rq2_omvacc_counter_newvar <- rq2_newvar %>%
  filter(strategy_name == "No additional doses", variant_scenario == "no new variant", variant_scenario != "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name, -strategy, -variant_scenario)

rq2_omvacc_newvar_averted <- rq2_newvar %>%
  filter(strategy_name != "No additional doses", variant_scenario == "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq2_omvacc_counter_newvar) %>%
  mutate(events_averted = value_counter - value) %>%
  select(name, strategy_name, variant_scenario, compartment, events_averted) %>%
  pivot_wider(names_from = compartment, values_from = events_averted) %>%
  mutate(total_doses_med = -total_doses_med) %>%
  mutate(cost_per_hosp1 = round(cost1 * total_doses_med / hosp_med,0),
         cost_per_hosp2 = round(cost2 * total_doses_med / hosp_med,0),
         cost_per_hosp3 = round(cost3 * total_doses_med / hosp_med,0),
         cost_per_death1 = round(cost1 * total_doses_med / deaths_med,0),
         cost_per_death2 = round(cost2 * total_doses_med / deaths_med,0),
         cost_per_death3 = round(cost3 * total_doses_med / deaths_med,0)) %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths, cost_per_hosp1, cost_per_hosp2, cost_per_hosp3, cost_per_death1, cost_per_death2, cost_per_death3)%>%
  arrange(name, total_doses_med)

write_csv(rq2_omvacc_newvar_averted, "tables/rq2_omvacc_averted.csv")
