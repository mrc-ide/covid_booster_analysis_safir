cost1 <- 2
cost2 <- 20
cost3 <- 50

### main analysis for the two HIC settings ###
rq1_3 <- readRDS("processed_outputs/df_summarise_totals_rq1_hic.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic.rds")) %>%
  filter(primary == "primary 10+") %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

# table of total events
totals_table_hic <- rq1_3 %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  arrange(name, total_doses_med)

write_csv(totals_table_hic, "tables/totals_table_hic.csv")

# table of events averted
rq1_3_counter <- rq1_3 %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_averted <- rq1_3 %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq1_3_counter) %>%
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

write_csv(rq1_3_averted, "tables/rq1_3_averted.csv")

### analysis for HIC new variant settings ###
rq1_3_newvar <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_newvariant.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_newvariant.rds")) %>%
  filter(primary == "primary 10+") %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

# table of total events
totals_table_hic_newvar <- rq1_3_newvar %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths)%>%
  arrange(name, total_doses_med)

write_csv(totals_table_hic_newvar, "tables/totals_table_hic_newvar.csv")

# table of events averted
rq1_3_counter_newvar <- rq1_3_newvar %>%
  filter(strategy_name == "primary 10+, 3 doses only", variant_scenario != "no new variant", variant_scenario != "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_newvar_averted <- rq1_3_newvar %>%
  filter(strategy_name != "primary 10+, 3 doses only", variant_scenario != "no new variant", variant_scenario != "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq1_3_counter_newvar) %>%
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

write_csv(rq1_3_newvar_averted, "tables/rq1_3_newvar_averted.csv")

# table of events averted
rq1_3_omcvacc_counter_newvar <- rq1_3_newvar %>%
  filter(strategy_name == "primary 10+, 3 doses only", variant_scenario == "no new variant") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name, -variant_scenario)

rq1_3_omvacc_newvar_averted <- rq1_3_newvar %>%
  filter(strategy_name != "primary 10+, 3 doses only", variant_scenario == "omicron-targeted vaccine") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq1_3_omcvacc_counter_newvar) %>%
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

write_csv(rq1_3_omvacc_newvar_averted, "tables/rq1_3_omvacc_averted.csv")
