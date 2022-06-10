# prevalence
name <- "rq1_hic"

df_summarise_rq1 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(date >= as.Date("2022/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  group_by(strategy_name) %>%
  mutate(prevalence = (E_t + IMild_t + ICase_t)/target_pop*100) %>%
  mutate(name = name) %>%
  select(name, strategy_name, date, prevalence)
df_summarise_rq1

ggplot(data = df_summarise_rq1, aes(x = date, y = prevalence, col = strategy_name)) +
  geom_line()

df_summarise_rq1 %>%
  group_by(strategy_name) %>%
  filter(date >= as.Date("2024/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  summarise(prevalence = mean(prevalence))

# daily infections per year in 2023
name <- "rq2_lmic"
ages_covered <- 9

df_summarise_rq2 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered) %>%
  filter(date >= as.Date("2022/01/01", format = "%Y/%m/%d"),
         date < as.Date("2023/12/31", format = "%Y/%m/%d")) %>%
  group_by(strategy_name) %>%
  mutate(prevalence = (E_t + IMild_t + ICase_t)/target_pop*100) %>%
  mutate(name = name) %>%
  select(name, strategy_name, date, prevalence)
df_summarise_rq2
ggplot(data = df_summarise_rq2, aes(x = date, y = prevalence, col = strategy_name)) +
  geom_line()

df_summarise_rq2 %>%
  group_by(strategy_name) %>%
  filter(date >= as.Date("2023/01/01", format = "%Y/%m/%d"),
         date < as.Date("2023/12/31", format = "%Y/%m/%d")) %>%
  summarise(prevalence = mean(prevalence))

# daily infections per year in 2024
name <- "rq3_hic"

df_summarise_rq3 <-  readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(date >= as.Date("2022/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  group_by(strategy_name) %>%
  mutate(prevalence = (E_t + IMild_t + ICase_t)/target_pop*100) %>%
  mutate(name = name) %>%
  select(name, strategy_name, date, prevalence)
df_summarise_rq3

ggplot(data = df_summarise_rq3, aes(x = date, y = prevalence, col = strategy_name)) +
  geom_line()

df_summarise_rq3 %>%
  group_by(strategy_name) %>%
  filter(date >= as.Date("2024/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  summarise(prevalence = mean(prevalence))


# daily infections per year in 2024
name <- "rq3_hic_newvariant"

df_summarise_rq3_newvar <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(primary == "primary 10+") %>%
  mutate(variant_scenario = "undefined") %>%
  mutate(
    variant_scenario = case_when(omicron_vaccine == 0 & vfr2 == vfr & hosp_scal_vfr2 == hosp_scal_vfr ~ "no new variant",
                                 omicron_vaccine == 0 & vfr2 == vfr & hosp_scal_vfr2 == 1 ~ "increased severity",
                                 omicron_vaccine == 0 & vfr2 == 10 & hosp_scal_vfr2 == hosp_scal_vfr ~ "additional immune escape",
                                 omicron_vaccine == 1 ~ "omicron-targeted vaccine",
                                 omicron_vaccine == 0 & vfr2 ==  10 & hosp_scal_vfr2 == 1 ~ "increased severity and immune escape",
                                 TRUE ~ variant_scenario)) %>%
  mutate(variant_scenario = factor(variant_scenario, levels = c("increased severity", "additional immune escape", "increased severity and immune escape", "omicron-targeted vaccine", "no new variant")))  %>%
  filter(date >= as.Date("2022/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  mutate(prevalence = (E_t + IMild_t + ICase_t)/target_pop*100) %>%
  mutate(name = name) %>%
  select(name, strategy_name, variant_scenario, date, prevalence)
df_summarise_rq3_newvar

ggplot(data = df_summarise_rq3_newvar, aes(x = date, y = prevalence, col = strategy_name)) +
  geom_line()

df_summarise_rq3_newvar %>%
  group_by(strategy_name, variant_scenario) %>%
  filter(date >= as.Date("2024/01/01", format = "%Y/%m/%d"),
         date < as.Date("2024/12/31", format = "%Y/%m/%d")) %>%
  summarise(prevalence = mean(prevalence))
