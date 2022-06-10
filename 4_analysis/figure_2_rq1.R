name <- "rq1_hic"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(primary == "primary 10+") %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      
      "primary 10+, 3 doses only",
      "primary 10+, boost 75+ yearly",
      "primary 10+, boost 75+ 6 monthly",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 60+ 6 monthly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  ))

# plot boosting vulnerable groups only: primary 10+
# could take out primary filter and put that as a plot facet instead
df1 <- df_summarise %>%
  filter(primary == "primary 10+") %>%
  #filter(boost_age %in% c("boost 75+", "boost 60+")) %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      "Pre-vaccine introduction",
      "primary 10+, 3 doses only",
      "primary 10+, boost 75+ yearly",
      "primary 10+, boost 75+ 6 monthly",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 60+ 6 monthly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  ))
 
rq1_doses <- ggplot(data = filter(df1,strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 0.6) +
  #facet_wrap(~boost_age_scenario)
  lims(x = c(as.Date("2021-01-01"), as.Date("2024-12-31"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = "none") + 
  scale_color_manual(values = col_set_fig2) +
  labs(x = "Time", y = "Cumulative doses per person", col = "Dose strategy")
rq1_doses

rq1_hosp <- ggplot(data = df1, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_fig2[1:6])) +
  scale_fill_manual(values = c("grey20", col_set_fig2[1:6])) +
  labs(x = "Time", y = "Daily hospitalisations per million", col = "Dose strategy", fill = "Dose strategy")
rq1_hosp
#ggsave("plots/rq1_hosp.png", rq1_hosp, height = 4, width = 10)

rq1_inc <- ggplot(data = df1, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_fig2[1:6])) +
  scale_fill_manual(values = c("grey20", col_set_fig2[1:6])) +
#  lims(x = c(as.Date("2020-01-01"), as.Date("2022-03-01"))) +
  labs(x = "Time", y = "Daily infections per million", col = "Dose strategy", fill = "Dose strategy")
  
rq1_inc
#ggsave("plots/rq1_incidence.png", rq1_inc, height = 4, width = 10)

rq1_sp <- ggplot(data = df1, aes(x = as.Date(date), y = sp_med, col = strategy_name)) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_fig2[1:6])) +
  scale_fill_manual(values = c("grey20", col_set_fig2[1:6])) +
#  lims(x = c(as.Date("2020-01-01"), as.Date("2022-03-01"))) +
  labs(x = "Time", y = "NAT > 50% threshold", col = "Dose strategy", fill = "Dose strategy")
rq1_sp
#ggsave("plots/rq1_sp.png", rq1_sp, height = 4, width = 10)

####################################################
# summary plots
df_summarydat <- df_summarise_totals %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(primary, boost_age, boost_frequency, strategy_name, deaths, infections, hospitalisations, target_pop)%>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

rq1_summary_plot <- ggplot(data = df_summarydat, aes(x = strategy_name, y = value / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0) +
  scale_fill_manual(values = col_set_fig2[1:6]) +
  labs(x = "Dose strategy", y = "Total events per million", fill = "Dose strategy")

rq1_summary_plot

######################################################
# plot additional events averted per additional vaccine dose
df_add1 <- df_summarise_totals %>%
  select(primary, boost_age, boost_frequency, strategy_name, deaths_med, inc_med, hosp_med, total_doses_med) %>%
  pivot_longer(cols = c(inc_med, deaths_med, hosp_med, total_doses_med))
df_add1_sub <- df_add1 %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  rename("value_counterfactual" = "value") %>%
  select(-c(primary, boost_age, boost_frequency, strategy_name))
df_add1 <- df_add1 %>%
  left_join(df_add1_sub) %>%
  mutate(difference = if_else(name == "total_doses_med", value - value_counterfactual, value_counterfactual - value)) %>%
  select(name, strategy_name, difference) %>%
  pivot_wider(names_from = name, values_from = difference) %>%
  mutate(infections = inc_med / total_doses_med * 100,
         hospitalisations = hosp_med / total_doses_med * 100,
         deaths = deaths_med / total_doses_med* 100) %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

rq1_additional <- ggplot(data = df_add1, aes(x = strategy_name, y = value, fill = strategy_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name, scale = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0,
        legend.position = "none") +
  labs(x = "Dose strategy", y = "additional events averted per 100 additional doses", fill = "Dose strategy") +
  scale_fill_manual(values = col_set_fig2[2:6])

rq1_additional

######################################################
df1_nat <- df_summarise %>%
  mutate(titre = nat_med, nat_label = "All") %>%
  select(date, titre, strategy_name, nat_label)

df1_vax_ab <- df_summarise %>%
  mutate(titre = vax_ab_med, nat_label = "Vaccination") %>%
  select(date, titre, strategy_name, nat_label)

df1_nat_ab <- df_summarise %>%
  mutate(titre = nat_ab_med, nat_label = "Infection-induced") %>%
  select(date, titre, strategy_name, nat_label)

df1_nat <- rbind(df1_nat, df1_vax_ab, df1_nat_ab) %>%
  filter(strategy_name == "primary 10+, boost 60+ yearly")

rq1_nat <- ggplot(data = df1_nat, aes(x = as.Date(date), y = titre, col = nat_label, linetype = nat_label)) +
  facet_wrap(~strategy_name) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  labs(x = "Time", y = "Mean neutralising antibody titre", col = "NAT measure", linetype = "NAT measure")
rq1_nat


library(patchwork)

layout <- "
AB
CD
EE
FF
"

combined <- rq1_doses +
  rq1_nat +
  rq1_hosp +
  rq1_inc +
  rq1_summary_plot +
  rq1_additional +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect", design = layout)
combined
ggsave("plots/fig2_rq1.png", combined, height = 13, width = 13)
