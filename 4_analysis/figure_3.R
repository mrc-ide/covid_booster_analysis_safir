
name <- "rq2_lmic"
ages_covered <- 9

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered) %>%
  mutate(strategy_name = factor(strategy_name, levels = c("No additional doses", "2 doses + booster", "Expand 2 doses", "Annual booster"))) %>%
  filter(date < as.Date("2023/12/31", format = "%Y/%m/%d"))

df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered)%>%
  mutate(strategy_name = factor(strategy_name, levels = c("No additional doses", "2 doses + booster", "Expand 2 doses", "Annual booster")))

#################################################
# plot total doses over time
ggplot(data = df_summarise, aes(x = as.Date(date), y = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("grey30", col8, col6, col2)) +
  scale_fill_manual(values = c("grey30", col8, col6, col2))+
theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.3, hjust=0.2)) +
  labs(x = "Time", y = "Vaccines per person", col = "Dose strategy")

#################################################
# blue-green doses barplot
df_doses <- df_summarise %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Booster" = "dose3_t", "Second boost" = "dose4_t") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Booster", "Second boost"), names_to = "dose") %>%
  mutate(dose = factor(dose, levels = c("Dose 1", "Dose 2", "Booster", "Second boost"), ordered = TRUE))

df1_doses_month <- df_doses %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, age_groups_covered, vaccine_doses, dose, year, month) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

plot_doses <- ggplot(data = df1_doses_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ strategy_name) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.3, hjust=0.2)) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Time", y = "Vaccinated (%)", fill = "Dose number")

plot_doses

df1 <- df_summarise


hosp_t <- ggplot(data = df1, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  #geom_vline(aes(xintercept = as.Date("1/6/2022", format = "%d/%m/%Y")), linetype = "dashed") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey30", col8, col6, col2)) +
  scale_fill_manual(values = c("grey30", col8, col6, col2)) +
  labs(x = "Time", y = "Daily hospitalisations per million", col = "Dose strategy", fill = "Dose strategy")

hosp_t

inc_t <- ggplot(data = df1, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey30", col8, col6, col2)) +
  scale_fill_manual(values = c("grey30", col8, col6, col2)) +
  labs(x = "Time", y = "Daily infections per million", col = "Dose strategy", fill = "Dose strategy")

inc_t

df1_nat <- df1 %>%
  filter(timestep < max(df_summarise$timestep)) %>%
  mutate(titre = nat_med, nat_label = "All") %>%
  select(date, titre, nat_label, max_ab, mu_ab_infection, strategy_name)

df1_vax_ab <- df1 %>%
  filter(timestep < max(df_summarise$timestep)) %>%
  mutate(titre = vax_ab_med, nat_label = "Vaccination") %>%
  select(date, titre, nat_label, max_ab, mu_ab_infection, strategy_name)

df1_nat_ab <- df1 %>%
  filter(timestep < max(df_summarise$timestep)) %>%
  mutate(titre = nat_ab_med, nat_label = "Infection-induced") %>%
  select(date, titre, nat_label, max_ab, mu_ab_infection, strategy_name)

df1_nat <- rbind(df1_nat, df1_vax_ab, df1_nat_ab)

nat_omicron <- ggplot(data = filter(df1_nat, strategy_name == "2 doses + booster"), aes(x = as.Date(date), y = titre, col = nat_label, linetype = nat_label)) +
  geom_line() +
  facet_wrap(~strategy_name, nrow = 1) +
  #geom_vline(aes(xintercept = as.Date("1/6/2022", format = "%d/%m/%Y")), linetype = "dashed") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  labs(x = "Time", y = "Mean neutralizing antibody titre", col = "NAT measure", linetype = "NAT measure")

nat_omicron
#ggsave("plots/lmic_abtitre.png", nat_omicron, height = 3.5, width = 7)
#########################
# summary barchart
df_summ <- df_summarise_totals %>%
  mutate(Infections = inc_med,
         Hospitalisations = hosp_med,
         Deaths = deaths_med) %>%
  select(strategy_name, Deaths, Infections, Hospitalisations, target_pop)%>%
  pivot_longer(cols = c(Infections, Hospitalisations, Deaths))

# barplot summary of events
p_deaths_summary <- ggplot(data = df_summ, aes(x = strategy_name, y = value/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 1) +
  facet_wrap(~name, scales = "free") +
  labs(x = "Dose strategy", y = "Total events per million", col = "Dose strategy", fill = "Dose strategy") +
  scale_color_manual(values = c("grey30", col8, col6, col2)) +
  scale_fill_manual(values = c("grey30", col8, col6, col2)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0)

p_deaths_summary

x <- df_summ %>% filter(strategy_name %in% c("Expand 2 doses", "2 doses + booster")) %>%
  select(strategy_name, name, value) %>%
  pivot_wider(names_from = strategy_name, values_from = value) %>%
  mutate(percent_reduction = (`Expand 2 doses` - `2 doses + booster`)/`Expand 2 doses` * 100)
x

############################################################
# plot additional events averted per additional vaccine dose
df_add1 <- df_summarise_totals %>%
  select(strategy_name, deaths_med, inc_med, hosp_med, total_doses_med) %>%
  pivot_longer(cols = c(inc_med, deaths_med, hosp_med, total_doses_med))

df_add1_sub <- df_add1 %>%
  filter(strategy_name == "No additional doses") %>%
  rename("value_counterfactual" = "value") %>%
  select(-c(strategy_name))

df_add1 <- df_add1 %>%
  left_join(df_add1_sub) %>%
  mutate(difference = if_else(name == "total_doses_med", value - value_counterfactual, value_counterfactual - value)) %>%
  select(name, strategy_name, difference) %>%
  pivot_wider(names_from = name, values_from = difference) %>%
  mutate(Infections = inc_med / total_doses_med * 100,
         Hospitalisations = hosp_med / total_doses_med * 100,
         Deaths = deaths_med / total_doses_med* 100) %>%
  filter(strategy_name != "No additional doses") %>%
  pivot_longer(cols = c(Infections, Hospitalisations, Deaths))

rq2_additional <- ggplot(data = df_add1, aes(x = strategy_name, y = value, fill = strategy_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name, scale = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0,
        legend.position = "none") +
  labs(x = "Dose strategy", y = "additional events averted \nper 100 additional doses", fill = "Dose strategy") +
  scale_fill_manual(values = c(col8, col6, col2))

rq2_additional
#############################################################
#########################
# combine plots
library(patchwork)
layout <- "
AAA
BCD
EEE
FFF
"
combined <- plot_doses+ nat_omicron + hosp_t + inc_t  + p_deaths_summary  + rq2_additional +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 4, design = layout)

combined
ggsave(paste0("plots/fig3.png"),combined, height = 12, width = 11)


