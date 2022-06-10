
df1 <- as.data.frame(readRDS("data/rt_out_hic.rds")) %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))
df2 <- as.data.frame(readRDS("data/rt_out_lmic.rds")) %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))
df3 <- as.data.frame(readRDS("data/rt_out_hic_zerocovid.rds")) %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))

p_Rt1 <- ggplot(data = df1, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt1


p_Rt2 <- ggplot(data = df2, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt2

p_Rt3 <- ggplot(data = df3, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "right",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt3

library(patchwork)
layout <- "
ABC
"
combined <- p_Rt1 + p_Rt2 + p_Rt3 +
  plot_annotation(tag_levels = "A") + 
  #plot_layout(guides = "collect") + 
  plot_layout(ncol = 3, design = layout)

combined
ggsave("plots/p_Rt.png", combined, height = 3, width = 9)
