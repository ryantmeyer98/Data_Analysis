# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(dplyr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ IN DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

full.df <- read_excel("Data/Other/popeco_lab_two.xlsx", sheet = "Sheet2") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# GRAPH IT UP ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

r0.plot <- full.df %>%
  filter(metric == "R0") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "R0") +
  theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

hours.plot <- full.df %>%
  filter(metric == "hours") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "Hours") +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

final_percent_infected.plot <- full.df %>%
  filter(metric == "final_percent_infected") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "Final Percent Infected") +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

peak_infection_rate.plot <- full.df %>%
  filter(metric == "peak_infection_rate") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "Peak Infection Rate") +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

peak_recovery_rate.plot <- full.df %>%
  filter(metric == "peak_recovery_rate") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "Peak Recovery Rate") +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

still_susceptible.plot <- full.df %>%
  filter(metric == "still_susceptible") %>%
  ggplot(aes(x = community, y = value, color = community)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) + 
  labs(x = "", y = "Still Susceptible") +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

r0.plot + hours.plot + final_percent_infected.plot + 
peak_infection_rate.plot + peak_recovery_rate.plot + 
still_susceptible.plot + plot_layout(guides = "collect")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# ANALYSIS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# doing a manova for this
full.aov <- manova(cbind(hours, R0) ~ community, data = full.df)





