# LOAD LIBRARIES ----
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
library(car)
library(emmeans)

# READ IN THE DATA ----
full.df <- read_csv("Data/laureldata.csv") %>%
  clean_names() %>%
  remove_empty(which=c("cols","rows"))

full.df <- full.df %>%
  

# MEAN BY SEASON WITH SITE ----
season_site.df <- full.df %>%
  group_by(site, season) %>%
  summarize(mean_mp = mean(total_mp),
            mean_fiber = mean(total_fiber),
            mean_fragment = mean(total_fragment),
            mean_microbead = mean(total_microbead),
            se_mp = sd(total_mp),
            se_fiber = sd(total_fiber),
            se_fratment = sd(total_fragment),
            se_microbead = sd(total_microbead))
            
  
# MEAN BY SITE ----
site.df <- full.df %>%
  group_by(site) %>%
  summarize(mean_mp = mean(total_mp),
            mean_fiber = mean(total_fiber),
            mean_fratment = mean(total_fragment),
            mean_microbead = mean(total_microbead),
            se_mp = sd(total_mp),
            se_fiber = sd(total_fiber),
            se_fratment = sd(total_fragment),
            se_microbead = sd(total_microbead))

# MEAN BY SEASON ----
season.df <- full.df %>%
  group_by(season) %>%
  summarize(mean_mp = mean(total_mp),
            mean_fiber = mean(total_fiber),
            mean_fratment = mean(total_fragment),
            mean_microbead = mean(total_microbead),
            se_mp = sd(total_mp),
            se_fiber = sd(total_fiber),
            se_fratment = sd(total_fragment),
            se_microbead = sd(total_microbead))

write.csv(season_site.df, "Outputs/Season and Site Means.csv", row.names = FALSE)
write.csv(site.df, "Outputs/Site Means.csv", row.names = FALSE)
write.csv(season.df, "Outputs/Season Means.csv", row.names = FALSE)

# SOME GRAPHS JUST FOR FUN ----
# mp
mp.plot <- ggplot(data = full.df, mapping=aes(x = site, y = total_mp, color = site)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "avg mp")

# fibers
fiber.plot <- ggplot(data = full.df, mapping=aes(x = site, y = total_fiber, color = site)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y= "avg fiber")

# fragments
fragment.plot <- ggplot(data = full.df, mapping=aes(x = site, y = total_fragment, color = site)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "avg fragment")

# microbeads
microbead.plot <- ggplot(data = full.df, mapping=aes(x = site, y = total_microbead, color = site)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "avg microbead")

# final plot that kinda just looks cool!
fiber.plot + fragment.plot + microbead.plot + mp.plot + plot_layout(guides = "collect")

# STATS BC WHY THE HELL NOT ----
# full analysis 
options(constrasts = c("contr.sum", "contr.poly"))
mp.lm <- lm(total_mp ~ season + source + season*source, data =  full.df)
Anova(mp.lm, type = "3")

resid <- resid(mp.lm)
plot(fitted(mp.lm), resid)

qqnorm(resid)
qqline(resid)

par(mfrow=c(2,2))
plot(mp.lm)


mp.emm <- emmeans(mp.lm, ~ season)
results.emminteraction <- emmeans(mp.lm, pairwise ~ season, adjust = "bonferroni")
results.emminteraction$contrasts
results.emminteraction$emmeans
