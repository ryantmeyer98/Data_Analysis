# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OBJECTIVE: DETERMINE HOW DIFFERENT LEVELS OF FERTILIZATION AFFECT LEMNA POPULATION GROWTH
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)
library(car)
library(lme4)
library(lmerTest)
library(nlme)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# THEME ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# create theme
theme_isco <- function(base_size = 14, base_family = "Times")
{
  theme(
    # PLOT AND LINES
    axis.line = element_line(size = 0.5, linetype = "solid"), 
    axis.ticks = element_line(colour = "black"), 
    panel.grid.major = element_line(colour = NA, linetype = "blank"),
    panel.grid.minor = element_line(colour = NA, linetype = "blank"), 
    panel.background = element_rect(fill = FALSE),
    # LEGEND
    legend.text = element_text(size = 10,  face = "bold"), 
    legend.title = element_text(size = 10, face = "bold"), 
    legend.key = element_rect(fill = NA, size = 0.9), 
    legend.background = element_rect(fill = NA, size = 0.9),
    legend.position = "bottom",
    # AXIS TEXT
    axis.text = element_text(size = 11, face = "bold", colour = "black"), 
    axis.text.x = element_text(size = 11, colour = "black", vjust = .5, hjust = .5, angle = 0), 
    axis.text.y = element_text(size = 11, colour = "black"),
    # AXIS TITLES
    axis.title = element_text(size = 12, face = "bold"),
    #FACETS
    strip.text = element_text(size = 12, face = "bold", colour = "black"),
    strip.background = element_rect(colour="white", fill="white"))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# read in wide data 
wide.df <- read_csv("Data/Other/popeco/lemna.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DATA MANIPULATIONS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# time, cup, and treatment need to be factors
wide.df <- wide.df %>%
  mutate(t = as.factor(t),
         cup = as.factor(cup),
         trt = as.factor(trt))

# ccalculate r 
wide.df <- wide.df %>%
  mutate(r = births - deaths / n)

# reproductive rate
wide.df <- wide.df %>%
  mutate(r0 = lx * births)

# # average table
# table.df <- wide.df %>%
#   group_by(trt) %>%
#   summarize(avg_n = mean(n),
#             avg_births = mean(births),
#             avd_deaths = mean(deaths),
#             avg_r = mean(r))
# 
# # convert wide to long
# long.df <- wide.df %>%
#   pivot_longer(cols = c(n, births, deaths),
#               names_to = "metric",
#               values_to = "value")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# all figures
r_by_t.plot
r_by_trt.plot
r0_t.plot
r0_trt.plot
n_t.plot
n_trt.plot


r_by_t.plot + r_by_trt.plot
r0_t.plot + r0_trt.plot
n_t.plot + n_trt.plot

# r by treatment for t = 1
r_by_trt.plot <- wide.df %>%
  filter(t !=0) %>%
  ggplot(mapping = aes(x = trt, y = r, color = t)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco() +
  labs(x = "Potassium Phosphate (mg/L)")
r_no_t0.plot

# r by time with treatment as a color
r_by_t.plot <- wide.df %>%
  filter(t != 0) %>%
  ggplot(mapping = aes(x = t, y = r, color = trt)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
r_by_t.plot  

# reproductive rate plot
r0_t.plot <- wide.df %>%
  filter(t != 0) %>%
  ggplot(aes(x = t, y = r0, color = trt)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
r0.plot
  
r0_trt.plot <- wide.df %>%
  filter(t != 0) %>%
  ggplot(aes(x = trt, y = r0, color = t)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
r0_trt.plot


# what about n
n_t.plot <- wide.df %>%
  filter(t != 0) %>%
  ggplot(aes(x = t, y = n, color = trt)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
n_t.plot

n_trt.plot <- wide.df %>%
  filter(t != 0) %>%
  ggplot(aes(x = trt, y = n, color = t)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
n_trt.plot


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STATS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# model 
one.lm <- lmer(r ~ trt + t + (1|cup),
               data = wide.df,
               REML = TRUE)
Anova(one.lm, type = "3")
ranova(one.lm)

# another way
rand.lm <- lme(r ~ trt + t, 
               random = ~1|cup,
               data = wide.df,
               method = "REML")
no_rand.lm <- gls(r ~ trt + t,
                  data = wide.df, 
                  method = "REML")
anova(rand.lm, no_rand.lm)

# both ways the random effect does not affect the model and the interaction does not affect the 
# model
final.lm <- lm(r ~ trt + t, data = wide.df)
Anova(final.lm, type = "3")

# assumptions
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)

