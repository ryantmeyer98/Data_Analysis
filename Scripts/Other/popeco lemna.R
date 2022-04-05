# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OBJECTIVE: Determine how additions of potassium phosphate affect phosphorus uptake and 
# growth rate of lemna
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
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# read in wide data 
wide.df <- read_csv("Data/Other/popeco/lemna.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

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

# average table
table.df <- wide.df %>%
  group_by(trt) %>%
  summarize(avg_n = mean(n),
            avg_births = mean(births),
            avd_deaths = mean(deaths),
            avg_r = mean(r))

# convert wide to long
long.df <- wide.df %>%
  pivot_longer(cols = c(n, births, deaths),
              names_to = "metric",
              values_to = "value")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# by treatment
rt.plot <- ggplot(data = wide.df, mapping = aes(x = trt, y = r, color = t)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3))
rt.plot

# r by treatment for t = 1
r_no_t0.plot <- wide.df %>%
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
r_by_t.plot <- ggplot(data = wide.df, mapping = aes(x = t, y = r, color = trt)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  theme_isco()
r_by_t.plot  

r_no_t0.plot + r_by_t.plot

# SOME STATS ----
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
plot(final.lm)
qqnorm(resid(final.lm))
qqline(resid(final.lm))

