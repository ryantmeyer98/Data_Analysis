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
library(knitr)
library(emmeans)
library(multcomp)

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

# r test
wide.df <- wide.df %>%
  mutate(tc = xlxbx / lxbx) %>%
  mutate(r = log(r0) / tc) %>%
  mutate(lambda = exp(r))

# average table
table.df <- wide.df %>%
  group_by(trt) %>%
  summarize(avg_n = mean(n),
            avg_births = mean(births),
            avg_deaths = mean(deaths),
            avg_r = mean(r, na.rm = TRUE),
            avg_r0 = mean(r0),
            avg_lambda = mean(lambda, na.rm = TRUE))

table_two.df <- wide.df %>%
  group_by(t) %>%
  filter(t != 0) %>%
  summarize(avg_n = mean(n),
            avg_births = mean(births),
            avg_deaths = mean(deaths),
            avg_r = mean(r, na.rm = TRUE),
            avg_r0 = mean(r0),
            avg_lambda = mean(lambda, na.rm = TRUE))

table.df <- table.df %>%
  rename(average_population_size = avg_n) %>%
  rename(average_births = avg_births) %>%
  rename(average_r = avg_r) %>%
  rename(average_R0 = avg_r0) %>%
  rename(treatment = trt)

library(gridExtra)
grid.table(table_two.df)
table.df

# convert wide to long
long.df <- wide.df %>%
  pivot_longer(cols = c(n, births, deaths),
              names_to = "metric",
              values_to = "value")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# final plots
final1.plot <- wide.df %>%
  filter(t !=0) %>%
  ggplot(mapping = aes(x = trt, y = lambda)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  facet_wrap(~t) + 
  labs(x = "Added Potassium Phosphate (mg/L)", y = "Lambda") +
  theme_light()
final1.plot

final2.plot <- wide.df %>%
  filter(t !=0) %>%
  ggplot(mapping = aes(x = trt, y = r0)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  labs(x = "Added Potassium Phosphate (mg/L)", y= "Net Reproductive Rate R0") +
  facet_wrap(~t) + 
  theme_light()

final3.plot <- wide.df %>%
  filter(t !=0) %>%
  ggplot(mapping = aes(x = t, y = lambda)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  facet_wrap(~trt) + 
  labs(x = "Time", y = "Lambda") +
  theme_light()

final4.plot <- wide.df %>%
  filter(t !=0) %>%
  ggplot(mapping = aes(x = t, y = r0)) +
  geom_point(size = 2, position = position_dodge2(width = 0.3), shape = 5) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5,
    position = position_dodge(width = 0.3)) +
  labs(x = "Time", y= "Net Reproductive Rate R0") +
  facet_wrap(~trt) + 
  theme_light()

final1.plot + final2.plot + plot_layout(ncol = 2)
final3.plot + final4.plot
         
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STATS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# does treatment effect r?
# model 
one.lm <- lmer(lambda ~ trt + t + (1|cup),
               data = test.df,
               REML = TRUE)
Anova(one.lm, type = "3")
ranova(one.lm)

# assumptions
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# another way
rand.lm <- lme(r ~ trt + t + t * trt, 
               random = ~1|cup,
               data = wide.df,
               method = "REML")
no_rand.lm <- gls(r ~ trt + t,
                  data = wide.df, 
                  method = "REML")
anova(rand.lm, no_rand.lm)
Anova(rand.lm, type = "3")

# both ways the random effect does not affect the model and the interaction does not affect the 
# model
final.lm <- lm(r ~ trt + t, data = wide.df)
Anova(final.lm, type = "3")

# assumptions
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)

# post-f tests
one.emm <- emmeans(one.lm, pairwise ~ t, adjust = "bonferroni")
plot(one.emm, comparisons = TRUE) + theme_classic()
emminteraction = emmeans(one.lm, pairwise ~ t, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(one.emm, Letters = letters)


# does treatment effect r0?
# model 
test.df <- wide.df %>%
  filter(t != 0)
two.lm <- lmer(r0 ~ trt + t + (1|cup),
               data = test.df,
               REML = TRUE)
Anova(two.lm, type = "3")
ranova(two.lm)

residuals <- resid(two.lm)
plot(fitted(two.lm), residuals)
qqnorm(residuals)
qqline(residuals)

two.emm <- emmeans(two.lm, pairwise ~ t, adjust = "bonferroni")
plot(two.emm, comparisons = TRUE) + theme_classic()
emminteraction = emmeans(two.lm, pairwise ~ t, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(one.emm, Letters = letters)
