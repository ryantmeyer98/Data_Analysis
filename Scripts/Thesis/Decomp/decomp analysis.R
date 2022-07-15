# LIBRARIES ----
library(tidyverse)
library(janitor)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcompView)
library(multcomp)
library(patchwork)

# READ IN THE DATA ----

# other data
no_data.df <- read.csv("Data/Decomp/n no data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# c and n data
n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# biomass data
biomass.df <- read.csv("Data/Decomp/biomass.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# PREP TO JOIN ----

# rename to id
no_data.df <- no_data.df %>%
  rename(id = bag)

# rename to id
n_data.df <- n_data.df %>%
  rename(id = sample)

# rename to id
biomass.df <- biomass.df %>%
  rename(id = bag_no)

# set biomass id to integer
biomass.df <- biomass.df %>%
  mutate(id = as.integer(id))

# remove species from biomass df 
biomass.df <- biomass.df %>%
  select(-spp)

# JOIN FILES ----

# create full data frame for n data
full.df <- full_join(n_data.df, no_data.df, by = "id")

# add biomass data 
full.df <- full_join(biomass.df, full.df, by =  "id")

# get cci df
cci.df <- full.df %>%
  filter(study == "cci")

# get decomp df
decomp.df <- full.df %>%
  filter(study == "decomp")

# set days to a factor
decomp.df <- decomp.df %>%
  mutate(days = as.factor(days))

# CALCULATE C:N RATIO ----

# cci 
# calculate total c and total n in sample
cci.df <- cci.df %>%
  mutate(total_c_g = percent_c * coll_wt_g) %>%
  mutate(total_n_g = percent_n * coll_wt_g)

# calculate C:N ratio
cci.df <- cci.df %>%
  mutate(c_n_ratio = total_c_g / total_n_g)

# decomp 
decomp.df <- decomp.df %>%
  mutate(total_c_g = percent_c * coll_wt_g) %>%
  mutate(total_n_g = percent_n * coll_wt_g)

# calculate C:N ratio
decomp.df <- decomp.df %>%
  mutate(c_n_ratio = total_c_g / total_n_g)

# remove t=9 because we did not send them the data for t=9
decomp.df <- decomp.df %>%
  rename(t = y_m_d)

decomp.df <- decomp.df %>%
  filter(t != 9)

# SUMMARY FIGURE (C:N ratio, %N, %C) ---- 
c_n.plot <- decomp.df %>%
  ggplot(aes(x = days, y = c_n_ratio, color = spp)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 2, size=.7) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "", y = "Carbon to Nitrogen Ratio") +
  theme_classic()
c_n.plot

n.plot <- decomp.df %>%
  ggplot(aes(x = days, y = percent_n, color = spp)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 2, size=.7) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "", y = "Percent Nitrogen Content") +
  theme_classic()
n.plot

c.plot <- decomp.df %>%
  ggplot(aes(x = days, y = percent_c, color = spp)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 2,size=.7) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "Days After Placement", y = "Percent Carbon Content") +
  theme_classic()
c.plot

summary.plot <- c_n.plot + c.plot + n.plot + plot_layout(guides = "collect", ncol = 3)
summary.plot

# STATISTICAL ANALYSIS (C:N ratio) ----

# steps
# prepare data, determine fixed/random effects, make model, validate model and check assumptions
# run model, do post-F tests

# The final graph will likely have carbon to nitrogen ration on the x axis and time on the y
# like so
c_n.plot

# dependent variable: carbon to nitrogen ratio
# fixed effects will be: time, species
# random effects: plot, subplot (potentially subplot nested within plot?)

# model
one.lm <- lmer(c_n_ratio ~ spp * days + (1|plot) * (1|subplot),
                 data = decomp.df,
                 REML = TRUE)

# only plot
two.lm <- lmer(c_n_ratio ~ spp * days + (1|plot),
               data = decomp.df,
               REML = TRUE)

# only subplot
three.lm <- lmer(c_n_ratio ~ spp * days + (1|subplot),
                 data = decomp.df,
                 REML = TRUE)

# model selection
anova(one.lm, two.lm, three.lm)

# final model
final.lm <- lmer(c_n_ratio ~ spp * days + (1|plot),
                 data = decomp.df,
                 REML = TRUE)

# bases on AIC and BIC model two with only plot explains the most variation in the data so
# so I will use model two 

# check assumptions
residuals <- resid(final.lm)
plot(fitted(final.lm), residuals)
qqnorm(residuals)

# could be better but we meet the assumptions of normal rediduals and homogenous variance

# run model
Anova(final.lm)
anova(final.lm)

# Reject the null hypothesis, the interaction between species and days after placement 
# has a significant effect on the carbon to nitrogen ratio - analyze post F with emmeans

# emmeans - spp
spp.emm <- emmeans(final.lm, pairwise ~ spp, adjust = "bonferroni")

# plot
plot(spp.emm, comparisons = TRUE) + theme_classic()

# pairwise comparisons
emminteraction = emmeans(final.lm, pairwise ~ spp, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(spp.emm, Letters = letters)

# EMM PLOT FOR C:N ~ SPP ----

decomp.df %>%
  ggplot(aes(x = spp, y = c_n_ratio)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  expand_limits(y = 15) +
  annotate("text", x = "ar", y = 17, label = "A", size = 7) +
  annotate("text", x = "pc", y = 17, label = "A", size = 7) +
  annotate("text", x = "cr", y = 17, label = "B", size = 7) +
  annotate("text", x = "gm", y = 17, label = "B", size = 7) +
  theme_classic()

# emmeans - days
days.emm <- emmeans(final.lm, pairwise ~ days, adjust = "bonderroni")

# plot
plot(days.emm, comparisons = TRUE) + theme_classic()

# pairwise comparisons
emminteraction = emmeans(final.lm, pairwise ~ days, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(days.emm)
pairs(days.emm)

# RESULTS ----
# SPP
# gm + cr = group 3, pc = group 2, ar = group 1
# DAYS
# significant differences between all groups

# STATISTICAL ANALYSIS (PERCENT N) ----

# model will be the same as above, just changing the dependent variable from C:N to %N

# dependent variable: carbon to nitrogen ratio
# fixed effects will be: time, species
# random effects: plot, subplot (potentially subplot nested within plot?)

# model
four.lm <- lmer(percent_n ~ spp * days + (1|plot) * (1|subplot),
               data = decomp.df,
               REML = TRUE)

# only plot
five.lm <- lmer(percent_n ~ spp * days + (1|plot),
               data = decomp.df,
               REML = TRUE)

# only subplot
six.lm <- lmer(percent_n ~ spp * days + (1|subplot),
                 data = decomp.df,
                 REML = TRUE)

# model selection
anova(four.lm, five.lm, six.lm)

# interesting results from model selection, six has lowest AIC, but five has lowest BIC
# for now going with six that has the lowest AIC with the random effect of subplot

# check assumptions
residuals <- resid(six.lm)
plot(fitted(six.lm), residuals)
qqnorm(residuals)

# variance could be good, but is acceptable and residuals look fine


# run model
Anova(six.lm)
anova(six.lm)

# model is showing there is no interaction between spp and days, still going to use emmeans 
# for post-f tests because that is what I know how to do best

# emmeans - spp
spp_n.emm <- emmeans(six.lm, pairwise ~ spp, adjust = "bonferroni")

# plot
plot(spp_n.emm, comparisons = TRUE) + theme_classic()

# pairwise comparisons
emminteraction = emmeans(final.lm, pairwise ~ spp, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(spp_n.emm)

# emmeans - days
days_n.emm <- emmeans(final.lm, pairwise ~ days, adjust = "bonderroni")

# plot
plot(days_n.emm, comparisons = TRUE) + theme_classic()

# pairwise comparisons
emminteraction = emmeans(final.lm, pairwise ~ days, adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts
cld(days.emm)
pairs(days.emm)


# LAST THINGS I WANT TO WORK IN ----
# do we want to work in the remaining biomass into this analysis? It could be a continuous
# covairate maybe? just do an ANCOVA?

