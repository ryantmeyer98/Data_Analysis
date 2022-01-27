# LOAD LIBRARIES ----
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(scales)){install.packages("scales")}
if(!require(readxl)){install.packages("readxl")}
if(!require(skimr)){install.packages("skimr")}
if(!require(janitor)){install.packages("janitor")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(plotly)){install.packages("plotly")}
if(!require(colorRamps)){install.packages("colorRamps")}
if(!require(car)){install.packages("car")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(forcats)){install.packages("forcats")}
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# OBJECTIVE: GRAPH AND ANALYZE SOIL NITRATE OVER TIME BY SEASON ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# RESULTS AND SUMMARY  ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ IN AND PREP DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# read in the data
full.df <- read_csv("Data/Soils/22_oct_2021_combined_soils.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# combine year and season
full.df <- full.df %>%
  unite("year_season", year:season, remove = FALSE)

# make required variables factor
full.df <- full.df %>%
  mutate(plot = as.factor(plot),
         treatment = as.factor(treatment),
         season = as.factor(season),
         year_season = as.factor(year_season),
         block = as.factor(block),
         sample = as.factor(sample)
  )


# reorder factors 
full.df <- full.df %>%
  mutate(treatment = fct_relevel(treatment, "ref", "pc", "pc_n"),
         year_season = fct_relevel(year_season, "2019_fall", "2020_fall", "2020_spring", "2021_fall", "2021_spring"))

# long format
full_long.df <- full.df %>%
  pivot_longer(
    -c(year, season, plot, sample, treatment, block, year_season),
    names_to = "variable", values_to = "value")

# summary statistics
# long
long_summary.df <- full_long.df %>% 
  group_by(year, season, year_season, block, treatment, variable, plot) %>% 
  summarize(
    value_mean = mean(value, na.rm=TRUE),
    value_sd = sd(value, na.rm = TRUE))

# wide
# mean
wide_summary_mean.df <- long_summary.df %>%
  pivot_wider(
    id_cols = c("year", "season", "year_season", "treatment", "block", "plot"),
    names_from = variable,
    values_from = value_mean)

# standard deviation
wide_summary_sd.df <- long_summary.df %>%
  pivot_wider(
    id_cols = c("year", "season", "year_season", "treatment", "block", "plot"),
    names_from = variable,
    values_from = value_sd)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PLOT ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# all data regardless of time
graph_data.df <- long_summary.df%>%
  filter(variable %in% c("nh4n_ppm", "no3n_ppm", "om_percent_loi", "p_ppm"))

ggplot(data = graph_data.df, mapping = aes(x = treatment, value_mean, color = variable)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3))

# data over time
ggplot(data = graph_data.df, mapping = aes(x = year_season, y = value_mean, shape = variable, color = treatment)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3))

# without phosphorus or loi - i think i like this one the most NEED TO ADD BLOCK EFFECT
graph_data.df %>%
  filter(variable %in% c("nh4n_ppm", "no3n_ppm")) %>%
  ggplot(mapping = aes(x = year_season, y = value_mean, shape = variable, color = treatment)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) +
  facet_wrap(~variable, scales = "free_y")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# STATISTICS: IS THERE A SIGNIFICANT CHANGE IN NO3-N BY TREATMENT ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# factors
# block = fixed block effect
# treatment = fixed effect
# plot = random 
# time = random 



# do the average of sample to account for independence
no3n.df <- full_long.df %>%
   filter(variable == "no3n_ppm") 

no3n.df <- no3n.df %>%
  group_by(year_season, year, season, plot, block, treatment, variable) %>%
  summarize(value = mean(value)) %>%
  filter(year_season != "2019_fall")

no3n.df <- no3n.df %>%
  mutate(treatment = case_when(
         treatment == "ref" ~ "Reference",
         treatment  == "pc" ~ "Pennycress",
         treatment == "pc_n" ~ "Pennycress+N"))

no3n.df <- no3n.df %>%
  mutate(treatment = as.factor(treatment)) %>%
  mutate(treatment = fct_relevel(treatment, "Reference", "Pennycress", "Pennycress+N"))

# create model
no3n.lm <- lmer(value ~ treatment + block + (1|year_season) * (1|plot/block), 
                data = no3n.df,
                REML = TRUE)
Anova(no3n.lm, type = "3")
rand(no3n.lm)
print(VarCorr(no3n.lm), comp = "Variance")

residuals <- resid(no3n.lm)
plot(fitted(no3n.lm),residuals)
qqnorm(residuals)
qqline(residuals)

# multiple comparisons and lsmeans  HAVE TO GRAPH WITH EMM TO REMOVE BLOCK EFFECT 

no3n.emm <- emmeans(no3n.lm, ~ treatment)
results.emminteractions <- emmeans(no3n.emm, pairwise ~ treatment, adjust = "bonferroni")
results.emminteractions$contrasts
results.emminteractions$emmeans
plot(results.emminteractions, comparisons = TRUE)

no3n_emm.df <- as.data.frame(results.emminteractions$emmeans)

no3n_emm.plot <- no3n_emm.df %>%
  ggplot(aes(x = treatment)) +
  geom_point(aes(y = emmean), size = 3) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                stat = "identity", width = 0.2) +
  annotate("text", x = "Reference", y = 6, label = "A", size = 7) +
  annotate("text", x = "Pennycress", y = 3, label = "A", size = 7) +
  annotate("text", x = "Pennycress+N", y = 3, label = "A", size = 7) + 
  labs(x = "", y = expression(bold("Nitrate Nitrogen ppm emmean +/- 1 SE"))) +
  expand_limits(y = 0) +
  theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = NA))

no3n_emm.plot
ggsave(filename = "Figures/Nitrate EMM.pdf", no3n_emm.plot,
       width = 7, height = 7, units = "in")








