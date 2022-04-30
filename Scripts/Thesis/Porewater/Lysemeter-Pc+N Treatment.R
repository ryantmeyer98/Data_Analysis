# LOAD LIBRARIES ----
library(readxl)
library(effects)
library(tidyverse) 
library(car)
library(multcompView)
library(nlme)
library(emmeans)
library(multcomp)
library(lubridate)
library(broom)
library(tidyr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  SOIL POREWATER ANALYSIS REMOVING THE EFFECT OF PC+N DUE TO APPLICATION ISSUES CAUSING HUGE SE----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Results:
# Reject the null hypothesis,  t-ratio=4.7, df=4, p-value=0.009
# There is a difference between pc and reference
# treatment   emmean   SE  df   lower.CL  upper.CL
# Reference    19.6   1.38  5    16.07     23.2
# Pennycress   10.4   1.40  4     6.51     14.3

# No significant block effect 
# Pennycress reduces N in soil porewater by almost 50% 




# READ IN THE DATA ----
no3.df <- read_excel("Data/Lysimeter/combined lysimiter pore water.xlsx")

# DATA MUTATIONS ----
# Create treatment columns
no3.df <- no3.df %>% 
  mutate(treatment = case_when(
    plot == 1 ~ "Pennycress",
    plot == 2 ~ "Reference",
    plot == 3 ~ "Pennycress_N",
    plot == 4 ~ "Pennycress",
    plot == 5 ~ "Pennycress_N",
    plot == 6 ~ "Reference",
    plot == 7 ~ "Reference",
    plot == 8 ~ "Pennycress",
    plot == 9 ~ "Pennycress_N"
  )) 

# create month and year column
no3.df <- no3.df %>% 
  mutate(year = year(date),
         month = month(date))

# remove and arrange stuff
no3.df <- no3.df %>% 
  filter(type =="lysimeter") %>% 
  filter(year != 2020 & month >= 3) %>% 
  arrange(month, treatment, plot, block) 

# make factors
no3.df <- no3.df %>% 
  mutate( 
    treatment = as.factor(treatment),
    plot=as.factor(plot),
    month=as.factor(month),
    date = as_date(date),
    treatment = as.factor(treatment),
    block = as.factor(block)
  ) 

# reorder factors
no3.df <- no3.df %>% 
  mutate(treatment = fct_relevel(treatment, "Reference", "Pennycress", "Pennycress_N" )) 

# summarize as means
no3.df <- no3.df %>% 
  ungroup() %>% 
  group_by(date, plot, treatment, block) %>% 
  summarize (
    porewater_no3_mgl = mean(porewater_no3_mgl, na.rm=TRUE),
    porewater_nh3_mgl = mean(porewater_nh3_mgl, na.rm=TRUE),
    porewater_drp_ugl = mean(porewater_drp_ugl, na.rm=TRUE),
  ) %>% 
  arrange(date, plot)

# remove pc+n treatment
no3.df <- no3.df %>%
  filter(treatment != "Pennycress_N")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  PLOTTING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

no3.df %>% 
  ggplot(aes(date, porewater_no3_mgl, color=treatment)) +
  geom_point() +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 3, size=.7,
    position = position_dodge2(width = 3)) +
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 5,
    position = position_dodge2(width = 3))

# ME MAKE PLOT ---
smooth.plot <- ggplot(data = no3.df, mapping = aes(x = date, y = porewater_no3_mgl, color = treatment)) +
  geom_smooth(se = FALSE) +
  geom_point(size = 3, position = position_dodge2(width = 0.7), shape = 5) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 3, size = 0.7,
    position = position_dodge2(width = 3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5, 
    position = position_dodge2(width = 3)) +
  scale_y_continuous(limits = c(0,40)) +
  labs(y = "Porewater Nitrate Nitrogen (mgL)") + theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.background = element_rect(fill = NA)) + theme(legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA), 
    legend.position = "NULL") +
  scale_color_manual(values = c("tan4", "forestgreen")) 
smooth.plot

ggplot(data = no3.df, mapping = aes(x = treatment, y = porewater_no3_mgl, color = treatment)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 3, size = 0.4,
    position = position_dodge2(width = 3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 5, 
    position = position_dodge2(width = 3))


# my brain broke at this one
# EFFECT OF BLOCK ----
with(no3.df, interaction.plot(date, plot, porewater_no3_mgl))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  STATISTICAL ANALYSIS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# create linear model
lme_fitno3.block <- lme(fixed = porewater_no3_mgl ~  treatment * date ,  
                        random = ~ 1 | plot/block, 
                        method='REML',
                        corr = corAR1( form= ~1 | plot/block),
                        data = no3.df)
# equal variance
plot(lme_fitno3.block)

# normality
qqnorm(resid(lme_fitno3.block))
qqline(resid(lme_fitno3.block))

# analysis
Anova(lme_fitno3.block, type="III")

# without block
lme_fitno3.noblock <- lme(fixed = porewater_no3_mgl ~  treatment * date ,  
                        random = ~ 1 | plot, 
                        method='REML',
                        corr = corAR1( form= ~1 | plot),
                        data = no3.df)
Anova(lme_fitno3.noblock, type = "3")

# no effect of block, just gonna leave it in to be safe

# treatment effects
emmeans(lme_fitno3.block, pairwise ~ treatment, adjust="bonferroni")

marginal = emmeans(lme_fitno3.block, 
                   ~ treatment*date)

plot(marginal, comparisons = TRUE)

# treatment by date interaction 
emminteraction = emmeans(lme_fitno3.block, 
                         pairwise ~ treatment*date,
                         adjust="bonferroni",
                         alpha=0.5)
emminteraction$contrasts

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FINAL PLOT ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

no3_lsmeans.df <- as.data.frame(lsmeans(lme_fitno3.block, eff ~ treatment*date)$lsmeans)

# The final Plot -----
no3_lsmeans.df %>% 
  ggplot(aes(date, lsmean, color=treatment, group=treatment)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lsmean-SE, ymax = lsmean+SE), 
                stat="identity", width = 0.2) 

no3_lsmeans.df %>% 
  ggplot(aes(date, lsmean, color=treatment, group=treatment)) +
  geom_point() +
  geom_line() +
  geom_pointrange(aes(ymin = lower.CL,
                      ymax = upper.CL))

  ggplot(data = all_data, mapping = aes(x = Condition, y = Support)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3, size=.7) +
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 5)


season.plot <- no3.df %>% 
  ggplot(aes(treatment, porewater_no3_mgl, color=treatment)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3, size=.7) +
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 5) +
  expand_limits(y=0) + theme(axis.line = element_line(linetype = "solid"),
    panel.background = element_rect(fill = NA)) +labs(y = "Soil Porewater No3(mg/L)", colour = "treatment") +
  labs(x = "") +
  scale_y_continuous(limits = c(0,40)) +
  annotate("text", x = "Reference", y = 16, label = "A", size = 7) +
  annotate("text", x = "Pennycress", y = 6, label = "B", size = 7) + 
  labs(y = "Porewater Nitrate Nitrogen (mgL)") +
  theme(axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("tan4", "forestgreen"))
season.plot
no3.df %>% 
  ggplot(aes(treatment, porewater_no3_mgl, color=treatment)) + 
  geom_blank() + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA)) +
  labs(y = "Soil Porewater No3(mg/L)", colour = "Treatment") +
  labs(x = "Treatment") +
  scale_color_manual(values = c("tan4", "forestgreen"))

# patchwork ----
library(patchwork)
smooth.plot + season.plot

full.plot <- smooth.plot + season.plot +
  plot_layout(ncol=2, guides = "collect", widths = c(4,1)) + theme(axis.title.y = element_blank(), 
                                                  axis.text.y = element_blank(),
                                                  axis.line.y = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  axis.text.x = element_blank()
                                                  )
                                                  
full.plot
ggsave(filename = "Figures/Porewater Final Plot.pdf", full.plot,
       width = 7, height = 7, units = "in")

ggsave(filename = "Figures/Nitrate EMM.pdf", no3n_emm.plot,
       width = 9, height = 7, units = "in")





