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
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

# READ IN THE DATA ----
full.df <- read_csv("Data/project_data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# VIASUALIZE DATA ----
# drift by sample group by salt slug and control

full.df <- full.df %>%
  mutate(treatment = case_when(
    sample == "ns1" ~ "control",
    sample == "ns2" ~ "control",
    sample == "ns3" ~ "control",
    sample == "ns4" ~ "control",
    sample == "ss1" ~ "salt slug",
    sample == "ss2" ~ "salt slug",
    sample == "ss3" ~ "salt slug",
    sample == "ss4" ~ "salt slug",
    sample == "rep" ~ "representative",
    family == "baet" ~ "baetidae"
  ))

full.df <- full.df %>%
  rename(drift = abundance)

plot1.df <- full.df %>%
  filter(sample != "rep")

# by treatment
treatment.plot <- ggplot(data = full.df, mapping = aes(x = treatment, y = drift, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) +
  labs(y = "Total Macroinvertebrate Drift") +
  labs(x = "Treatment") + 
    theme(axis.line = element_line(linetype = "solid"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA))

# family with color treatment
family.plot <- ggplot(data = plot1.df, mapping = aes(x = family, y = drift, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  labs(y = "Total Macroinvertebrate Drift")

  

# family with order treatment
order.plot <- ggplot(data = plot1.df, mapping = aes(x = order, y = drift, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) +
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  labs(y = "Total Macroinvertebrate Drift")

treatment.plot
order.plot
family.plot

# now, we have a reason to look solely at baetidae, so lets do that
baetidae.df <- plot1.df %>%
  filter(family == "baetidae")

baetidae.plot <- ggplot(data = baetidae.df, mapping = aes(x = treatment, y = drift, color = treatment)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) +
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +
  labs(y = "Total Baetidae Drift")
  
baetidae.plot

ggsave(filename = "Figures/Ent Project/Treatment Plot.pdf", 
       treatment.plot, width = 4, height = 4, units = "in")

ggsave(filename = "Figures/Ent Project/Order Plot.pdf",
       order.plot, width = 4, height = 4, units = "in")

ggsave(filename = "Figures/Ent Project/Family Plot.pdf",
       family.plot, width = 4, height = 4, units = "in")

ggsave(filename = "Figures/Ent Project/Baetidae Plot.pdf",
       baetidae.plot, width = 4, height = 4, units = "in")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FINAL MODEL ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

norep.df <- full.df %>%
  filter(treatment != "rep") %>%
  filter(sample != "rep")

norep.df <- norep.df %>%
  mutate(logdrift = log(drift))

norep.lm <- lmer(drift ~ treatment + (1|order:family) + (1|sample), 
                 data = norep.df,
                 REML = TRUE)

Anova(norep.lm, type = "3")

residuals <- resid(norep.lm)
plot(fitted(norep.lm),residuals)
qqnorm(residuals)
qqline(residuals)

# log transformation
norep.df <- norep.df %>%
  mutate(logdrift = log(drift))

noreplog.lm <- lmer(logdrift ~ treatment + (1|order:family) * (1|sample), 
                    data = norep.df,
                    REML = TRUE)

Anova(noreplog.lm, type = "3")

residuals1 <- resid(noreplog.lm)
plot(fitted(noreplog.lm),residuals1)
qqnorm(residuals1)
qqline(residuals1)

rand(noreplog.lm)

# random effects 
# There is a significant amount of variation explain by order family effect
# log lik order family effect 39.872 - 43.964 = 4.092 > 3.841 = p-value < 0.05

# there is not a significant amount of variation in the model explained by sample 
# 39.872 - 41.146 = 1.274 < 3.841 = p-value > 0.05

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# POST-HOC ANALYSES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

final.emm <- emmeans(noreplog.lm, ~ treatment)
results.emminteractions <- emmeans(final.emm, pairwise ~ treatment, adjust = "bonferroni")
results.emminteractions$contrasts
results.emminteractions$emmeans
plot(results.emminteractions, comparisons = TRUE)

# RESULTS ----
# Fail to reject the null hypothesis, there is no difference in pairwise comparisons.
# The salt slug has no effect on total macroinvertebrate drift (Chi=0.0687, df=1, p-value=0.8)

# There is a significant amount of variation explain by order family effect
# log lik order family effect 39.872 - 43.964 = 4.092 > 3.841 = p-value < 0.05

# there is not a significant amount of variation in the model explained by sample 
# 39.872 - 41.146 = 1.274 < 3.841 = p-value > 0.05

# FINAL PLOT ----

noreplog_emm.df <- as.data.frame(noreplog.emm)

noreplog_emm.plot <- noreplog_emm.df %>%
  ggplot(aes(x = treatment)) +
  geom_point(aes(y = emmean), size = 3) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                stat = "identity", width = 0.2) +
  annotate("text", x = "control", y = 2.0, label = "A", size = 7) +
  annotate("text", x = "salt slug", y = 2.2, label = "A", size = 7) +
  annotate("text", x = "control", y = 2.3, label = "p-value=0.8", size = 5) +
  labs(x = "", y = expression(bold("Log Transformed Total M.I. Drift emmean +/- 1 SE"))) +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14),
        panel.background = element_rect(fill = NA))

noreplog_emm.plot

ggsave(filename = "Figures/Ent Project/Final Plot.pdf",
       noreplog_emm.plot, width = 4, height = 4, units = "in")


# are there differences by family does drift differ by family?

test3.lm <- lmer(logdrift ~ treatment * family + (1|sample), 
                    data = norep.df,
                    REML = TRUE)
Anova(test3.lm, type = "3")


test3.emm <- emmeans(test3.lm, ~ treatment * family)
results.emminteractions1 <- emmeans(test3.emm, pairwise ~ treatment * family, adjust = "bonferroni")
results.emminteractions1$contrasts
results.emminteractions1$emmeans
plot(results.emminteractions, comparisons = TRUE)

test4.lm <- lm(logdrift ~ treatment * order, data = norep.df)
Anova(test4.lm, type = "3")


citation("lme4")

summary(norep.lm)
