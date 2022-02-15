# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PLAYING WITH EMILY'S DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load Libraries
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

wide.df <- read_csv("Data/Other/emily_harders/Emily_Rfuntime_7Feb22.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

long.df <- wide.df %>%
  pivot_longer(cols = c(ends_with("normalized")),
               names_to = "name",
               values_to = "value")

long.df <- long.df %>%
  mutate(sampling_day = as.factor(sampling_day))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SOME SIMPLE GRAPHS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

full.plot <- long.df %>%
  ggplot(aes(x = treatment, y = value, color = name, shape = sampling_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) 

trt.plot <- long.df %>%
  ggplot(aes(x = treatment, y = value, color = name)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3))

day.plot <- long.df %>%
  ggplot(aes(x = treatment, y = value, color = sampling_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.3)) 

long.df %>%
  ggplot(aes(x = treatment, y = value)) +
  geom_violin() +
  geom_point()

trt.plot + day.plot + plot_layout(ncol = 2)
