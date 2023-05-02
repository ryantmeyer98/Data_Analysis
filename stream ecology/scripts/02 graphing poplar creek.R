# LIBRARIES ----
library(tidyverse)
library(patchwork)
library(lubridate)

# OBJECTIVES ----
# ri v cfs
# prob v cfs

# READ IN THE DATA ----

# poplar creek dataset
poplar.df <- read_csv("stream ecology/output/poplar.csv")

# PLOTTING ----

# ri v cfs
ri.plot <- poplar.df %>%
  ggplot(mapping = aes(ri, cfs)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  stat_smooth(method= lm, fullrange = TRUE, se = FALSE, color = "blue") +
  expand_limits(x = 100) +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 250)) +
  labs(x = "Recurrence Interval", y = "Discharge ") +
  theme_classic() 

ri.plot

ggsave(ri.plot, file = "stream ecology/output/ri by q.pdf")

#  plot for divided data set homework questions
poplar.df %>%
  ggplot(mapping = aes(ri, cfs, shape = time)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  stat_smooth(method= lm, fullrange = TRUE, se = FALSE, color = "blue") +
  expand_limits(x = 100) +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 250)) +
  labs(x = "Recurrence Interval", y = "Discharge ") +
  theme_classic() 

# prob v cfs
poplar.df %>%
  ggplot(mapping = aes(prob, cfs)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_classic()

# what if we want to know by different time periods?
poplar.df <- poplar.df %>%
  mutate(time = case_when(
    ymd <= ymd("1985-11-20") ~ "pre_1985",
    ymd >= ymd("1987-08-27") ~ "post_1985"
  ))

poplar.df %>%
  ggplot(mapping = aes(ri, cfs, color = time)) +
  geom_jitter() +
  geom_smooth(se = FALSE) +
  theme_classic()

poplar.df %>%
  ggplot(mapping = aes(prob, cfs , color = time)) +
  geom_jitter() +
  geom_smooth(se = FALSE) +
  theme_classic()

poplar.df %>%
  ggplot(mapping = aes(ymd, cfs, color = time)) +
  geom_jitter() +
  geom_smooth(se = FALSE) +
  theme_classic()

poplar.df %>%
  mutate(time = as.factor(time)) %>%
  mutate(time = fct_relevel(time, "pre_1985", "post_1985")) %>%
  ggplot(mapping = aes(time, cfs, shape = time)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.1) +
  geom_point(position = position_dodge2(width = 0.3)) +
  scale_shape_manual(name = "Time Period",
                     label = c("Pre 1985", "Post 1985"),
                     values = c(1, 2)) +
  labs(x = "Time Period", y = "Discharge (cfs)") +
  scale_x_discrete(labels = c("Pre 1985", "Post 1985")) +
  theme_classic()

# lets save the plots we are really interested in 
ggsave()


