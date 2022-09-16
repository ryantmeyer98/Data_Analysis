# LIBRARIES ----
library(tidyverse)
library(patchwork)

# OBJECTIVES ----
# ri v cfs
# prob v cfs

# READ IN THE DATA ----

# poplar creek dataset
poplar.df <- read_csv("stream ecology/output/poplar.csv")

# PLOTTING ----

# ri v cfs
poplar.df %>%
  ggplot(mapping = aes(ri, cfs)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_classic()

# prob v cfs
poplar.df %>%
  ggplot(mapping = aes(prob, cfs)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_classic()

# what if we want to know by different time periods?
poplar.df %>%
  mutate(ymd = as.factor(ymd)) %>%
  mutate(time = case_when(
    ymd < "2000-04-21" ~ "pre_2000",
    ymd > "2000-04-21" ~ "post_2000"
  ))




