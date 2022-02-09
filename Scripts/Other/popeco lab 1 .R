# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ IN DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# reads in the data from a specific excel sheet
full.df <- read_excel("Data/Other/popecolab11.xlsx", sheet = "long") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# need to change the name of only lambda and case when removes other column so here is the
# workaround I figures out
# create a dataframe of only lambda
lambda.df <- full.df %>%
  filter(metric == "lambda")

# create a data frame with enverything else
full.df <- full.df %>%
  filter(metric != "lambda")

# change the name of lambda to what I want it to be
lambda.df <- lambda.df %>%
  mutate(metric = case_when(
    metric == "lambda" ~ "final_lambda"
  ))

# put the dataframes back together
full.df <- bind_rows(full.df, lambda.df)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# MAKE SOME PLOTS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

full.df %>%
  ggplot(aes(x = age_class, y = value, color = model, shape = metric)) +
  geom_point() +
  labs(x = "Age Class", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA))
