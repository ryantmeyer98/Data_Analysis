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

full.df <- full.df %>%
  mutate(age_class = as.factor(age_class))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# MAKE SOME PLOTS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

full.plot <- full.df %>%
  ggplot(aes(x = age_class, y = value, color = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Age Class", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA))
full.plot

stable_distribution.plot <- full.df %>%
  filter(metric == "stable_distribution") %>%
  ggplot(aes(x = age_class, y = value, color = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Age Class", y = "Stable Distribution") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))


final_distribution.plot <- full.df %>%
  filter(metric == "final_distribution") %>%
  ggplot(aes(x = age_class, y = value, color = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Age Class", y = "Final Distribution") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

reproductive_value.plot <- full.df %>%
  filter(metric == "reproductive_value") %>%
  ggplot(aes(x = age_class, y = value, color = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Age Class", y = "Reproductive Value") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))


lambda.plot <- full.df %>%
  filter(metric == "final_lambda") %>%
  ggplot(aes(x = age_class, y = value, color = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Age Class", y = "Final Lambda") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

final_distribution.plot
lambda.plot
reproductive_value.plot
stable_distribution.plot

plot.plot <- final_distribution.plot + lambda.plot + 
  reproductive_value.plot + stable_distribution.plot + plot_layout(guides = "collect")
plot.plot

full.df %>%
  ggplot(aes(x = model, y = value, color = age_class, shape = metric)) +
  geom_point() +
  geom_line()
  
stable_distribution_model.plot <- full.df %>%
  filter(metric == "stable_distribution") %>%
  ggplot(aes(x = model, y = value, color = age_class, shape = metric)) +
  geom_line() +
  geom_point() +
  labs(x = "Model", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

final_distribution_model.plot <- full.df %>%
  filter(metric == "final_distribution") %>%
  ggplot(aes(x = model, y = value, color = age_class, shape = metric)) +
  geom_line() +
  geom_point() +
  labs(x = "Model", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

reproductive_value_model.plot <- full.df %>%
  filter(metric == "reproductive_value") %>%
  ggplot(aes(x = model, y = value, color = age_class, shape = metric)) +
  geom_line() +
  geom_point() +
  labs(x = "Model", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

lambda_model.plot <- full.df %>%
  filter(metric == "final_lambda") %>%
  ggplot(aes(x = model, y = value, color = age_class, shape = metric)) +
  geom_line() +
  geom_point() +
  labs(x = "Model", y = "Value") + 
  theme(axis.line = element_line(linetype = "solid"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA))

stable_distribution_model.plot + final_distribution_model.plot +
reproductive_value_model.plot + lambda_model.plot
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# MAKE WIDE AND DO PLOTS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# make wide

