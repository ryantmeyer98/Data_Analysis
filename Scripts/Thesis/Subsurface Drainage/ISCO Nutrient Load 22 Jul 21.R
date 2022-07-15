# LOAD LIBRARIES ----
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
library(car)
library(emmeans)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# SCIENTIFIC NOTATION AND FACET CAPITALIZATION ----
# turn off sci notation 
options(scipen=999)

# capitalize the facets
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string}

# CREATE REUSABLE THEME FOR GRAPHS CALLED ISCO ----
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# LOAD IN AND CLEAN DATA  ----
# nutrient data 
# load in the nutrient data and save as a data frame
isco_nutrient.df <- read_csv("Data/ISCO/ISCO Nutrients/Nutrient July 22 CSV.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# water data comes from multiple files so it takes a but more work, read it in here
# get file path and file list
data_path <- "Data/ISCO/ISCO Water/"
files <- dir(data_path, pattern = "*.csv")

# read in the water data 
isco.df <- tibble(filename = files) %>% 
  mutate(contents = map(filename, ~read.csv(file.path(data_path, .x), encoding = "UTF-8", skipNul = TRUE, header=FALSE)),
         plot = map_chr(contents, ~.x[[1,2]]),
         header_line = map(contents, ~.x[2, ]),
         data_contents = map2(contents, header_line, ~slice(.x, -c(1:12)) %>% 
                                set_names(.y)))  %>% 
  select(-contents, -header_line, -filename) %>%
  unnest(data_contents) %>% 
  rename(datetime = `Isco Quantity`, w_velocity_ms = Velocity, w_level_m = Level, bottle = `Sample Event`) %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  mutate(bottle = as.numeric(bottle),
         w_level_m = as.numeric(w_level_m),
         w_velocity_ms = as.numeric(w_velocity_ms))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# REMOVE NON-15 MINUTE DATA ----
isco_water.df <- isco.df %>% 
  mutate(minute = minute(datetime)) %>% 
  fill(bottle, .direction = "down") %>% 
  filter(minute %in% c(0, 15, 30, 45))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# FIX PLOT NAME AND ADD TREATMENT COLUMN
# before I can join the files I need to select out the plot data in the water file 
isco_water.df <- isco_water.df %>% 
  mutate(plot = case_when(
    plot == "PLOT-1-2" ~ "1",
    plot == "PLOT-2-2" ~ "2",
    plot == "PLOT-4-2" ~ "4",
    plot == "PLOT-6-2" ~ "6",
    plot == "PLOT-7-2" ~ "7",
    plot == "PLOT-8-2" ~ "8",
  )) %>% 
  mutate(treatment = case_when(
    plot == "1" ~ "Pennycress",
    plot == "2" ~ "Reference",
    plot == "4" ~ "Pennycress",
    plot == "6" ~ "Reference",
    plot == "7" ~ "Reference",
    plot == "8" ~ "Pennycress",
  ))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# JOIN FILES ----
# plot needs to be factor in both data files to join them
isco_water.df <- isco_water.df %>%
  mutate(plot = as.factor(plot))
isco_nutrient.df <- isco_nutrient.df %>%
  mutate(plot = as.factor(plot))

# get rid of sample ID in nutrient file bc it is bloat
isco_nutrient.df <- isco_nutrient.df %>%
  select(drp_ppb, nh3_ppm, no3_ppm, date, plot, sample)

# need to work with the date in nutrient so the dates in the files line up
isco_nutrient.df <- isco_nutrient.df %>%
  mutate(datetime = mdy(date)) %>%
  rename(date = datetime, datetime = date)

isco_water.df <- isco_water.df %>%
  mutate(date = as_date(datetime, tz = NULL))

# rename sample to bottle
isco_nutrient.df <- isco_nutrient.df %>%
  rename(bottle = sample)

# joins the files
isco_full.df <- full_join(isco_water.df, isco_nutrient.df, by = c("plot", "date", "bottle"))

# REMOVE PLOT 8 BECAUSE THE TRANSDUCER BROKE ----
isco_full.df <- isco_full.df %>%
  filter(plot != 8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CALCULATE Q ----
isco_full.df <- isco_full.df %>% 
  mutate(discharge_cfs = 4.28*5.8*tan(45/2)*w_level_m*3.28084* 0.005) 

# convert cfs to Liter per second 
isco_full.df <- isco_full.df %>%
  mutate(discharge_Ls = discharge_cfs * 28.3168)

# account for time between sampling
isco_full.df <- isco_full.df %>%
  mutate(adjusted_q_l = discharge_Ls * 60 *15)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# this fixes some NA values on treatment type and is needed before load and conc. calcs
isco_full.df <- isco_full.df %>%
  mutate(treatment = case_when(
  plot == "1" ~ "Pennycress",
  plot == "2" ~ "Reference",
  plot == "4" ~ "Pennycress",
  plot == "6" ~ "Reference",
  plot == "7" ~ "Reference",
  plot == "8" ~ "Pennycress",
))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CREATE DATA FILES

# CALCULATE LOAD ----
# nitrate / calculate load
no3_load.df <- isco_full.df %>%
  mutate(no3_mg_L_load = adjusted_q_l * no3_ppm)
# summarize load by date
no3_load.df <- no3_load.df %>%
  group_by(plot, treatment, date) %>%
  summarize(no3_mg_day = sum(no3_mg_L_load, na.rm = TRUE)) %>%
  mutate(no3_kg_day = no3_mg_day / 1000000)

# ammonia / calculate load
nh3_load.df <- isco_full.df %>%
  mutate(nh3_mg_L_load = adjusted_q_l * nh3_ppm)
# summarize load by date
nh3_load.df <- nh3_load.df %>%
  group_by(plot, treatment, date) %>%
  summarize(nh3_mg_day = sum(nh3_mg_L_load, na.rm = TRUE)) %>%
  mutate(nh3_kg_day = nh3_mg_day / 1000000)

# phosphorus
drp_load.df <- isco_full.df %>%
  mutate(drp_ug_L_load = adjusted_q_l * drp_ppb)
# summarize load by date
drp_load.df <- drp_load.df %>%
  group_by(plot, treatment, date) %>%
  summarize(drp_ug_day = sum(drp_ug_L_load, na.rm = TRUE)) %>%
  mutate(drp_kg_day = drp_ug_day / 1000000000)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# SUMMARIZE LOAD CALCS ----
# join the summary files
load_summary.df <- full_join(no3_load.df, nh3_load.df, by = c("plot", "treatment", "date"))
load_summary.df <- full_join(load_summary.df, drp_load.df, by = c("plot", "treatment", "date"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# LOAD FOR SINGLE STORM EVENT ----
# doing this removes all values not in our storm event 
nutrient_storm_event.df <- load_summary.df %>%
  filter(no3_mg_day != 0)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# WATER LOSS ----
# create data frame
discharge.df <- isco_full.df %>%
  select(plot, treatment, date, discharge_cfs)

# total discharge
discharge.df <- discharge.df %>%
  group_by(plot, treatment, date) %>%
  summarize(q_day = sum(discharge_cfs, na.rm = TRUE))

# conver to cubic meters per day
discharge_full.df <- discharge.df %>%
  mutate(m3_day = q_day / 31.315)

# removing plot 4 breaks flow weighted storm event calcs so we make a new data frame for no 4
# remove plot 4 from discharge because we don't think its legit and remove NA
discharge.df <- discharge.df %>%
  filter(plot != 4) %>%
  filter(!is.na(treatment)) %>%
  rename(cfs_day = q_day)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#divide total load by total streamflow (kg/day) / (meter3/day) for flow weighted load

# CALCULATE FLOW WEIGHTED LOAD PER DAY
# join files
flow_weighted.df <- full_join(load_summary.df, discharge_full.df, by = c("plot", "treatment", "date"))

# select the columns that I want
flow_weighted.df <- flow_weighted.df %>%
  select(plot, treatment, date, no3_kg_day, nh3_kg_day, drp_kg_day, m3_day)


# ASK BILL ABOUT THESE UNITS ----


# calculate flow weighted load for nitrate
flow_weighted.df <- flow_weighted.df %>%
  mutate(flow_weighted_no3_concentration_mgl = no3_kg_day / m3_day) %>%
  mutate(flow_weighted_nh3_concentration_mgl = nh3_kg_day / m3_day) %>%
  mutate(flow_weighted_drp_concentration_mgl = drp_kg_day / m3_day)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# single event for flow weighted load
flow_weighted_storm_event.df <- flow_weighted.df %>%
  filter(no3_kg_day != 0)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CREATE PLOTS ---- 

# Loads for whole time periods ----
# nitrate
# reorder some factors 
load_summary.df <- load_summary.df %>%
  mutate(treatment = fct_relevel(treatment, "Reference", "Pennycress"))

no3_load.plot <- ggplot(data = load_summary.df, mapping = aes(x = treatment, y = no3_mg_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# ammonia
nh3_load.plot <- ggplot(data = load_summary.df, mapping = aes(x = treatment, y = nh3_mg_day)) +
stat_summary(
  fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# phosphorus
drp_load.plot <- ggplot(data = load_summary.df, mapping = aes(x = treatment, y = drp_ug_day)) +
stat_summary(
  fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# discharge
discharge.plot <- ggplot(data = discharge.df, mapping = aes(x = treatment, y = cfs_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load for storm event ----
no3_load_storm_event.plot <- ggplot(data = nutrient_storm_event.df, mapping = aes(x = treatment, y = no3_kg_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

nh3_load_storm_event.plot <- ggplot(data = nutrient_storm_event.df, mapping = aes(x = treatment, y = nh3_kg_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()


drp_load_storm_event.plot <- ggplot(data = nutrient_storm_event.df, mapping = aes(x = treatment, y = drp_kg_day)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# flow weighted storm event concentration plot ----
flow_weighted_storm_event.df <- flow_weighted_storm_event.df %>%
  mutate(treatment = fct_relevel(treatment, "Reference", "Pennycress"))

flow_weighted_no3.plot <- ggplot(data = flow_weighted_storm_event.df, mapping = aes(x = treatment, y = flow_weighted_no3_concentration_mgl, color = treatment)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  labs(x = "") +
  labs(y = "Flow Weighted NO3-N Concentration (mgL)") + theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +
  scale_shape_manual(
    name = "Treatment",
    labels = c("Reference", "Pennycress"),
    values = c(21, 22), drop=FALSE)+
  scale_color_manual(
    name = "Treatment",
    labels = c("Reference", "Pennycress"),
    values = c("coral4", "green3"), drop=FALSE)+
  scale_fill_manual(
    name = "Treatment",
    labels = c("Reference", "Pennycress"),
    values = c("coral4", "green3"), drop=FALSE) +
  expand_limits(y = 0) +
  theme(legend.position = "bottom")
flow_weighted_no3.plot

flow_weighted_nh3.plot <- ggplot(data = flow_weighted_storm_event.df, mapping = aes(x = treatment, y = flow_weighted_nh3_concentration_mgl)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()


flow_weighted_drp.plot <- ggplot(data = flow_weighted_storm_event.df, mapping = aes(x = treatment, y = flow_weighted_drp_concentration_mgl)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7) +
  theme_isco()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CALL PLOTS ----
# load
no3_load.plot
nh3_load.plot
drp_load.plot
discharge.plot

# load per storm event
no3_load_storm_event.plot
nh3_load_storm_event.plot
drp_load_storm_event.plot

# flow weighted storm event concentration
flow_weighted_no3.plot
flow_weighted_nh3.plot
flow_weighted_drp.plot

# saves the plots ----
ggsave(flow_weighted_no3.plot, file = "Figures/NO3 Concentration.pdf", units = "in", width = 3, height = 3)
ggsave(flow_weighted_nh3.plot, file = "Figures/NH3 Concentration.pdf", units = "in", width = 3, height = 3)
ggsave(flow_weighted_drp.plot, file = "Figures/DRP Concentration.pdf", units = "in", width = 3, height = 3)
ggsave(discharge.plot, file = "Figures/Discharge.pdf", units = "in", width = 3, height = 3)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# STATISTICS ----

# this is just a simple t-test from our first storm event to see what is going on and learn a bit

# creates a boxplot for some visualization
flow_weighted_storm_event.df %>%
  ggplot(aes(x = treatment, y = flow_weighted_no3_concentration_mgl)) +
  geom_boxplot()

# outputs mean and standard deviation 
flow_weighted_storm_event.df %>%
  group_by(treatment) %>%
  summarize(mean = mean(flow_weighted_no3_concentration_mgl),
            SD = sd(flow_weighted_no3_concentration_mgl))
# output
# treatment  mean    SD
# PC        7.35    1.78
# Reference 12.6    1.38

# hypotheses 
# H0: There is no different in no3 concentration by treatment for single storm event
# Ha: There is a diffrence in no3 concentration by treatment for single storm event

# runs the t-test
t.test(flow_weighted_no3_concentration_mgl ~ treatment, data = flow_weighted_storm_event.df, var.equal = TRUE)

# RESULTS (alpha = 0.05)
# Two Sample t-test assuming equal variance
# data:  flow_weighted_no3_concentration_mgl by treatment
# t = -4.6923, df = 6, p-value = 0.003353

# reject the null hypothesis, there is a significant difference in no3 concentration by treatment
# PC removes a statistically significant amount of nitrate concentration from tile water drainage

ggplot(data = discharge_full.df, mapping = aes(x = q_day)) +
  geom_histogram()

ggplot(data = isco_full.df, mapping = aes(x = no3_ppm)) +
  geom_histogram()

ggplot(data - )
