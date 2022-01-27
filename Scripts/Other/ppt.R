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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OBJECTIVE ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# determine how many rain events over 0.5 inches there were at the ISU farm in 
# Lexington, Illinois in 2020.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RESULTS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# there are 25 single day rain events over 0.5 inches in 2020
# there are 5 single day 1 inch rain events during the pennycress growing season

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# read in data 
full.df <- read.csv("Data/ppt.csv") %>%
  clean_names() %>% 
  remove_empty(which = c("cols", "rows"))

# we want the julian date for this
full.df <- full.df %>%
  mutate(date = as.Date(date, "%m/%d/%y"))
full.df <- full.df %>%
  mutate(julian = yday(date))

# add week number
full.df <- full.df %>%
  mutate(week = week(date))

# keep everything from Jan 1 to June 1, skip july august septemt, october 31 to december 31
growing_season.df <- full.df %>%
  filter(julian < 153 | julian > 305)

growing_season.df <- growing_season.df %>%
  filter(ppt_in >= 1)
  
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PRELIMINARY PLOTTING AND DATA MANIPULATION ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# full plot
ggplot(data = full.df, mapping = aes(x = julian, y = ppt_in)) +
  geom_point()

# okay, lets remove the zeros
ppt.df <- full.df %>%
  filter(ppt_in != 0)

# replot the data
ggplot(data = ppt.df, mapping = aes(x = julian, y = ppt_in)) +
  geom_point()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ENGAGING BRAIN POWER TO DETERMINE OBJECTIVE ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# alright, lets get every single day even that is over 0.5 inches
half_inch.df <- full.df %>%
  filter(ppt_in >= 0.5)

# we have 25 events, now lets plot this up
ggplot(data = half_inch.df, mapping = aes(x = julian, y = ppt_in)) +
  geom_point()

# growing season 0.5 inch
ggplot(data = growing_season.df, mapping = aes(x = julian, y = ppt_in)) +
  geom_point()













