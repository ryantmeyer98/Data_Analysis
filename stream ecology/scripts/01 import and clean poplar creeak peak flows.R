# LIBRARIES ----
library(tidyverse)
library(janitor)
library(lubridate)

# READ IN THE DATA ----

# read in data and clean up a little
poplar.df <- read_csv("stream ecology/data/poplar creek peak flows.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# CLEAN DATA ----

# we only want year gague height and velocity
poplar.df <- poplar.df %>%
  select(c(peak_dt, peak_va, gage_ht)) 

# remove the odd value in the year
poplar.df <- poplar.df %>%
  filter(peak_dt != "10d")

# remove na values
poplar.df <- poplar.df %>%
  na.omit()

# need to make the dates a date
poplar.df <- poplar.df %>%
  mutate(peak_dt = ymd(peak_dt))

# rename columns
poplar.df <- poplar.df %>%
  rename(ymd = peak_dt,
         height_f = gage_ht,
         cfs = peak_va)

# add ranks
# this adds ranks to the column based on magnitude. This is really fucking cool!
poplar.df <- poplar.df %>%
  mutate(rank = rank(-cfs, na.last = "keep"))


# calculate recurrance interval as n+1 / rank
poplar.df <- poplar.df %>%
  mutate(ri = 69 / rank)

# calculate probability
poplar.df <- poplar.df %>%
  mutate(prob = 1 / ri) 

write_csv(poplar.df, file = "stream ecology/output/poplar.csv")


