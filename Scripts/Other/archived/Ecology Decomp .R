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

# do soil type or leaves affect decomposition?

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RESULTS ---- 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#read in the data
full.df <- read_csv("Data/ecology_decomp.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# remove bag weights
full.df <- full.df %>%
  mutate(before_green = before_green_mass - green_bag) %>%
  mutate(before_rooibos = before_rooibos_mass - rooibos_bag) %>%
  mutate(after_green = after_green_mass - green_bag) %>%
  mutate(after_rooibos = after_rooibos_mass - rooibos_bag)

reduced.df <- full.df %>%
  select(section, cup, soil, leaves, before_green, before_rooibos, after_green, after_rooibos)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LETS MAKE A PLOT TO SEE HOW IT'S LOOKING ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# this is in wide format, im gonna need to put it into long format, what a pain



