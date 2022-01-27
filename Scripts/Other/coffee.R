# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LOAD LIBRARIES ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ IN AND MODIFY THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# reads in the data and clean up the spreadsheet
long.df <- read_csv("Data/Other/coffee.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# we also want the data in wide format
wide.df <- pivot_wider(long.df, id_cols = coffee, names_from = metric, values_from = value)

# I made a typo
wide.df <- wide.df %>%
  rename(onyx_price = onxy_price)

# everything is a character right now so we need to set the numeric things to numeric
wide.df <- wide.df %>%
  mutate(onyx_price = as.numeric(onyx_price),
         sale_10oz = as.numeric(sale_10oz),
         green_cost = as.numeric(green_cost),
         transportation = as.numeric(transportation),
         production = as.numeric(production),
         c_market = as.numeric(c_market),
         cup_score = as.numeric(cup_score),
         fair_trade_min = as.numeric(fair_trade_min),
         elevation = as.numeric(elevation))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LETS ASK SOME QUESTIONS ABOUT THESE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# What is the best predictor of sale price?
# How much does cup score influence sale price?
# Does elevation alone influence cup score?

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# WHAT IS THE BEST PREDICTOR OF SALE PRICE?
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Gonna just use a simple GLM to answer this question
# Sale_10oz ~ onyx_price, green_cost, transportation, c_market, cup_score, elevation


# Start by checking the assumptions
# Linear Relationships

# this names and makes the plots 
onyx_price_linear.plot <- ggplot(data = wide.df, mapping = aes(x = onyx_price, y = sale_10oz)) +
  geom_point()

green_cost_linear.plot <- ggplot(data = wide.df, mapping = aes(x = green_cost, y = sale_10oz)) +
  geom_point()

transportation_linear.plot <- ggplot(data = wide.df, mapping = aes(x = transportation, y = sale_10oz)) +
  geom_point()

c_market_linear.plot <- ggplot(data = wide.df, mapping = aes(x = c_market, y = sale_10oz)) +
  geom_point()

cup_score_linear.plot <- ggplot(data = wide.df, mapping = aes(x = cup_score, y = sale_10oz)) +
  geom_point()

elevation_linear.plot <- ggplot(data = wide.df, mapping = aes(x = elevation, y = sale_10oz)) +
  geom_point()

# this puts them all together in one frame 
onyx_price_linear.plot + green_cost_linear.plot + transportation_linear.plot + 
  c_market_linear.plot + cup_score_linear.plot + elevation_linear.plot

# now we visually check normality and homogeneity of variance 
# we start by making the model
multiple.lm <- lm(sale_10oz ~ onyx_price + green_cost + transportation + c_market + cup_score +
                  elevation, data = wide.df)

# these look fine to me
residuals1 <- resid(multiple.lm)
plot(fitted(multiple.lm),residuals1)
qqnorm(residuals1)
qqline(residuals1)

# run the ANOVA
Anova(multiple.lm, type = "3")

# going to drop the non-significant terms out of the model (elevation, cup_score, c_market)
multiple2.lm <- lm(sale_10oz ~ onyx_price + green_cost + transportation, data = wide.df)

# recheck assumptions
residuals2 <- resid(multiple.lm)
plot(fitted(multiple.lm),residuals2)
qqnorm(residuals2)
qqline(residuals2)

# run the model 
Anova(multiple2.lm, type = "3")

# going to drop the non-significant terms again(onyx_price, transportation)
multiple3.lm <- lm(sale_10oz ~ green_cost, data = wide.df)

# assumptions again
residuals3 <- resid(multiple.lm)
plot(fitted(multiple.lm),residuals3)
qqnorm(residuals3)
qqline(residuals3)

# run the model 
Anova(multiple3.lm, data = "3")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# RESULTS ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# green cost (the cost of the unroasted beans) is the best predictor of sale price
# this is given the information I was able to collect, I am sure there is more to it 
# given more data
