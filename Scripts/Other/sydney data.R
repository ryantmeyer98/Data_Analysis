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
if(!require(forcats)){install.packages("forcats")}

if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ IN THE DATA ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

wide.df <- read_excel("Data/sydney/2020 KS vs IL soil Lobelia Datasheet.xlsx", 
                      sheet = "For SAS") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# make it long?
long.df <- wide.df %>%
  mutate(plant = as.factor(plant),
         block = as.factor(block),
         flat = as.factor(flat),
         pos = as.factor(pos),
         germflat = as.factor(germflat),
         momnum = as.factor(momnum),
         plantday = as.factor(plantday),
         transday = as.factor(transday),
         firstros = as.factor(firstros),
         harvdueday = as.factor(harvdueday),
         harvday = as.factor(harvday),
         aginoven = as.factor(aginoven),
         agbiomeas = as.factor(agbiomeas),
         bginoven = as.factor(bginoven),
         bgbiomeas = as.factor(bgbiomeas)
  )

long.df <- pivot_longer(long.df, cols = c(
                         "plant", "block", "flat", "pos", "germflat", "momnum", "plantday",
                         "transday", "firstros", "harvdueday", "harvday", "aginoven", 
                         "aginoven", "agbiomeas", "bginoven", "bgbiomeas"),
                         names_to = "metric", 
                         values_to = "value")

long1.df <- pivot_longer(long.df, "plant", "flat", 
                         names_to = "metric", values_to = "value")

ggplot(data = wide.df, mapping = aes(x = popsoil, y = agbiomeas, color = statesoil)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) + 
      theme(axis.line = element_line(linetype = "solid"),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            panel.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA),
            legend.background = element_rect(fill = NA),
            axis.text.x = element_text(angle = 90))

ggplot(data = wide.df, mapping = aes(x = stategarden, y = agbiomeas, color = statesoil)) +
  geom_boxplot() +
  geom_jitter()

wide.df <- wide.df %>%
  filter(stategarden != "CNTRL")

wide.df <- wide.df %>%
  filter(popsoil != "CenAnd")
wide.df <- wide.df %>%
  filter(popsoil != "CenLes")
wide.df <- wide.df %>%
  filter(popsoil != "EasAnd")
wide.df <- wide.df %>%
  filter(popsoil != "EasLes")

ggplot(data = wide.df, mapping = aes(x = stategarden, y = agbiomeas, color = popsoil)) +
  geom_boxplot() +
  geom_jitter()

interaction.plot(
  x.factor = test.df$date,
  trace.factor = test.df$trpopid,
  respons = test.df$width_rep,
  ylab = "Width",
  xlab = "Date",
  col = c("#999999", "#000000", "#993300", "#FF6699", "#99CC00", "#CC0033", "#0033FF"),
  lty = 1,
  lwd = 2,
  trace.label = "Population"
  
interaction.plot(
  x.factor = wide.df$stategarden,
  trace.factor = wide.df$popseed,
  response = wide.df$agbiomeas,
  ylab = "y",
  xlab = "x",
  lty = 1,
  lwd = 2,
  trace.label = "lab"
)
