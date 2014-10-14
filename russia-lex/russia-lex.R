## Interactive chart of Russia's life expectancy by gender

library(tidyr)
library(dplyr)
library(rCharts)

dat <- read.table('lex.txt', skip = 3)

names(dat) <- c("Year", "Female", "Male", "Total")

dlong <- dat %>%
  select(-Total) %>%
  gather(Gender, lex, Female:Male)

lex <- hPlot(
  x = "Year", 
  y = "lex", 
  group = "Gender", 
  data = dlong)

lex$colors(c("red", "blue"))
lex$yAxis(title = list(enabled = TRUE, text = 'Life expectancy at birth'))
lex$title(text = "Life expectancy in Russia, 1959-2010")
lex$subtitle(text = "Data source: The Human Mortality Database (mortality.org)")

lex$save('russia-lex.html', cdn = TRUE)