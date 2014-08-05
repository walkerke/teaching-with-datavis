library(dplyr)
library(rCharts)
library(RColorBrewer)

dat <- read.csv("http://esa.un.org/wpp/ASCII-Data/ASCII_FILES/WPP2012_DB02_POPULATIONS_ANNUAL.csv")

## Alternatively, download the file from the above link and save it in your working directory

# library(data.table)
# dat <- fread("WPP2012_DB02_POPULATIONS_ANNUAL.csv")

regions <- c("Africa", "Latin America and the Caribbean", "Northern America", "Europe", "Oceania", "Asia")

region_dat <- dat %>%
  filter(VarID == 2, 
         Location %in% regions) %>%
  mutate(billions = PopTotal / 1000000) %>%
  select(Location, Time, billions)

# Stacked area chart by region

c1 <- nPlot(billions ~ Time, 
            group = "Location", 
            data = region_dat, 
            type = "stackedAreaChart")

c1$chart(color = brewer.pal(6, "Set2"))
c1$yAxis(tickFormat= "#!d3.format(',.1f')!#")
c1$yAxis(axisLabel = "Population (billions)", width = 62)
c1$xAxis(axisLabel = "Year")

c1$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + y + ' billion in ' + x + '</p>'
        } !#")

# c1$chart(useInteractiveGuideline = "true")

# Projected population by variant

variants <- c("Low", "Medium", "High", "Constant fertility")

global_dat <- dat %>%
  filter(Location == "World", 
         Variant %in% variants,
         Time >= 2012) %>%
  mutate(varfactor = factor(Variant,
                            levels = c("Constant fertility",
                                       "High",
                                       "Medium",
                                       "Low")), 
         billions = PopTotal / 1000000) %>%
  select(varfactor, Time, billions) %>%
  arrange(varfactor)

c2 <- nPlot(billions ~ Time, 
            group = "varfactor", 
            data = global_dat, 
            type="lineChart")

c2$chart(color = brewer.pal(4, "Set2"))
c2$yAxis(tickFormat= "#!d3.format(',.1f')!#")
c2$yAxis(axisLabel = "Population (billions)", width = 62)
c2$xAxis(axisLabel = "Year")


c2$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + 'Variant: ' + key + '</h3>' + 
        '<p>' + y + ' billion in ' + x + '</p>'
        } !#")


  