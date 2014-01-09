# First, download the Excel file from the Pew Hispanic Center, and save it as a CSV in your working directory

library(stringr)
library(plyr)
library(rCharts)
library(reshape2)

dat <- read.csv("all_counties_by_top_six_groups.csv")

keep <- seq(1, 25, 3)

dat <- dat[,keep]

nms <- c('Name', 'Total.Hisp', 'Mexican', 'Puerto.Rican', 'Cuban', 'Salvadoran', 'Dominican', 'Guatemalan', 'Other')

names(dat) <- nms

dat <- dat[-c(1:3),]

dat <- cbind(dat, ldply(str_split(dat$Name, ", ")))

names(dat) <- c(nms, 'County', 'State')

convCols <- 2:9

dat[,convCols] <- apply(dat[,convCols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))

sums <- ddply(dat, .(State), numcolwise(sum))

sorteddf <- sums[order(-sums$Total.Hisp),][1:10,]

newdf <- data.frame(sorteddf$State)

vals <- c('Mexican', 'Puerto.Rican', 'Cuban', 'Salvadoran', 'Dominican', 'Guatemalan', 'Other')

for (v in vals) {
  newdf[[v]] <- round(((sorteddf[[v]] / sorteddf$Total.Hisp) * 100), 1)
}

names(newdf) <- c('State', vals)

df.melt <- melt(newdf, variable.name = 'Ancestry', value.name = 'Share')



d1 <- dPlot(
  x = "Share", 
  y = "State", 
  groups = "Ancestry", 
  data = df.melt, 
  type = 'bar')

d1$xAxis(type = "addPctAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "State")

d1$legend( x = 60, y = 10, width = 700, height = 20, horizontalAlign = "left", orderRule = "Ancestry")

