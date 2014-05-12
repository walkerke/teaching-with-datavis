# Load packages and data

library(reshape2)
library(ggplot2)
library(plotly)
library(zoo)

dat <- read.csv('tourism.csv')

## Define the "tidy" function and call it

tidy_WTTC <- function(df) {
  
  start <- df[7, 2]
  end <- df[7, ncol(df)]
  
  df <- df[8:nrow(df), ]
  
  nms <- c("country", paste0("y", seq(start, end, 1)))
  
  names(df) <- nms
  
  df <- na.locf(df, fromLast = TRUE)
    
  df <- df[seq(1, nrow(df), 2), ]
  
  df.melt <- melt(df, id.vars = "country", value.name="value", variable.name = "year")
  
  df.melt$year <- as.numeric(gsub("y", "", df.melt$year))
  
  df.melt$value <- as.numeric(df.melt$value)
  
  df.melt
}

tidy_dat <- tidy_WTTC(dat)

## Create the ggplot, initialize the plotly object, and convert the ggplot to plotly

t1 <- ggplot(tidy_dat, aes(x = year, y = value, color = country)) + 
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Set1") + 
  labs(list(x = "Year", 
            y = "Total contribution of tourism to GDP (percent)", 
            title = "Total contribution of tourism to GDP (percent), select Pacific Island countries.  Data source: WTTC"))

py <- plotly("YOUR USERNAME HERE", "YOUR API KEY HERE")

py$ggplotly(t1)



