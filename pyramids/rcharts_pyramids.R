## Script to generate population pyramids from the Census Bureau's International Database with rCharts ##

library(XML)
library(reshape2)
library(rCharts)
library(plyr)


getAgeTable <- function(country, year) {
  c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="  
  c2 <- "&R=-1&C="
  yrs <- gsub(" ", "", toString(year))
  url <- paste0(c1, yrs, c2, country)
  df <- data.frame(readHTMLTable(url))
  nms <- c("Year", "Age", "total", "Male", "Female", "percent", "pctMale", "pctFemale", "sexratio")  
  names(df) <- nms  
  cols <- c(1, 3:9)
  df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df <- df[df$Age != 'Total', ]  
  ord <- 1:nrow(df)
  df <- cbind(df, ord)
  return(df)
}


# DimpleJS pyramid

dPyramid <- function(country, year, colors=NULL) {
  dat <- getAgeTable(country, year)
  dat$Male <- -1 * dat$Male
  
  keep <- c("Year", "Age", "Male", "Female", "ord")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars=c('Age', 'ord', 'Year') )
  
  dat.melt$gencode <- ifelse(dat.melt$Gender == 'Male', 1, 2)
  
  d1 <- dPlot(
    x = "Population", 
    y = "Age", 
    groups = "Gender", 
    data = dat.melt, 
    type = 'bar')
  
  
  d1$yAxis(type = "addCategoryAxis", orderRule = "ord")
  d1$xAxis(type = "addMeasureAxis")
  d1$legend( x = 60, y = 10, width = 700, height = 20, horizontalAlign = "right")
  
  if (!is.null(colors)){
    d1$colorAxis(
      type = "addColorAxis", 
      colorSeries = "gencode", 
      palette = colors
    )
  }
  if (length(year) > 1) {
    d1$set(storyboard = "Year")
    max_x <- round_any(max(dat.melt$Population), 10000, f = ceiling)
    min_x <- round_any(min(dat.melt$Population), 10000, f = floor)
    d1$xAxis(overrideMax = max_x, overrideMin = min_x)
  }
  
  if (max(dat.melt$Population >= 1000000)) {
    d1$setTemplate( afterScript = 
                      "
                  <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.1f')(Math.abs(d) / 1000000) + 'm';
                     };
                    };
                  myChart.draw()
                  </script>
                  ")
  } else {
    d1$setTemplate( afterScript = 
                      "
                  <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.0f')(Math.abs(d) / 1000) + 'k';
                     };
                    };
                  myChart.draw()
                  </script>
                  ")
  }

  d1
}

# Highcharts pyramid

hPyramid <- function(country, year, colors = NULL) {
  dat <- getAgeTable(country, year)
  dat$Male <- -1 * dat$Male
  
  dat$Age <- factor(dat$Age, levels = rev(dat$Age), labels = rev(dat$Age))
    
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  h1 <- hPlot(
    y = 'Population', 
    x = 'Age', 
    type = 'bar', 
    data = dat.melt,
    group = 'Gender')
  
  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
  
  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', age '+ this.point.category +'</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
  
  h1$legend(reversed = "true")
  
  if (max(dat.melt$Population >= 1000000)) {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000000) + 'M';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  } else {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000) + 'K';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  }
  
  if (!is.null(colors)) {
    h1$colors(colors)
  }
  if (length(year) > 1) {
    stop('Right now, hPyramid only accepts one year')
  }
  
  h1$exporting(enabled = TRUE)
  
  h1
}


# NVD3 pyramid

nPyramid <- function(country, year, colors = NULL) {
  dat <- getAgeTable(country, year)
  dat$Male <- -1 * dat$Male
  
  dat <- dat[order(rev(dat$ord)), ]
  
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  dat.melt$abs <- abs(dat.melt$Population)
  
  n1 <- nPlot(
    y = 'Population', 
    x = 'Age', 
    group = 'Gender', 
    type = 'multiBarHorizontalChart', 
    data = dat.melt)
  
  # n1$xAxis(axisLabel = "Age") ## Need to work out label placement
  
  n1$chart(stacked = TRUE)
  
  n1$chart(tooltipContent = "#! function(key, x, y, e){
        var format = d3.format('0,000');
        return '<h3>' + key + ', age ' + x + '</h3>' + 
        '<p>' + 'Population: ' + format(e.point.abs) + '</p>'
        } !#")
  
    
  if (max(dat.melt$Population >= 1000000)) {    
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.1f')(Math.abs(d) / 1000000) + 'M'
                          } !#")
  } else {
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.0f')(Math.abs(d) / 1000) + 'K'
                          } !#")    
    
  }
  
  if (!is.null(colors)) {
    n1$chart(color = colors)
  }
  
  n1
}

