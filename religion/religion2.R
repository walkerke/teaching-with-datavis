## Script to get religion map ready - be sure to set your working directory

library(foreign)
library(dplyr)
library(rgdal)
library(magrittr)
library(stringr)
library(tidyr)

dat <- read.dta("U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).dta")

# dat <- read.dta("http://www.thearda.com/download/download.aspx?file=U.S.%20Religion%20Census%20Religious%20Congregations%20and%20Membership%20Study,%202010%20(County%20File).DTA")

dir <- getwd()
 
counties <- readOGR(dir, layer = "counties100m", stringsAsFactors = FALSE)

# Process the data.  Stick with the major adherence categories, but parse out LDS.  

dat2 <- dat %>%
  select(evanadh, bprtadh, mprtadh, 
         cathadh, orthadh, othadh, ldsadh, fips:POP2010) %>%
  mutate(othadh = othadh - ldsadh)

cols <- c("evanadh", "bprtadh", "mprtadh", 
          "cathadh", "orthadh", "othadh", "ldsadh")

# Calculate percentage columns for each adherence category

for (c in cols) {
  pct_col <- paste0("pct_", c)
  dat2[[pct_col]] <- round((100 * (dat2[[c]] / dat2$POP2010)), 1)
}

# Need to account for over-estimation of adherents

trunc100 <- function(col) {
  col <- ifelse(col > 100, 100, col)
}


# Find the top adherence category by county.  
# There are no counties where "other" is the top category, and all the top Orthodox counties
# are in Alaska, which will get dropped.  

dat2 %<>%
  mutate_each(funs(trunc100), pct_evanadh:pct_ldsadh) %>%
  mutate(fips = str_pad(fips, 5, "left", "0")) %>%
  select(fips, stname, pct_evanadh:pct_ldsadh) %>%
  gather(key = religion, value = percent, starts_with("pct")) %>%
  group_by(fips) %>%
  filter(percent == max(percent, na.rm = TRUE)) %>%
  distinct(fips) %>%           
  mutate(religion = ifelse(religion == "pct_evanadh", "Evangelical", 
                           ifelse(religion == "pct_cathadh", "Catholic", 
                                  ifelse(religion == "pct_mprtadh", "Mainline", 
                                         ifelse(religion == "pct_ldsadh", "LDS", "African-American")))))

# Merge the data and write it to a shapefile
# The "sort = FALSE" argument is key as otherwise all of the values will get shuffled.  

counties@data <- merge(counties@data, dat2, by.x = "GEOID10", by.y = "fips", sort = FALSE)

writeOGR(counties, dir, layer = "cty_religion", 
        driver = "ESRI Shapefile", overwrite_layer = TRUE)


