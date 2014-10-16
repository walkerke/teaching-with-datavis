library(tidyr)
library(dplyr)

russia <- read.table('russia.txt', skip = 3)

names(russia) <- c("Year", "Female", "Male", "Total")

russia$country <- "Russia"

ukraine <- read.table('ukraine.txt', skip = 3)

names(ukraine) <- c("Year", "Female", "Male", "Total")

ukraine$country <- "Ukraine"

belarus <- read.table('belarus.txt', skip = 3)

names(belarus) <- c("Year", "Female", "Male", "Total")

belarus$country <- "Belarus"

dat <- rbind(russia, ukraine, belarus)

dlong <- dat %>%
  select(-Total) %>%
  gather(Gender, lex, Female:Male)

write.csv(dlong, 'data.csv')