## Should you use a map or a chart?  Visualizing the Gini index

library(rgdal)
library(WDI)
library(dplyr)


indicator <- "SI.POV.GINI"

url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

tmp <- tempdir()

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

countries <- readOGR(dsn = tmp, 
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8",
                     verbose = FALSE)


dat <- WDI(indicator = indicator, 
           start = 1990, 
           end = 2014)

dat[[indicator]] <- round(dat[[indicator]], 1)

datyr <- filter(dat, year == 2013)

dat2 <- dat %>%
  filter(!is.na(SI.POV.GINI)) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  arrange(desc(SI.POV.GINI)) %>%
  mutate(g_rank = row_number())
  

full_dat <- left_join(datyr, dat2, by = "iso2c")

keep_cols <- c(1, 2, 6:8)

full_dat <- full_dat[, keep_cols]

countries2 <- merge(countries, 
                    full_dat, 
                    by.x = "iso_a2", 
                    by.y = "iso2c",                    
                    sort = FALSE)


# Leaflet map

library(leaflet)

pal <- colorQuantile("YlGnBu", countries2$SI.POV.GINI.y, n = 6)

labs <- c("24.8-31.6", "31.6-38.5", "38.5-45.3", "45.3-52.1", "52.1-59.0", "59.0-65.8", "Data unavailable")

popup <- paste0("<strong>", countries2$name, "</strong><br>",
                "<strong>Gini index: </strong>", countries2$SI.POV.GINI.y,  
                      "<br><strong>Global inequality rank: </strong>", countries2$g_rank, 
                      "<br><strong>Year of estimate: </strong>", countries2$year.y)

mb_tiles <- "http://a.tiles.mapbox.com/v3/kwalkertcu.l1fc0hab/{z}/{x}/{y}.png"

mb_attribution <- 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a>'

gini_map <- leaflet(data = countries2) %>%
  addTiles(urlTemplate = mb_tiles,  
           attribution = mb_attribution) %>%
  setView(0, 0, zoom = 3) %>%
  addPolygons(fillColor = ~pal(SI.POV.GINI.y), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup) %>%
  addLegend(colors = c(RColorBrewer::brewer.pal(6, "YlGnBu"), "#808080"),  
            values = countries2$SI.POV.GINI.y, 
            bins = 6, 
            position = 'bottomright', 
            title = "Gini index", 
            labels = labs)

htmlwidgets::saveWidget(gini_map, file = "gini.html", selfcontained = FALSE)

## Dot plot

library(ggplot2)
library(extrafont)

df <- countries2@data

df1 <- df %>%
  select(gini = SI.POV.GINI.y, Continent = continent, country = admin) %>%
  filter(!is.na(gini)) %>%
  arrange(desc(gini)) %>%
  filter(row_number() < 21)

gini_chart <- ggplot(df1, aes(x = reorder(country, gini), y = gini, color = Continent)) + 
  geom_point(size = 12, stat = "identity") + 
  geom_text(aes(label = gini, fontface = "bold"), color = "white", size = 4) + 
  coord_flip() + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        legend.position = "bottom", 
        text = element_text(family = "Trebuchet MS")) + 
  xlab("") + 
  ylab("Gini index (most recent estimate)") + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Most unequal countries (World Bank Gini index estimate)")

ggsave("gini_coef.png", gini_chart, dpi = 300)