library(rCharts)

dat <- read.csv('data.csv')

options(RCHART_WIDTH = 500)

shinyServer(function(input, output) {
  output$lexplot <- renderChart2({
    selected <- input$country
    country <- subset(dat, country == selected & Year %in% seq(input$range[1], input$range[2], 1))
    h1 <- hPlot(
      x = "Year", 
      y = "lex", 
      group = "Gender", 
      data = country, 
      type = "line")
    
    h1$colors(c("red", "blue"))
    h1$yAxis(title = list(enabled = TRUE, text = 'Life expectancy at birth'))
    return(h1)
  })
})