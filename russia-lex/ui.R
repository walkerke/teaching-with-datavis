library(rCharts)

dat <- read.csv('data.csv')

# Define UI 
shinyUI(fluidPage(
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "country", 
                  label = "Select a country:", 
                  choices = c("Russia", "Belarus", "Ukraine"), 
                  selected = "Russia"), 
      sliderInput("range",
                  label = "Years to display:",
                  min = 1959,
                  max = 2010,
                  value = c(1959, 2010), 
                  format = "0000"), 
      helpText("Data reflect the life expectancy ", 
                "at birth from 1959 to 2010 ", 
               "for men and women in Russia, ", 
                "Belarus, and Ukraine.  Data are ", 
               "obtained from the Human Mortality ", 
                "Database, a project of ", 
                " the University of California, ", 
                "Berkeley (USA), and Max Planck ", 
                "Institute for Demographic Research ", 
                "(Germany). Data are available ", 
                "at www.mortality.org or www.humanmortality.de ", 
               "(downloaded on 14 Oct 2014).")
    ),
    
    # Show the plot 
    mainPanel(
      showOutput("lexplot", "highcharts")
    )
  )
))