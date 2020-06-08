
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Household Internet Access"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2("Chart Options"),
      selectInput("variable:", "Independent Variable (y)",
                  c("Percent of Children in Poverty" = "povpct",
                    "Median Age" = "medage",
                    "Percent Non-Hispanic White Alone" = "pctwht")),
      h2("County Characteristics"),
      
      sliderInput("pctpov",
                   "Percent Children in Poverty",
                   min = 0,
                   max = 60,
                   value = c(0,60)),
        sliderInput("medage",
                "Median Age",
                 min = 20,
                 max = 70,
                 value = c(20,70)),
       sliderInput("pctwht",
                   "Percent Non-Hispanic White Alone",
                   min = 0,
                   max = 100,
                   value = c(0,100))
    ),
   
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Counties in the United States"),
      h3("with population of 65,000 or more"),
      h4("2018 American Community Survey"),
       plotOutput("sctPlot"),
      h3(textOutput("R2"))
      
    )
  )
))
