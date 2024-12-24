#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(cranlogs)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CRAN download dashboard"),

    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("33%", "33%", "34%"),
       plotOutput("weekPlot"),
       plotOutput("monthPlot"),
       plotOutput("longPlot")))
    )
)

checkPackages <- c("GUILDS", "nLTT", "STEPCAM",
                   "junctions", "GenomeAdmixR", "nodeSub", "simRestore",
                   "treestats", 
                   "DAISIE", "DAISIEprep", "DAISIEmainland",
                   "DAMOCLES", "SADISA",
                   "DDD", "PBD", "secsse",
                   "babette", "beautier", "tracerer", "mauricer")

# Define server logic required to draw a histogram
server <- function(input, output) {

  global <- reactiveValues(data_storage = NULL)
  
  week_data <- reactive({
    cran_downloads(packages = checkPackages, "last-week")
  })
  
  month_data <- reactive({
    cran_downloads(packages = checkPackages, "last-month")
  })
  
  long_data <- reactive({
    cran_downloads(packages = checkPackages, from = "2010-01-01", to = lubridate::today())
  })
  
  output$weekPlot <- renderPlot({
    vz <- week_data()
    vz %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumulative_count" = cumsum(count)) %>%
      ggplot(aes(x = date, y = cumulative_count, col = package)) +
      geom_line(size = 1) +
      theme_classic() +
    #  scale_color_brewer(type = "qual", palette = 3) +
      ylab("Cumulative number of downloads") +
      ggtitle("Last week")
    })
  
  output$monthPlot <- renderPlot({
    vz <- month_data()
    vz %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumulative_count" = cumsum(count)) %>%
      ggplot(aes(x = date, y = cumulative_count, col = package)) +
      geom_line(size = 1) +
      theme_classic() +
      #  scale_color_brewer(type = "qual", palette = 3) +
      ylab("Cumulative number of downloads") +
      ggtitle("Last Month")
  })
  
  output$longPlot <- renderPlot({
    vz <- long_data()
    vz %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumulative_count" = cumsum(count)) %>%
      ggplot(aes(x = date, y = cumulative_count, col = package)) +
      geom_line(size = 1) +
      theme_classic() +
      #  scale_color_brewer(type = "qual", palette = 3) +
      ylab("Cumulative number of downloads") + 
      ggtitle("Long time")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
