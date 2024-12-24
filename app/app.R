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
require(curl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TRES CRAN download dashboard"),

    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabs1",
                  tabPanel("Summary", value = 0,
                           plotOutput("summaryPlot")),
                  tabPanel("Weekly", value = 1,
                           plotOutput("weekPlot")),
                  tabPanel("Monthly", value = 2,
                           plotOutput("monthPlot")),
                  tabPanel("Long term", value = 3,
                           plotOutput("longPlot"))
      )
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
  
  output$summaryPlot <- renderPlot({
    vz <- long_data()
    p1 <- vz %>%
      filter(count > 0) %>%
      ggplot(aes(x = reorder(package, count), y = count, fill = package)) +
      geom_boxplot() +
      theme_classic() +
      scale_y_log10() +
      theme(legend.position = "none") +
      scale_color_brewer(type = "qual", palette = 3) +
      ylab("Downloads per day") +
      theme(axis.text.x = element_text(angle = 90))
    
    p2 <- vz %>%
      group_by(package) %>%
      summarise("total" = sum(count)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(package, total), y = total, fill = package)) +
      geom_bar(stat = "identity", ) + 
      theme(axis.text.x = element_text(angle = 90)) +
      ylab("Total number of downloads") +
      xlab("")
      scale_y_log10() + 
      theme_classic() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90))
    
    egg::ggarrange(p1, p2, nrow = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
