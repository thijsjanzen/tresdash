
checkPackages <- c("GUILDS", "nLTT", "STEPCAM",
                   "junctions", "GenomeAdmixR", "nodeSub", "simRestore",
                   "treestats", 
                   "DAISIE", "DAISIEprep", "DAISIEmainland",
                   "DAMOCLES", "SADISA",
                   "DDD", "PBD", "secsse",
                   "babette", "beautier", "tracerer", "mauricer", "mcbette")


library(shiny)

library(cranlogs)
library(ggplot2)
require(curl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TRES CRAN downloads dashboard"),

    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabs1",
                  tabPanel("Weekly", value = 1,
                           plotOutput("weekPlot")),
                  tabPanel("Monthly", value = 2,
                           plotOutput("monthPlot")),
                  tabPanel("Summary", value = 3,
                           plotOutput("summaryPlot")),
                  tabPanel("Long term", value = 4,
                           plotOutput("longPlot")),
                  tabPanel("per_package", value = 5,
                           plotOutput("packagewise"))
      )
    )
)


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
      ggplot(aes(x = reorder(package, count, FUN = median), y = count, fill = package)) +
      geom_boxplot() +
      theme_classic() +
      scale_y_log10() +
      theme(legend.position = "none") +
      scale_color_brewer(type = "qual", palette = 3) +
      ylab("Downloads per day") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 90))
    
    p2 <- vz %>%
      group_by(package) %>%
      summarise("total" = sum(count)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(package, total, decreasing = TRUE), y = total, fill = package)) +
      geom_bar(stat = "identity", ) + 
      theme(axis.text.x = element_text(angle = 90)) +
      ylab("Total number of downloads") +
      xlab("") +
      theme_classic() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90))
    
    egg::ggarrange(p1, p2)
  })
}

get_plots <- function(package_name) {
  local_data <- cran_downloads(packages = package_name, from = "2008-01-01", to = lubridate::today())
  to_remove <- min(which(local_data$count > 0))
  local_data <- local_data[-c(1:to_remove), ]
  
  
  local_data$year <- lubridate::year(local_data$date)
  local_data$month <- lubridate::month(local_data$date)
  
  local_data$runsum <- cumsum(local_data$count)
  local_data$lubri_date <- lubridate::as_date(local_data$date)
  
  p1 <- ggplot(local_data, aes(x = lubri_date, y = runsum)) +
    geom_line() +
    theme_classic() +
    xlab("Time") +
    ylab("Number of Downloads")
  
  p2 <- local_data %>%
    group_by(year) %>%
    summarise("total" = sum(count)) %>%
    ggplot(aes(x = year, y = total)) +
      geom_bar(stat = "identity") +
    ylab("Number of Downloads per Year") +
    xlab("Year") +
    theme_classic()
  
  d4 <- local_data %>%
    group_by(year, month) %>%
    summarise("total" = sum(count)) %>%
    mutate("number" = paste0(year, "-", month))
  
  d4$number[d4$month < 10] <- paste0(d4$year[d4$month < 10], "-0", d4$month[d4$month < 10])
  
  
  p3 <- d4 %>%
    ggplot(aes(x = number, y = total)) +
    geom_bar(stat = "identity") +
    ylab("Number of Downloads per Month") +
    xlab("Year") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, size = 5))
  p3
  
  egg::ggarrange(p1, p2, p3, nrow = 2)
}

# Run the application 
shinyApp(ui = ui, server = server)
