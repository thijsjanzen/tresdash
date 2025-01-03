require(lubridate)
require(cranlogs)
require(ggplot2)
require(magrittr)
require(ggnewscale)
require(gridExtra)

library(shiny)
require(curl)

packages_thijs <- c("GUILDS", "nLTT", "STEPCAM", "junctions", "GenomeAdmixR", "nodeSub", "simRestore", "treestats")
packages_rampal <- c("DDD", "PBD", "SADISA", "DAMOCLES", "secsse")
packages_richel <- c("babette", "beautier", "tracerer", "mauricer", "mcbette")
packages_luis <- c("DAISIE", "DAISIEprep", "DAISIEmainland")



checkPackages <- c(packages_thijs, packages_rampal, packages_richel, packages_luis)

# starting_packages <- c("DDD", "secsse", "DAISIE", "treestats")
starting_packages <- checkPackages

colors_thijs <- ggpubr::get_palette("GnBu", k = length(packages_thijs))
colors_rampal <- ggpubr::get_palette("RdPu", k = 2*length(packages_rampal))[-c(1:length(packages_rampal))]
colors_richel <- ggpubr::get_palette("OrRd", k = 2*length(packages_richel))[-c(1:length(packages_richel))]
colors_luis   <- ggpubr::get_palette("BuGn", k = 2*length(packages_luis))[-c(1:length(packages_luis))]

used_colors <- c(colors_thijs, colors_rampal, colors_richel, colors_luis)

long_data <- cran_downloads(packages = checkPackages, from = "2010-01-01", to = lubridate::today())



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("TRES CRAN downloads dashboard"),
  
  # Sidebar with a slider input for number of bins 
  # Show a plot of the generated distribution
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("set_thijs", "Thijs", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("set_rampal", "Rampal", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("set_luis", "Luis", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("set_richel", "Richel", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      width = 2
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabs1",
                  tabPanel("Weekly", value = 1,
                           plotOutput("weekPlot")),
                  tabPanel("Monthly", value = 2,
                           plotOutput("monthPlot")),
                  tabPanel("All time", value = 3,
                           plotOutput("mainPlot")),
                  tabPanel("Summary", value = 4,
                           plotOutput("summaryPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mainPlot <- renderPlot({
    
    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"
    
    long_data2 <- 
      long_data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumsum" = cumsum(count))
    
    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        new_scale_color() +
        
        geom_line(
          aes(x = date, y = cumsum, col = package),
          filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))
      return(p1)
    }
    
    
    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Number of Downloads") +
      ggtitle("All Time")
    
    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"
    
    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)
    
    print(p1)
  })
  
  output$monthPlot <- renderPlot({
    
    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"
    
    last_month <- lubridate::today() - 30
    long_data2 <- 
      long_data %>%
      filter(date >= last_month) %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumsum" = cumsum(count))
    
    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        new_scale_color() +
        
        geom_line(
          aes(x = date, y = cumsum, col = package),
          filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))
      return(p1)
    }
    
    
    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Number of Downloads") +
      ggtitle("Last Month")
    
    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"
    
    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)
    
    print(p1)
  })
  
  output$weekPlot <- renderPlot({
    
    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"
    
    last_month <- lubridate::today() - 7
    
    long_data2 <- 
      long_data %>%
      filter(date >= last_month) %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate("cumsum" = cumsum(count))
    
    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        new_scale_color() +
        
        geom_line(
          aes(x = date, y = cumsum, col = package),
          filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))
      return(p1)
    }
    
    
    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Number of Downloads") +
      ggtitle("Last Week")
    
    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"
    
    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)
    
    print(p1)
  })
  
  output$summaryPlot <- renderPlot({
    plots <- list()
    
    add_plot <- function(group_used, colors_used) {
      long_data$group <- "Thijs"
      long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
      long_data$group[long_data$package %in% packages_luis] <- "Luis"
      long_data$group[long_data$package %in% packages_richel] <- "Richel"
      
      long_data2 <- long_data %>%
        filter(group == group_used) %>%
        filter(count > 0)
      
      p1 <- long_data2 %>%
        ggplot(aes(x = reorder(package, count, FUN = median), 
                 y = count, fill = package)) +
        geom_boxplot() +
        theme_classic() +
        scale_y_log10() +
        scale_fill_manual(values = colors_used) +
        xlab("") +
        ylab("Downloads per day") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(legend.position = "none") +
        ggtitle(group_used)
      return(p1)
    }
    
    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"
    
    if (use_thijs) plots[[length(plots) + 1]] <- add_plot("Thijs", colors_thijs)
    if (use_rampal) plots[[length(plots) + 1]] <- add_plot("Rampal", colors_rampal)
    if (use_luis) plots[[length(plots) + 1]] <- add_plot("Luis", colors_luis)
    if (use_richel) plots[[length(plots) + 1]] <- add_plot("Richel", colors_richel)
    
    # grid.arrange(plots, ncol = 1)
    do.call("grid.arrange", c(plots, ncol = 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
