packages_thijs <- c("GUILDS", "nLTT", "STEPCAM", "junctions",
                    "GenomeAdmixR", "nodeSub", "simRestore", "treestats")
packages_rampal <- c("DDD", "PBD", "SADISA", "DAMOCLES", "secsse")
packages_richel <- c("babette", "beautier", "tracerer", "mauricer",
                     "mcbette", "pirouette")
packages_luis <- c("DAISIE", "DAISIEprep")



checkPackages <- c(packages_thijs, packages_rampal,
                   packages_richel, packages_luis)

starting_packages <- checkPackages

long_data <- cranlogs::cran_downloads(packages = checkPackages,
                                      from = "2010-01-01",
                                      to = lubridate::today() - 2)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("TRES CRAN downloads dashboard"),

  # Sidebar with a slider input for number of bins
  # Show a plot of the generated distribution

  sidebarLayout(
    sidebarPanel(
      radioButtons("set_thijs", "Thijs",
                   choices = c("Show", "Hide"),
                   selected = "Show",
                   inline = TRUE),
      radioButtons("set_rampal", "Rampal",
                   choices = c("Show", "Hide"),
                   selected = "Show",
                   inline = TRUE),
      radioButtons("set_luis", "Luis",
                   choices = c("Show", "Hide"),
                   selected = "Show",
                   inline = TRUE),
      radioButtons("set_richel", "Richel",
                   choices = c("Show", "Hide"),
                   selected = "Show",
                   inline = TRUE),
      width = 2
    ),

    mainPanel(
      tabsetPanel(type = "tabs", id = "tabs1",
                  tabPanel("Last Week", value = 1,
                           plotOutput("weekPlot")),
                  tabPanel("Last Month", value = 2,
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
  require(ggplot2)
  require(magrittr)

  colors_thijs <- ggpubr::get_palette("GnBu",
                                      k = 2*length(packages_thijs))[-c(1:length(packages_thijs))]
  colors_rampal <- ggpubr::get_palette("RdPu",
                                       k = 2*length(packages_rampal))[-c(1:length(packages_rampal))]
  colors_richel <- ggpubr::get_palette("OrRd",
                                       k = 2*length(packages_richel))[-c(1:length(packages_richel))]
  colors_luis   <- ggpubr::get_palette("BuGn",
                                       k = 2*length(packages_luis))[-c(1:length(packages_luis))]

  used_colors <- c(colors_thijs, colors_rampal, colors_richel, colors_luis)

  output$mainPlot <- renderPlot({

    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"

    long_data2 <-
      long_data %>%
      dplyr::group_by(package) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate("cumsum" = cumsum(count))

    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        ggnewscale::new_scale_color() +

        geom_line(
          aes(x = date, y = cumsum, col = package),
          dplyr::filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))

      long_data3 <- long_data2 %>%
        dplyr::filter(group == group_name) %>%
        dplyr::mutate("yval" = max(cumsum)) %>%
        dplyr::filter(date == max(date))

      p1 <- p1 +
        geom_text(data = long_data3,
                  aes(x = 0.3 + date, y = yval, col = package, label = package),
                  hjust = 0) +
        scale_color_manual(values = used_colors)

      return(p1)
    }


    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Cumulative number of downloads") +
      ggtitle("All Time")

    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"

    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)


    long_data3 <-
      long_data %>%
      dplyr::group_by(package) %>%
      dplyr::mutate("total" = sum(count))

    if (!use_thijs) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_thijs))
    }
    if (!use_rampal) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_rampal))
    }
    if (!use_luis) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_luis))
    }
    if (!use_richel) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_richel))
    }

    p1 <- p1 +
      coord_cartesian(xlim = c(min(long_data3$date),
                               max(long_data3$date) + 10),
                      clip = "off")
    p1 <- p1 +
      theme(legend.position = "none")

    p2 <- long_data3 %>%
      summarise("cumsum" = mean(total)) %>%
      ggplot(aes(x = reorder(package, cumsum), y = cumsum)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      coord_flip() +
      xlab("") +
      ylab("Total number of downloads")

    print(egg::ggarrange(p1, p2, nrow = 1, widths = c(0.7, 0.3)))
  })

  output$monthPlot <- renderPlot({

    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"

    last_month <- lubridate::today() - 30
    long_data2 <-
      long_data %>%
      dplyr::filter(date >= last_month) %>%
      dplyr::group_by(package) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate("cumsum" = cumsum(count))

    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        ggnewscale::new_scale_color() +

        geom_line(
          aes(x = date, y = cumsum, col = package),
          dplyr::filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))

      long_data3 <- long_data2 %>%
        dplyr::filter(group == group_name) %>%
        dplyr::mutate("yval" = max(cumsum)) %>%
        dplyr::filter(date == max(date))

      p1 <- p1 +
        geom_text(data = long_data3,
                  aes(x = 0.3 + date, y = yval, col = package, label = package),
                  hjust = 0) +
        scale_color_manual(values = used_colors)

      return(p1)
    }


    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Cumulative number of downloads") +
      ggtitle("Last Month")

    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"

    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)

    long_data3 <-
      long_data %>%
      dplyr::filter(date >= last_month) %>%
      dplyr::group_by(package) %>%
      dplyr::mutate("total" = sum(count))

    if (!use_thijs) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_thijs))
    }
    if (!use_rampal) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_rampal))
    }
    if (!use_luis) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_luis))
    }
    if (!use_richel) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_richel))
    }

    p1 <- p1 +
      coord_cartesian(xlim = c(min(long_data3$date),
                               max(long_data3$date) + 5),
                      clip = "off")
    p1 <- p1 +
      theme(legend.position = "none")

    p2 <- long_data3 %>%
      summarise("cumsum" = mean(total)) %>%
      ggplot(aes(x = reorder(package, cumsum), y = cumsum)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      coord_flip() +
      xlab("") +
      ylab("Number of downloads last month")


    print(egg::ggarrange(p1, p2, nrow = 1, widths = c(0.7, 0.3)))
  })

  output$weekPlot <- renderPlot({

    long_data$group <- "Thijs"
    long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
    long_data$group[long_data$package %in% packages_luis] <- "Luis"
    long_data$group[long_data$package %in% packages_richel] <- "Richel"

    last_month <- lubridate::today() - 7

    long_data2 <-
      long_data %>%
      dplyr::filter(date >= last_month) %>%
      dplyr::group_by(package) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate("cumsum" = cumsum(count))

    add_group <- function(p1, group_name, used_colors) {
      p1 <- p1 +
        ggnewscale::new_scale_color() +

        geom_line(
          aes(x = date, y = cumsum, col = package),
          dplyr::filter(long_data2, group == group_name),
          lwd = 1.3) +
        scale_color_manual(values = used_colors) +
        labs(color = group_name) +
        guides(col = guide_legend(ncol = 2))

      long_data3 <- long_data2 %>%
        dplyr::filter(group == group_name) %>%
        dplyr::mutate("yval" = max(cumsum)) %>%
        dplyr::filter(date == max(date))

      p1 <- p1 +
        geom_text(data = long_data3,
                  aes(x = 0.1 + date, y = yval, col = package, label = package),
                  hjust = 0) +
        scale_color_manual(values = used_colors)

      return(p1)
    }


    p1 <- long_data2 %>%
      ggplot() +
      theme_classic() +
      xlab("Date") +
      ylab("Cumulative number of downloads") +
      ggtitle("Last Week")

    use_thijs <- input$set_thijs == "Show"
    use_rampal <- input$set_rampal == "Show"
    use_luis <- input$set_luis == "Show"
    use_richel <- input$set_richel == "Show"

    if (use_thijs) p1 <- add_group(p1, "Thijs", colors_thijs)
    if (use_rampal) p1 <- add_group(p1, "Rampal", colors_rampal)
    if (use_luis) p1 <- add_group(p1, "Luis", colors_luis)
    if (use_richel) p1 <- add_group(p1, "Richel", colors_richel)

    long_data3 <-
      long_data %>%
      dplyr::filter(date >= last_month) %>%
      dplyr::group_by(package) %>%
      dplyr::mutate("total" = sum(count))

    if (!use_thijs) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_thijs))
    }
    if (!use_rampal) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_rampal))
    }
    if (!use_luis) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_luis))
    }
    if (!use_richel) {
      long_data3 <- long_data3 %>%
        dplyr::filter(!(package %in% packages_richel))
    }

    p2 <- long_data3 %>%
      summarise("cumsum" = mean(total)) %>%
      ggplot(aes(x = reorder(package, cumsum), y = cumsum)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      coord_flip() +
      xlab("") +
      ylab("Number of downloads last week")

    p1 <- p1 +
      coord_cartesian(xlim = c(min(long_data3$date),
                               max(long_data3$date) + 1),
                      clip = "off")

    p1 <- p1 + theme(legend.position = "none")


    print(egg::ggarrange(p1, p2, nrow = 1, widths = c(0.7, 0.3)))
  })

  output$summaryPlot <- renderPlot({
    plots <- list()

    add_plot <- function(group_used, colors_used) {
      long_data$group <- "Thijs"
      long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
      long_data$group[long_data$package %in% packages_luis] <- "Luis"
      long_data$group[long_data$package %in% packages_richel] <- "Richel"

      long_data2 <- long_data %>%
        dplyr::filter(group == group_used) %>%
        dplyr::filter(count > 0)

      p1 <- long_data2 %>%
        ggplot(aes(x = reorder(package, count, FUN = median),
                 y = count, fill = package)) +
        geom_boxplot() +
        theme_minimal() +
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

    if (use_thijs)
      plots[[length(plots) + 1]] <- add_plot("Thijs", colors_thijs)
    if (use_rampal)
      plots[[length(plots) + 1]] <- add_plot("Rampal", colors_rampal)
    if (use_luis)
      plots[[length(plots) + 1]] <- add_plot("Luis", colors_luis)
    if (use_richel)
      plots[[length(plots) + 1]] <- add_plot("Richel", colors_richel)

    egg::ggarrange(plots = plots, ncol = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
