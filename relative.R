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


long_data$group <- "Thijs"
long_data$group[long_data$package %in% packages_rampal] <- "Rampal"
long_data$group[long_data$package %in% packages_luis] <- "Luis"
long_data$group[long_data$package %in% packages_richel] <- "Richel"

long_data$year <- lubridate::year(long_data$date)

long_data2 <- 
  long_data %>%
  group_by(group, year) %>%
  arrange(date) %>%
  summarise("total" = sum(count)) 

long_data2$rel <- 0
for (i in 1:length(long_data2$group)) {
  focal_year <- long_data2$year[i]
  total_year <- sum(long_data2$total[long_data2$year == focal_year])
  long_data2$rel[i] <- long_data2$total[i] / total_year
}

long_data2 %>%
  ggplot(aes(x = year, y = rel, fill = group)) +
    geom_area() +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 0.7) +
  theme_classic() +
  xlab("Year") +
  ylab("% of downloads")


long_data3 <- 
  long_data %>%
  filter(package != "DAISIEmainland") %>%
  group_by(package, year) %>%
  arrange(date) %>%
  summarise("total" = sum(count)) 

long_data3$rel <- 0
for (i in 1:length(long_data3$package)) {
  focal_year <- long_data3$year[i]
  total_year <- sum(long_data3$total[long_data3$year == focal_year])
  long_data3$rel[i] <- long_data3$total[i] / total_year
}



long_data4 <- 
long_data3 %>%
  ungroup() %>%
  filter(year == max(year) - 2) 

long_data4 <- long_data4[order(long_data4$package), ]
long_data4$cumsum <- 1 - cumsum(long_data4$rel)
long_data4$pos <- 0
yprev <- 1
for (i in 1:length(long_data4$pos)) {
  ycurr <- long_data4$cumsum[i]
  long_data4$pos[i] <- (yprev + ycurr) / 2
  yprev <- ycurr
}


long_data3 %>%
  ggplot(aes(x = year, y = rel, fill = package)) +
  geom_area() +
 # scale_fill_brewer(type = "qual", palette = 5)  + 
  scale_fill_viridis_d() +
  theme_classic() +
  xlab("Year") +
  ylab("% of downloads") +
  theme(legend.position = "none") +
  geom_text(data = long_data4, aes(x = year, y = pos, label = package),
            size = 1.5, col = "white")
  





