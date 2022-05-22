library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(usmap)
library(ggsn)

# Load data

incarceration <- read.csv("../a3-data-visualization-Shirong13/incarceration_trends.csv") 

#Summary Information

summary_info <- list()
summary_info$num_variable <- ncol(incarceration)
summary_info$division_data <- incarceration %>%
  distinct(division) %>%
  select(division)
summary_info$state_data <- incarceration %>%
  distinct(state) %>%
  select(state)
summary_info$region_data <- incarceration %>%
  distinct(region) %>%
  select(region)
summary_info$avg_total_jail_pop <- mean(incarceration$total_jail_pop, na.rm = T)
summary_info$max_total_jail_pop_2018 <- incarceration %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(total_jail_pop)
summary_info$max_white_jail_pop <- max(incarceration$white_jail_pop, na.rm = T)
summary_info$min_white_jail_pop <- min(incarceration$white_jail_pop, na.rm = T)
summary_info$max_black_jail_pop <- max(incarceration$black_jail_pop, na.rm = T)
summary_info$min_black_jail_pop <- min(incarceration$black_jail_pop, na.rm = T)
summary_info$difference_black_white_jail_pop<- summary_info$max_black_jail_pop - summary_info$max_white_jail_pop
summary_info$max_state_other_race_jail_pop <- incarceration %>%
  filter(state == max(state, na.rm = TRUE)) %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>%
  distinct(other_race_jail_pop) %>%
  pull(other_race_jail_pop)
summary_info$min_state_other_race_jail_pop <- incarceration %>%
  filter(state == min(state, na.rm = TRUE)) %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>%
  distinct(other_race_jail_pop) %>%
  pull(other_race_jail_pop)

#A chart that shows trends over time for a variable of your choice 

urbanicity_jail <- na.omit(incarceration) %>%
  select(urbanicity, total_jail_pop, year)

barchart <- urbanicity_jail %>%
  ggplot(aes(fill = urbanicity, y = total_jail_pop, x = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(fill = "Urban Cities") +
  scale_fill_discrete(labels = c("Rural", "Small/Mid", "Suburban", "Urban")) +
  ylab("Jail Population") +
  xlab("Years") +
  ggtitle("Total Jail Population in Urban Cities Throughout the Years") + 
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  theme(axis.title.x.bottom = element_text(face = "bold")) +
  theme(axis.title.y.left = element_text(face = "bold"))
barchart

#A chart that compares two variables to one another

black_white_jail_pop_region <- na.omit(incarceration) %>%
  select(white_jail_pop, black_jail_pop, region)

scatterplot <- ggplot(black_white_jail_pop_region , aes(x = white_jail_pop, y = black_jail_pop, color=region)) + 
  geom_point(size=4) +
  labs(color = "Regions") +
  xlab("White") +
  ylab("Black") +
  ggtitle("Jail Population in Different Regions") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  theme(axis.title.x.bottom = element_text(face = "bold")) +
  theme(axis.title.y.left = element_text(face = "bold")) 
theme_ipsum() 
scatterplot

#A map that shows how your measure of interest varies geographically

other_race_jail_pop_state <- na.omit(incarceration) %>%
  select(state, other_race_jail_pop)

us_map <- usmap::us_map()
map <- usmap::plot_usmap(data = other_race_jail_pop_state, values = "other_race_jail_pop", labels = T)+
  scale_fill_continuous(
    low = "green", high = "red", name = "Other Race Jail Population", label = scales::comma
  ) + 
  theme_linedraw() +
  xlab("Long") +
  ylab("Lat") +
  theme(legend.position = "right") +
  labs(title = "State's of Other Race Jail Population") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype ="solid", 
                                         colour ="black"))
map





