library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(usmap)
library(ggsn)

incarceration <- read.csv("../a3-data-visualization-Shirong13/incarceration_trends.csv") 

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
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(axis.title.x.bottom = element_text(face = "bold")) +
  theme(axis.title.y.left = element_text(face = "bold"))
barchart

#A chart that compares two variables to one another

gender_jail_pop_region <- na.omit(incarceration) %>%
  select(female_jail_pop, male_jail_pop, region)

scatterplot <- ggplot(gender_jail_pop_region , aes(x = female_jail_pop, y = male_jail_pop, color=region)) + 
  geom_point(size=4) +
  labs(color = "Regions") +
  ylab("Male") +
  xlab("Female") +
  ggtitle("Jail Population in Different Regions") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(axis.title.x.bottom = element_text(face = "bold")) +
  theme(axis.title.y.left = element_text(face = "bold")) 
theme_ipsum() 
scatterplot

#A map that shows how your measure of interest varies geographically

jail_pop_rate_state <- na.omit(incarceration) %>%
  select(state, total_jail_pop_rate)

us_map <- usmap::us_map()
usmap::plot_usmap(data = jail_pop_rate_state, values = "total_jail_pop_rate", labels = T)+
  scale_fill_continuous(
    low = "green", high = "red", name = "Jail Population Rate", label = scales::comma
  ) + 
  theme_linedraw() +
  theme(legend.position = "right") +
  labs(title = "State's Jail Population Rate") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype ="solid", 
                                         colour ="black"))






