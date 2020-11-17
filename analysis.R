library(tidyverse)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

testing <- ggplot(data = incarceration %>% top_n(10)) +
  geom_point(mapping = aes(x = year,
                           y = total_pop, col = state))