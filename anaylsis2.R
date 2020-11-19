library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# 2018 data of black and white people
black_and_white_2018 <- incarceration %>%
  select(white_jail_pop_rate, black_jail_pop_rate, total_jail_pop_rate,
         year, fips, state, county_name) %>% 
  mutate(black_white_jail_ratio = black_jail_pop_rate / white_jail_pop_rate) %>%
  filter(year == 2018)

# My 2 variable scatter plot
black_white_by_state <- ggplot(data = black_and_white_2018 %>% top_n(10)) +
  geom_point(mapping = aes(x = black_jail_pop_rate,
                           y = white_jail_pop_rate, col = state)) +
  labs(title = "2018 Black vs. White Incarceration Rates",
       x = "Black Jail Population Rate", y = "White Jail Population Rate")


# My Map

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(black_and_white_2018, by = "fips") %>%
  filter(state == "TX")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

blackwhite_ratio_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_white_jail_ratio)) +
  coord_map() +
  labs(title = "Black to White Jail Population Ratio in Texas") +
  labs(fill = "Black/White Ratio") +
  blank_theme
