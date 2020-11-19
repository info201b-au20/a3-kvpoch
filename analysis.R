library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#summary
summary_info <- list()
summary_info$num_observations <- nrow(incarceration) # number of rows
summary_info$num_features <- ncol(incarceration) # number of columns

#Which state has the highest black/white jail ratio in 2018?
summary_info$highest_black_white_ratio <- black_and_white_2018 %>%
  filter(black_white_jail_ratio == max(black_white_jail_ratio, na.rm = T)) %>%
  pull(state)
  
#When did Texas have the highest rate of black people in jail?
summary_info$highest_black_texas_incarceration <- texas_data %>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = T))%>%
  pull(year)
  
#Which state had the highest rate of latin people in jail of 2018?
summary_info$highest_latin_incarceration <- incarceration %>%
  filter(year == 2018) %>%
  filter(latinx_jail_pop_rate == max(latinx_jail_pop_rate, na.rm = T)) %>%
  pull(state)

#Which state had the lowest rate of white people in jail of 2018?
summary_info$lowest_white_incarceration <- incarceration %>%
  filter(year == 2018) %>%
  filter(white_jail_pop_rate == max(white_jail_pop_rate, na.rm = T)) %>%
  pull(state)

#Which state had the highest rate of Asian Americans in jail of 2018?
summary_info$highest_latin_incarceration <- incarceration %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop_rate == max(aapi_jail_pop_rate, na.rm = T)) %>%
  pull(state)

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

# where the black/white ratio is > 1
map_data1 <- map_data %>% 
  mutate(black_white_jail_ratio = ifelse(black_white_jail_ratio > 1, black_white_jail_ratio, NA))
View(map_data1)

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


black_white_ratio_map <- ggplot(map_data1) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_white_jail_ratio)) +
  coord_map() +
  labs(fill = "Black/White Ratio")+
  labs(title = "Black to White Jail Population Ratio in Texas") +
  blank_theme

#where the black/white ratio is < 1  
map_data2 <- map_data %>% 
  mutate (black_white_jail_ratio = ifelse(black_white_jail_ratio < 1, black_white_jail_ratio, NA))
View(map_data2)

white_greater <- ggplot(map_data2) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_white_jail_ratio)) +
  blank_theme +
  ggtitle("White incarceration rate > Black incarceration rate") +
  coord_map() +
  labs(fill = "Black/White Ratio")

comparison_plots <- black_white_ratio_map/white_greater

#Trends chart Over Time 

#grab texas data 
texas_data <- incarceration %>%
  select(aapi_jail_pop_rate, white_jail_pop_rate, black_jail_pop_rate,
         total_jail_pop_rate, latinx_jail_pop_rate,
         year, fips, state, county_name) %>% 
  filter(state == "TX")

texas_data <- texas_data %>% group_by(year) %>% ungroup()

black_and_white_2018 %>% filter(year == 2018) %>%
  filter(state == "TX") %>% filter(black_white_jail_ratio == max(black_white_jail_ratio, na.rm = T)) %>% 
  select(county_name, black_white_jail_ratio
)






