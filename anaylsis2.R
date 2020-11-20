library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#grab texas data 
texas_data <- incarceration %>%
  select(aapi_jail_pop_rate, white_jail_pop_rate, black_jail_pop_rate,
         total_jail_pop_rate, latinx_jail_pop_rate,
         year, fips, state, county_name) %>% 
  filter(state == "TX")

# 2018 data of black and white people
black_and_white_2018 <- incarceration %>%
  select(white_jail_pop_rate, black_jail_pop_rate, total_jail_pop_rate,
         year, fips, state, county_name) %>%
  mutate(black_white_jail_ratio = black_jail_pop_rate / white_jail_pop_rate) %>%
  filter(year == 2018)

#### Summary ####

summary_info <- list()
summary_info$num_observations <- nrow(incarceration) # number of rows
summary_info$num_features <- ncol(incarceration) # number of columns

#Which state has the highest black/white jail ratio in 2018?
summary_info$highest_black_white_ratio <- black_and_white_2018 %>%
  filter(black_white_jail_ratio == max(black_white_jail_ratio, na.rm = T)) %>%
  pull(state)

#When did Texas have the highest rate of black people in jail?
summary_info$highest_black_texas <- texas_data %>%
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
summary_info$highest_aapi_incarceration <- incarceration %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop_rate == max(aapi_jail_pop_rate, na.rm = T)) %>%
  pull(state)

#### My 2 variable scatter plot #####
black_white_by_state <- ggplot(data = black_and_white_2018 %>% top_n(10)) +
  geom_point(mapping = aes(x = black_jail_pop_rate,
                           y = white_jail_pop_rate, col = state)) +
  labs(title = "2018 Black vs. White Incarceration Rates",
       x = "Black Jail Population Rate", y = "White Jail Population Rate")


##### My Map ####

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
  labs(title = "2018 Black to White Jail Population Ratio in Texas") +
  labs(fill = "Black/White Ratio") +
  blank_theme

#### My Trend Over Time ####

#Grab one county data
terrell_county <- texas_data %>%
  filter(county_name == "Terrell County")
terrell_county2 <- terrell_county[-(1:20),]

library(reshape2)

meltdf <- melt(terrell_county2, id = "year", measure = c("white_jail_pop_rate",
                                                        "latinx_jail_pop_rate"))
latin_white <- ggplot(meltdf, aes(year, value, colour = variable)) +
  geom_line() +
  labs(title = "Terrell County White vs. Latin Incarceration Rates",
       x = "year", y = "Jail Population Rate", color = "Race") +
  scale_color_hue(labels = c("White", "Latin"))

meltdf2 <- melt(terrell_county2, id = "year", measure = c("white_jail_pop_rate",
                                                        "black_jail_pop_rate"))

black_white <- ggplot(meltdf2, aes(year, value, colour = variable)) +
  geom_line() +
  labs(title = "Terrell County White vs. Black Incarceration Rates",
       x = "year", y = "Jail Population Rate", color = "Race") +
  scale_color_hue(labels = c("White", "Black"))

comparing_trends <- black_white / latin_white




