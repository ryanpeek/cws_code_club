# 2020-11-19 DeBugging Buddies --------------------------------------------

# today we'll play with data but with a spatial spin. We'll look at some turkey data and make a map.



# first load libraries
library(tidyverse)
library(sf)
library(USAboundaries)
library(mapview)
library(janitor)


# read in our turkey data!
# this is lbs and $ of turkey for each state for each year in the US
turkeys <- read_csv("https://raw.githubusercontent.com/ryanpeek/cws_code_club/main/data/annual_turkey_data_by_state_USDA_1929-2020.csv")

table(turkeys$`Data Item`)
summary(turkeys)

# make a clean dataset with the data we need to map
turkey_trimmed <- turkeys %>%
  select(Year, State, `Data Item`, Value) %>%
  janitor::clean_names() %>%
  # fix the value (remove commas and make integer)
  mutate(value=as.integer(gsub(pattern = ",", replacement = "", value))) %>%
  # warning about NA's is ok! drop the NA values
  filter(!is.na(value))


# let's recode these awkward data_item categories with case_when
turkey_trimmed <- turkey_trimmed %>%
  mutate(data_type=case_when(
    data_item=="TURKEYS - PRODUCTION, MEASURED IN $" ~ "production_dollars",
    data_item=="TURKEYS - PRODUCTION, MEASURED IN HEAD" ~ "production_head",
    # here's a way to use grep to search for any thing with "LB" in it
    grepl(pattern = "LB", data_item) ~ "production_lbs"
  ))


# Get State Boundaries ----------------------------------------------------

# get state boundaries from USAboundaries
states <- USAboundaries::us_states(resolution = "low")

plot(states$geometry)

# filter out states that make map really big (and no turkey there anyway)
states_48 <- states %>% filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico"))

plot(states_48$geometry)

# save to a shapefile with sf
st_write(states_48, dsn = "data_output/us_states_48.shp")

# read it back in
states2 <- st_read("data_output/us_states_48.shp")

# Join Data ---------------------------------------------------------------

# to join we need states to be all lowercase full names
turkey_trimmed <- turkey_trimmed %>%
  mutate(state=tolower(state))

states_48 <- states_48 %>%
  mutate(state_name=tolower(state_name))

# now we can join by state name
turkey_states <- left_join(turkey_trimmed, states_48, by=c("state"="state_name")) %>%
  # and make a sf object...works because of sticky geometry column
  st_as_sf()

# mapview to make a quick map:
t2019 <- turkey_states %>%
  filter(year==2019, data_type=="production_lbs")

# make an interactive map
mapview(t2019, zcol="value")


# make a plot of just production by $
ggplot() +
  geom_line(data=turkey_states,
            aes(x=year, y=value, color=state, group=state), show.legend = T) +
  facet_grid(data_type~., scales = "free_y") +
  scale_y_continuous("Value", labels = scales::comma) +
  theme_minimal()
