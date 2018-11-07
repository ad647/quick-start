library(nycflights13)
library(tidyverse)

#Filter Exercises

#1.1. Arrival delay of 2 or more hours
filter(flights, arr_delay >= 120)

#1.2. Flew to Houston (IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU"))

#1.3. Operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))

#1.4. Departed in summer
filter(flights, month %in% c(7, 8, 9))

#1.5. Arrived >2 hours late, but didnt leave late
filter(flights, arr_delay > 120 & dep_delay <= 0)

#1.6. Delayed >= 1 hour, made up > 30 min in flight
filter(flights, dep_delay >= 60 & (dep_delay - arr_delay) > 30)

#1.7. Departed between midnight and 6am (inclusive)
filter(flights, dep_time == 2400 | dep_time <= 600)

#2.0 between()
filter(flights, between(month, 7, 9))

#3.0 missing dep_time
filter(flights, is.na(dep_time))

#Arrange Exercises
#1. Arrange all missing values to start
arrange(flights, desc(is.na(dep_time)))

#2. Sort flights to find most delayed flights, then left earliest
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

#3. Sort flights to find fastest
arrange(flights, distance / air_time)

#4. Longest distance travel and shortest travel
arrange(flights, desc(distance))
View(arrange(flights, distance))

#Select Exercises

#1. select dep_time, dep_delay, arr_time, arr_delay
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep"), starts_with("arr"))

#2. inlcude name of variable multiple times in select
select(flights, dep_time, dep_time)
#only returns once

#3. one_of()
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
select(flights, vars)
#using one_of means if there is a variable name in vars that isnt in the data frame, it will not cause error.

#4. "contains" case sensitivity
select(flights, contains("TIME"))
#not case sensitive?

#Mutate Exercises

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)

#Pipe!

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL") %>%
  ggplot(mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth()

delays

#Exercises
#1. Which destination has the most flights arriving from NYC?

library(nycflights13)
library(tidyverse)

nyc_origin <- filter(flights, origin %in% c("EWR", "LGA", "JFK")) %>%
  group_by(dest) %>%
  summarise(from_nyc = n()) %>%
  arrange(desc(from_nyc))
View(nyc_origin)

## by default, count(any_variable) will assign n = count(any_variable)
## so, arrange(desc(n)) will arrange the grouped count

#2. Which destinations have the most different carriers among flights from NYC?
nyc_origin <- filter(flights, origin %in% c("EWR", "LGA", "JFK")) %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))
View(nyc_origin)

## can also use length to do same thing as n_distinct(carriers)

#3. What time of day should you fly to avoid delays as much as possible?
flights %>%
  group_by(hour) %>%
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(mean_delay) %>%
  ggplot(mapping = aes(x = hour, y = mean_delay)) +
  geom_point()

## to identify why hour 1 has NaN (not a number)
flights %>%
  group_by(hour) %>%
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            count = n()) %>%
  arrange(mean_delay) %>%
  ggplot(mapping = aes(x = hour, y = mean_delay, size = count)) + 
  geom_point()

# Is there a pattern in # of cancelled flights per day? Is proportion of cancelled flights related to average delay?
flights %>%
  group_by(month, day) %>%
  summarize(count = n(),
            prop_cancelled = sum(is.na(dep_delay)) / count,
            mean_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = mean_delay, y = prop_cancelled)) +
  geom_point()
  