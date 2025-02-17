library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(janitor)
library(lubridate)
library(stringr)

# old tidyverse style
c0 = cars %>%
  select(speed) %>%
  mutate(speed_sqr = speed^2)

# tidyverse style
c1 = cars |>
  select(speed) |>
  mutate(speed_sqr = speed^2)

# base R style
c2 = data.frame(
  speed = cars$speed,
  speed_sqr = cars$speed^2
)

obscure_dollar = function(data) {
  data.frame(
    speed = data$speed,
    speed_sqr = data$speed^2
  )
}
c3 = obscure_dollar(cars)

identical(c1, c2)
identical(c1, c3)
