# This is a test script

# regular function
dplyr::select(cars, speed)

# special function for
for (i in c(1, 2)) {
  # special function +
  2 + 2
}

base::c(1, 2)

while (FALSE) {
  2 + 2
}

library(dplyr)
require(ggplot2)

library(ggplot2)

test_func = function(x) {
  return(x + 2)
}

test_func(3)

evil_test_func = function(x) {
  x + 3
}

evil_test_func2 = function(six) {
  six * 6
}

good_test_func = function(longvariablename) {
  return(longvariablename + 17)
}

bad_pipe = mtcars %>%
  filter(mpg > 20) |>
  filter(disp > 120)

good_pipe = mtcars |>
  filter(mpg > 20)
