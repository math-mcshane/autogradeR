# This is a test script
library(gt)
library(gtExtras)

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(janitor)
library(lubridate)
library(stringr)
library(cowplot)
library(GGally)

my_sim = function(parm1, parm2){
  my_obs = rnorm(n = 10000, mean = parm1, sd = parm2)
  return(mean(my_obs))
}

# These are the parameter settings I would want
settingsA = seq(from = 2, to = 10, by = 2)
settingsB = seq(from = 0.5, to = 2, by = 0.5)
# expand.grid gives us all combinations we want
sim_data1 = expand.grid(mu = settingsA, sigma = settingsB)

# A for-loop works fine
set.seed(123)
for (i in seq_len(length.out = nrow(sim_data1))) {
  sim_data1$observedmean[i] = my_sim(
    parm1 = sim_data1$mu[i], 
    parm2 = sim_data1$sigma[i]
  )
}

# But we should use dplyr
set.seed(123)
sim_data2 = sim_data1 |> 
  rowwise() |>
  mutate(observedmean = my_sim(parm1 = mu, parm2 = sigma))

sim_data1 |> 
  pivot_wider(names_from = mu, values_from = observedmean) |> 
  gt::gt() |> 
  gt::tab_options(latex.use_longtable = TRUE)
sim_data2 |> 
  pivot_wider(names_from = mu, values_from = observedmean) |> 
  gt::gt() |> 
  gt::tab_options(latex.use_longtable = TRUE)




