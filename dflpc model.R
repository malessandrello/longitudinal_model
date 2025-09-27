library(tidyverse)
library(readr)


executive_function <- read_csv("datasets/executive-function.csv")

ex.func_dlpfc <- executive_function %>% 
  select(1:7) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "dlpfc") %>% 
  mutate(wawe = case_when(wawe == "dlpfc1" ~ 1,
                          wawe == "dlpfc2" ~ 2,
                          wawe == "dlpfc3" ~ 3,
                          wawe == "dlpfc4" ~ 4))

ex.func_ef <- executive_function %>% 
  select(1:3, 8:11) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "ef") %>% 
  mutate(wawe = case_when(wawe == "ef1" ~ 1,
                          wawe == "ef2" ~ 2,
                          wawe == "ef3" ~ 3,
                          wawe == "ef4" ~ 4))

ex.func_age <- executive_function %>% 
  select(1:3, 12:15) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "age") %>% 
  mutate(wawe = case_when(wawe == "age1" ~ 1,
                          wawe == "age2" ~ 2,
                          wawe == "age3" ~ 3,
                          wawe == "age4" ~ 4))



data <- list(ex.func_dlpfc, ex.func_ef, ex.func_age)

full_data <- reduce(data, full_join)

