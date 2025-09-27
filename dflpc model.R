library(tidyverse)
library(readr)


executive_function <- read_csv("longitudinal_model/datasets/executive-function.csv")

ex.func_dlpfc <- executive_function %>% 
  select(1:7) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "dlpfc") %>% 
  mutate(wawe = case_when(wawe == "dlpfc1" ~ as_factor(0),
                          wawe == "dlpfc2" ~ as_factor(1),
                          wawe == "dlpfc3" ~ as_factor(2),
                          wawe == "dlpfc4" ~ as_factor(3)))

ex.func_ef <- executive_function %>% 
  select(1:3, 8:11) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "ef") %>% 
  mutate(wawe = case_when(wawe == "ef1" ~ as_factor(0),
                          wawe == "ef2" ~ as_factor(1),
                          wawe == "ef3" ~ as_factor(2),
                          wawe == "ef4" ~ as_factor(3)))

ex.func_age <- executive_function %>% 
  select(1:3, 12:15) %>% 
  pivot_longer(cols = 4:7, names_to = "wawe", values_to = "age") %>% 
  mutate(wawe = case_when(wawe == "age1" ~ as_factor(0),
                          wawe == "age2" ~ as_factor(1),
                          wawe == "age3" ~ as_factor(2),
                          wawe == "age4" ~ as_factor(3)))



data <- list(ex.func_dlpfc, ex.func_ef, ex.func_age)

full_data <- reduce(data, full_join)

dlpfc_stats <- full_data %>% 
  group_by(wawe) %>%
  get_summary_stats(dlpfc)

ef_stats <- full_data %>% 
  group_by(wawe) %>%
  get_summary_stats(ef)

age_stats <- full_data %>% 
  group_by(wawe) %>%
  get_summary_stats(age)

dlpfc_box <- full_data %>%
  ggplot(aes(wawe, dlpfc, fill = wawe))+
  geom_boxplot()+
  geom_jitter(alpha= 0.2) + 
  theme_minimal() + 
  theme(legend.position = "none")

