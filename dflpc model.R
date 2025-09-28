library(tidyverse)
library(readr)
library(lmerTest)
library(ggpubr)
library(rstatix)

executive_function <- read_csv("datasets/executive-function.csv")



ex.func_dlpfc <- executive_function %>% 
  select(1:7) %>% 
  pivot_longer(cols = 4:7, names_to = "wave", values_to = "dlpfc") %>% 
  mutate(wave = case_when(wave == "dlpfc1" ~ as_factor(0),
                          wave == "dlpfc2" ~ as_factor(1),
                          wave == "dlpfc3" ~ as_factor(2),
                          wave == "dlpfc4" ~ as_factor(3)),
         id = as.factor(id))

ex.func_ef <- executive_function %>% 
  select(1:3, 8:11) %>% 
  pivot_longer(cols = 4:7, names_to = "wave", values_to = "ef") %>% 
  mutate(wave = case_when(wave == "ef1" ~ as_factor(0),
                          wave == "ef2" ~ as_factor(1),
                          wave == "ef3" ~ as_factor(2),
                          wave == "ef4" ~ as_factor(3)),
         id = as.factor(id))

ex.func_age <- executive_function %>% 
  select(1:3, 12:15) %>% 
  pivot_longer(cols = 4:7, names_to = "wave", values_to = "age") %>% 
  mutate(wave = case_when(wave == "age1" ~ as_factor(0),
                          wave == "age2" ~ as_factor(1),
                          wave == "age3" ~ as_factor(2),
                          wave == "age4" ~ as_factor(3)),
         id = as.factor(id))



data <- list(ex.func_dlpfc, ex.func_ef, ex.func_age)

full_data <- reduce(data, full_join)

dlpfc_stats <- full_data %>% 
  group_by(wave) %>%
  get_summary_stats(dlpfc)

dlpfc_stats <- full_data %>% 
  group_by(wave) %>%
  filter(sex == 1) %>% 
  get_summary_stats(dlpfc)


dlpfc_stats_sex <- full_data %>% 
  group_by(wave, sex) %>%
  get_summary_stats(dlpfc)

ef_stats <- full_data %>% 
  group_by(wave) %>%
  get_summary_stats(ef)

age_stats <- full_data %>% 
  group_by(wave) %>%
  get_summary_stats(age)

dlpfc_box <- full_data %>%
  ggplot(aes(wave, dlpfc, fill = wave))+
  geom_boxplot()+
  geom_signif() + 
  theme_minimal() + 
  theme(legend.position = "none")+
  facet_grid(~sex)

full_data %>% 
  mutate(sex = as_factor(sex)) %>% 
ggplot(aes(wave, dlpfc, fill = sex))+
  geom_boxplot()+
geom_pwc()+
  theme_minimal() + 
  theme(legend.position = "none")+
  facet_grid(~tx)

full_data %>% 
  mutate(tx = as_factor(tx)) %>% 
  ggplot(aes(wave, dlpfc, fill = tx))+
  geom_boxplot()+
  geom_pwc()+
  theme_minimal() + 
  theme(legend.position = "none")



ef_box <- full_data %>%
  mutate(tx = as_factor(tx)) %>% 
  ggplot(aes(wave, ef, fill = tx))+
  geom_boxplot()+
  geom_pwc()+ 
  theme_minimal() + 
  theme(legend.position = "none")

lines_plot_1 <- full_data %>% 
  mutate(wave = as.numeric(wave)) %>% 
  ggplot(aes(wave, dlpfc, color = id))+
  geom_line()+
  theme_minimal() + 
  theme(legend.position = "none")+
  facet_grid(~sex)

lines_plot_0 <- full_data %>% 
  filter(sex == 0) %>% 
  mutate(wave = as.numeric(wave)-1) %>% 
  ggplot(aes(wave, dlpfc, color = id))+
  geom_line()+
  theme_minimal() + 
  theme(legend.position = "none")

ids <- sample(full_data$id, 100, replace = FALSE, prob = NULL)
data_ids <- full_data %>% 
  filter(id %in% ids) %>% 
  mutate(wave = as.numeric(wave)) %>% 
  ggplot(aes(wave, dlpfc, color = id))+
  geom_line()+
  theme_minimal() + 
  theme(legend.position = "none")+
  facet_grid(~sex)


data <- full_data %>% 
  mutate(wave = as.numeric(wave)-1)

summary(lm(dlpfc ~ wave + sex, data = data))


dlpfc_model_sex <- lme4::lmer(dlpfc ~  wave + sex +(wave | id), 
                       na.action = na.omit,
                       REML = TRUE,
                       data = data)

dlpfc_model <- lme4::lmer(dlpfc ~  wave +(wave | id), 
                              na.action = na.omit,
                              REML = TRUE,
                              data = data)

dlpfc_model_tx <- lme4::lmer(dlpfc ~  wave + factor(sex) + tx +(wave | id), 
                          na.action = na.omit,
                          REML = TRUE,
                          data = data)

summary(dlpfc_model)

summary(dlpfc_model_sex)

summary(dlpfc_model_tx)

anova(dlpfc_model_sex, dlpfc_model)

anova(dlpfc_model_sex, dlpfc_model_tx)

summary(lmerTest::lmer(dlpfc ~  wave + sex + (wave | id), 
               na.action = na.omit,
               REML = TRUE,
               data = data))

dlpfc_model_sex <- lmerTest::lmer(dlpfc ~  wave + sex  +(wave | id), 
                              na.action = na.omit,
                              REML = TRUE,
                              data = data)

summary(dlpfc_model_sex)


data1 <- data %>% 
  filter(wave == 0 & dlpfc > 2) %>% 
  mutate(id = as.numeric(id)) %>% 
  pull(id)

data2 <- data %>% 
  filter(id %in% data1) %>%
  mutate(wave = as.factor(wave)) %>% 
  ggplot(aes(wave, dlpfc))+
  geom_boxplot()+
  geom_pwc()

full <- full_data %>% 
  filter(wave == 0)

mean(full$dlpfc>2)

data2 <- data %>% 
  filter(id %in% data1) %>% 
  group_by(wave) %>% 
  get_summary_stats(dlpfc)

get_summary_stats(data2)

data2 <- data %>% 
  filter(wave == 0) %>% 
  mutate(penalty = ifelse(dlpfc >= 1.5, 1, 0))

data <- full_join(data, data2) %>% 
  mutate(penalty = ifelse(is.na(penalty), 0, penalty))




dlpfc_model_sex <- lmerTest::lmer(dlpfc ~  wave + sex + penalty +(wave | id), 
                                  na.action = na.omit,
                                  REML = TRUE,
                                  data = data1)

summary(dlpfc_model_sex)

check_model(dlpfc_model_sex, base_size = 8, size_title = 8)
performance(dlpfc_model_sex)


ids <- data %>% 
  filter(wave == 0 & dlpfc >= 1.5) %>% 
  mutate(id = as.numeric(id)) %>% 
  pull(id)

data1 <- data %>% 
  mutate(penalty = ifelse(id %in% ids, 1, 0))

