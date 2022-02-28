library(readr)
library(tidyverse)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# LOAD, PRE-PROCESS DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# load data
exp_df <- read_csv("NR_Diss_EXP1.csv")

# create additional columns, remove columns
exp_pp <- exp_df %>%
  filter(!is.na(PARTICIPANT)) %>%
  mutate(is_practice = ifelse(practice == "yes", TRUE, FALSE)) %>%
  mutate(TRIAL = count_trial_sequence) %>%
  mutate(DT_ = anytime::anytime(datetime)) %>%
  mutate(DT_DOW = lubridate::wday(DT_),
         DT_DOY = lubridate::yday(DT_),
         DT_HOUR = lubridate::hour(DT_)) %>%
  select(-condition, -count_trial_sequence, -subject_nr, -response_time_breakscreen, -response_time, -practice) %>%
  select(datetime, contains("DT_"), PARTICIPANT, BLOCK, TRIAL, is_practice, correct, RT, everything())

exp_pp_ = exp_pp %>%
  mutate(EXP_BLOCK_RANDOM = ifelse(isRandom == "blocked", FALSE, TRUE))

# load above from URL
# exp_pp <- read_csv(...) # from url

# metadata
metadata = exp_pp_  %>%
  select(datetime, contains("DT_"), PARTICIPANT, contains("EXP_")) %>%
  distinct()

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# QUICK SUMMARIES ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# overall means and record count by participant
part_means = exp_pp_ %>%
  group_by(PARTICIPANT, datetime) %>%
  summarise(mean_rt = mean(RT, na.rm=T),
            sd_rt = sd(RT, na.rm=T),
            q10_rt = quantile(RT, probs=0.1, na.rm=T),
            q90_rt = quantile(RT, probs=0.9, na.rm=T),
            n_correct = sum(correct),
            n = n()) %>%
  inner_join(metadata)

# overall means by block
part_means_byblock = exp_pp_ %>%
  group_by(PARTICIPANT, datetime, BLOCK, is_practice) %>%
  summarise(mean_rt = mean(RT, na.rm=T),
            sd_rt = sd(RT, na.rm=T),
            q10_rt = quantile(RT, probs=0.1, na.rm=T),
            q90_rt = quantile(RT, probs=0.9, na.rm=T),
            n_correct = sum(correct),
            n = n()) %>%
  inner_join(metadata)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# QUICK VIZ ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ggplot(part_means_byblock %>% filter(!is_practice), aes(BLOCK, mean_rt, group=BLOCK, color=EXP_BLOCK_RANDOM)) + 
  geom_boxplot() +
  facet_grid(. ~ EXP_BLOCK_RANDOM) +
  theme_bw() +
  labs(x= "Experiment Block", y = "Mean Response Time (ms)")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PREDICTIVE MODELING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(equatiomatic)
fit = lm(mean_rt ~ BLOCK, data=part_means_byblock)
summary(fit)
extract_eq(fit, use_coefs=TRUE)
