##### Baseball Pitch Swing Analysis ############################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# load data challenge csv file
full_table <- read.csv("statcast_pitch_swing_data_20240402_20241030_with_arm_angle2.csv")

############## Defining consistent batters #####################################

# define IQR for bat speed and swing length, filter down to bottom 50% for both swing length and bat speed IQR
consistent_batters <- full_table %>%
  arrange(game_date, home_team, at_bat_number, pitch_number) %>%
  filter(!is.na(bat_speed)) %>%
  group_by(player_name) %>%
  summarise(bat_speed_IQR = IQR(bat_speed), bat_speed_25_percentile = quantile(bat_speed, probs = (.25)),
            bat_speed_75_percentile = quantile(bat_speed, probs = (.75)),
            swing_length_IQR= IQR(swing_length), swing_length_25_percentile = quantile(swing_length, probs = (.25)),
            swing_length_75_percentile = quantile(swing_length, probs = (.75))) %>% ungroup() %>%
  mutate(bat_speed_IQR_median = quantile(bat_speed_IQR, probs = (.50)),
         swing_length_IQR_median = quantile(swing_length_IQR, probs = (.50))) %>%
  filter(bat_speed_IQR < bat_speed_IQR_median & swing_length_IQR <swing_length_IQR_median & bat_speed_IQR != 0) %>%
  select(-bat_speed_IQR_median, -swing_length_IQR_median)
  
plot(consistent_batters$bat_speed_IQR, consistent_batters$swing_length_IQR)

####### Join tables together for views to do analysis on #######################

# taking only true outliers (3415 outliers for outlier of one of two, 3300 for just bat speed, 900 for just swing length)
consistent_batters_outlier_swings <- full_table %>%
  inner_join(consistent_batters, by = 'player_name') %>%
  filter(!is.na(bat_speed) & !is.na(swing_length)) %>% # 52000 pitches with swing data for these players
  filter((bat_speed < bat_speed_25_percentile - 1.5*bat_speed_IQR | bat_speed > bat_speed_75_percentile + 1.5*bat_speed_IQR) | (swing_length < swing_length_25_percentile - 1.5*swing_length_IQR | swing_length > swing_length_75_percentile + 1.5*swing_length_IQR))

consistent_batters_all_swings <- full_table %>%
  inner_join(consistent_batters, by = 'player_name') %>%
  filter(!is.na(bat_speed) & !is.na(swing_length)) %>%
  mutate(outlier = case_when((bat_speed < bat_speed_25_percentile - 1.5*bat_speed_IQR | bat_speed > bat_speed_75_percentile + 1.5*bat_speed_IQR) | (swing_length < swing_length_25_percentile - 1.5*swing_length_IQR | swing_length > swing_length_75_percentile + 1.5*swing_length_IQR) ~ 1,
                             TRUE ~ 0)) %>%
  mutate(outlier_bat_speed = case_when(bat_speed< bat_speed_25_percentile - 1.5*bat_speed_IQR ~ "Low",
                                       bat_speed> bat_speed_75_percentile + 1.5*bat_speed_IQR ~ "High"),
         outlier_swing_length = case_when(swing_length < swing_length_25_percentile - 1.5*swing_length_IQR ~ "Low",
                                          swing_length > swing_length_75_percentile + 1.5*swing_length_IQR ~ "High"))

count_high_low_outliers <- consistent_batters_all_swings %>%
  group_by(outlier_bat_speed, outlier_swing_length) %>% summarise(n())

# plot shows the cluster of what we assume are bunts
plot(consistent_batters_all_swings$bat_speed, consistent_batters_all_swings$swing_length)

######## Analysis of Outlier Swings ############################################

# ball strike count chi square test and graph by strikes
pitches_by_balls_strikes <- consistent_batters_outlier_swings %>%
  group_by(balls, strikes) %>%
  summarise(outlier_total = n(), .groups = 'drop') %>%
  mutate(outlier_percent = outlier_total/sum(outlier_total)) %>%
  full_join(consistent_batters_all_swings %>% group_by(balls, strikes) %>% summarise(overall_total = n(), .groups = 'drop') %>%
                mutate(overall_percent = overall_total/sum(overall_total)),
              by = c('balls', 'strikes')) %>%
    mutate(outlier_total = case_when(is.na(outlier_total) ~ 0, TRUE ~ outlier_total))
test_balls_strikes <- chisq.test(pitches_by_balls_strikes$outlier_total, p = pitches_by_balls_strikes$overall_percent)

balls_strikes_diff <- pitches_by_balls_strikes %>%
  mutate(difference = test_balls_strikes$observed - test_balls_strikes$expected) %>% # Difference (Observed - Expected)
  group_by(strikes) %>% summarise(difference = sum(difference))
ggplot(balls_strikes_diff, aes(x = strikes, y = difference)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  labs(title = "Difference Between Obs and Exp Outlier Swings", 
       x = "Strikes", 
       y = "Difference (Observed - Expected)")

# pitch number chi square test
pitch_number <- consistent_batters_outlier_swings %>%
  group_by(pitch_number) %>% summarise(outlier_total = n()) %>% 
  mutate(outlier_percent = outlier_total/sum(outlier_total)) %>%
  full_join(consistent_batters_all_swings %>% group_by(pitch_number) %>% summarise(overall_total = n()) %>%
              mutate(overall_percent = overall_total/sum(overall_total)),
            by = 'pitch_number') %>%
  mutate(outlier_total = case_when(is.na(outlier_total) ~ 0, TRUE ~ outlier_total))
test_pitch_number <- chisq.test(pitch_number$outlier_total, p = pitch_number$overall_percent)


# inning chi square test and graph by inning
innings <- consistent_batters_outlier_swings %>%
  group_by(inning) %>% summarise(outlier_swings_per_inning = n()) %>%
  mutate(outlier_percent = outlier_swings_per_inning / sum(outlier_swings_per_inning)) %>%
  full_join(consistent_batters_all_swings %>% 
              group_by(inning) %>% summarise(overall_swings_per_inning = n()) %>%
              mutate(overall_swings_percent = overall_swings_per_inning / sum(overall_swings_per_inning)),
            by = c('inning'))
innings$consistent_swings_per_inning <- innings$overall_swings_per_inning - innings$outlier_swings_per_inning
innings$consistent_swings_percent <- innings$consistent_swings_per_inning / sum(innings$consistent_swings_per_inning)
innings_filtered <- innings[innings$inning <= 9, ]
test_innings <- chisq.test(innings_filtered$outlier_swings_per_inning, p = innings_filtered$overall_swings_percent, rescale.p = TRUE)

innings_diff <- data.frame(
  inning = 1:9,  # Inning numbers (1-9)
  obs = test_innings$observed,  # Outlier swings (first column of observed counts)
  exp = test_innings$expected,  # Expected outlier swings (first column of expected counts)
  difference = test_innings$observed - test_innings$expected) # Difference (Observed - Expected)
ggplot(innings_diff, aes(x = factor(inning), y = difference)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  labs(title = "Difference Between Obs and Exp Outlier Swings", 
        x = "Inning", 
        y = "Difference (Observed - Expected)")

# number of outs chi sqaure test
outs <- consistent_batters_outlier_swings %>%
  group_by(outs_when_up) %>% summarise(outs_outlier_swings = n()) %>%
  mutate(outlier_percent = outs_outlier_swings / sum(outs_outlier_swings)) %>%
  full_join(consistent_batters_all_swings %>% 
              group_by(outs_when_up) %>% summarise(outs_overall_swings = n()) %>%
              mutate(overall_swings_percent = outs_overall_swings / sum(outs_overall_swings)))
test_outs <- chisq.test(outs$outs_outlier_swings, p=outs$overall_swings_percent)

# Score Difference chi square test
score_diff <- consistent_batters_outlier_swings %>%
  group_by(bat_score_diff) %>% summarise(outlier_score_diff = n()) %>%
  mutate(outlier_percent = outlier_score_diff / sum(outlier_score_diff)) %>%
  full_join(consistent_batters_all_swings %>% 
              group_by(bat_score_diff) %>% summarise(overall_score_diff = n()) %>%
              mutate(overall_scorediff_percent = overall_score_diff / sum(overall_score_diff)))
scorediff_filtered <- score_diff %>%
  filter(bat_score_diff >= -9 & bat_score_diff <= 9)
test_score_diff <- chisq.test(scorediff_filtered$outlier_score_diff, p=scorediff_filtered$overall_scorediff_percent, rescale.p = TRUE)

# Base States chi square test
df <- data.frame(consistent_batters_outlier_swings$on_1b,
                  consistent_batters_outlier_swings$on_2b,
                  consistent_batters_outlier_swings$on_3b)
combinations_table <- data.frame(Base_Combination = apply(df, 1, function(row) {
    base_combination <- ifelse(is.na(row), 0, 1)
    paste(base_combination, collapse = "-")
    }))
df2 <- data.frame(consistent_batters_all_swings$on_1b,
                  consistent_batters_all_swings$on_2b,
                  consistent_batters_all_swings$on_3b)
combinations_table2 <- data.frame(Base_Combination = apply(df2, 1, function(row) {
    base_combination <- ifelse(is.na(row), 0, 1)
    paste(base_combination, collapse = "-")
    }))
combination_counts <- table(combinations_table$Base_Combination)
combination_counts2 <- table(combinations_table2$Base_Combination)
combination_counts_df <- as.data.frame(combination_counts)
combination_counts_df2 <- as.data.frame(combination_counts2)
colnames(combination_counts_df) <- c("Base_Combination", "Count")
colnames(combination_counts_df2) <- c("Base_Combination", "Count")
base_states <- left_join(combination_counts_df, combination_counts_df2, by="Base_Combination")
colnames(base_states) <- c("Base_Combination", "Outlier_count", "Overall_count")
test_base_states <- chisq.test(base_states$Outlier_count, base_states$Overall_count)

# pitch name/type analysis and graph
pitch_name <- consistent_batters_outlier_swings %>%
  group_by(pitch_name) %>% summarise(outlier_pitch = n()) %>%
  mutate(outlier_percent = outlier_pitch / sum(outlier_pitch)) %>%
  full_join(consistent_batters_all_swings %>% 
              group_by(pitch_name) %>% summarise(overall_pitch = n()) %>%
              mutate(overall_pitch_percent = overall_pitch / sum(overall_pitch)))
pitch_name <- pitch_name[order(pitch_name$outlier_pitch),]
pitch_name_filtered <- pitch_name %>%
  filter(outlier_pitch >= 5)
test_pitch_name <- chisq.test(pitch_name_filtered$outlier_pitch, p= pitch_name_filtered$overall_pitch_percent, rescale.p = TRUE)

pitch_diff <- data.frame(
  pitch_name = pitch_name_filtered$pitch_name,
  obs = test_pitch_name$observed,  
  exp = test_pitch_name$expected,  
  difference = test_pitch_name$observed - test_pitch_name$expected )
ggplot(pitch_diff, aes(x = factor(pitch_name), y = difference)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Difference Between Obs and Exp Outlier Swings by Pitch", 
       x = "Pitch", 
       y = "Difference (Observed - Expected)")

# Swing result binomial tests
swing_result <- consistent_batters_outlier_swings %>%
  filter(description %in% c("swinging_strike","foul", "hit_into_play")) %>%
  group_by(description) %>% summarise(outlier_descripton = n()) %>%
  mutate(outlier_percent = outlier_descripton / sum(outlier_descripton)) %>%
  full_join(consistent_batters_all_swings %>% 
              filter(description %in% c("swinging_strike","foul", "hit_into_play")) %>%
              group_by(description) %>% summarise(overall_description = n()) %>%
              mutate(overall_percent = overall_description / sum(overall_description))) %>%
  mutate(overall_percent = as.numeric(overall_percent))
test_swinging_strike <- binom.test(1337, 2971, .235, alternative = "greater")
test_foul <- binom.test(1011, 2971, .399, alternative = 'less')
test_hit_into_play <- binom.test(623, 2971, .366, alternative = 'less')

####### End of Code ############################################################