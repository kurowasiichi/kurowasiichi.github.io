library(tidyverse)

# Function fire
fire <- function(bullet, reuse_rate){
  
  current <- bullet
  counter <- 0
  
  while(current > 0){
    # Reuse Fire
    if(runif(1) < reuse_rate){
      counter <- counter + 1
    }
    # Normal fire
    else {
      current <- current - 1
      counter <- counter + 1
    }
  }
  return(counter)
}

# Count time usage per reload
time_per_reload <- function(shot, t_shot, t_reload){
  return(shot * t_shot + t_reload)
}

# Damage per minute
dpm <- function(base_dmg, shot, tpr){
  return(60/tpr * shot * base_dmg)
}

# Create data
df <- tibble(bullet = numeric(), no = numeric())
for(i in 1:10){
  df <- add_row(df, bullet = i, no = 1:10000)
}

# Simulate fire
result <- df %>% rowwise() %>%  mutate(
  shot_17 = fire(bullet, 0.17), 
  shot_20 = fire(bullet, 0.20), 
  shot_25 = fire(bullet, 0.25),
  shot_30 = fire(bullet, 0.30))

# Summary
summary <- result %>% ungroup() %>% group_by(bullet) %>% 
  summarise(
    mean_17 = mean(shot_17), 
    mean_20 = mean(shot_20),
    mean_25 = mean(shot_25),
    mean_30 = mean(shot_30)) %>% 
  mutate(
    trp_17 = time_per_reload(mean_17, 0.8, 2.33),
    trp_20 = time_per_reload(mean_20, 0.8, 2.33),
    trp_25 = time_per_reload(mean_25, 0.8, 2.33),
    trp_30 = time_per_reload(mean_30, 0.8, 2.33),
    trp = time_per_reload(bullet, 0.8, 2.33)) %>% 
  mutate(
    dpm_17 = dpm(100, mean_17, trp_17),
    dpm_20 = dpm(100, mean_20, trp_20),
    dpm_25 = dpm(100, mean_25, trp_25),
    dpm_30 = dpm(100, mean_30, trp_30),
    dpm = dpm(100, bullet, trp)) %>% 
  mutate(
    increase_rate_0_17 = dpm_17/dpm, 
    increase_rate_0_20 = dpm_20/dpm, 
    increase_rate_0_25 = dpm_25/dpm, 
    increase_rate_0_30 = dpm_30/dpm, 
    increase_rate_17_25 = dpm_25/dpm_17,
    increase_rate_20_30 = dpm_30/dpm_20)

write.csv(summary, file = "summary")