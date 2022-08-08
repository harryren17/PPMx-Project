library(tidyverse)

# Example with one component
start_time <- Sys.time()

df <- tibble(fail=rep(c(F, F, F, F, F, T, F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,T,F,F),38462))

df_02 <- df %>% 
  mutate(fail_count=cumsum(fail))

df_03 <- df_02 %>% 
  mutate(dummy_count=1)

df_04 <- df_03 %>% 
  group_by(fail_count) %>% 
  mutate(age=cumsum(dummy_count)-1) %>% 
  ungroup()

end_time <- Sys.time()

end_time-start_time


# Example with multiple components
df_05 <- df %>% 
  rename(comp1_fail=fail) %>% 
  mutate(
    comp2_fail=comp1_fail,
    comp3_fail=comp1_fail,
    comp4_fail=comp1_fail
  )

start_time <- Sys.time()

df_06 <- df_05 %>% 
  mutate(
    dummy_count=1,
    comp1_fail_count = cumsum(comp1_fail),
    comp2_fail_count = cumsum(comp2_fail),
    comp3_fail_count = cumsum(comp3_fail),
    comp4_fail_count = cumsum(comp4_fail)
  ) %>% 
  group_by(comp1_fail_count) %>% 
  mutate(comp1_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(comp2_fail_count) %>% 
  mutate(comp2_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(comp3_fail_count) %>% 
  mutate(comp3_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(comp4_fail_count) %>% 
  mutate(comp4_age=cumsum(dummy_count)-1) %>% 
  ungroup()

end_time <- Sys.time()

end_time-start_time
