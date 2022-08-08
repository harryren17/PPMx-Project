# File: Age by Hour
# Jacob Blatt

library(datasets)  # Load base packages manually

if (!require("pacman")) install.packages("pacman")


pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, 
               lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)


errors <- import("C:\\Users\\624383\\Desktop\\PdM_errors.csv")
machines <- import("C:\\Users\\624383\\Desktop\\PdM_machines.csv")
telemetry <- import("C:\\Users\\624383\\Desktop\\PdM_telemetry.csv")
maint <- import("C:\\Users\\624383\\Desktop\\PdM_maint.csv")
failures <- import("C:\\Users\\624383\\Desktop\\PdM_failures.csv")

fails <- failures %>% select(datetime, machineID, failure) %>%
  mutate(comp1F = as.integer(failure == "comp1"),
         comp2F = as.integer(failure == "comp2"),
         comp3F = as.integer(failure == "comp3"),
         comp4F = as.integer(failure == "comp4"))

age_test <- merge(telemetry, machines, by = 'machineID')
age_test <- merge(age_test, fails, by = c('machineID','datetime'), all.x = TRUE)

age_test <- age_test %>% group_by(machineID, datetime) %>% summarise_each(funs(max))

age_test[order(c(age_test$machineID, age_test$datetime)),],

age_test <- select(age_test, -(failure))
age_test[is.na(age_test)] <- 0

age_02 <- age_test %>% 
  # groups by machine ID so that it resets after each machine instead of counting through
  group_by(machineID) %>% 
  # Creates cumsums of the fails so that we can group by how many fails there have been
  mutate(dummy_count = 1,
         comp1_fail_count = cumsum(comp1F),
         comp2_fail_count = cumsum(comp2F),
         comp3_fail_count = cumsum(comp3F),
         comp4_fail_count = cumsum(comp4F),
  ) %>% 
  ungroup() %>% 
  # increments counts between fails by dummy_count magic
  group_by(machineID, comp1_fail_count) %>% 
  mutate(comp1_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(machineID, comp2_fail_count) %>% 
  mutate(comp2_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(machineID, comp3_fail_count) %>% 
  mutate(comp3_age=cumsum(dummy_count)-1) %>% 
  ungroup() %>% 
  group_by(machineID, comp4_fail_count) %>% 
  mutate(comp4_age=cumsum(dummy_count)-1) %>% 
  ungroup()

# removes the extra rows and leaves us with the age
age_02 <- select(age_02, -(comp1_fail_count))
age_02 <- select(age_02, -(comp2_fail_count))
age_02 <- select(age_02, -(comp3_fail_count))
age_02 <- select(age_02, -(comp4_fail_count))
age_02 <- select(age_02, -(dummy_count))
