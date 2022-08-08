#seed, packages
set.seed(123)
library(tidyverse)
library(caret)
library(ROSE)
library(doParallel)
library(doSNOW)
library(roll)
library(DataExplorer)

#load data
master<- read.csv("C:\\Users\\624386\\BOOZ ALLEN HAMILTON\\Summer Games 2022 - Documents\\Raw Data\\master.csv")

##transformations
master <- master %>% 
  mutate(
    #create 48 hr rolling averages for telemetry
    volt_rmean_48 = roll_mean(volt, width = 48, min_obs = 1),
    rotate_rmean_48 = roll_mean(rotate, width = 48, min_obs = 1),
    pressure_rmean_48 = roll_mean(pressure, width = 48, min_obs = 1),
    vibe_rmean_48 = roll_mean(vibration, width = 48, min_obs = 1),
    #create 48 hr rolling sum for errors
    error1_rsum_168 = roll_sum(error1, width = 168, min_obs = 1),
    error2_rsum_168 = roll_sum(error2, width = 168, min_obs = 1),
    error3_rsum_168 = roll_sum(error3, width = 168, min_obs = 1),
    error4_rsum_168 = roll_sum(error4, width = 168, min_obs = 1),
    error5_rsum_168 = roll_sum(error5, width = 168, min_obs = 1),
    #create response variable of failures (0,1,2,3,4)
    fcomp_all = ifelse(fcomp1==1,1,
                       ifelse(fcomp2==1,2,
                              ifelse(fcomp3==1,3,
                                     ifelse(fcomp4==1,4,0)))))


#fill out NA values and convert response variable to factor
master$fcomp3 <- as.factor(master$fcomp3)


#remove all useless,EDA only, or ancillary data for model building on comp3   
master_model <- subset(master,select= c(fcomp3,comp3_age,
                                        volt_rmean_48,rotate_rmean_48,pressure_rmean_48,vibe_rmean_48,
                                        error1_rsum_168,error2_rsum_168,error3_rsum_168,error4_rsum_168,error5_rsum_168))

#split dataset into train/test dfs
train_index <-  createDataPartition(master_model$fcomp3, p=.25, list=FALSE,times=1)
ppmx_train <- master_model[ train_index,]
ppmx_test <- master_model[-train_index,]

write.csv(master_model,"final_model_data.csv", row.names = FALSE)
write.csv(ppmx_train,"final_train_data.csv", row.names = FALSE)
write.csv(ppmx_test,"final_test_data.csv", row.names = FALSE)

